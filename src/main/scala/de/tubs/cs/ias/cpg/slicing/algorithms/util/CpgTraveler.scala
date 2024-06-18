package de.tubs.cs.ias.cpg.slicing.algorithms.util

import io.joern.bytecode.passes.utility.MethodDetectionAndAssociation.{
  KNOWN_FUNCTION_ENDS,
  KNOWN_FUNCTION_STARTS
}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, nodes}
import io.shiftleft.semanticcpg.language._

import scala.annotation.tailrec
import scala.collection.mutable.{Set => MSet}
import scala.jdk.CollectionConverters.IteratorHasAsScala

/**
  * provide some traversals on the PHP cpg
  */
object CpgTraveler {

  @tailrec
  def getMethod(node: nodes.AstNode): Option[nodes.Method] = {
    node match {
      case result: nodes.Method => Some(result)
      case _ =>
        if (node._astIn.hasNext) {
          getMethod(node._astIn.next().asInstanceOf[nodes.AstNode])
        } else {
          None
        }
    }
  }

  @tailrec
  def getClass(node: nodes.AstNode): Option[nodes.TypeDecl] = {
    node match {
      case result: nodes.TypeDecl => Some(result)
      case _ =>
        if (node._astIn.hasNext) {
          getClass(node._astIn.next().asInstanceOf[nodes.AstNode])
        } else {
          None
        }
    }
  }

  @tailrec
  def getNamespace(node: nodes.AstNode)(
      implicit cpg: Cpg): Option[nodes.NamespaceBlock] = {
    node match {
      case result: nodes.TypeDecl =>
        Some(cpg.namespaceBlock.fullNameExact(result.astParentFullName).head)
      case result: nodes.Method =>
        if (node._astIn.hasNext) {
          getNamespace(node._astIn.next().asInstanceOf[nodes.AstNode])
        } else {
          Some(cpg.namespaceBlock.fullNameExact(result.astParentFullName).head)
        }
      case result: nodes.NamespaceBlock => Some(result)
      case _ =>
        if (node._astIn.hasNext) {
          getNamespace(node._astIn.next().asInstanceOf[nodes.AstNode])
        } else {
          None
        }
    }
  }

  def getFile(node: nodes.AstNode)(implicit cpg: Cpg): nodes.File = {
    getNamespace(node) match {
      case Some(x) =>
        x._astIn.next().head.asInstanceOf[nodes.File]
      case None => ???
    }
  }

  @tailrec
  def getLineNumber(node: nodes.CfgNode): Int = {
    node.lineNumber match {
      // its parent has to
      case Some(x) if x == -1 =>
        getLineNumber(node.astParent.next().asInstanceOf[nodes.CfgNode])
      case None =>
        getLineNumber(node.astParent.next().asInstanceOf[nodes.CfgNode])
      // if the cfg node has no line number
      case Some(x) => x
    }
  }

  def getInit(doCall: Call): Option[Call] = {
    var activeDo = 1
    val visited = MSet[Long]()
    def getActualInit(current: nodes.CfgNode): List[Option[Call]] = {
      //println(s"looking at ${current.code}")
      if (visited.contains(current.id())) {
        List()
      } else {
        visited.addOne(current.id())
        (current match {
          case call: Call if KNOWN_FUNCTION_STARTS.contains(call.name) =>
            activeDo = activeDo - 1
            //println(s"found one ... $activeDo")
            if (activeDo == 0) {
              Some(call)
            } else {
              None
            }
          case call: Call if KNOWN_FUNCTION_ENDS.contains(call.name) =>
            activeDo = activeDo + 1
            None
          case _ =>
            None
        }) match {
          case Some(value) =>
            List(Some(value))
          case None =>
            val ret = current
              .in(EdgeTypes.CFG)
              .asScala
              .map(_.asInstanceOf[CfgNode])
              .flatMap(getActualInit)
              .toList
              .filter(_.nonEmpty)
            ret
        }
      }
    }

    getActualInit(doCall) match {
      case Nil =>
        throw new RuntimeException("we did not find an init ... that is weird")
      case single :: Nil => single
      case _ =>
        throw new RuntimeException(
          "we found multiple init ... that is really weird")
    }
  }
}
