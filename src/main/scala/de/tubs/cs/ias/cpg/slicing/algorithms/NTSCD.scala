package de.tubs.cs.ias.cpg.slicing.algorithms

import io.joern.bytecode.util.extensions.NodeExtension.ExtendedCFG
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, Method}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.EdgeTypes

import scala.collection.mutable.{Map => MMap, Set => MSet, ListBuffer}
import scala.jdk.CollectionConverters.IteratorHasAsScala

class NTSCD(method: Method) {

  private val dependencies: Map[CfgNode, List[CfgNode]] = {
    val cfgNodes: Set[CfgNode] = method.ast
      .filter(_.isInstanceOf[CfgNode])
      .map(_.asInstanceOf[CfgNode])
      .toSet
      .toSet
    val predicates: Set[CfgNode] =
      cfgNodes.filter(_.out(EdgeTypes.CFG).asScala.toList.length > 1)
    val colored: MMap[CfgNode, Boolean] = MMap()
    val counter: MMap[CfgNode, Int] = MMap()
    val dependencies: MMap[CfgNode, ListBuffer[CfgNode]] = MMap()

    def visit(node: CfgNode): Unit = {
      counter(node) = counter(node) - 1
      if (counter(node) == 0 && !colored(node)) {
        colored(node) = true
        node
          .in(EdgeTypes.CFG)
          .asScala
          .map(_.asInstanceOf[CfgNode])
          .foreach(visit)
      }
    }

    def compute(node: CfgNode): Unit = {
      cfgNodes.foreach { node =>
        colored(node) = false
        counter(node) = node.out(EdgeTypes.CFG).asScala.toList.length
      }
      colored(node) = true
      node.in(EdgeTypes.CFG).asScala.map(_.asInstanceOf[CfgNode]).foreach(visit)
    }

    cfgNodes.foreach { cfgNode: CfgNode =>
      compute(cfgNode)
      predicates.foreach { p: CfgNode =>
        val coloredSucc = p
          .out(EdgeTypes.CFG)
          .asScala
          .exists(node => colored(node.asInstanceOf[CfgNode]))
        val uncoloredSucc = p
          .out(EdgeTypes.CFG)
          .asScala
          .exists(node => !colored(node.asInstanceOf[CfgNode]))
        //println(s"we have ${controller.code} -NTSCD-> ${dependent.code} => ${uncoloredSucc && coloredSucc}")
        if (coloredSucc && uncoloredSucc) {
          dependencies.get(cfgNode) match {
            case Some(value) =>
              value.addOne(p)
            case None =>
              dependencies.addOne(cfgNode -> ListBuffer(p))
          }
        }
      }
    }
    dependencies.map { case (key, value) => key -> value.toList }.toMap
  }

  def getControlDependencies(node: CfgNode): List[CfgNode] =
    dependencies.getOrElse(node, List())

}

class NTSCDCache {

  private val cache: MMap[Method, NTSCD] = MMap()

  def getControlDependencies(call: CfgNode): List[CfgNode] = {
    val method = call.getParentMethod.get
    cache.get(method) match {
      case Some(ntscd) => ntscd.getControlDependencies(call)
      case None =>
        cache.addOne(method -> new NTSCD(method))
        cache(method).getControlDependencies(call)
    }
  }

}
