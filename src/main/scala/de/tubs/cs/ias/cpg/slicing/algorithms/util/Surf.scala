package de.tubs.cs.ias.cpg.slicing.algorithms.util

import de.tubs.cs.ias.cpg.slicing.algorithms.util.implicits.OneableIterableOnce
import io.joern.bytecode.Defines
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{
  Call,
  CfgNode,
  Literal,
  Method
}
import io.shiftleft.semanticcpg.language._
import overflowdb.Edge
import overflowdb.traversal.jIteratortoTraversal
import wvlet.log.LogSupport

class InternalFunctionParameterMappingError(message: String)
    extends Exception(message) {
  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }

  def this(cause: Throwable) = {
    this(Option(cause).map(_.toString).orNull, cause)
  }

  def this() = {
    this(null: String)
  }

  override def getMessage: String =
    s"InternalFunctionParameterMappingError: " + this.message
}

/**
  * provides handy traversals and helper functions, stolen from my master's project
  *
  * @author Malte Wessels
  */
object Surf extends LogSupport {
  def ifIsArgGetCaller(call: CfgNode): Option[List[CfgNode]] = {
    if (call.inE(EdgeTypes.ARGUMENT).nonEmpty) {
      return Some(
        call
          .inE(EdgeTypes.ARGUMENT)
          .map(_.outNode)
          .map(_.asInstanceOf[CfgNode])
          .l)
    }
    None
  }

  def ifIsAssignmentGetRhs(call: CfgNode): Option[Call] = {
    if (call.asInstanceOf[Call].name != "=") return None
    Some(call.out(EdgeTypes.ARGUMENT).collectAll[Call].order(1).one)
  }

  def getREACHING_DEF_EdgeFromParameter(a: CfgNode,
                                        paramindex: Int): Option[Edge] = {
    val params = a
      .out(EdgeTypes.ARGUMENT)
      .collectAll[CfgNode]
      .filter(_.order == paramindex)
      .l
    if (params.isEmpty) return None
    val edges = a.inE(EdgeTypes.REACHING_DEF).l
    if (edges.length == 1) return Some(edges.head)
    if (edges.isEmpty) return None
    val param = params.head
    edges.find(_.property("VARIABLE").asInstanceOf[String] match {
      case s"CV($$$x)" => x == param.code
      case x           => x == param.code
    })
  }

  /**
    * Retrieves the edge that satifies these criteria.
    * [a] --[label]-> [b]
    */
  def getEdge(a: CfgNode, b: CfgNode, label: String): Edge =
    a.outE(label).l.filter(_.inNode() == b).one

  /**
    *
    * @param call target
    * @param edge incoming edge carrying a parameter
    * @return index as int, 0-indexed
    */
  def get_argument_index(call: CfgNode, edge: Edge): Int = {
    assert(edge.label == EdgeTypes.REACHING_DEF)
    // now we get the argument index of the incoming flow
    val passed_var_name = edge.property("VARIABLE").asInstanceOf[String]
    debug("passed var name: " + passed_var_name)
    debug(
      s"available : (${call.asInstanceOf[Call].argument.code.l.mkString(",")})")
    val parameter = call
      .asInstanceOf[Call]
      .argument
      .filter(
        x =>
          (passed_var_name.startsWith("V") || passed_var_name
            .startsWith("T")) && x.code == passed_var_name || // match T,V vars
            x.code == s"CV($$$passed_var_name)" // match compiled (CV) php vars
      )
      .one
    parameter.order
  }

  /**
    * Get the parameter index for a named parameter from it's send.
    * Throws if the called function is an internal function.
    *
    * @param send the send bytecode
    * @return the index
    */
  def getParameterIndexFromSend(send: Call): Int =
    send.out(EdgeTypes.ARGUMENT).collectAll[Literal].one.code match {
      case number if number.forall(Character.isDigit) => number.toInt
      case string =>
        val method = send
          .in(EdgeTypes.ARGUMENT)
          .collectAll[Call]
          .one
          .out(EdgeTypes.CALL)
          .collectAll[Method]
          .one
        method.code match {
          case Defines.INTERNAL_FUNCTION =>
            throw new InternalFunctionParameterMappingError(
              "INTERNAL_FUNCTION can't be used for parameter name to position mapping")
          case _ => parameterNameToIndex(string, method)
        }
    }

  def parameterNameToIndex(name: String, method: Method): Int = {
    method.parameter.nameExact(name).order.one + 1
  }

  /**
    * Determine if a cfgNext of a JMP-bytecode is the consecutive bytecode or reached via the triggered JMP
    *
    * @param jmp  a call with multiple outgoing CFG edges, e.g. a JMP or FE_*
    * @param node has to be a direct cfgNext of `jmp`
    * @return true iff the node is reached by triggering the JMP
    */
  def isCfgNodeBehindJump(jmp: Call, node: CfgNode): Boolean = {
    assert(jmp.cfgNext.l.contains(node))
    node.astParent.order > jmp.cfgNext.astParent.order.min
  }
}
