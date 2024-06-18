package de.tubs.cs.ias.cpg.slicing.algorithms

import de.tubs.cs.ias.cpg.slicing.algorithms.util.FunctionCallHandling
import de.tubs.cs.ias.cpg.slicing.algorithms.util.FunctionCallHandling.ArgumentPosition
import de.tubs.cs.ias.cpg.slicing.representation.{
  ProgramSlice,
  SliceEdge,
  SliceEdgeTypes,
  SliceNode
}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language._
import wvlet.log.LogSupport

import scala.jdk.CollectionConverters.IteratorHasAsScala

/** API to cross call borders while slicing
  *
  * @author Simon Koch
  *
  */
object InterFunctionSlicing extends LogSupport {

  /** get the send_ of a given function call slice node
    *
    * @param functionCall the function call slice node
    * @return the set of send_ slice nodes and their argument number
    */
  private[algorithms] def getSendVals(
      functionCall: SliceNode): Set[(SliceNode, ArgumentPosition)] = {
    functionCall
      .inE(SliceEdgeTypes.DATA_DEPENDENCE)
      .filterNot(edge => edge.attributes.contains("obj"))
      .map { edge =>
        (edge.out(),
         FunctionCallHandling.getArgumentNumber(edge.out().getCpgCall))
      }
      .toSet
  }

  /** get the method that is called at the given function call slice node
    *
    * @param call the call slice node
    * @return the method that is being called here
    */
  private[algorithms] def getCalledMethod(call: SliceNode): Method = {
    val methods = call.getCpgCall
      .out(EdgeTypes.CALL)
      .asScala
      .toList
      .map(_.asInstanceOf[Method])
    assert(
      methods.length == 1,
      "there must be only one method being called - everything else is not (yet) supported")
    methods.head
  }

  /** get all recv nodes of a given method in the cpg
    *
    * @param method the method we want the recv nodes of
    * @return the recv calls of that method
    */
  private[algorithms] def getRecv(method: Method): List[Call] = {
    method.ast
      .filter(node => node.isInstanceOf[Call] && "RECV.*".r.matches(node.code))
      .map(_.asInstanceOf[Call])
      .toList
  }

  /** convert the stack to a string representation
    *
    * @param stack the stack to convert
    * @return the string representation
    */
  private def stackToString(stack: List[Method]): String = {
    stack.map(_.name).mkString(",")
  }

  /** add the SEND_VAR edges between identified SEND_ and RECV_ calls of the method that is top of the stack
    *
    * @param slice the slice to add the edges to
    * @param sendVals the set of (SliceNode,ArgumentNumber) the edges start at
    * @param stack the current stack with the .head being the method we want to draw the edges into
    */
  private[algorithms] def addSendRecvEdges(
      slice: ProgramSlice,
      sendVals: Set[(SliceNode, ArgumentPosition)],
      stack: List[Method]): Unit = {
    getRecv(stack.head).foreach { recv =>
      val paramNumber = FunctionCallHandling.getParameterNumber(recv)
      val recvNode: SliceNode = SliceNode(recv)(slice)
      slice.addNode(recvNode)
      sendVals.foreach {
        case (send, argNumber) =>
          if (paramNumber == argNumber) {
            slice.addEdge(
              SliceEdge(argNumber.getNumber,
                        SliceEdgeTypes.SEND_VAR,
                        Map("callStack" -> stackToString(stack)),
                        send,
                        recvNode))
          }
      }
    }
  }

  /** get all return nodes of a given method
    *
    * @param method the method we want the return nodes of
    * @return the list of return calls
    */
  private[algorithms] def getReturns(method: Method): List[Call] = {
    method.ast
      .filter(node => node.isInstanceOf[Call] && node.code.matches("RETURN.*"))
      .filterNot(node => node.code.matches("RETURN NULL")) // we do ignore the always existing RETURN NULL node
      .map(_.asInstanceOf[Call])
      .toList
  }

  /** add the RETURN_VAR edges between CALL and RETURN edges where the RETURN edges belong to the method on top of the stack
    *
    * @param slice the slice to add the edges to
    * @param call the call from which we come from
    * @param stack the current stack on which the top method is
    * @return the identified slice nodes that are the return nodes on the top of the stack method
    */
  private[algorithms] def addCallReturnEdges(
      slice: ProgramSlice,
      call: SliceNode,
      stack: List[Method]): List[SliceNode] = {
    getReturns(stack.head).map { ret =>
      val returnNode = SliceNode(ret)(slice)
      slice.addEdge(
        SliceEdge(0,
                  SliceEdgeTypes.RETURN_VAR,
                  Map("callStack" -> stackToString(stack)),
                  returnNode,
                  call))
      returnNode
    }
  }

  /** go up the call chain from the method that is on top of the stack and add the corresponding edges
    *
    * @param slice the slice to go up the call chain in
    * @param current the current method
    * @param stack the call stack
    */
  private[algorithms] def goUpTheCallChain(slice: ProgramSlice,
                                           current: Method,
                                           stack: List[Method]): Unit = {
    // get all function calls on our stack level in our slice
    val callReturns: Set[SliceNode] = slice.getFunctionCalls(current.name)
    // go over each of those calls
    callReturns.foreach { call =>
      // and get the corresponding send_ elements
      val sendVals = getSendVals(call)
      // get the called method
      val calledMethod = getCalledMethod(call)
      // create the updated call stack
      val newStack = calledMethod :: stack
      // add the SEND -[SEND_VAR]-> RECV edges
      addSendRecvEdges(slice, sendVals, newStack)
      // add the RETURN -[RETURN_VAR]-> CALL edges
      addCallReturnEdges(slice, call, newStack)
      // and for each of those returning vars
        .foreach {
          // we want to continue slicing
          returnNode =>
            FunctionSlicing.slice(slice, returnNode, newStack)
        }
    }
  }

  /** get the sends of a given function call slice node
    *
    * @param current the current method we want to get the send_ for (i.e., the argument provider)
    * @return the set of send_ nodes and their argument number
    */
  private[algorithms] def getCallingPointSends(
      current: Method): Set[(Call, ArgumentPosition)] = {
    current
      .in(EdgeTypes.CALL)
      .asScala
      .map(_.asInstanceOf[Call])
      .toList
      .flatMap(
        _.out(EdgeTypes.ARGUMENT).asScala.toList.map(_.asInstanceOf[Call]))
      .map(send => (send, FunctionCallHandling.getArgumentNumber(send)))
      .toSet
  }

  /** going down the call chain and adding the corresponding SEND_VAR edges
    *
    * @param slice the slice to which to add the edges to
    * @param current the current method we want to go down from
    */
  private[algorithms] def goDownTheCallChain(slice: ProgramSlice,
                                             current: Method): Unit = {
    // get all recv nodes in our slice of the current method
    val recv = slice.getRecvNodes(current.name).map { node =>
      (node, FunctionCallHandling.getParameterNumber(node.getCpgCall))
    }
    debug(s"we have $recv receive edges")
    // get all sends to the current method
    getCallingPointSends(current)
      .flatMap {
        // for each
        case (send, argNum) =>
          // find the recv counterpart
          recv.find { case (_, paramNum) => argNum == paramNum } match {
            case Some((recv, num)) =>
              // if there is a counter part create a slice ndoe
              val sendNode = SliceNode(send)(slice)
              // add an edge from the counterpart to the recv
              slice.addEdge(
                SliceEdge(num.getNumber,
                          SliceEdgeTypes.SEND_VAR,
                          Map("callStack" -> ""),
                          sendNode,
                          recv))
              // and return the new slice node
              List(sendNode)
            case None =>
              // otherwise do nothing
              List()
          }
      }
      .foreach {
        // for each send counterpart continue the slicing there
        send =>
          FunctionSlicing.slice(slice, send, List())
      }
  }

}
