package de.tubs.cs.ias.cpg.slicing.algorithms

import de.tubs.cs.ias.cpg.slicing.algorithms.util.implicits.LineInBCDump
import de.tubs.cs.ias.cpg.slicing.algorithms.util.{
  CpgTraveler,
  FunctionCallHandling
}
import de.tubs.cs.ias.cpg.slicing.representation._
import io.joern.bytecode.passes.utility.MethodDetectionAndAssociation
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{
  Call,
  CfgNode,
  Identifier
}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.{iterableToTraversal, jIteratortoTraversal}
import wvlet.log.LogSupport

import scala.collection.mutable
import scala.collection.mutable.{Set => MSet}
import scala.jdk.CollectionConverters.IteratorHasAsScala

/** Dataflow implementation for a traversal on CfgNodes
  *
  * @author Simon Koch
  *
  */
object IntraFunctionSlicing extends LogSupport {

  /** utility function to handle slicing equal assignments
    *
    * @param current  the current call (assumed to be =)
    * @param slice    the current slice that is being expanded
    * @param frontier the current frontier of the slicing
    * @param known    the already visited nodes
    */
  private def handleEqualAssignment(current: SliceNode, slice: ProgramSlice)(
      implicit frontier: MSet[SliceNode],
      known: MSet[Long]): Unit = {
    val callChildArgumentEdge =
      current.getCpgCall
        .outE(EdgeTypes.ARGUMENT)
        .asScala
        .filter(_.inNode().isInstanceOf[Call])
        .toList
    assert(
      callChildArgumentEdge.length == 1,
      s"we have an assign, there should not be ${callChildArgumentEdge.length} but only one in ${current.code}")
    val edge = callChildArgumentEdge.head
    val argumentUser = SliceNode(edge.outNode().asInstanceOf[Call])(slice)
    val argumentProvider = SliceNode(edge.inNode().asInstanceOf[Call])(slice)
    val newEdge = SliceEdge(0,
                            SliceEdgeTypes.DATA_DEPENDENCE,
                            Map("arg" -> "1"),
                            argumentProvider,
                            argumentUser)
    slice.addEdge(newEdge)
    if (!known.contains(argumentProvider.id())) {
      frontier.addOne(argumentProvider)
    }
  }

  private def handlePossibleObjDependency(current: SliceNode,
                                          slice: ProgramSlice)(
      implicit frontier: MSet[SliceNode],
      known: MSet[Long]): Unit = {
    CpgTraveler.getInit(current.getCpgCall) match {
      case Some(value) if value.name == "INIT_METHOD_CALL" =>
        known.addOne(value.id())
        value.astChildren.order(1).next() match {
          case x: Identifier =>
            value.inE(EdgeTypes.REACHING_DEF).find { edge =>
              edge.property("VARIABLE").asInstanceOf[String] == x.name
            } match {
              case Some(value) =>
                val objNode =
                  SliceNode(value.outNode().asInstanceOf[Call])(slice)
                slice.addEdge(
                  SliceEdge(
                    -1,
                    SliceEdgeTypes.DATA_DEPENDENCE,
                    Map("var" -> x.name, "pos" -> "-1", "obj" -> "true"),
                    objNode,
                    current))
                frontier.addOne(objNode)
              case None =>
                // this can happen if we have incomplete code
                warn(s"no edge for obj var named ${x.name}")
            }
          case weird =>
            throw new RuntimeException(
              s"INIT_METHOD_CALL expects identifier as second ast child but $value got $weird"
            )
        }
      case Some(_) => // we found something but it aint no method call -> pass
      case None    => throw new RuntimeException("retrieving the init went wrong")
    }
  }

  /** utility function to handle function call bytecodes
    *
    * @param current  the current call assumed to be of DO_ name
    * @param slice    the slice that is being expanded
    * @param frontier the current frontier of the slicing
    * @param known    the already visited nodes
    */
  private def handleFunctionCall(current: SliceNode, slice: ProgramSlice)(
      implicit frontier: MSet[SliceNode],
      known: MSet[Long]): Unit = {
    handlePossibleObjDependency(current, slice)
    current.getCpgCall
      .outE(EdgeTypes.ARGUMENT)
      .asScala
      .filter(_.inNode().isInstanceOf[Call])
      .map(edge => (edge, edge.inNode().asInstanceOf[Call]))
      .toList
      //.sortBy { case (_, node) => FunctionCallHandling.getArgumentNumber(node) } //we assert that each argument call has to be a SEND_
      .foreach {
        case (edge, provider) =>
          //println(s"${edge.inNode().asInstanceOf[Call].code} -[${edge.label()}]-> ${edge.outNode().asInstanceOf[Call].code}")
          val sendArgumentNode =
            SliceNode(edge.inNode().asInstanceOf[Call])(slice)
          val doCall = SliceNode(edge.outNode().asInstanceOf[Call])(slice)
          val newEdge =
            SliceEdge(
              FunctionCallHandling.getArgumentNumber(provider).getNumber,
              SliceEdgeTypes.DATA_DEPENDENCE,
              Map(
                "arg" -> FunctionCallHandling
                  .getArgumentNumber(provider)
                  .getName),
              sendArgumentNode,
              doCall
            )
          slice.addEdge(newEdge)
          if (!known.contains(provider.id())) frontier.addOne(sendArgumentNode)
      }
  }

  /** utility function to handle generic reaching edges/i.e., non special bytecodes
    *
    * @param current  the current call assumed not to be = or DO_
    * @param slice    the slice that is being expanded
    * @param frontier the current frontier of the slicing
    * @param known    the already visited nodes
    */
  private def handleGenericReachingEdge(current: SliceNode,
                                        slice: ProgramSlice)(
      implicit frontier: MSet[SliceNode],
      known: MSet[Long]): Unit = {
    val variableChildren: List[String] =
      current.getCpgCall.astChildren
        .sortBy(_.order)
        .filter(_.isInstanceOf[Identifier])
        .map(_.asInstanceOf[Identifier].name)
        .toList
    current.getCpgCall
      .inE(EdgeTypes.REACHING_DEF)
      .asScala
      .toList
      .sortBy { edge =>
        val varName: String = edge.property("VARIABLE").asInstanceOf[String]
        val pos =
          if (Set("INIT_ARRAY", "ADD_ARRAY_ELEMENT").contains(
                current.getCpgCall.name)) {
            0
          } else {
            variableChildren.indexOf(varName) + 1
          }
        assert(
          pos != -1,
          s"there is an edge vor $varName but there is no identifier child in ${current.code}")
        pos
      }
      .foreach { edge =>
        val varName: String = edge.property("VARIABLE").asInstanceOf[String]
        val pos = variableChildren.indexOf(varName) + 1
        val from: SliceNode =
          SliceNode(edge.outNode().asInstanceOf[Call])(slice)
        val args = Map(
          "var" -> varName,
          "pos" -> pos.toString
        )
        val newEdge =
          SliceEdge(pos, SliceEdgeTypes.DATA_DEPENDENCE, args, from, current)
        slice.addEdge(newEdge)
        if (!known.contains(from.id())) frontier.addOne(from)
      }
  }

  private val OP_DATA_DEPENDENT_CALLS: Set[String] = Set("ASSIGN_DIM")

  private def findNextOpData(current: CfgNode,
                             amount: Int,
                             visisted: MSet[Long] = MSet()): List[Call] = {
    if (visisted.add(current.id())) {
      try {
        current match {
          case call: Call if call.name == "OP_DATA" =>
            if (amount == 1) {
              call :: Nil
            } else {
              val nextOps = current
                .out(EdgeTypes.CFG)
                .asScala
                .flatMap(
                  next =>
                    findNextOpData(next.asInstanceOf[CfgNode],
                                   amount - 1,
                                   visisted))
                .toList
              call :: nextOps
            }
          case otherwise =>
            otherwise
              .out(EdgeTypes.CFG)
              .asScala
              .flatMap(next =>
                findNextOpData(next.asInstanceOf[CfgNode], amount, visisted))
              .toList
        }
      } finally {
        visisted.remove(current.id())
      }
    } else {
      Nil
    }
  }

  private def handleOpData(node: SliceNode, slice: ProgramSlice)(
      implicit frontier: MSet[SliceNode],
      known: MSet[Long]): Unit = {
    node.getCpgCall.name match {
      case "ASSIGN_DIM" =>
        findNextOpData(node.getCpgCall, 1).foreach { opData =>
          val opDataSliceNode = SliceNode(opData)(slice)
          frontier.addOne(opDataSliceNode)
          slice.addEdge(
            SliceEdge(-1,
                      SliceEdgeTypes.DATA_DEPENDENCE,
                      attributes = Map("var" -> "<OP_DATA>"),
                      fromWhere = opDataSliceNode,
                      intoWhere = node))
        }
    }
    handleGenericReachingEdge(node, slice)
  }

  /** main routine to perform the backward slicing starting from start
    *
    * @param start the start node
    * @param slice the slice to fill
    * @return the filled slice
    */
  def doBackwardsDDGSlicing(start: SliceNode,
                            slice: ProgramSlice): ProgramSlice = {
    //todo: add optimization where we do not add stuff to the frontier if the node was already known to the slice
    implicit val frontier: MSet[SliceNode] = MSet(start)
    implicit val known: MSet[Long] = MSet()
    while (frontier.nonEmpty) {
      //get an element of the frontier
      val current = frontier.head
      // add it to the now known elements
      known.addOne(current.id())
      // remove it from the frontier
      frontier -= current
      // add it to the slice and only continue if the node has not been known yet
      if (slice.addNode(current)) {
        // and start processing the dataflow
        if (current.name == "=") {
          // if we have a =
          handleEqualAssignment(current, slice)
        } else if (OP_DATA_DEPENDENT_CALLS.contains(current.name)) {
          handleOpData(current, slice)
        } else if (MethodDetectionAndAssociation.KNOWN_FUNCTION_ENDS.contains(
                     current.code)) {
          // or a DO_ call
          handleFunctionCall(current, slice)
        } else { // otherwise we simply can follow all the incoming reaching defs
          handleGenericReachingEdge(current, slice)
        }
      }
    }
    slice
  }

  /** adds control dependency graph edges
    *
    * algorithm by Chalupa et al.
    * https://doi.org/10.1007/978-3-030-81688-9_41
    *
    * @param slice the slice to which to add the edges
    * @return
    */
  def addCDGSlicing(slice: ProgramSlice): ProgramSlice = {
    val NTSCD: NTSCDCache = new NTSCDCache()
    val remaining: MSet[SliceNode] = MSet[SliceNode](slice.getNodes.toList: _*)
    val done: MSet[SliceNode] = MSet()
    while (remaining.nonEmpty) {
      val addedControllers: MSet[SliceNode] = MSet()
      remaining.foreach { node =>
        done.addOne(node)
        NTSCD
          .getControlDependencies(node.getCpgCall)
          .filter(_.isInstanceOf[Call])
          .foreach { controller =>
            //println(s"adding controller ${controller.code}")
            val controllerNode = SliceNode(controller.asInstanceOf[Call])(slice)
            //slice.addNode(controllerNode)
            slice.addEdge(
              SliceEdge(0,
                        SliceEdgeTypes.CONTROL_DEPENDENCE,
                        Map(),
                        controllerNode,
                        node))
            addedControllers.addOne(controllerNode)
          }
      }
      addedControllers.foreach(added =>
        IntraFunctionSlicing.doBackwardsDDGSlicing(added, slice))
      remaining.clear()
      remaining.addAll(slice.getNodes.diff(done))
      //println(remaining)
    }
    slice
  }

  /**
    * update `CONTROL_DEPENDENCE` edges in the slice with the `fallthrough` parameter, that is true if the edge is the "fallthrough" flow and "false" if the edge represent the triggered jump
    *
    * @author Malte Wessels
    */
  def markCDGEdges(slice: ProgramSlice): ProgramSlice = {
    val cdes = slice.getEdges(SliceEdgeTypes.CONTROL_DEPENDENCE)
    for (old_cde <- cdes) {
      val new_edge = if (old_cde.out().code.startsWith("JMP")) {
        old_cde.copy(
          attributes = old_cde.attributes + ("fallthrough" -> getFallthroughString(
            old_cde.out(),
            old_cde.in())))
      } else {
        old_cde
      }
      slice.removeEdge(old_cde)
      slice.addEdge(new_edge)
    }
    slice
  }

  /**
    *
    * @param sjmp   SliceNode representing a jump
    * @param target a node controlled by sjmp
    * @return "true" if this is the fallthrough-case for this JMP, "else" if the jump needs to be executed, "?" if something went wrong
    */
  def getFallthroughString(sjmp: SliceNode, target: SliceNode): String = {
    val jmp: Call = sjmp.getCpgCall
    val nodes = jmp.out(EdgeTypes.CFG).collectAll[CfgNode].l
    var fallthroughNode: Option[CfgNode] = None
    var jmpedNode: Option[CfgNode] = None
    try {
      val vfallthroughNode :: vjmpedNode :: Nil =
        nodes.sortBy(_.lineAstParent.order)
      fallthroughNode = Some(vfallthroughNode)
      jmpedNode = Some(vjmpedNode)
    } catch {
      case _: RuntimeException => return "?"
    }

    val targets = sjmp.out(SliceEdgeTypes.CONTROL_DEPENDENCE)
    assert(targets.contains(target), "passed target isn't controlled by sjmp!")

    findInCfgReverse(target, List(fallthroughNode.get, jmpedNode.get)) match {
      case Some(value) if value == fallthroughNode.get => true.toString
      case Some(value) if value == jmpedNode.get       => false.toString
      case None                                        => "N"
      case _                                           => "?"
    }
  }

  /**
    * Follows the CFG backwards from `start`. Returns the candidate from candidates that is found first. If none is found "None" is returned.
    * Set-driven, i.e. loops shouldn't be a problem.
    *
    * @author Malte Wessels
    */
  def findInCfgReverse(start: SliceNode,
                       candidates: List[CfgNode]): Option[CfgNode] = {
    val visited = mutable.Set[CfgNode]()
    val todo = mutable.Set[CfgNode](start.getCpgCall)
    candidates.find(_ == start) match {
      case Some(value) => Some(value)
      case None =>
        while (todo.nonEmpty) {
          val tmp = todo.toList
          todo.clear()
          for (node <- tmp) {
            val prev = node.in(EdgeTypes.CFG).collectAll[CfgNode].l
            for (p <- prev) {
              if (candidates.contains(p)) {
                return Some(p)
              }
              visited.add(node)
              if (!visited.contains(p)) {
                todo.add(p)
              }
            }
          }
        }
        None
    }
  }

}
