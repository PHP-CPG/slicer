package de.tubs.cs.ias.cpg.slicing.representation

import de.halcony.creator.dotfile.DirectedGraph
import de.tubs.cs.ias.cpg.dotFileExporter.conf.ExportConfig
import de.tubs.cs.ias.cpg.slicing.algorithms.FunctionSlicing
import de.tubs.cs.ias.cpg.slicing.algorithms.util.FunctionCallHandling
import de.tubs.cs.ias.cpg.slicing.representation.SliceEdgeTypes.SliceEdgeType
import io.joern.bytecode.passes.utility.MethodDetectionAndAssociation
import io.joern.bytecode.util.extensions.NodeExtension.ExtendedAST
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language._

import java.io.FileWriter
import java.nio.file.{Files, Path}
import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.sys.process.{ProcessLogger, _}
import scala.util.matching.Regex

class ProgramSlice(start: Call) {

  def getStartNode: Call = start

  private val nodes: MSet[SliceNode] = MSet()
  private val edges: MSet[SliceEdge] = MSet()

  def getNodes: Set[SliceNode] = nodes.toSet

  def getNodes(code: Regex): List[SliceNode] =
    getNodes.filter(node => code.matches(node.code)).toList

  def getNodes(codeExact: String): List[SliceNode] =
    getNodes.filter(_.code == codeExact).toList

  def getNodes(call: Call, calls: Call*): List[SliceNode] = {
    (List(call) ++ calls).map { call =>
      nodes
        .find(_.getCpgCall == call)
        .getOrElse(throw new RuntimeException(
          s"the node ${call.code} | ${call.name} is not part of the slice"))
    }
  }

  def getNode(uuid: Long): SliceNode =
    nodes
      .find(_.id() == uuid)
      .getOrElse(
        throw new RuntimeException(s"the node with id $uuid does not exist"))

  val ALL_SLICE_EDGE_TYPES: Set[SliceEdgeType] = Set(
    SliceEdgeTypes.CONTROL_DEPENDENCE,
    SliceEdgeTypes.DATA_DEPENDENCE,
    SliceEdgeTypes.SEND_VAR,
    SliceEdgeTypes.RETURN_VAR)

  /** gives you the end nodes, i.e., all nodes without outgoing edges
    *
    * @param edgeTypes the list of edge types to which that definition shall hold
    * @return the list of start nodes
    */
  def getEndNodes(
      edgeTypes: Set[SliceEdgeType] = ALL_SLICE_EDGE_TYPES): Set[SliceNode] =
    getNodes.filter(
      node =>
        !edges
          .filter(edge => edgeTypes.contains(edge.label))
          .exists(_.out() == node))

  /** gives you the start nodes, i.e., all nodes without incoming edges
    *
    * @param edgeTypes the list of edge types to which that definition shall hold
    * @return the list of start nodes
    */
  def getStartNodes(
      edgeTypes: Set[SliceEdgeType] = ALL_SLICE_EDGE_TYPES): Set[SliceNode] =
    getNodes.filter(
      node =>
        !edges
          .filter(edge => edgeTypes.contains(edge.label))
          .exists(_.in() == node))

  def getEdges: Set[SliceEdge] = edges.toSet

  def getEdges(label: SliceEdgeType): Set[SliceEdge] =
    getEdges.filter(_.label == label)

  /**
    * @return true if the edge existed before
    */
  def removeEdge(edge: SliceEdge): Boolean = edges.remove(edge)

  private[representation] def outE(node: SliceNode): List[SliceEdge] =
    getEdges.filter(_.out() == node).toList.sortBy(_.getOrder)

  private[representation] def inE(node: SliceNode): List[SliceEdge] =
    getEdges.filter(_.in() == node).toList.sortBy(_.getOrder)

  def getReachabilityMap(
      labels: SliceEdgeType*): Map[(SliceNode, SliceNode), Boolean] =
    getReachabilityMap(labels.toSet)

  def getReachabilityMap: Map[(SliceNode, SliceNode), Boolean] =
    getReachabilityMap(
      Set[SliceEdgeType](
        SliceEdgeTypes.CONTROL_DEPENDENCE,
        SliceEdgeTypes.DATA_DEPENDENCE,
        //SliceEdgeTypes.SEND_VAR, we only want intra functional reachability
        //SliceEdgeTypes.RETURN_VAR
      ))

  /** get all function calls contained in the slice
    *
    * @return the contained function calls
    */
  def getFunctionCalls: Set[SliceNode] = {
    getFunctionCalls(None)
  }

  /** get all fucntion calls belong to method of methodName contained in the slice
    *
    * @param methodName the method name to filter for
    * @return the contained function calls
    */
  def getFunctionCalls(methodName: String): Set[SliceNode] = {
    getFunctionCalls(Some(methodName))
  }

  private def getFunctionCalls(method: Option[String]): Set[SliceNode] = {
    getNodes
      .filter(
        node =>
          if (method.nonEmpty)
            method.get.toLowerCase == node.getCpgCall.getParentMethod.get.name.toLowerCase
          else true)
      .filter(node =>
        MethodDetectionAndAssociation.KNOWN_FUNCTION_ENDS.contains(node.code))
  }

  def getRecvNodes: Set[SliceNode] = getRecvNodes(None)

  def getRecvNodes(method: String): Set[SliceNode] = getRecvNodes(Some(method))

  private def getRecvNodes(current: Option[String]): Set[SliceNode] = {
    getNodes
      .filter(
        node =>
          if (current.nonEmpty)
            node.getCpgCall.getParentMethod.get.name == current.get
          else true)
      .filter(node => "RECV.*".r.matches(node.name))
  }

  private def getReachabilityMap(
      label: Set[SliceEdgeType]): Map[(SliceNode, SliceNode), Boolean] = {
    // floyd warshall reachability calculation using either all edges or only specified
    val reachability: MMap[(SliceNode, SliceNode), Boolean] = MMap()
    getNodes.foreach { from =>
      getNodes.foreach { to =>
        reachability.addOne((from, to) -> (from == to))
      }
    }

    getEdges
      .filter(edge => if (label.nonEmpty) label.contains(edge.label) else true)
      .foreach { edge =>
        reachability.addOne((edge.out(), edge.in()) -> true)
      }

    getNodes.foreach { k =>
      getNodes.foreach { i =>
        getNodes.foreach { j =>
          if (reachability((i, k)) && reachability((k, j))) {
            reachability.addOne((i, j) -> true)
          }
        }
      }
    }
    reachability.toMap
  }

  def addNode(node: Call): Boolean = {
    this.nodes.add(SliceNode(node)(this))
  }

  def addNode(node: SliceNode): Boolean = {
    this.nodes.add(node)
  }

  def addEdge(edge: SliceEdge): Boolean = {
    this.edges.add(edge)
  }

  def toDot(implicit conf: ExportConfig): String = {
    val graph: DirectedGraph = new DirectedGraph("slice")
    getNodes.foreach { node =>
      graph.addNode(node.toDot)
    }
    getEdges.foreach { edge =>
      graph.addEdge(edge.toDot)
    }
    graph.dotString
  }

  def showGraph(dot: String = "dot", view: String = "xdg-open")(
      implicit conf: ExportConfig): Unit = {
    val dotTmp: Path = Files.createTempFile("cpgdot", ".dot")
    val svgTmp: Path = Files.createTempFile("cpgdot", ".svg")
    val svgTmpWriter = new FileWriter(svgTmp.toFile)
    val dotTmpWriter = new FileWriter(dotTmp.toFile)
    try {
      dotTmpWriter.write(toDot)
      dotTmpWriter.flush()
      val output = new StringBuilder()
      s"$dot ${dotTmp.toFile.getAbsolutePath} -Tsvg" ! ProcessLogger(
        out => output.append(out),
        _ => ())
      svgTmpWriter.write(output.toString())
      svgTmpWriter.flush()
      s"$view ${svgTmp.toFile.getAbsolutePath}" ! ProcessLogger(_ => (),
                                                                _ => ())
    } finally {
      dotTmpWriter.close()
      Files.delete(dotTmp)
    }
  }

}

object ProgramSlice {

  def slice(from: Call, noControlDependeny : Boolean = false): ProgramSlice = {
    val slice = new ProgramSlice(from)
    FunctionSlicing.slice(slice, SliceNode(from)(slice), List(), controlDenpendencyEdges = !noControlDependeny)
    //List(from.getParentMethod.get)) this breaks going down the call chain and I do not know
    // why this was ever added?

  }

}
