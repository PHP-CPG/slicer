package de.tubs.cs.ias.cpg.slicing.representation

import de.halcony.creator.dotfile.{Node => DotNode}
import de.tubs.cs.ias.cpg.dotFileExporter.DotFileCreatorExpansion.NodeDotNode
import de.tubs.cs.ias.cpg.dotFileExporter.conf.ExportConfig
import de.tubs.cs.ias.cpg.slicing.representation.SliceEdgeTypes.SliceEdgeType
import io.shiftleft.codepropertygraph.generated.nodes.Call

/** Wrapper class for a node within a program slice
  *
  * @author Simon Koch
  *
  * @param original the original node
  * @param slice the corresponding program slice this node belongs to
  */
case class SliceNode(original: Call)(implicit slice: ProgramSlice) {

  /** identifier to ensure that we can differentiate between different nodes
    */
  val identifier: Long = original.id()

  def getSlice: ProgramSlice = slice

  override def toString: String =
    s"[${original.label} | ${original.code} | ${original.name}]"

  override def equals(obj: Any): Boolean = {
    obj match {
      case comp: SliceNode => comp.id() == this.id()
      case _               => false
    }
  }

  /** getter for the identifier
    *
    * @return the identifier
    */
  def id(): Long = identifier

  /** getter for the label
    *
    * @return the label
    */
  def label: String = original.label

  /** getter for the code
    *
    * @return the code
    */
  def code: String = original.code

  /** getter for the name
    *
    * @return the name
    */
  def name: String = original.name

  /** get the underlying CPG node
    *
    * throws an exception if there is no such node stored
    *
    * @return the call
    */
  def getCpgCall: Call = original

  /** the nodes coming after this node
    *
    * @return the list of following nodes
    */
  def out(): List[SliceNode] = {
    this.outE(None).map(_.in())
  }

  /** the nodes coming after this node
    *
    * @param label the label for the edge to restrict on
    * @return the list of following nodes
    */
  def out(label: SliceEdgeType): List[SliceNode] = {
    this.outE(label).map(_.in())
  }

  /** the nodes coming before this node
    *
    * @return the list of preceding nodes
    */
  def in(): List[SliceNode] = {
    this.inE(None).map(_.out())
  }

  /** the nodes coming before this node
    *
    * @param label the label for the edge to restrict on
    * @return the list of preceding nodes
    */
  def in(label: SliceEdgeType): List[SliceNode] = {
    this.inE(Some(label)).map(_.out())
  }

  /** the edges coming out of this node
    *
    * @return the edges coming out of this node
    */
  def outE(): List[SliceEdge] = outE(None)

  /** the edges coming out of this node
    *
    * @param label the label to restrict the edges to
    * @return the edges coming out of this node
    */
  def outE(label: SliceEdgeType): List[SliceEdge] = outE(Some(label))

  /** the edges leaving this node
    *
    * @param label the label for the edge to restrict on
    * @return the list of edges leaving this node
    */
  private def outE(label: Option[SliceEdgeType]): List[SliceEdge] = {
    slice
      .outE(this)
      .filter(edge => if (label.nonEmpty) edge.getLabel == label.get else true)

  }

  /** the edges going into this node
    *
    * @return the list of edges leaving this node
    */
  def inE(): List[SliceEdge] = inE(None)

  /** the edges going into this node
    *
    * @param label the label for the edge to restrict on
    * @return the list of edges leaving this node
    */
  def inE(label: SliceEdgeType): List[SliceEdge] = inE(Some(label))

  /** the edges leading into this node
    *
    * @param label the label for the edge to restrict on
    * @return the list of edges leading into this node
    */
  private def inE(label: Option[SliceEdgeType]): List[SliceEdge] = {
    slice
      .inE(this)
      .filter(edge => if (label.nonEmpty) edge.getLabel == label.get else true)
  }

  /** convert this node to dot representation
    *
    * @param conf the export conf to use
    * @return the dot representation
    */
  def toDot(implicit conf: ExportConfig): DotNode = {
    original.toDotFileNode(conf.getNodeType(original.label))
  }
}
