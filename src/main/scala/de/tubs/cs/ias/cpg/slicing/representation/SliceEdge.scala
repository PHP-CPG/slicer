package de.tubs.cs.ias.cpg.slicing.representation

import de.halcony.creator.dotfile.{DirectedEdge, Edge => DotEdge}
import de.tubs.cs.ias.cpg.dotFileExporter.conf.ExportConfig
import de.tubs.cs.ias.cpg.slicing.representation.SliceEdgeTypes._

import scala.annotation.nowarn

/** wrapper class representing a CPG edge in the context of a program slice
  *
  * @author Simon Koch
  *
  * @param label the label of the edge
  * @param attributes the attributes of the edge
  * @param intoWhere the node out of which this edge comes
  * @param fromWhere the node into which this edge goes
  */
case class SliceEdge(order: Int,
                     label: SliceEdgeType,
                     attributes: Map[String, String],
                     fromWhere: SliceNode,
                     intoWhere: SliceNode) {

  override def toString: String = s"$fromWhere -[$label]-> $intoWhere"

  /** equal check soley based on the id
    *
    * @param obj the object to compare equality with
    * @return equality
    */
  override def equals(obj: Any): Boolean = {
    obj match {
      case comp: SliceEdge =>
        comp.in() == this.in() && comp.out() == this
          .out() && comp.getLabel == this.getLabel && comp.attributes == this.attributes
      case _ => false
    }
  }

  def getOrder: Int = order

  /** in what node does the edge go
    *
    * @return the node into which the edge goes
    */
  def in(): SliceNode = intoWhere

  /** out of what node does the edge come
    *
    * @return the node out of which the edge comes
    */
  def out(): SliceNode = fromWhere

  /** getter for the label
    *
    * @return the label
    */
  def getLabel: SliceEdgeType = label

  /** get an attribute of the edge
    *
    * @param name the name of the attribute in question
    * @return the attribute value
    */
  def getAttribute(name: String): Option[String] = attributes.get(name)

  /** convert this edge into a DotEdge for export purposes
    *
    * @param conf the corresponding export config
    * @return the DotEdge
    */
  def toDot(implicit @nowarn conf: ExportConfig): DotEdge = {
    val from: SliceNode = out()
    val to: SliceNode = in()
    DirectedEdge(from.getCpgCall.get.id().toString,
                 to.getCpgCall.get.id().toString,
                 getLabel.toString)
  }

}
