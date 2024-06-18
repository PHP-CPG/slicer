package de.tubs.cs.ias.cpg.slicing.representation

/** Enum to wrap the possible edges within a program slice
  *
  * @author Simon Koch
  *
  */
object SliceEdgeTypes extends Enumeration {
  type SliceEdgeType = Value
  val DATA_DEPENDENCE, CONTROL_DEPENDENCE, SEND_VAR, RETURN_VAR = Value
}
