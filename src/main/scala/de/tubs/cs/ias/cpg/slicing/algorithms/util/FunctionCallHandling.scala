package de.tubs.cs.ias.cpg.slicing.algorithms.util

import io.shiftleft.codepropertygraph.generated.nodes.{
  Call,
  CfgNode,
  Identifier,
  Literal
}
import io.shiftleft.semanticcpg.language._

object FunctionCallHandling {

  case class ArgumentPosition(name: Option[String], position: Option[Int]) {

    def getNumber: Int = position match {
      case Some(value) => value
      case None        => -1
    }

    def getName: String = (name, position) match {
      case (Some(name), _) => name
      case (_, Some(pos))  => pos.toString
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case ArgumentPosition(name, pos) =>
          name == this.name || pos == this.position
        case _ => false
      }
    }

  }

  /*object ArgumentPositionOrdering extends Ordering[ArgumentPosition] {
    override def compare(x: ArgumentPosition, y: ArgumentPosition): Int = {

    }

  }*/

  def getArgumentNumber(send: Call): ArgumentPosition = {
    try {
      val literal = send.name match {
        // based on io.joern.bytecode.util.extensions.NodeExtension.scala
        case "SEND_ARRAY" =>
          send.astChildren.order(0).next().asInstanceOf[Literal]
        case _ =>
          //println(send.code)
          //println(send.astChildren.order(1).next.asInstanceOf[CfgNode].code)
          send.astChildren.order(1).next().asInstanceOf[Literal]
      }
      try {
        // if we can parse an integer then the SEND contains the position
        ArgumentPosition(None, Some(literal.code.toInt))
      } catch {
        case _: NumberFormatException => // if there is a parsing exception for the position
          // then it is a named argument
          ArgumentPosition(Some(literal.code), None)
      }
    } catch {
      case _: NumberFormatException => throw new NotImplementedError(send.code)
    }
  }

  def getParameterNumber(recv: Call): ArgumentPosition = {
    val parent = recv.astParent.asInstanceOf[Call]
    assert(parent.name == "=")
    parent.astChildren.order(0).head match {
      case ident: Identifier =>
        ArgumentPosition(
          Some(ident.name),
          Some(
            recv.astChildren.order(0).next().asInstanceOf[Literal].code.toInt))
      case x =>
        throw new RuntimeException(
          s"expected lhs identifier ${x.label} in ${parent.code}")
    }
  }

}
