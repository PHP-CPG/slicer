package de.tubs.cs.ias.cpg.slicing.algorithms.util

import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, Method}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

/**
  * handy implicits, stolen from my master's project
  *
  * @author Malte Wessels
  */
object implicits {
  implicit class OneableIterableOnce[T](l: IterableOnce[T]) {
    def one: T = {
      l.knownSize match {
        case -1 =>
          val iterator = l.iterator
          if (iterator.hasNext) {
            val tmp = iterator.next()
            if (iterator.hasNext) {
              throw new RuntimeException(
                "expected one in IterableOnce, got more")
            } else {
              tmp
            }
          } else {
            throw new RuntimeException("expected one in IterableOnce, got 0")
          }
        case x =>
          if (x == 1) {
            l.iterator.next()
          } else {
            throw new RuntimeException(
              "expected one in IterableOnce, got " + x + " (knownSize)")
          }
      }
    }
  }

  /**
    * Some internal functions are not detected as such, because we run the CPG conversion without extensions
    */
  val ADDITIONAL_INTERNAL_FUNCTIONS: List[String] =
    List("curl_init", "curl_setopt")

  implicit class JumpToMethod(c: Call) {

    /**
      * custom version of Call.methodFullName.
      *
      * Returns the full name of the called method, **BUT** only returns the last part of a namespace chain,
      * if it is equal to an additional internal sink like curl_setopt
      *
      * @return
      */
    def methodFullNameFixed: String = {
      val name = c.out(EdgeTypes.CALL).collectAll[Method].one.fullName
      val ret =
        if (name.contains("\\") && ADDITIONAL_INTERNAL_FUNCTIONS.contains(
              name.split("\\\\").last)) {
          name.split("\\\\").last
        } else {
          // the converter doesn't set methodFullName yet :/
          // assert(name == c.methodFullName, s"$name != ${c.methodFullName}")
          name
        }
      ret
    }
  }

  implicit class LineInBCDump(c: CfgNode) {
    def lineAstParent: Call = {
      val astp = c.astParent.collectAll[Call].one
      astp.astParent match {
        case a: Call if a.name == "=" => a
        case _                        => astp
      }
    }
  }
}
