package de.tubs.cs.ias.cpg.slicing.algorithms

import de.tubs.cs.ias.cpg.slicing.algorithms.InterFunctionSlicing.{
  goDownTheCallChain,
  goUpTheCallChain
}
import de.tubs.cs.ias.cpg.slicing.representation.{ProgramSlice, SliceNode}
import io.joern.bytecode.util.extensions.NodeExtension.ExtendedAST
import io.shiftleft.codepropertygraph.generated.nodes.Method
import wvlet.log.LogSupport

import scala.language.implicitConversions

object FunctionSlicing extends LogSupport {

  private[slicing] def slice(slice: ProgramSlice,
                             from: SliceNode,
                             stack: List[Method],
                             controlDenpendencyEdges : Boolean = true): ProgramSlice = {
    val currentNodes = slice.getNodes.size
    debug("backwards slicing")
    IntraFunctionSlicing.doBackwardsDDGSlicing(from, slice) // this ensures that we do all
    if(controlDenpendencyEdges) {
      debug("control dependency slicing")
      IntraFunctionSlicing.addCDGSlicing(slice)
      debug("marking CDG edges")
      IntraFunctionSlicing.markCDGEdges(slice)
    }
    // we can always go up the call chain in the current method level
    debug("going up the call chain")
    // we only go up or down the call chain if there were new nodes added
    // otherwise there cannot be a new call we have not already covered
    if (currentNodes < slice.getNodes.size) {
      goUpTheCallChain(slice, from.getCpgCall.getParentMethod.get, stack)
      if (stack.isEmpty) { // but only go down if there is no element on the call stack (we already did the lower levels otherwise)
        debug("going down the call chain")
        goDownTheCallChain(slice, from.getCpgCall.getParentMethod.get)
      } else {
        debug(
          s"not going down the call chain as the stack is not empty : $stack")
      }
    }
    slice
  }

}
