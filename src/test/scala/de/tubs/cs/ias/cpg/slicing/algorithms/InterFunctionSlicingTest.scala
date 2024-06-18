package de.tubs.cs.ias.cpg.slicing.algorithms

import de.tubs.cs.ias.cpg.slicing.ExportAndCpgTestingUtility
import de.tubs.cs.ias.cpg.slicing.algorithms.util.FunctionCallHandling
import de.tubs.cs.ias.cpg.slicing.algorithms.util.FunctionCallHandling.ArgumentPosition
import de.tubs.cs.ias.cpg.slicing.representation.{ProgramSlice, SliceEdge, SliceEdgeTypes, SliceNode}
import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import wvlet.log.{LogLevel, Logger}

class InterFunctionSlicingTest extends AnyWordSpec with Matchers with PHPVersions with ExportAndCpgTestingUtility {


  implicit val version: PHPVersion.Value = PHPVersion.V8

  "give me the send val and its arg number" in new CpgFromCodeTestFixture(
    """$x = 1;
      |call($x);
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    implicit val slice: ProgramSlice = new ProgramSlice(null)
    val call :: send :: Nil = getCallNodesByName(cpg, "call", "SEND.*")
    val scall: SliceNode = SliceNode(call)
    val ssend: SliceNode = SliceNode(send)
    val edge: SliceEdge = SliceEdge(0, SliceEdgeTypes.DATA_DEPENDENCE, Map(), ssend, scall)
    slice.addNode(scall)
    slice.addNode(ssend)
    slice.addEdge(edge)
    InterFunctionSlicing.getSendVals(scall) shouldBe Set((ssend, ArgumentPosition(None, Some(1))))
  }

  "give me the right call target" in new CpgFromCodeTestFixture(
    """function call($var) {
      | echo $var;
      |}
      |call("test");
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    implicit val slice: ProgramSlice = new ProgramSlice(null)
    val call :: Nil = getCallNodesByName(cpg, "call")
    val scall: SliceNode = SliceNode(call)
    val callMethod: Method = getMethod(cpg, "call")
    InterFunctionSlicing.getCalledMethod(scall) shouldBe callMethod
  }

  "give me a single recv node" in new CpgFromCodeTestFixture(
    """function call($var) {
      | echo $var;
      |}
      |""".stripMargin, configFile = Some("cpg.conf")) {
    val callMethod: Method = getMethod(cpg, "call")
    InterFunctionSlicing.getRecv(callMethod).length shouldBe 1
  }

  "add on send recv add" in new CpgFromCodeTestFixture(
    """function call($var) {
      | echo $var;
      |}
      |call("test");
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    implicit val slice: ProgramSlice = new ProgramSlice(null)
    val call :: send :: recv :: Nil = getCallNodesByName(cpg, "call", "SEND.*", "RECV.*")
    val scall: SliceNode = SliceNode(call)
    val ssend: SliceNode = SliceNode(send)
    val srecv: SliceNode = SliceNode(recv)
    slice.addNode(scall)
    slice.addNode(ssend)
    slice.addNode(srecv)
    slice.addEdge(SliceEdge(0, SliceEdgeTypes.DATA_DEPENDENCE, Map(), ssend, scall))
    val sendVals: Set[(SliceNode, ArgumentPosition)] = InterFunctionSlicing.getSendVals(scall)
    sendVals shouldBe Set((ssend, ArgumentPosition(None, Some(1))))
    val callMethod: Method = getMethod(cpg, "call")
    val mainMethod: Method = getMethod(cpg, "dlr_main")
    InterFunctionSlicing.getRecv(callMethod) shouldBe List(recv)
    FunctionCallHandling.getParameterNumber(recv).getNumber shouldBe 1
    InterFunctionSlicing.addSendRecvEdges(slice, sendVals, List(callMethod, mainMethod))
    slice.getEdges(SliceEdgeTypes.SEND_VAR) shouldBe Set(
      SliceEdge(1, SliceEdgeTypes.SEND_VAR, Map("callStack" -> "call,dlr_main"), ssend, srecv)
    )
  }

  "give me a single return" in new CpgFromCodeTestFixture(
    """function call($var) {
      | return $var;
      |}
      |""".stripMargin, configFile = Some("cpg.conf")) {
    val method: Method = getMethod(cpg, "call")
    val ret :: Nil = getCallNodes(cpg, "RETURN CV.*var.*")
    InterFunctionSlicing.getReturns(method) shouldBe List(ret)
  }

  "add one return edge" in new CpgFromCodeTestFixture(
    """function call($var) {
      | return $var;
      |}
      |call("test");
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    implicit val slice: ProgramSlice = new ProgramSlice(null)
    val main: Method = getMethod(cpg, "dlr_main")
    val call: Method = getMethod(cpg, "call")
    val fCall :: Nil = getCallNodesByName(cpg, "call")
    val ret: Call = InterFunctionSlicing.getReturns(call).head
    slice.addNode(fCall)
    InterFunctionSlicing.addCallReturnEdges(slice, SliceNode(fCall), List(call, main))
    slice.getEdges(SliceEdgeTypes.RETURN_VAR) shouldBe Set(
      SliceEdge(0, SliceEdgeTypes.RETURN_VAR, Map("callStack" -> "call,dlr_main"), SliceNode(ret), SliceNode(fCall))
    )
  }

  "add both the send and the return edge" in new CpgFromCodeTestFixture(
    """function call($var) {
      | return $var;
      |}
      |call("test");
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    implicit val slice: ProgramSlice = new ProgramSlice(null)
    val call :: send :: recv :: Nil = getCallNodesByName(cpg, "call", "SEND.*", "RECV.*")
    val scall: SliceNode = SliceNode(call)
    val ssend: SliceNode = SliceNode(send)
    val srecv: SliceNode = SliceNode(recv)
    slice.addNode(scall)
    slice.addNode(ssend)
    slice.addNode(srecv)
    slice.addEdge(SliceEdge(0, SliceEdgeTypes.DATA_DEPENDENCE, Map(), ssend, scall))
    val sendVals: Set[(SliceNode, ArgumentPosition)] = InterFunctionSlicing.getSendVals(scall)
    sendVals shouldBe Set((ssend, ArgumentPosition(None, Some(1))))
    val callMethod: Method = getMethod(cpg, "call")
    val mainMethod: Method = getMethod(cpg, "dlr_main")
    val ret: Call = InterFunctionSlicing.getReturns(callMethod).head
    InterFunctionSlicing.goUpTheCallChain(slice, mainMethod, List(mainMethod))
    slice.getEdges(SliceEdgeTypes.SEND_VAR) shouldBe Set(
      SliceEdge(1, SliceEdgeTypes.SEND_VAR, Map("callStack" -> "call,dlr_main"), ssend, srecv)
    )
    slice.getEdges(SliceEdgeTypes.RETURN_VAR) shouldBe Set(
      SliceEdge(0, SliceEdgeTypes.RETURN_VAR, Map("callStack" -> "call,dlr_main"), SliceNode(ret), scall)
    )
  }

  "give me the correct send location" in new CpgFromCodeTestFixture(
    """function call($var) {
      | return $var;
      |}
      |call("test");
      |""".stripMargin, configFile = Some("cpg.conf")) {
    val callMethod: Method = getMethod(cpg, "call")
    val send :: Nil = getCallNodesByName(cpg, "SEND.*")
    InterFunctionSlicing.getCallingPointSends(callMethod) shouldBe Set((send, ArgumentPosition(None, Some(1))))
  }

  "add a single send edge" in new CpgFromCodeTestFixture(
    """function call($var) {
      | return $var;
      |}
      |call("test");
      |""".stripMargin, configFile = Some("cpg.conf")) {
    implicit val slice: ProgramSlice = new ProgramSlice(null)
    val recv :: send :: Nil = getCallNodes(cpg, "RECV.*", "SEND.*")
    val call: Method = getMethod(cpg, "call")
    slice.addNode(recv)
    slice.getRecvNodes(call.name).size shouldBe 1
    InterFunctionSlicing.getCallingPointSends(call) shouldBe Set((send, ArgumentPosition(None, Some(1))))
    InterFunctionSlicing.goDownTheCallChain(slice, call)
    slice.getEdges shouldBe Set(
      SliceEdge(1, SliceEdgeTypes.SEND_VAR, Map("callStack" -> ""), SliceNode(send), SliceNode(recv))
    )
  }


  "give me the correct argument name for named arguments" in new CpgFromCodeTestFixture(
    """function call($prev,$var) {
      | return $var;
      |}
      |call(var:"test",prev:"test");
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    implicit val slice: ProgramSlice = new ProgramSlice(null)
    val recv1 :: recv2 :: sendvar :: sendprev :: Nil = getCallNodes(cpg, "RECV.*1.*", "RECV.*2.*", "SEND.*var.*", "SEND.*prev.*")
    val call: Method = getMethod(cpg, "call")
    slice.addNode(recv1)
    slice.addNode(recv2)
    slice.getRecvNodes(call.name).size shouldBe 2
    InterFunctionSlicing.getCallingPointSends(call) shouldBe Set(
      (sendvar, ArgumentPosition(Some("var"), None)),
      (sendprev, ArgumentPosition(Some("prev"), None))
    )
    InterFunctionSlicing.goDownTheCallChain(slice, call)
    //slice.showGraph()
    slice.getEdges shouldBe Set(
      SliceEdge(-1, SliceEdgeTypes.SEND_VAR, Map("callStack" -> ""), SliceNode(sendvar), SliceNode(recv2)),
      SliceEdge(-1, SliceEdgeTypes.SEND_VAR, Map("callStack" -> ""), SliceNode(sendprev), SliceNode(recv1))
    )
  }

  "set a send edge even if it is a constant" in new CpgFromCodeTestFixture(
    """function addPrefix($prefix){
      |     echo($prefix."x");
      |}
      |addPrefix("pre");
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    Logger.rootLogger.setLogLevel(LogLevel.DEBUG)
    val send :: echo :: Nil = getCallNodes(cpg,"SEND_.*","ECHO.*")
    implicit val slice : ProgramSlice = ProgramSlice.slice(echo)
    //slice.showGraph()
    val sendSliceNode :: Nil = slice.getNodes(send)
    sendSliceNode.out(SliceEdgeTypes.SEND_VAR).nonEmpty shouldBe true
  }
}
