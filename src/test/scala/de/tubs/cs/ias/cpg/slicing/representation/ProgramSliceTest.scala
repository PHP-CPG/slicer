package de.tubs.cs.ias.cpg.slicing.representation

import de.tubs.cs.ias.cpg.dotFileExporter.DotFileCreatorExpansion.CpgDotFileCreator
import de.tubs.cs.ias.cpg.slicing.ExportAndCpgTestingUtility
import io.joern.bytecode.parser.{PHPVersion, PHPVersions}
import io.joern.bytecode.passes.utility.MethodDetectionAndAssociation
import io.joern.bytecode.util.unittesting.CpgFromCodeTestFixture
import io.shiftleft.codepropertygraph.generated.nodes.Call
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters.IteratorHasAsScala

class ProgramSliceTest extends AnyWordSpec with Matchers with PHPVersions with ExportAndCpgTestingUtility {

  implicit val version: PHPVersion.Value = PHPVersion.V8

  "give the correct in and out for the edge" in new CpgFromCodeTestFixture(
    """echo $x;
      |echo $y;
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    implicit val slice: ProgramSlice = new ProgramSlice(null)
    val calls: List[Call] = cpg.graph.nodes().asScala.filter(_.isInstanceOf[Call]).map(_.asInstanceOf[Call]).toList
    val ex: SliceNode = SliceNode(calls.find(node => "ECHO.*x.*".r.matches(node.code)).get)
    val ey: SliceNode = SliceNode(calls.find(node => "ECHO.*y.*".r.matches(node.code)).get)
    val edge: SliceEdge = SliceEdge(0, SliceEdgeTypes.DATA_DEPENDENCE, Map(), ex, ey)
    slice.addNode(ex)
    slice.addNode(ey)
    slice.addEdge(edge)
    edge.out() shouldBe ex
    edge.in() shouldBe ey
  }
  "give the correct inE and outE for the node" in new CpgFromCodeTestFixture(
    """echo $x;
      |echo $y;
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    implicit val slice: ProgramSlice = new ProgramSlice(null)
    val calls: List[Call] = cpg.graph.nodes().asScala.filter(_.isInstanceOf[Call]).map(_.asInstanceOf[Call]).toList
    val ex: SliceNode = SliceNode(calls.find(node => "ECHO.*x.*".r.matches(node.code)).get)
    val ey: SliceNode = SliceNode(calls.find(node => "ECHO.*y.*".r.matches(node.code)).get)
    val edge: SliceEdge = SliceEdge(0, SliceEdgeTypes.DATA_DEPENDENCE, Map(), ex, ey)
    slice.addNode(ex)
    slice.addNode(ey)
    slice.addEdge(edge)
    ex.outE() shouldBe List(edge)
    ey.inE() shouldBe List(edge)
  }
  "give the correct in and out for the node" in new CpgFromCodeTestFixture(
    """echo $x;
      |echo $y;
      |echo $z;
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    implicit val slice: ProgramSlice = new ProgramSlice(null)
    val calls: List[Call] = cpg.graph.nodes().asScala.filter(_.isInstanceOf[Call]).map(_.asInstanceOf[Call]).toList
    val ex: SliceNode = SliceNode(calls.find(node => "ECHO.*x.*".r.matches(node.code)).get)
    val ey: SliceNode = SliceNode(calls.find(node => "ECHO.*y.*".r.matches(node.code)).get)
    val ez: SliceNode = SliceNode(calls.find(node => "ECHO.*z.*".r.matches(node.code)).get)
    slice.addNode(ex)
    slice.addNode(ey)
    slice.addNode(ez)
    val e1: SliceEdge = SliceEdge(0, SliceEdgeTypes.DATA_DEPENDENCE, Map(), ex, ey)
    val e2: SliceEdge = SliceEdge(0, SliceEdgeTypes.DATA_DEPENDENCE, Map(), ey, ez)
    slice.addEdge(e1)
    slice.addEdge(e2)
    ex.outE() shouldBe List(e1)
    ex.out() shouldBe List(ey)
    ey.out() shouldBe List(ez)
    ez.in() shouldBe List(ey)
    ey.in() shouldBe List(ex)
  }
  "give the correct contained function calls" in new CpgFromCodeTestFixture(
    """function called($param) {
      | echo $param;
      |};
      |function wrongCallee() {
      | $var = "wrong";
      | called($var);
      |};
      |function rightCallee() {
      | $var = "right";
      | called($var);
      | called($var);
      |}
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    implicit val slice: ProgramSlice = new ProgramSlice(null)
    val calls: List[Call] = cpg.graph.nodes().asScala.filter(node => node.isInstanceOf[Call])
      .filter(node => MethodDetectionAndAssociation.KNOWN_FUNCTION_ENDS.contains(node.asInstanceOf[Call].code))
      .map(_.asInstanceOf[Call]).toList
    calls.length shouldBe 3
    calls.foreach {
      call =>
        slice.addNode(SliceNode(call))
        assert(slice.getNodes.nonEmpty)
    }
    slice.getNodes.size shouldBe 3
    slice.getFunctionCalls.size shouldBe 3
    slice.getFunctionCalls("rightCallee").size shouldBe 2
    slice.getFunctionCalls("wrongCallee").size shouldBe 1
  }
  "give the correct two step connection matrix" in new CpgFromCodeTestFixture(
    """echo $a;
      |echo $b;
      |echo $c;
      |echo $d;
      |""".stripMargin, configFile = Some("cpg.conf")
  ) {
    implicit val slice: ProgramSlice = new ProgramSlice(null)
    val ea :: eb :: ec :: ed :: Nil = getCallNodes(cpg, "ECHO.*a.*", "ECHO.*b.*", "ECHO.*c.*", "ECHO.*d.*")
    val sea :: seb :: sec :: sed :: Nil = List(SliceNode(ea), SliceNode(eb), SliceNode(ec), SliceNode(ed))
    slice.addNode(sea)
    slice.addNode(seb)
    slice.addNode(sec)
    slice.addNode(sed)
    slice.addEdge(SliceEdge(0, SliceEdgeTypes.DATA_DEPENDENCE, Map(), sea, seb))
    slice.addEdge(SliceEdge(0, SliceEdgeTypes.DATA_DEPENDENCE, Map(), seb, sec))
    slice.addEdge(SliceEdge(0, SliceEdgeTypes.DATA_DEPENDENCE, Map(), sec, sed))
    slice.getReachabilityMap((sea, sec)) shouldBe true
    slice.getReachabilityMap((sec, sea)) shouldBe false
    slice.getReachabilityMap((sea, sed)) shouldBe true
    slice.getReachabilityMap((sed, sec)) shouldBe false
    slice.getReachabilityMap(SliceEdgeTypes.DATA_DEPENDENCE)((sea, sec)) shouldBe true
    slice.getReachabilityMap(SliceEdgeTypes.DATA_DEPENDENCE)((sec, sea)) shouldBe false
    slice.getReachabilityMap(SliceEdgeTypes.DATA_DEPENDENCE)((sea, sed)) shouldBe true
    slice.getReachabilityMap(SliceEdgeTypes.DATA_DEPENDENCE)((sed, sea)) shouldBe false
  }
  "give me the connection in nested calls" in new CpgFromCodeTestFixture(
    """function query($x) {
      |   return "concat stuff" . $x;
      |}
      |$x = query(query($_GET['test']));
      |echo $x;
      |""".stripMargin
  ) {
    val s1 :: s2 :: echo :: Nil = getCallNodes(cpg, "SEND_.*T2.*", "SEND_.*V3.*", "ECHO.*")
    implicit val slice: ProgramSlice = ProgramSlice.slice(echo)
    slice.getReachabilityMap(SliceEdgeTypes.DATA_DEPENDENCE)((SliceNode(s1), SliceNode(s2))) shouldBe true
  }
}
