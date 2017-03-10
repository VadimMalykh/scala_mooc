package factor

import org.scalactic.TolerantNumerics
import org.scalatest._

/**
  * Created by vadim on 27/12/2016.
  */
class FactorTest extends FunSuite with BeforeAndAfter with PrivateMethodTester{

  var factor: Factor = _

  val epsilon = 0.00001

  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  before {
    factor = Factor(List(
      Variable("X_3", List(1, 2)),
      Variable("X_1", List(1, 2)),
      Variable("X_2", List(1, 2))),
      List(1, 2, 3, 4, 5, 6, 7, 8))
  }

  test("Can create empty factor") {
    val emptyFactor = EmptyFactor()

  }

  test("Cannot create factor with duplicate variables") {
    assertThrows[AssertionError] {
      val factor = Factor(List(
        Variable("X_1", List(1, 2)),
        Variable("X_2", List(1, 2)),
        Variable("X_2", List(1, 2))),
        List(1, 2, 3, 4, 5, 6, 7, 8))
    }
  }

  test("Returns right factor values by list assignment") {
    assert(factor(List(1, 1, 1)) === 1.0)
    assert(factor(List(1, 1, 2)) === 5.0)
    assert(factor(List(2, 1, 2)) === 6.0)
    assert(factor(List(2, 2, 2)) === 8.0)
  }

  test("Returns right factor values by full assignment") {
    assert(factor(Map(
      "X_1" -> 1,
      "X_2" -> 1,
      "X_3" -> 1)) === 1.0)
    assert(factor(Map(
      "X_1" -> 1,
      "X_2" -> 2,
      "X_3" -> 1)) === 5.0)
    assert(factor(Map(
      "X_1" -> 1,
      "X_2" -> 2,
      "X_3" -> 2)) === 6.0)
    assert(factor(Map(
      "X_1" -> 2,
      "X_2" -> 2,
      "X_3" -> 2)) === 8.0)
  }

  test("Can return variable by name") {
    val variable = factor.getVar("X_1").get
    assert(variable.name === "X_1")
    assert(variable.cardinality === 2)
    assert(variable.scope == List(1, 2))
  }

  test("Can calculate Assignment by index") {
    val indexToAssignment = PrivateMethod[Map[String, Any]]('indexToAssignment)
    var assignment: Map[String, Any] = Map()

    assignment = factor invokePrivate indexToAssignment(0)
    assert(assignment("X_1") === 1)
    assert(assignment("X_2") === 1)
    assert(assignment("X_3") === 1)

    assignment = factor invokePrivate indexToAssignment(4)
    assert(assignment("X_1") === 1)
    assert(assignment("X_2") === 2)
    assert(assignment("X_3") === 1)

    assignment = factor invokePrivate indexToAssignment(5)
    assert(assignment("X_1") === 1)
    assert(assignment("X_2") === 2)
    assert(assignment("X_3") === 2)

    assignment = factor invokePrivate indexToAssignment(7)
    assert(assignment("X_1") === 2)
    assert(assignment("X_2") === 2)
    assert(assignment("X_3") === 2)

  }

  test("Can multipy two factors") {

    val factor1 = Factor(List(
      Variable("a", List(1, 2, 3)),
      Variable("b", List(1, 2))))

    factor1(Map("a" -> 1, "b" -> 1)) = 0.5
    factor1(Map("a" -> 1, "b" -> 2)) = 0.8
    factor1(Map("a" -> 2, "b" -> 1)) = 0.1
    factor1(Map("a" -> 2, "b" -> 2)) = 0
    factor1(Map("a" -> 3, "b" -> 1)) = 0.3
    factor1(Map("a" -> 3, "b" -> 2)) = 0.9

    val factor2 = Factor(List(
      Variable("b", List(1, 2)),
      Variable("c", List(1, 2))))

    factor2("b1, c1") = 0.5
    factor2("b1, c2") = 0.7
    factor2("b2, c1") = 0.1
    factor2("b2, c2") = 0.2

    val mulFactor = factor1 * factor2
    assert(mulFactor.vars.map(_.name).toSet === Set("a", "b", "c"))
    assert(mulFactor("a1,b1,c1") === 0.25)
    assert(mulFactor("a1, b1, c2") === 0.35)
    // most ugly way
    assert(mulFactor("a1,c1,b2") === 0.08)
    assert(mulFactor(Map("a" -> 1, "b" ->2, "c" -> 2)) === 0.16)
    assert(mulFactor(Map("a" -> 2, "b" ->1, "c" -> 1)) === 0.05)
    assert(mulFactor(Map("a" -> 2, "b" ->1, "c" -> 2)) === 0.07)
    assert(mulFactor(Map("a" -> 2, "b" ->2, "c" -> 1)) === 0.0)
    assert(mulFactor(Map("a" -> 2, "b" ->2, "c" -> 2)) === 0.0)
    assert(mulFactor(Map("a" -> 3, "b" ->1, "c" -> 1)) === 0.15)
    assert(mulFactor(Map("a" -> 3, "b" ->1, "c" -> 2)) === 0.21)
    assert(mulFactor(Map("a" -> 3, "b" ->2, "c" -> 1)) === 0.09)
    assert(mulFactor(Map("a" -> 3, "b" ->2, "c" -> 2)) === 0.18)
  }
}
