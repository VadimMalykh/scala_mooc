package factor

import org.scalatest._

/**
  * Created by vadim on 27/12/2016.
  */
class FactorTest extends FunSuite with BeforeAndAfter with PrivateMethodTester{

  var factor: Factor = _

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

  test("Cannot create factor with dublicate variables") {
    assertThrows[AssertionError] {
      val factor = Factor(List(
        Variable("X_1", List(1, 2)),
        Variable("X_2", List(1, 2)),
        Variable("X_2", List(1, 2))),
        List(1, 2, 3, 4, 5, 6, 7, 8))
    }
  }

  test("Returns right factor values by list assignment") {
    assert(factor(List(1, 1, 1)) === 1)
    assert(factor(List(1, 1, 2)) === 5)
    assert(factor(List(2, 1, 2)) === 6)
    assert(factor(List(2, 2, 2)) === 8)
  }

  test("Returns right factor values by full assignment") {
    assert(factor(Map(
      "X_1" -> 1,
      "X_2" -> 1,
      "X_3" -> 1)) === 1)
    assert(factor(Map(
      "X_1" -> 1,
      "X_2" -> 2,
      "X_3" -> 1)) === 5)
    assert(factor(Map(
      "X_1" -> 1,
      "X_2" -> 2,
      "X_3" -> 2)) === 6)
    assert(factor(Map(
      "X_1" -> 2,
      "X_2" -> 2,
      "X_3" -> 2)) === 8)
  }

  test("Can return variable by name") {
    val variable = factor("X_1").get
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
    val mulFactor = factor *
                    Factor(List(
                      Variable("X_2", List(1, 2)),
                      Variable("X_3", List(1, 2)),
                      Variable("X_4", List(1, 2))),
                      List(8, 7, 6, 5, 4, 3, 2, 1))
    assert(mulFactor.vars.map(v => v.name).toSet.equals(Set("X_1, X_2, X_3, X_4")))
  }
}
