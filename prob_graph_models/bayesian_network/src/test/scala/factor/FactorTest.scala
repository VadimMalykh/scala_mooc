package factor

import org.scalatest._

/**
  * Created by vadim on 27/12/2016.
  */
class FactorTest extends FunSuite with BeforeAndAfter {

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

  test("Returns right factor values") {
    assert(factor(List(1, 1, 1)) === 1)
    assert(factor(List(1, 1, 2)) === 5)
    assert(factor(List(2, 1, 2)) === 6)
    assert(factor(List(2, 2, 2)) === 8)
  }
}
