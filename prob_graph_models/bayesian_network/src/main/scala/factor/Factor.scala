package factor

/**
  * Created by vadim on 23/12/2016.
  */

trait BaseFactor {
  val vars: List[Variable]
  val vals: List[Double]
}

case class EmptyFactor() extends BaseFactor {
  override val vars = List()
  override val vals: List[Double] = List()
}

// variables are saved as List because the order is important
case class Factor(vars: List[Variable], vals: List[Double]) extends BaseFactor{
  type Assignment = List[Any]

  def apply(varName: String): Variable =
    vars.filter(v => v.name.equals(varName)).head

  def apply(assignment: Assignment): Double = {
    assert(vars.length == assignment.length,
      "Number of variables and size of assignment must agree")
    vals(assignmentToIndex(assignment))
  }

  private def assignmentToIndex(assignment: Assignment): Integer =
    assignment.zipWithIndex
      .map(t => {
        val index = t._2
        val assignmentValue = t._1

        
      })

  def *(that: Factor): BaseFactor = {
    if (this.vars.isEmpty || that.vars.isEmpty)
      EmptyFactor()
    else {
      // common variables must have the same cardinality
      assert(
        this.vars.map(v => v.name).toSet.intersect(that.vars.map(v => v.name).toSet)
          .forall(varName => this(varName) == that(varName)),
        "Common variables must have the same cardinality")
      EmptyFactor()
    }
  }
}


