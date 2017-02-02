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

  def apply(varName: String): Option[Variable] =
    Option(vars.filter(v => v.name.equals(varName)).head)

  def apply(assignment: Assignment): Double = {
    assert(vars.length == assignment.length,
      "Number of variables and size of assignment must agree")
    vals(assignmentToIndex(assignment))
  }

  private def assignmentToIndex(assignment: Assignment): Integer = {
    val cards = vars.map(v => v.cardinality)
                          .scanLeft(1)(_ * _)
    assignment.zipWithIndex
      .map(t => {
        val index = t._2
        val assignmentValue = t._1
        val assignmentValIndex = vars(index).scope.indexOf(assignmentValue)
        cards(index) * assignmentValIndex
      }).sum
  }

  def *(that: Factor): BaseFactor = {
    if (this.vars.isEmpty || that.vars.isEmpty)
      EmptyFactor()
    else {
      val commonVarnames = this.vars.map(_.name).toSet.intersect(that.vars.map(_.name).toSet)
      assert(commonVarnames.forall(varName => this(varName).get.scope.equals(that(varName).get.scope)),
        "Common variables must have the same scope")
      val allVars = this.vars ::: that.vars
      EmptyFactor()
    }
  }
}


