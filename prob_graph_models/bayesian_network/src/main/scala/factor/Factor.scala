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

  type Assignment = Map[String, Any]

  assert(vars.map(_.name).distinct.size == vars.size,
    "Variable names must be unique")

  def apply(varName: String): Option[Variable] =
    Option(vars.filter(v => v.name.equals(varName)).head)

  def apply(assignment: Assignment): Double = {
    assert(assignment.keySet == vars.map(_.name).toSet,
      "Assignment must contain all and only factor variables")

    apply(vars.map(v => assignment(v.name)))
  }

  def apply(listAssignment: List[Any]): Double = {
    assert(vars.length == listAssignment.length,
      "Number of variables and size of assignment must agree")
    vals(assignmentToIndex(listAssignment))
  }

  private def assignmentToIndex(assignment: List[Any]): Integer = {
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

  private def indexToAssignment(index: Integer): Assignment = {
    val cards = vars.map(v => v.cardinality)
      .scanLeft(1)(_ * _)
    vars.zipWithIndex
      .map(t => {
        val varIndex = index / cards(t._2) % t._1.cardinality
        (t._1.name, t._1.scope(varIndex))
      }).toMap
  }

  /**
    * Get value by assignment with more variables than in factor
    * (useful for product and other manipulation functions)
    *
    * @param assignment
    * @return
    */
  private def valForPartAssignment(assignment: Assignment): Double = {
    assert(vars.map(_.name).toSet subsetOf assignment.keySet,
      "Assignment must contain all factor variables")
    apply(vars.map(v => assignment(v.name)))
  }

  def *(that: Factor): BaseFactor = {
    if (this.vars.isEmpty || that.vars.isEmpty)
      EmptyFactor()
    else {
      val commonVarnames = this.vars.map(_.name).toSet.intersect(that.vars.map(_.name).toSet)
      assert(commonVarnames.forall(varName => this(varName).get.scope.equals(that(varName).get.scope)),
        "Common variables must have the same scope")
      val allVars = this.vars ::: that.vars
      Factor(allVars, List())
    }
  }
}


