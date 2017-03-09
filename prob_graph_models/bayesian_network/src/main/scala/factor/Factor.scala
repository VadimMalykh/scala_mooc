package factor

/**
  * Created by vadim on 23/12/2016.
  */


trait BaseFactor {
  type Assignment = Map[String, Any]

  def apply(assignment: Assignment): Double = ???

  def getVar(varName: String): Option[Variable] =
    vars.find(_.name == varName)

  val vars: List[Variable]
  val vals: Array[Double]
}

case class EmptyFactor() extends BaseFactor {
  override val vars = List()
  override val vals: Array[Double] = Array()
}

// variables are saved as List because the order is important
case class Factor(vars: List[Variable], vals: Array[Double]) extends BaseFactor{

  assert(vars.map(_.name).distinct.size == vars.size,
    "Variable names must be unique")

  def this(vars: List[Variable]) = this(vars, Array.fill(vars.map(_.cardinality).reduce(_ * _))(0.))

  def this(vars: List[Variable], vals: List[AnyVal]) = this(vars, vals.map(_.toString.toDouble).toArray)

  def update(listAssignment: List[Any], value: Double): Unit =
    vals(assignmentToIndex(listAssignment)) = value

  def update(assignment: Assignment, value: Double): Unit =
    update(assignmentToList(assignment), value)

  def update(strAssignment: String, value: Double): Unit =
    update(stringToAssignment(strAssignment), value)

  def apply(strAssignment: String): Double =
    apply(stringToAssignment(strAssignment))

  override def apply(assignment: Assignment): Double = {

    assert(assignment.keySet == vars.map(_.name).toSet,
      "Assignment must contain all and only factor variables")

    apply(assignmentToList(assignment))
  }

  def apply(listAssignment: List[Any]): Double = {
    assert(vars.length == listAssignment.length,
      "Number of variables and size of assignment must agree")
    vals(assignmentToIndex(listAssignment))
  }

  private def assignmentToList(assignment: Assignment): List[Any] = vars.map(v => assignment(v.name))

  private def stringToAssignment(strAssignment: String): Assignment =
    strAssignment.split(',')
    .map(token => {
      val items = token.split("_")
      if (items.length == 2) {
        (items(0).toString.trim, items(1).toInt)
      }
      else {
        val varName = token.split("\\d")(0)
        (varName.trim, token.substring(varName.length).toInt)
      }
    }).toMap

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

  private def indexToAssignment(index: Integer, vars: List[Variable]): Assignment = {
    val cards = vars.map(v => v.cardinality)
      .scanLeft(1)(_ * _)
    vars.zipWithIndex
      .map(t => {
        val varIndex = index / cards(t._2) % t._1.cardinality
        (t._1.name, t._1.scope(varIndex))
      }).toMap
  }

  private def indexToAssignment(index: Integer): Assignment =
    indexToAssignment(index, vars)

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
      assert(commonVarnames.forall(varName => this.getVar(varName).get.scope
                                              .equals(that.getVar(varName).get.scope)),
        "Common variables must have the same scope")
      val allVars = (this.vars ::: that.vars).distinct
      Factor(allVars,
        (0 until allVars.map(_.cardinality).reduce(_ * _))
        .map(ind => indexToAssignment(ind, allVars))
        .map(assignment =>
            this.valForPartAssignment(assignment) * that.valForPartAssignment(assignment))
          .toArray)
    }
  }
}

object Factor {
  def apply(vars: List[Variable]) = new Factor(vars)
  def apply(vars: List[Variable], vals: List[AnyVal]) = new Factor(vars, vals)
}


