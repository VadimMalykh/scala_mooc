package factor

/**
  * Created by vadim on 23/12/2016.
  */

trait BaseFactor;

case class EmptyFactor() extends BaseFactor;

case class Factor(vars: List[Variable], values: List[Double]) extends BaseFactor{

  def apply(varName: String): Variable =
    vars.filter(v => v.name.equals(varName)).head

  // todo assignmet to value
  def apply(assignment: List[(String, Integer)]): Double = 0.0

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


