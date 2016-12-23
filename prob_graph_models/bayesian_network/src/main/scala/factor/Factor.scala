package factor

/**
  * Created by vadim on 23/12/2016.
  */

case class Factor(vars: Map[String, List[Double]]) {

  def *(that: Factor): Factor = {
    if (this.vars.isEmpty || that.vars.isEmpty)
      Factor(Map[String, List[Double]]())
    else {
      // common variables must have the same cardinality
      assert(
        this.vars.keySet.intersect(that.vars.keySet)
          .forall(key => this.vars.get(key).size == that.vars.get(key).size),
        "Common variables must have the same cardinality")
      Factor(
        this.vars.keySet.union(that.vars.keySet)
          .map(key => )
      )
    }
  }

}


