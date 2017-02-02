package factor

/**
  * Created by vadim on 27/12/2016.
  */

// scope is saved as List because the order is important
case class Variable(val name: String, val scope: List[Any]) {

  def this(name: String, cardinality: Integer) = this(name, (0 until cardinality).toList)
  def cardinality: Integer = scope.size
}
