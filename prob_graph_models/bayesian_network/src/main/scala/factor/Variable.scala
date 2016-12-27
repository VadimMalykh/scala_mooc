package factor

/**
  * Created by vadim on 27/12/2016.
  */
class Variable(val name: String, val options: List[Any]) {

  def this(name: String, cardinality: Integer) = this(name, 0 until cardinality)
  def cardinality: Integer = options.length
}
