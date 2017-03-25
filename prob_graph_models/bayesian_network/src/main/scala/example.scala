import factor.{Factor, Variable}

/**
  * Created by vadim on 25.03.17.
  * example from the lecture "Maximum expected utility"
  * (for some reason doesn't work well in worksheet)
  */
object example {
  def main(args: Array[String]): Unit = {
    val m = Variable("m", List(0,1,2))
    val s = Variable("s", List(0,1,2))
    val f = Variable("f", List(0,1))

    val M = Factor(List(m))
    val S = Factor(List(m, s))
    val U = Factor(List(m, f))

    M("m0") = 0.5; M("m1") = 0.3; M("m2") = 0.2

    S("m0,s0")=0.6; S("m0,s1")=0.3; S("m0,s2")=0.1
    S("m1,s0")=0.3; S("m1,s1")=0.4; S("m1,s2")=0.3
    S("m2,s0")=0.1; S("m2,s1")=0.4; S("m2,s2")=0.5

    U("m0,f0")=0; U("m0,f1")= -7
    U("m1,f0")=0; U("m1,f1")= 5
    U("m2,f0")=0; U("m1,f1")= 20

    print(M * S * U)
  }
}
