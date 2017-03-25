import factor.{Factor, Variable}
// example from the lecture
// "Maximum expected utility"

val m = Variable("m", List(0,1,2))
val s = Variable("s", List(0,1,2))
val f = Variable("f", List(0,1))

val M = Factor(List(m))
val S = Factor(List(m, s))
val U = Factor(List(m, f))

// for some reason worksheet doesn't work with M("m0") = 0.5 :(
M.update("m0",0.5); M.update("m1",0.3); M.update("m2",0.2)

S.update("m0,s0",0.6); S.update("m0,s1",0.3); S.update("m0,s2",0.1)
S.update("m1,s0",0.3); S.update("m1,s1",0.4); S.update("m1,s2",0.3)
S.update("m2,s0",0.1); S.update("m2,s1",0.4); S.update("m2,s2",0.5)

U.update("m0,f0",0); U.update("m0,f1",-7)
U.update("m1,f0",0); U.update("m1,f1", 5)
U.update("m2,f0",0); U.update("m1,f1", 20)

// why it doesn't work?
M * S