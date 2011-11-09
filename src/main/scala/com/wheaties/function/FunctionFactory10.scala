package com.wheaties.function

trait ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K] extends Function10[A,B,C,D,E,F,G,H,I,J,K]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J):Function10[A,B,C,D,E,F,G,H,I,J,K]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J):(K,Function10[A,B,C,D,E,F,G,H,I,J,K])
}

case class WrappedFunction10[A,B,C,D,E,F,G,H,I,J,K](that: Function10[A,B,C,D,E,F,G,H,I,J,K]) extends ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)

	override def toString() = that.toString()
}