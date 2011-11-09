package com.wheaties.function

trait ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L] extends Function11[A,B,C,D,E,F,G,H,I,J,K,L]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K):Function11[A,B,C,D,E,F,G,H,I,J,K,L]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K):(L,Function11[A,B,C,D,E,F,G,H,I,J,K,L])
}

case class WrappedFunction11[A,B,C,D,E,F,G,H,I,J,K,L](that: Function11[A,B,C,D,E,F,G,H,I,J,K,L]) extends ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)

	override def toString() = that.toString()
}