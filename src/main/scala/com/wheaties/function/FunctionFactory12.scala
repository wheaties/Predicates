package com.wheaties.function

trait ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M] extends Function12[A,B,C,D,E,F,G,H,I,J,K,L,M]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L):Function12[A,B,C,D,E,F,G,H,I,J,K,L,M]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L):(M,Function12[A,B,C,D,E,F,G,H,I,J,K,L,M])
}

case class WrappedFunction12[A,B,C,D,E,F,G,H,I,J,K,L,M](that: Function12[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)

	override def toString() = that.toString()
}