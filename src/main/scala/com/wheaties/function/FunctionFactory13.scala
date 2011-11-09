package com.wheaties.function

trait ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] extends Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M):Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M):(N,Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N])
}

case class WrappedFunction13[A,B,C,D,E,F,G,H,I,J,K,L,M,N](that: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) extends ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)

	override def toString() = that.toString()
}