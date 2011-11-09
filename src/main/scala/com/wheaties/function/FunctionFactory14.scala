package com.wheaties.function

trait ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] extends Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N):Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N):(O,Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O])
}

case class WrappedFunction14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](that: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13)

	override def toString() = that.toString()
}