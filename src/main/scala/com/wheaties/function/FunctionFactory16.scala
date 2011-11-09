package com.wheaties.function

trait ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] extends Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P):Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P):(Q,Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q])
}

case class WrappedFunction16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](that: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15)

	override def toString() = that.toString()
}