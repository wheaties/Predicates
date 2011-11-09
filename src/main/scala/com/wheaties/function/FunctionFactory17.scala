package com.wheaties.function

trait ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] extends Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q):Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q):(R,Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R])
}

case class WrappedFunction17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](that: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16)

	override def toString() = that.toString()
}