package com.wheaties.function

trait ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] extends Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R):Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R):(S,Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])
}

case class WrappedFunction18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](that: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) extends ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17)

	override def toString() = that.toString()
}