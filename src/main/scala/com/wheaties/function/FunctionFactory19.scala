package com.wheaties.function

trait ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] extends Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S):Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S):(T,Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T])
}

case class WrappedFunction19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](that: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18)

	override def toString() = that.toString()
}