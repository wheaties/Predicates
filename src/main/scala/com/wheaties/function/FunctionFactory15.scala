package com.wheaties.function

trait ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] extends Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O):Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O):(P,Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P])
}

case class WrappedFunction15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](that: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14)

	override def toString() = that.toString()
}