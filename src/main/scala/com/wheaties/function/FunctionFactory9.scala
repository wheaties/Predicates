package com.wheaties.function

trait ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J] extends Function9[A,B,C,D,E,F,G,H,I,J]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I):Function9[A,B,C,D,E,F,G,H,I,J]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I):(J,Function9[A,B,C,D,E,F,G,H,I,J])
}

case class WrappedFunction9[A,B,C,D,E,F,G,H,I,J](that: Function9[A,B,C,D,E,F,G,H,I,J]) extends ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)

	override def toString() = that.toString()
}