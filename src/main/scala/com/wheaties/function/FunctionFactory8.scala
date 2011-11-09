package com.wheaties.function

trait ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I] extends Function8[A,B,C,D,E,F,G,H,I]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H):Function8[A,B,C,D,E,F,G,H,I]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H):(I,Function8[A,B,C,D,E,F,G,H,I])
}

case class WrappedFunction8[A,B,C,D,E,F,G,H,I](that: Function8[A,B,C,D,E,F,G,H,I]) extends ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)

	override def toString() = that.toString()
}