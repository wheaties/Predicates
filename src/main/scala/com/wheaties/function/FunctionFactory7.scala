package com.wheaties.function

trait ClosedFunctionFactory7[A,B,C,D,E,F,G,H] extends Function7[A,B,C,D,E,F,G,H]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G):Function7[A,B,C,D,E,F,G,H]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G):(H,Function7[A,B,C,D,E,F,G,H])
}

case class WrappedFunction7[A,B,C,D,E,F,G,H](that: Function7[A,B,C,D,E,F,G,H]) extends ClosedFunctionFactory7[A,B,C,D,E,F,G,H]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6)

	override def toString() = that.toString()
}