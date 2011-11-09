package com.wheaties.function

trait ClosedFunctionFactory6[A,B,C,D,E,F,G] extends Function6[A,B,C,D,E,F,G]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F):Function6[A,B,C,D,E,F,G]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F):(G,Function6[A,B,C,D,E,F,G])
}

case class WrappedFunction6[A,B,C,D,E,F,G](that: Function6[A,B,C,D,E,F,G]) extends ClosedFunctionFactory6[A,B,C,D,E,F,G]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = (apply(arg0,arg1,arg2,arg3,arg4,arg5), query(arg0,arg1,arg2,arg3,arg4,arg5))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = that(arg0,arg1,arg2,arg3,arg4,arg5)

	override def toString() = that.toString()
}