package com.wheaties.function

trait ClosedFunctionFactory5[A,B,C,D,E,F] extends Function5[A,B,C,D,E,F]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E):Function5[A,B,C,D,E,F]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E):(F,Function5[A,B,C,D,E,F])
}

case class WrappedFunction5[A,B,C,D,E,F](that: Function5[A,B,C,D,E,F]) extends ClosedFunctionFactory5[A,B,C,D,E,F]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E) = (apply(arg0,arg1,arg2,arg3,arg4), query(arg0,arg1,arg2,arg3,arg4))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E) = that(arg0,arg1,arg2,arg3,arg4)

	override def toString() = that.toString()
}