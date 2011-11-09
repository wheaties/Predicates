package com.wheaties.function

trait ClosedFunctionFactory3[A,B,C,D] extends Function3[A,B,C,D]{
	def query(arg0: A,arg1: B,arg2: C):Function3[A,B,C,D]
	def memoize(arg0: A,arg1: B,arg2: C):(D,Function3[A,B,C,D])
}

case class WrappedFunction3[A,B,C,D](that: Function3[A,B,C,D]) extends ClosedFunctionFactory3[A,B,C,D]{
	def query(arg0: A,arg1: B,arg2: C) = that
	def memoize(arg0: A,arg1: B,arg2: C) = (apply(arg0,arg1,arg2), query(arg0,arg1,arg2))

	def apply(arg0: A,arg1: B,arg2: C) = that(arg0,arg1,arg2)

	override def toString() = that.toString()
}