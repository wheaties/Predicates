package com.wheaties.function

trait ClosedFunctionFactory4[A,B,C,D,E] extends Function4[A,B,C,D,E]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D):Function4[A,B,C,D,E]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D):(E,Function4[A,B,C,D,E])
}

case class WrappedFunction4[A,B,C,D,E](that: Function4[A,B,C,D,E]) extends ClosedFunctionFactory4[A,B,C,D,E]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D) = that
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D) = (apply(arg0,arg1,arg2,arg3), query(arg0,arg1,arg2,arg3))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D) = that(arg0,arg1,arg2,arg3)

	override def toString() = that.toString()
}