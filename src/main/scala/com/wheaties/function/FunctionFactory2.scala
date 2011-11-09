package com.wheaties.function

trait ClosedFunctionFactory2[A,B,C] extends Function2[A,B,C]{
	def query(arg0: A,arg1: B):Function2[A,B,C]
	def memoize(arg0: A,arg1: B):(C,Function2[A,B,C])
}

case class WrappedFunction2[A,B,C](that: Function2[A,B,C]) extends ClosedFunctionFactory2[A,B,C]{
	def query(arg0: A,arg1: B) = that
	def memoize(arg0: A,arg1: B) = (apply(arg0,arg1), query(arg0,arg1))

	def apply(arg0: A,arg1: B) = that(arg0,arg1)

	override def toString() = that.toString()
}