package com.wheaties.application

import com.wheaties.predicate.Predicate2
import com.wheaties.function.{WrappedFunction2, ClosedFunctionFactory2}

trait PredicatedApplication2[A,B,C] extends ClosedFunctionFactory2[A,B,C]{
	def elif(pred0: Predicate2[A,B]):ClosedApplicationElif2[A,B,C]
	def elseApply(func: Function2[A,B,C]):PredicatedApplication2[A,B,C] = elseApply(WrappedFunction2(func))
	def elseApply(func: ClosedFunctionFactory2[A,B,C]):PredicatedApplication2[A,B,C]
}

trait ClosedApplicationElif2[A,B,C]{
	def apply(func: Function2[A,B,C]):PredicatedApplication2[A,B,C] = apply(WrappedFunction2(func))
	def apply(func: ClosedFunctionFactory2[A,B,C]):PredicatedApplication2[A,B,C]
}