package com.wheaties.application

import com.wheaties.predicate.Predicate3
import com.wheaties.function.{WrappedFunction3, ClosedFunctionFactory3}

trait PredicatedApplication3[A,B,C,D] extends ClosedFunctionFactory3[A,B,C,D]{
	def elif(pred0: Predicate3[A,B,C]):ClosedApplicationElif3[A,B,C,D]
	def elseApply(func: Function3[A,B,C,D]):PredicatedApplication3[A,B,C,D] = elseApply(WrappedFunction3(func))
	def elseApply(func: ClosedFunctionFactory3[A,B,C,D]):PredicatedApplication3[A,B,C,D]
}

trait ClosedApplicationElif3[A,B,C,D]{
	def apply(func: Function3[A,B,C,D]):PredicatedApplication3[A,B,C,D] = apply(WrappedFunction3(func))
	def apply(func: ClosedFunctionFactory3[A,B,C,D]):PredicatedApplication3[A,B,C,D]
}