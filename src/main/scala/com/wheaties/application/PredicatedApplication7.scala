package com.wheaties.application

import com.wheaties.function.{WrappedFunction7, ClosedFunctionFactory7}
import com.wheaties.predicate.Predicate7

trait PredicatedApplication7[A,B,C,D,E,F,G,H] extends ClosedFunctionFactory7[A,B,C,D,E,F,G,H]{
	def elif(pred0: Predicate7[A,B,C,D,E,F,G]):ClosedApplicationElif7[A,B,C,D,E,F,G,H]
	def elseApply(func: Function7[A,B,C,D,E,F,G,H]):PredicatedApplication7[A,B,C,D,E,F,G,H] = elseApply(WrappedFunction7(func))
	def elseApply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]):PredicatedApplication7[A,B,C,D,E,F,G,H]
}

trait ClosedApplicationElif7[A,B,C,D,E,F,G,H]{
	def apply(func: Function7[A,B,C,D,E,F,G,H]):PredicatedApplication7[A,B,C,D,E,F,G,H] = apply(WrappedFunction7(func))
	def apply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]):PredicatedApplication7[A,B,C,D,E,F,G,H]
}