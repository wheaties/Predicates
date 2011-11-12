package com.wheaties.application

import com.wheaties.predicate.Predicate6
import com.wheaties.function.{WrappedFunction6, ClosedFunctionFactory6}

trait PredicatedApplication6[A,B,C,D,E,F,G] extends ClosedFunctionFactory6[A,B,C,D,E,F,G]{
	def elif(pred0: Predicate6[A,B,C,D,E,F]):ClosedApplicationElif6[A,B,C,D,E,F,G]
	def elseApply(func: Function6[A,B,C,D,E,F,G]):PredicatedApplication6[A,B,C,D,E,F,G] = elseApply(WrappedFunction6(func))
	def elseApply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G]):PredicatedApplication6[A,B,C,D,E,F,G]
}

trait ClosedApplicationElif6[A,B,C,D,E,F,G]{
	def apply(func: Function6[A,B,C,D,E,F,G]):PredicatedApplication6[A,B,C,D,E,F,G] = apply(WrappedFunction6(func))
	def apply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G]):PredicatedApplication6[A,B,C,D,E,F,G]
}