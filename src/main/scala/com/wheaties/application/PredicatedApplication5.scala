package com.wheaties.application

import com.wheaties.function.{WrappedFunction5, ClosedFunctionFactory5}
import com.wheaties.predicate.Predicate5

trait PredicatedApplication5[A,B,C,D,E,F] extends ClosedFunctionFactory5[A,B,C,D,E,F]{
	def elif(pred0: Predicate5[A,B,C,D,E]):ClosedApplicationElif5[A,B,C,D,E,F]
	def elseApply(func: Function5[A,B,C,D,E,F]):PredicatedApplication5[A,B,C,D,E,F] = elseApply(WrappedFunction5(func))
	def elseApply(func: ClosedFunctionFactory5[A,B,C,D,E,F]):PredicatedApplication5[A,B,C,D,E,F]
}

trait ClosedApplicationElif5[A,B,C,D,E,F]{
	def apply(func: Function5[A,B,C,D,E,F]):PredicatedApplication5[A,B,C,D,E,F] = apply(WrappedFunction5(func))
	def apply(func: ClosedFunctionFactory5[A,B,C,D,E,F]):PredicatedApplication5[A,B,C,D,E,F]
}