package com.wheaties.application

import com.wheaties.predicate.Predicate10
import com.wheaties.function.{WrappedFunction10, ClosedFunctionFactory10}

trait PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K] extends ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]{
	def elif(pred0: Predicate10[A,B,C,D,E,F,G,H,I,J]):ClosedApplicationElif10[A,B,C,D,E,F,G,H,I,J,K]
	def elseApply(func: Function10[A,B,C,D,E,F,G,H,I,J,K]):PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K] = elseApply(WrappedFunction10(func))
	def elseApply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]):PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K]
}

trait ClosedApplicationElif10[A,B,C,D,E,F,G,H,I,J,K]{
	def apply(func: Function10[A,B,C,D,E,F,G,H,I,J,K]):PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K] = apply(WrappedFunction10(func))
	def apply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]):PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K]
}