package com.wheaties.application

import com.wheaties.predicate.Predicate11
import com.wheaties.function.{WrappedFunction11, ClosedFunctionFactory11}

trait PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L] extends ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]{
	def elif(pred0: Predicate11[A,B,C,D,E,F,G,H,I,J,K]):ClosedApplicationElif11[A,B,C,D,E,F,G,H,I,J,K,L]
	def elseApply(func: Function11[A,B,C,D,E,F,G,H,I,J,K,L]):PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L] = elseApply(WrappedFunction11(func))
	def elseApply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]):PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L]
}

trait ClosedApplicationElif11[A,B,C,D,E,F,G,H,I,J,K,L]{
	def apply(func: Function11[A,B,C,D,E,F,G,H,I,J,K,L]):PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L] = apply(WrappedFunction11(func))
	def apply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]):PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L]
}