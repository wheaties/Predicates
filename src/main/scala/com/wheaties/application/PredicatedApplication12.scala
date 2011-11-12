package com.wheaties.application

import com.wheaties.predicate.Predicate12
import com.wheaties.function.{WrappedFunction12, ClosedFunctionFactory12}

trait PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M] extends ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]{
	def elif(pred0: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]):ClosedApplicationElif12[A,B,C,D,E,F,G,H,I,J,K,L,M]
	def elseApply(func: Function12[A,B,C,D,E,F,G,H,I,J,K,L,M]):PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M] = elseApply(WrappedFunction12(func))
	def elseApply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]):PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M]
}

trait ClosedApplicationElif12[A,B,C,D,E,F,G,H,I,J,K,L,M]{
	def apply(func: Function12[A,B,C,D,E,F,G,H,I,J,K,L,M]):PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M] = apply(WrappedFunction12(func))
	def apply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]):PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M]
}