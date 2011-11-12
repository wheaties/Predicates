package com.wheaties.application

import com.wheaties.predicate.Predicate13
import com.wheaties.function.{WrappedFunction13, ClosedFunctionFactory13}

trait PredicateApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] extends ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
	def elif(pred0: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]):ClosedApplicationElif13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
	def elseApply(func: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):PredicateApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] = elseApply(WrappedFunction13(func))
	def elseApply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):PredicateApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
}

trait ClosedApplicationElif13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
	def apply(func: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):PredicateApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] = apply(WrappedFunction13(func))
	def apply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):PredicateApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
}