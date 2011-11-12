package com.wheaties.application

import com.wheaties.predicate.Predicate15
import com.wheaties.function.{WrappedFunction15, ClosedFunctionFactory15}

trait PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] extends ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
	def elif(pred0: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):ClosedApplicationElif15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
	def elseApply(func: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] = elseApply(WrappedFunction15(func))
	def elseApply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
}

trait ClosedApplicationElif15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
	def apply(func: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] = apply(WrappedFunction15(func))
	def apply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
}