package com.wheaties.application

import com.wheaties.predicate.Predicate17
import com.wheaties.function.{WrappedFunction17, ClosedFunctionFactory17}

trait PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] extends ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
	def elif(pred0: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):ClosedApplicationElif17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
	def elseApply(func: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] = elseApply(WrappedFunction17(func))
	def elseApply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
}

trait ClosedApplicationElif17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
	def apply(func: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] = apply(WrappedFunction17(func))
	def apply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
}