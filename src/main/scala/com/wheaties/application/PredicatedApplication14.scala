package com.wheaties.application

import com.wheaties.predicate.Predicate14
import com.wheaties.function.{WrappedFunction14, ClosedFunctionFactory14}

trait PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] extends ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
	def elif(pred0: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):ClosedApplicationElif14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
	def elseApply(func: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] = elseApply(WrappedFunction14(func))
	def elseApply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
}

trait ClosedApplicationElif14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
	def apply(func: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] = apply(WrappedFunction14(func))
	def apply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
}