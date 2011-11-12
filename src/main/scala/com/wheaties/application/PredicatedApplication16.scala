package com.wheaties.application

import com.wheaties.predicate.Predicate16
import com.wheaties.function.{WrappedFunction16, ClosedFunctionFactory16}

trait PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] extends ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
	def elif(pred0: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):ClosedApplicationElif16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
	def elseApply(func: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] = elseApply(WrappedFunction16(func))
	def elseApply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
}

trait ClosedApplicationElif16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
	def apply(func: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] = apply(WrappedFunction16(func))
	def apply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
}