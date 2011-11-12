package com.wheaties.application

import com.wheaties.predicate.Predicate19
import com.wheaties.function.{WrappedFunction19, ClosedFunctionFactory19}

trait PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] extends ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
	def elif(pred0: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):ClosedApplicationElif19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
	def elseApply(func: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] = elseApply(WrappedFunction19(func))
	def elseApply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
}

trait ClosedApplicationElif19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
	def apply(func: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] = apply(WrappedFunction19(func))
	def apply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
}