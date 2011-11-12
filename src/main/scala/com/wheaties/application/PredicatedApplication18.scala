package com.wheaties.application

import com.wheaties.predicate.Predicate18
import com.wheaties.function.{WrappedFunction18, ClosedFunctionFactory18}

trait PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] extends ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
	def elif(pred0: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):ClosedApplicationElif18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
	def elseApply(func: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] = elseApply(WrappedFunction18(func))
	def elseApply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
}

trait ClosedApplicationElif18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
	def apply(func: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] = apply(WrappedFunction18(func))
	def apply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
}