package com.wheaties.application

import com.wheaties.predicate.Predicate9
import com.wheaties.function.{WrappedFunction9, ClosedFunctionFactory9}

trait PredicatedApplication9[A,B,C,D,E,F,G,H,I,J] extends ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]{
	def elif(pred0: Predicate9[A,B,C,D,E,F,G,H,I]):ClosedApplicationElif9[A,B,C,D,E,F,G,H,I,J]
	def elseApply(func: Function9[A,B,C,D,E,F,G,H,I,J]):PredicatedApplication9[A,B,C,D,E,F,G,H,I,J] = elseApply(WrappedFunction9(func))
	def elseApply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]):PredicatedApplication9[A,B,C,D,E,F,G,H,I,J]
}

trait ClosedApplicationElif9[A,B,C,D,E,F,G,H,I,J]{
	def apply(func: Function9[A,B,C,D,E,F,G,H,I,J]):PredicatedApplication9[A,B,C,D,E,F,G,H,I,J] = apply(WrappedFunction9(func))
	def apply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]):PredicatedApplication9[A,B,C,D,E,F,G,H,I,J]
}