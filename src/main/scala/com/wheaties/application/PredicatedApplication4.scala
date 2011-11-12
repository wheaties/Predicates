package com.wheaties.application

import com.wheaties.predicate.Predicate4
import com.wheaties.function.{WrappedFunction4, ClosedFunctionFactory4}

trait PredicatedApplication4[A,B,C,D,E] extends ClosedFunctionFactory4[A,B,C,D,E]{
	def elif(pred0: Predicate4[A,B,C,D]):ClosedApplicationElif4[A,B,C,D,E]
	def elseApply(func: Function4[A,B,C,D,E]):PredicatedApplication4[A,B,C,D,E] = elseApply(WrappedFunction4(func))
	def elseApply(func: ClosedFunctionFactory4[A,B,C,D,E]):PredicatedApplication4[A,B,C,D,E]
}

trait ClosedApplicationElif4[A,B,C,D,E]{
	def apply(func: Function4[A,B,C,D,E]):PredicatedApplication4[A,B,C,D,E] = apply(WrappedFunction4(func))
	def apply(func: ClosedFunctionFactory4[A,B,C,D,E]):PredicatedApplication4[A,B,C,D,E]
}