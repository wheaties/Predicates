package com.wheaties.application

import com.wheaties.function.{WrappedFunction8, ClosedFunctionFactory8}
import com.wheaties.predicate.Predicate8

trait PredicatedApplication8[A,B,C,D,E,F,G,H,I] extends ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]{
	def elif(pred0: Predicate8[A,B,C,D,E,F,G,H]):ClosedApplicationElif8[A,B,C,D,E,F,G,H,I]
	def elseApply(func: Function8[A,B,C,D,E,F,G,H,I]):PredicatedApplication8[A,B,C,D,E,F,G,H,I] = elseApply(WrappedFunction8(func))
	def elseApply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]):PredicatedApplication8[A,B,C,D,E,F,G,H,I]
}

trait ClosedApplicationElif8[A,B,C,D,E,F,G,H,I]{
	def apply(func: Function8[A,B,C,D,E,F,G,H,I]):PredicatedApplication8[A,B,C,D,E,F,G,H,I] = apply(WrappedFunction8(func))
	def apply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]):PredicatedApplication8[A,B,C,D,E,F,G,H,I]
}