package com.wheaties.application

import com.wheaties.predicate.Predicate2
import com.wheaties.function.{UnclosedFunctionFactory2, WrappedFunction2, ClosedFunctionFactory2}

trait PredicatedApplication2[A,B,C] extends ClosedFunctionFactory2[A,B,C]{
	def elif(pred0: Predicate2[A,B]):ClosedApplicationElif2[A,B,C]
	def elseApply(func: Function2[A,B,C]):PredicatedApplication2[A,B,C] = elseApply(WrappedFunction2(func))
	def elseApply(func: ClosedFunctionFactory2[A,B,C]):PredicatedApplication2[A,B,C]
}

trait ClosedApplicationElif2[A,B,C]{
	def apply(func: Function2[A,B,C]):PredicatedApplication2[A,B,C] = apply(WrappedFunction2(func))
	def apply(func: ClosedFunctionFactory2[A,B,C]):PredicatedApplication2[A,B,C]
}

trait UnclosedApplication2[A,B,C] extends UnclosedFunctionFactory2[A,B,C]{
	def elif(pred0: Predicate2[A,B]):UnclosedApplicationElif2[A,B,C]
	def elseApply(func: Function2[A,B,C]):PredicatedApplication2[A,B,C] = elseApply(WrappedFunction2(func))
	def elseApply(func: ClosedFunctionFactory2[A,B,C]):PredicatedApplication2[A,B,C]
}

trait UnclosedApplicationElif2[A,B,C]{
	def apply(func: Function2[A,B,C]):UnclosedApplication2[A,B,C] = apply(WrappedFunction2(func))
	def apply(func: ClosedFunctionFactory2[A,B,C]):UnclosedApplication2[A,B,C]
}

case class ApplyIf2[A,B,C](pred: Predicate2[A,B], thatTrue: ClosedFunctionFactory2[A,B,C]) extends UnclosedApplication2[A,B,C]{
	def elif(pred0: Predicate2[A,B]) = new UnclosedApplicationElif2[A,B,C] {
		def apply(func: ClosedFunctionFactory2[A,B,C])= ApplyEitherIf2(pred, thatTrue, ApplyIf2(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory2[A,B,C]) = ApplyEither2(pred, thatTrue, ApplyElse2(func))

	def query(arg0: A, arg1: B) = if(pred(arg0, arg1)) Some(thatTrue.query(arg0, arg1)) else None
	def memoize(arg0: A, arg1: B) = if(pred(arg0: A, arg1: B)) Some(thatTrue.memoize(arg0: A, arg1: B)) else None

	def apply(arg0: A, arg1: B) = if(pred(arg0, arg1)) Some(thatTrue(arg0, arg1)) else None
}

case class ApplyEitherIf2[A,B,C](pred: Predicate2[A,B],
thatTrue: ClosedFunctionFactory2[A,B,C],
thatFalse: UnclosedApplication2[A,B,C])
	extends UnclosedApplication2[A,B,C]{

	def elif(pred0: Predicate2[A,B]) = new UnclosedApplicationElif2[A,B,C] {
		def apply(func: ClosedFunctionFactory2[A,B,C])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory2[A,B,C]) = ApplyEither2(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B) = if(pred(arg0, arg1)) Some(thatTrue.query(arg0, arg1)) else thatFalse.query(arg0, arg1)
	def memoize(arg0: A, arg1: B) = if(pred(arg0, arg1)) Some(thatTrue.memoize(arg0, arg1)) else thatFalse.memoize(arg0, arg1)

	def apply(arg0: A, arg1: B) = if(pred(arg0, arg1)) Some(thatTrue(arg0, arg1)) else thatFalse(arg0, arg1)
}

case class ApplyEither2[A,B,C](pred: Predicate2[A,B],
                               thatTrue: ClosedFunctionFactory2[A,B,C],
                               thatFalse: PredicatedApplication2[A,B,C])
	extends PredicatedApplication2[A,B,C]{

	def elif(pred0: Predicate2[A,B]) = new ClosedApplicationElif2[A,B,C] {
		def apply(func: ClosedFunctionFactory2[A,B,C]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory2[A,B,C]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B) = if(pred(arg0, arg1)) thatTrue.query(arg0, arg1) else thatFalse.query(arg0, arg1)
	def memoize(arg0: A, arg1: B) = if(pred(arg0, arg1)) thatTrue.memoize(arg0, arg1) else thatFalse.memoize(arg0, arg1)

	def apply(arg0: A, arg1: B) = if(pred(arg0, arg1)) thatTrue(arg0, arg1) else thatFalse(arg0, arg1)
}

case class ApplyElse2[A,B,C](thatFalse: ClosedFunctionFactory2[A,B,C]) extends PredicatedApplication2[A,B,C]{

	def elif(pred0: Predicate2[A,B]) = new ClosedApplicationElif2[A,B,C] {
		def apply(func: ClosedFunctionFactory2[A,B,C]) = ApplyEither2(pred0, func, ApplyElse2.this)
	}
	def elseApply(func: ClosedFunctionFactory2[A,B,C]) = ApplyElse2(func)

	def query(arg0: A, arg1: B) = thatFalse.query(arg0, arg1)
	def memoize(arg0: A, arg1: B) = thatFalse.memoize(arg0, arg1)

	def apply(arg0: A, arg1: B) = thatFalse(arg0, arg1)
}
