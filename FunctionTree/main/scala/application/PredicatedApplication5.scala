package com.wheaties.application

import com.wheaties.predicate.Predicate5
import com.wheaties.function.{UnclosedFunctionFactory5, WrappedFunction5, ClosedFunctionFactory5}

trait PredicatedApplication5[A,B,C,D,E,F] extends ClosedFunctionFactory5[A,B,C,D,E,F]{
	def elif(pred0: Predicate5[A,B,C,D,E]):ClosedApplicationElif5[A,B,C,D,E,F]
	def elseApply(func: Function5[A,B,C,D,E,F]):PredicatedApplication5[A,B,C,D,E,F] = elseApply(WrappedFunction5(func))
	def elseApply(func: ClosedFunctionFactory5[A,B,C,D,E,F]):PredicatedApplication5[A,B,C,D,E,F]
}

trait ClosedApplicationElif5[A,B,C,D,E,F]{
	def apply(func: Function5[A,B,C,D,E,F]):PredicatedApplication5[A,B,C,D,E,F] = apply(WrappedFunction5(func))
	def apply(func: ClosedFunctionFactory5[A,B,C,D,E,F]):PredicatedApplication5[A,B,C,D,E,F]
}

trait UnclosedApplication5[A,B,C,D,E,F] extends UnclosedFunctionFactory5[A,B,C,D,E,F]{
	def elif(pred0: Predicate5[A,B,C,D,E]):UnclosedApplicationElif5[A,B,C,D,E,F]
	def elseApply(func: Function5[A,B,C,D,E,F]):PredicatedApplication5[A,B,C,D,E,F] = elseApply(WrappedFunction5(func))
	def elseApply(func: ClosedFunctionFactory5[A,B,C,D,E,F]):PredicatedApplication5[A,B,C,D,E,F]
}

trait UnclosedApplicationElif5[A,B,C,D,E,F]{
	def apply(func: Function5[A,B,C,D,E,F]):UnclosedApplication5[A,B,C,D,E,F] = apply(WrappedFunction5(func))
	def apply(func: ClosedFunctionFactory5[A,B,C,D,E,F]):UnclosedApplication5[A,B,C,D,E,F]
}

case class ApplyIf5[A,B,C,D,E,F](pred: Predicate5[A,B,C,D,E],
                                 thatTrue: ClosedFunctionFactory5[A,B,C,D,E,F])
  extends UnclosedApplication5[A,B,C,D,E,F]{

	def elif(pred0: Predicate5[A,B,C,D,E]) = new UnclosedApplicationElif5[A,B,C,D,E,F] {
		def apply(func: ClosedFunctionFactory5[A,B,C,D,E,F])= ApplyEitherIf5(pred, thatTrue, ApplyIf5(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory5[A,B,C,D,E,F]) = ApplyEither5(pred, thatTrue, ApplyElse5(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = if(pred(arg0, arg1, arg2, arg3, arg4)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = if(pred(arg0, arg1, arg2, arg3, arg4)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4)) else None
}

case class ApplyEitherIf5[A,B,C,D,E,F](pred: Predicate5[A,B,C,D,E],
                                       thatTrue: ClosedFunctionFactory5[A,B,C,D,E,F],
                                       thatFalse: UnclosedApplication5[A,B,C,D,E,F])
	extends UnclosedApplication5[A,B,C,D,E,F]{

	def elif(pred0: Predicate5[A,B,C,D,E]) = new UnclosedApplicationElif5[A,B,C,D,E,F] {
		def apply(func: ClosedFunctionFactory5[A,B,C,D,E,F])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory5[A,B,C,D,E,F]) = ApplyEither5(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = if(pred(arg0, arg1, arg2, arg3, arg4)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = if(pred(arg0, arg1, arg2, arg3, arg4)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = if(pred(arg0, arg1, arg2, arg3, arg4)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4)) else thatFalse(arg0, arg1, arg2, arg3, arg4)
}

case class ApplyEither5[A,B,C,D,E,F](pred: Predicate5[A,B,C,D,E],
                                     thatTrue: ClosedFunctionFactory5[A,B,C,D,E,F],
                                     thatFalse: PredicatedApplication5[A,B,C,D,E,F])
	extends PredicatedApplication5[A,B,C,D,E,F]{

	def elif(pred0: Predicate5[A,B,C,D,E]) = new ClosedApplicationElif5[A,B,C,D,E,F] {
		def apply(func: ClosedFunctionFactory5[A,B,C,D,E,F]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory5[A,B,C,D,E,F]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = if(pred(arg0, arg1, arg2, arg3, arg4)) thatTrue.query(arg0, arg1, arg2, arg3, arg4) else thatFalse.query(arg0, arg1, arg2, arg3, arg4)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = if(pred(arg0, arg1, arg2, arg3, arg4)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = if(pred(arg0, arg1, arg2, arg3, arg4)) thatTrue(arg0, arg1, arg2, arg3, arg4) else thatFalse(arg0, arg1, arg2, arg3, arg4)
}

case class ApplyElse5[A,B,C,D,E,F](thatFalse: ClosedFunctionFactory5[A,B,C,D,E,F]) extends PredicatedApplication5[A,B,C,D,E,F]{

	def elif(pred0: Predicate5[A,B,C,D,E]) = new ClosedApplicationElif5[A,B,C,D,E,F] {
		def apply(func: ClosedFunctionFactory5[A,B,C,D,E,F]) = ApplyEither5(pred0, func, ApplyElse5.this)
	}
	def elseApply(func: ClosedFunctionFactory5[A,B,C,D,E,F]) = ApplyElse5(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = thatFalse.query(arg0, arg1, arg2, arg3, arg4)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = thatFalse(arg0, arg1, arg2, arg3, arg4)
}
