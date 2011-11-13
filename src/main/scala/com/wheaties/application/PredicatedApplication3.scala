package com.wheaties.application

import com.wheaties.predicate.Predicate3
import com.wheaties.function.{UnclosedFunctionFactory3, WrappedFunction3, ClosedFunctionFactory3}

trait PredicatedApplication3[A,B,C,D] extends ClosedFunctionFactory3[A,B,C,D]{
	def elif(pred0: Predicate3[A,B,C]):ClosedApplicationElif3[A,B,C,D]
	def elseApply(func: Function3[A,B,C,D]):PredicatedApplication3[A,B,C,D] = elseApply(WrappedFunction3(func))
	def elseApply(func: ClosedFunctionFactory3[A,B,C,D]):PredicatedApplication3[A,B,C,D]
}

trait ClosedApplicationElif3[A,B,C,D]{
	def apply(func: Function3[A,B,C,D]):PredicatedApplication3[A,B,C,D] = apply(WrappedFunction3(func))
	def apply(func: ClosedFunctionFactory3[A,B,C,D]):PredicatedApplication3[A,B,C,D]
}

trait UnclosedApplication3[A,B,C,D] extends UnclosedFunctionFactory3[A,B,C,D]{
	def elif(pred0: Predicate3[A,B,C]):UnclosedApplicationElif3[A,B,C,D]
	def elseApply(func: Function3[A,B,C,D]):PredicatedApplication3[A,B,C,D] = elseApply(WrappedFunction3(func))
	def elseApply(func: ClosedFunctionFactory3[A,B,C,D]):PredicatedApplication3[A,B,C,D]
}

trait UnclosedApplicationElif3[A,B,C,D]{
	def apply(func: Function3[A,B,C,D]):UnclosedApplication3[A,B,C,D] = apply(WrappedFunction3(func))
	def apply(func: ClosedFunctionFactory3[A,B,C,D]):UnclosedApplication3[A,B,C,D]
}

case class ApplyIf3[A,B,C,D](pred: Predicate3[A,B,C], thatTrue: ClosedFunctionFactory3[A,B,C,D]) extends UnclosedApplication3[A,B,C,D]{
	def elif(pred0: Predicate3[A,B,C]) = new UnclosedApplicationElif3[A,B,C,D] {
		def apply(func: ClosedFunctionFactory3[A,B,C,D])= ApplyEitherIf3(pred, thatTrue, ApplyIf3(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory3[A,B,C,D]) = ApplyEither3(pred, thatTrue, ApplyElse3(func))

	def query(arg0: A, arg1: B, arg2: C) = if(pred(arg0, arg1, arg2)) Some(thatTrue.query(arg0, arg1, arg2)) else None
	def memoize(arg0: A, arg1: B, arg2: C) = if(pred(arg0: A, arg1: B, arg2: C)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C)) else None

	def apply(arg0: A, arg1: B, arg2: C) = if(pred(arg0, arg1, arg2)) Some(thatTrue(arg0, arg1, arg2)) else None
}

case class ApplyEitherIf3[A,B,C,D](pred: Predicate3[A,B,C],
                                   thatTrue: ClosedFunctionFactory3[A,B,C,D],
                                   thatFalse: UnclosedApplication3[A,B,C,D])
	extends UnclosedApplication3[A,B,C,D]{

	def elif(pred0: Predicate3[A,B,C]) = new UnclosedApplicationElif3[A,B,C,D] {
		def apply(func: ClosedFunctionFactory3[A,B,C,D])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory3[A,B,C,D]) = ApplyEither3(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C) = if(pred(arg0, arg1, arg2)) Some(thatTrue.query(arg0, arg1, arg2)) else thatFalse.query(arg0, arg1, arg2)
	def memoize(arg0: A, arg1: B, arg2: C) = if(pred(arg0, arg1, arg2)) Some(thatTrue.memoize(arg0, arg1, arg2)) else thatFalse.memoize(arg0, arg1, arg2)

	def apply(arg0: A, arg1: B, arg2: C) = if(pred(arg0, arg1, arg2)) Some(thatTrue(arg0, arg1, arg2)) else thatFalse(arg0, arg1, arg2)
}

case class ApplyEither3[A,B,C,D](pred: Predicate3[A,B,C],
                                 thatTrue: ClosedFunctionFactory3[A,B,C,D],
                                 thatFalse: PredicatedApplication3[A,B,C,D])
	extends PredicatedApplication3[A,B,C,D]{

	def elif(pred0: Predicate3[A,B,C]) = new ClosedApplicationElif3[A,B,C,D] {
		def apply(func: ClosedFunctionFactory3[A,B,C,D]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory3[A,B,C,D]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C) = if(pred(arg0, arg1, arg2)) thatTrue.query(arg0, arg1, arg2) else thatFalse.query(arg0, arg1, arg2)
	def memoize(arg0: A, arg1: B, arg2: C) = if(pred(arg0, arg1, arg2)) thatTrue.memoize(arg0, arg1, arg2) else thatFalse.memoize(arg0, arg1, arg2)

	def apply(arg0: A, arg1: B, arg2: C) = if(pred(arg0, arg1, arg2)) thatTrue(arg0, arg1, arg2) else thatFalse(arg0, arg1, arg2)
}


case class ApplyElse3[A,B,C,D](thatFalse: ClosedFunctionFactory3[A,B,C,D]) extends PredicatedApplication3[A,B,C,D]{

	def elif(pred0: Predicate3[A,B,C]) = new ClosedApplicationElif3[A,B,C,D] {
		def apply(func: ClosedFunctionFactory3[A,B,C,D]) = ApplyEither3(pred0, func, ApplyElse3.this)
	}
	def elseApply(func: ClosedFunctionFactory3[A,B,C,D]) = ApplyElse3(func)

	def query(arg0: A, arg1: B, arg2: C) = thatFalse.query(arg0, arg1, arg2)
	def memoize(arg0: A, arg1: B, arg2: C) = thatFalse.memoize(arg0, arg1, arg2)

	def apply(arg0: A, arg1: B, arg2: C) = thatFalse(arg0, arg1, arg2)
}
