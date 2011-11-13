package com.wheaties.application

import com.wheaties.predicate.Predicate7
import com.wheaties.function.{UnclosedFunctionFactory7, WrappedFunction7, ClosedFunctionFactory7}

trait PredicatedApplication7[A,B,C,D,E,F,G,H] extends ClosedFunctionFactory7[A,B,C,D,E,F,G,H]{
	def elif(pred0: Predicate7[A,B,C,D,E,F,G]):ClosedApplicationElif7[A,B,C,D,E,F,G,H]
	def elseApply(func: Function7[A,B,C,D,E,F,G,H]):PredicatedApplication7[A,B,C,D,E,F,G,H] = elseApply(WrappedFunction7(func))
	def elseApply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]):PredicatedApplication7[A,B,C,D,E,F,G,H]
}

trait ClosedApplicationElif7[A,B,C,D,E,F,G,H]{
	def apply(func: Function7[A,B,C,D,E,F,G,H]):PredicatedApplication7[A,B,C,D,E,F,G,H] = apply(WrappedFunction7(func))
	def apply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]):PredicatedApplication7[A,B,C,D,E,F,G,H]
}

trait UnclosedApplication7[A,B,C,D,E,F,G,H] extends UnclosedFunctionFactory7[A,B,C,D,E,F,G,H]{
	def elif(pred0: Predicate7[A,B,C,D,E,F,G]):UnclosedApplicationElif7[A,B,C,D,E,F,G,H]
	def elseApply(func: Function7[A,B,C,D,E,F,G,H]):PredicatedApplication7[A,B,C,D,E,F,G,H] = elseApply(WrappedFunction7(func))
	def elseApply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]):PredicatedApplication7[A,B,C,D,E,F,G,H]
}

trait UnclosedApplicationElif7[A,B,C,D,E,F,G,H]{
	def apply(func: Function7[A,B,C,D,E,F,G,H]):UnclosedApplication7[A,B,C,D,E,F,G,H] = apply(WrappedFunction7(func))
	def apply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]):UnclosedApplication7[A,B,C,D,E,F,G,H]
}

case class ApplyIf7[A,B,C,D,E,F,G,H](pred: Predicate7[A,B,C,D,E,F,G], thatTrue: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]) extends UnclosedApplication7[A,B,C,D,E,F,G,H]{
	def elif(pred0: Predicate7[A,B,C,D,E,F,G]) = new UnclosedApplicationElif7[A,B,C,D,E,F,G,H] {
		def apply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H])= ApplyEitherIf7(pred, thatTrue, ApplyIf7(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]) = ApplyEither7(pred, thatTrue, ApplyElse7(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) else None
}

case class ApplyEitherIf7[A,B,C,D,E,F,G,H](pred: Predicate7[A,B,C,D,E,F,G],
                                           thatTrue: ClosedFunctionFactory7[A,B,C,D,E,F,G,H],
                                           thatFalse: UnclosedApplication7[A,B,C,D,E,F,G,H])
	extends UnclosedApplication7[A,B,C,D,E,F,G,H]{

	def elif(pred0: Predicate7[A,B,C,D,E,F,G]) = new UnclosedApplicationElif7[A,B,C,D,E,F,G,H] {
		def apply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]) = ApplyEither7(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6)
}

case class ApplyEither7[A,B,C,D,E,F,G,H](pred: Predicate7[A,B,C,D,E,F,G],
                                         thatTrue: ClosedFunctionFactory7[A,B,C,D,E,F,G,H],
                                         thatFalse: PredicatedApplication7[A,B,C,D,E,F,G,H])
	extends PredicatedApplication7[A,B,C,D,E,F,G,H]{

	def elif(pred0: Predicate7[A,B,C,D,E,F,G]) = new ClosedApplicationElif7[A,B,C,D,E,F,G,H] {
		def apply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6)
}

case class ApplyElse7[A,B,C,D,E,F,G,H](thatFalse: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]) extends PredicatedApplication7[A,B,C,D,E,F,G,H]{

	def elif(pred0: Predicate7[A,B,C,D,E,F,G]) = new ClosedApplicationElif7[A,B,C,D,E,F,G,H] {
		def apply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]) = ApplyEither7(pred0, func, ApplyElse7.this)
	}
	def elseApply(func: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]) = ApplyElse7(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6)
}
