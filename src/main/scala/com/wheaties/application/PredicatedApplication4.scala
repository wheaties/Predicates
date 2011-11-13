package com.wheaties.application

import com.wheaties.predicate.Predicate4
import com.wheaties.function.{UnclosedFunctionFactory4, WrappedFunction4, ClosedFunctionFactory4}

trait PredicatedApplication4[A,B,C,D,E] extends ClosedFunctionFactory4[A,B,C,D,E]{
	def elif(pred0: Predicate4[A,B,C,D]):ClosedApplicationElif4[A,B,C,D,E]
	def elseApply(func: Function4[A,B,C,D,E]):PredicatedApplication4[A,B,C,D,E] = elseApply(WrappedFunction4(func))
	def elseApply(func: ClosedFunctionFactory4[A,B,C,D,E]):PredicatedApplication4[A,B,C,D,E]
}

trait ClosedApplicationElif4[A,B,C,D,E]{
	def apply(func: Function4[A,B,C,D,E]):PredicatedApplication4[A,B,C,D,E] = apply(WrappedFunction4(func))
	def apply(func: ClosedFunctionFactory4[A,B,C,D,E]):PredicatedApplication4[A,B,C,D,E]
}

trait UnclosedApplication4[A,B,C,D,E] extends UnclosedFunctionFactory4[A,B,C,D,E]{
	def elif(pred0: Predicate4[A,B,C,D]):UnclosedApplicationElif4[A,B,C,D,E]
	def elseApply(func: Function4[A,B,C,D,E]):PredicatedApplication4[A,B,C,D,E] = elseApply(WrappedFunction4(func))
	def elseApply(func: ClosedFunctionFactory4[A,B,C,D,E]):PredicatedApplication4[A,B,C,D,E]
}

trait UnclosedApplicationElif4[A,B,C,D,E]{
	def apply(func: Function4[A,B,C,D,E]):UnclosedApplication4[A,B,C,D,E] = apply(WrappedFunction4(func))
	def apply(func: ClosedFunctionFactory4[A,B,C,D,E]):UnclosedApplication4[A,B,C,D,E]
}

case class ApplyIf4[A,B,C,D,E](pred: Predicate4[A,B,C,D],
                               thatTrue: ClosedFunctionFactory4[A,B,C,D,E])
  extends UnclosedApplication4[A,B,C,D,E]{

	def elif(pred0: Predicate4[A,B,C,D]) = new UnclosedApplicationElif4[A,B,C,D,E] {
		def apply(func: ClosedFunctionFactory4[A,B,C,D,E])= ApplyEitherIf4(pred, thatTrue, ApplyIf4(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory4[A,B,C,D,E]) = ApplyEither4(pred, thatTrue, ApplyElse4(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred(arg0, arg1, arg2, arg3)) Some(thatTrue.query(arg0, arg1, arg2, arg3)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred(arg0, arg1, arg2, arg3)) Some(thatTrue(arg0, arg1, arg2, arg3)) else None
}

case class ApplyEitherIf4[A,B,C,D,E](pred: Predicate4[A,B,C,D],
                                     thatTrue: ClosedFunctionFactory4[A,B,C,D,E],
                                     thatFalse: UnclosedApplication4[A,B,C,D,E])
	extends UnclosedApplication4[A,B,C,D,E]{

	def elif(pred0: Predicate4[A,B,C,D]) = new UnclosedApplicationElif4[A,B,C,D,E] {
		def apply(func: ClosedFunctionFactory4[A,B,C,D,E])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory4[A,B,C,D,E]) = ApplyEither4(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred(arg0, arg1, arg2, arg3)) Some(thatTrue.query(arg0, arg1, arg2, arg3)) else thatFalse.query(arg0, arg1, arg2, arg3)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred(arg0, arg1, arg2, arg3)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3)) else thatFalse.memoize(arg0, arg1, arg2, arg3)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred(arg0, arg1, arg2, arg3)) Some(thatTrue(arg0, arg1, arg2, arg3)) else thatFalse(arg0, arg1, arg2, arg3)
}

case class ApplyEither4[A,B,C,D,E](pred: Predicate4[A,B,C,D],
                                   thatTrue: ClosedFunctionFactory4[A,B,C,D,E],
                                   thatFalse: PredicatedApplication4[A,B,C,D,E])
	extends PredicatedApplication4[A,B,C,D,E]{

	def elif(pred0: Predicate4[A,B,C,D]) = new ClosedApplicationElif4[A,B,C,D,E] {
		def apply(func: ClosedFunctionFactory4[A,B,C,D,E]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory4[A,B,C,D,E]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred(arg0, arg1, arg2, arg3)) thatTrue.query(arg0, arg1, arg2, arg3) else thatFalse.query(arg0, arg1, arg2, arg3)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred(arg0, arg1, arg2, arg3)) thatTrue.memoize(arg0, arg1, arg2, arg3) else thatFalse.memoize(arg0, arg1, arg2, arg3)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D) = if(pred(arg0, arg1, arg2, arg3)) thatTrue(arg0, arg1, arg2, arg3) else thatFalse(arg0, arg1, arg2, arg3)
}

case class ApplyElse4[A,B,C,D,E](thatFalse: ClosedFunctionFactory4[A,B,C,D,E]) extends PredicatedApplication4[A,B,C,D,E]{

	def elif(pred0: Predicate4[A,B,C,D]) = new ClosedApplicationElif4[A,B,C,D,E] {
		def apply(func: ClosedFunctionFactory4[A,B,C,D,E]) = ApplyEither4(pred0, func, ApplyElse4.this)
	}
	def elseApply(func: ClosedFunctionFactory4[A,B,C,D,E]) = ApplyElse4(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D) = thatFalse.query(arg0, arg1, arg2, arg3)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D) = thatFalse.memoize(arg0, arg1, arg2, arg3)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D) = thatFalse(arg0, arg1, arg2, arg3)
}
