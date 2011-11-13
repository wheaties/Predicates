package com.wheaties.application

import com.wheaties.predicate.Predicate6
import com.wheaties.function.{UnclosedFunctionFactory6, WrappedFunction6, ClosedFunctionFactory6}

trait PredicatedApplication6[A,B,C,D,E,F,G] extends ClosedFunctionFactory6[A,B,C,D,E,F,G]{
	def elif(pred0: Predicate6[A,B,C,D,E,F]):ClosedApplicationElif6[A,B,C,D,E,F,G]
	def elseApply(func: Function6[A,B,C,D,E,F,G]):PredicatedApplication6[A,B,C,D,E,F,G] = elseApply(WrappedFunction6(func))
	def elseApply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G]):PredicatedApplication6[A,B,C,D,E,F,G]
}

trait ClosedApplicationElif6[A,B,C,D,E,F,G]{
	def apply(func: Function6[A,B,C,D,E,F,G]):PredicatedApplication6[A,B,C,D,E,F,G] = apply(WrappedFunction6(func))
	def apply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G]):PredicatedApplication6[A,B,C,D,E,F,G]
}

trait UnclosedApplication6[A,B,C,D,E,F,G] extends UnclosedFunctionFactory6[A,B,C,D,E,F,G]{
	def elif(pred0: Predicate6[A,B,C,D,E,F]):UnclosedApplicationElif6[A,B,C,D,E,F,G]
	def elseApply(func: Function6[A,B,C,D,E,F,G]):PredicatedApplication6[A,B,C,D,E,F,G] = elseApply(WrappedFunction6(func))
	def elseApply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G]):PredicatedApplication6[A,B,C,D,E,F,G]
}

trait UnclosedApplicationElif6[A,B,C,D,E,F,G]{
	def apply(func: Function6[A,B,C,D,E,F,G]):UnclosedApplication6[A,B,C,D,E,F,G] = apply(WrappedFunction6(func))
	def apply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G]):UnclosedApplication6[A,B,C,D,E,F,G]
}

case class ApplyIf6[A,B,C,D,E,F,G](pred: Predicate6[A,B,C,D,E,F], 
                                   thatTrue: ClosedFunctionFactory6[A,B,C,D,E,F,G]) 
  extends UnclosedApplication6[A,B,C,D,E,F,G]{
  
	def elif(pred0: Predicate6[A,B,C,D,E,F]) = new UnclosedApplicationElif6[A,B,C,D,E,F,G] {
		def apply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G])= ApplyEitherIf6(pred, thatTrue, ApplyIf6(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G]) = ApplyEither6(pred, thatTrue, ApplyElse6(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5)) else None
}

case class ApplyEitherIf6[A,B,C,D,E,F,G](pred: Predicate6[A,B,C,D,E,F],
                                         thatTrue: ClosedFunctionFactory6[A,B,C,D,E,F,G],
                                         thatFalse: UnclosedApplication6[A,B,C,D,E,F,G])
	extends UnclosedApplication6[A,B,C,D,E,F,G]{

	def elif(pred0: Predicate6[A,B,C,D,E,F]) = new UnclosedApplicationElif6[A,B,C,D,E,F,G] {
		def apply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G]) = ApplyEither6(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5)
}

case class ApplyEither6[A,B,C,D,E,F,G](pred: Predicate6[A,B,C,D,E,F],
                                       thatTrue: ClosedFunctionFactory6[A,B,C,D,E,F,G],
                                       thatFalse: PredicatedApplication6[A,B,C,D,E,F,G])
	extends PredicatedApplication6[A,B,C,D,E,F,G]{

	def elif(pred0: Predicate6[A,B,C,D,E,F]) = new ClosedApplicationElif6[A,B,C,D,E,F,G] {
		def apply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5)
}

case class ApplyElse6[A,B,C,D,E,F,G](thatFalse: ClosedFunctionFactory6[A,B,C,D,E,F,G]) 
  extends PredicatedApplication6[A,B,C,D,E,F,G]{

	def elif(pred0: Predicate6[A,B,C,D,E,F]) = new ClosedApplicationElif6[A,B,C,D,E,F,G] {
		def apply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G]) = ApplyEither6(pred0, func, ApplyElse6.this)
	}
	def elseApply(func: ClosedFunctionFactory6[A,B,C,D,E,F,G]) = ApplyElse6(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5)
}
