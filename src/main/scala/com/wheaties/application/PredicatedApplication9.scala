package com.wheaties.application

import com.wheaties.predicate.Predicate9
import com.wheaties.function.{UnclosedFunctionFactory9, WrappedFunction9, ClosedFunctionFactory9}

trait PredicatedApplication9[A,B,C,D,E,F,G,H,I,J] extends ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]{
	def elif(pred0: Predicate9[A,B,C,D,E,F,G,H,I]):ClosedApplicationElif9[A,B,C,D,E,F,G,H,I,J]
	def elseApply(func: Function9[A,B,C,D,E,F,G,H,I,J]):PredicatedApplication9[A,B,C,D,E,F,G,H,I,J] = elseApply(WrappedFunction9(func))
	def elseApply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]):PredicatedApplication9[A,B,C,D,E,F,G,H,I,J]
}

trait ClosedApplicationElif9[A,B,C,D,E,F,G,H,I,J]{
	def apply(func: Function9[A,B,C,D,E,F,G,H,I,J]):PredicatedApplication9[A,B,C,D,E,F,G,H,I,J] = apply(WrappedFunction9(func))
	def apply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]):PredicatedApplication9[A,B,C,D,E,F,G,H,I,J]
}

trait UnclosedApplication9[A,B,C,D,E,F,G,H,I,J] extends UnclosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]{
	def elif(pred0: Predicate9[A,B,C,D,E,F,G,H,I]):UnclosedApplicationElif9[A,B,C,D,E,F,G,H,I,J]
	def elseApply(func: Function9[A,B,C,D,E,F,G,H,I,J]):PredicatedApplication9[A,B,C,D,E,F,G,H,I,J] = elseApply(WrappedFunction9(func))
	def elseApply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]):PredicatedApplication9[A,B,C,D,E,F,G,H,I,J]
}

trait UnclosedApplicationElif9[A,B,C,D,E,F,G,H,I,J]{
	def apply(func: Function9[A,B,C,D,E,F,G,H,I,J]):UnclosedApplication9[A,B,C,D,E,F,G,H,I,J] = apply(WrappedFunction9(func))
	def apply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]):UnclosedApplication9[A,B,C,D,E,F,G,H,I,J]
}

case class ApplyIf9[A,B,C,D,E,F,G,H,I,J](pred: Predicate9[A,B,C,D,E,F,G,H,I], thatTrue: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]) extends UnclosedApplication9[A,B,C,D,E,F,G,H,I,J]{
	def elif(pred0: Predicate9[A,B,C,D,E,F,G,H,I]) = new UnclosedApplicationElif9[A,B,C,D,E,F,G,H,I,J] {
		def apply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J])= ApplyEitherIf9(pred, thatTrue, ApplyIf9(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]) = ApplyEither9(pred, thatTrue, ApplyElse9(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) else None
}

case class ApplyEitherIf9[A,B,C,D,E,F,G,H,I,J](pred: Predicate9[A,B,C,D,E,F,G,H,I],
                                               thatTrue: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J],
                                               thatFalse: UnclosedApplication9[A,B,C,D,E,F,G,H,I,J])
	extends UnclosedApplication9[A,B,C,D,E,F,G,H,I,J]{

	def elif(pred0: Predicate9[A,B,C,D,E,F,G,H,I]) = new UnclosedApplicationElif9[A,B,C,D,E,F,G,H,I,J] {
		def apply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]) = ApplyEither9(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
}

case class ApplyEither9[A,B,C,D,E,F,G,H,I,J](pred: Predicate9[A,B,C,D,E,F,G,H,I],
                                             thatTrue: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J],
                                             thatFalse: PredicatedApplication9[A,B,C,D,E,F,G,H,I,J])
	extends PredicatedApplication9[A,B,C,D,E,F,G,H,I,J]{

	def elif(pred0: Predicate9[A,B,C,D,E,F,G,H,I]) = new ClosedApplicationElif9[A,B,C,D,E,F,G,H,I,J] {
		def apply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
}

case class ApplyElse9[A,B,C,D,E,F,G,H,I,J](thatFalse: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]) 
  extends PredicatedApplication9[A,B,C,D,E,F,G,H,I,J]{

	def elif(pred0: Predicate9[A,B,C,D,E,F,G,H,I]) = new ClosedApplicationElif9[A,B,C,D,E,F,G,H,I,J] {
		def apply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]) = ApplyEither9(pred0, func, ApplyElse9.this)
	}
	def elseApply(func: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]) = ApplyElse9(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
}
