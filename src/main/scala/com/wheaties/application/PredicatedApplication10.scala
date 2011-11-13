package com.wheaties.application

import com.wheaties.predicate.Predicate10
import com.wheaties.function._

trait PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K] extends ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]{
	def elif(pred0: Predicate10[A,B,C,D,E,F,G,H,I,J]):ClosedApplicationElif10[A,B,C,D,E,F,G,H,I,J,K]
	def elseApply(func: Function10[A,B,C,D,E,F,G,H,I,J,K]):PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K] = elseApply(WrappedFunction10(func))
	def elseApply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]):PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K]
}

trait ClosedApplicationElif10[A,B,C,D,E,F,G,H,I,J,K]{
	def apply(func: Function10[A,B,C,D,E,F,G,H,I,J,K]):PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K] = apply(WrappedFunction10(func))
	def apply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]):PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K]
}

trait UnclosedApplication10[A,B,C,D,E,F,G,H,I,J,K] extends UnclosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]{
	def elif(pred0: Predicate10[A,B,C,D,E,F,G,H,I,J]):UnclosedApplicationElif10[A,B,C,D,E,F,G,H,I,J,K]
	def elseApply(func: Function10[A,B,C,D,E,F,G,H,I,J,K]):PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K] = elseApply(WrappedFunction10(func))
	def elseApply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]):PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K]
}

trait UnclosedApplicationElif10[A,B,C,D,E,F,G,H,I,J,K]{
	def apply(func: Function10[A,B,C,D,E,F,G,H,I,J,K]):UnclosedApplication10[A,B,C,D,E,F,G,H,I,J,K] = apply(WrappedFunction10(func))
	def apply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]):UnclosedApplication10[A,B,C,D,E,F,G,H,I,J,K]
}

case class ApplyIf10[A,B,C,D,E,F,G,H,I,J,K](pred: Predicate10[A,B,C,D,E,F,G,H,I,J],
                                            thatTrue: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K])
  extends UnclosedApplication10[A,B,C,D,E,F,G,H,I,J,K]{

	def elif(pred0: Predicate10[A,B,C,D,E,F,G,H,I,J]) = new UnclosedApplicationElif10[A,B,C,D,E,F,G,H,I,J,K] {
		def apply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K])= ApplyEitherIf10(pred, thatTrue, ApplyIf10(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]) = ApplyEither10(pred, thatTrue, ApplyElse10(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) else None
}

case class ApplyEitherIf10[A,B,C,D,E,F,G,H,I,J,K](pred: Predicate10[A,B,C,D,E,F,G,H,I,J],
                                                  thatTrue: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K],
                                                  thatFalse: UnclosedApplication10[A,B,C,D,E,F,G,H,I,J,K])
	extends UnclosedApplication10[A,B,C,D,E,F,G,H,I,J,K]{

	def elif(pred0: Predicate10[A,B,C,D,E,F,G,H,I,J]) = new UnclosedApplicationElif10[A,B,C,D,E,F,G,H,I,J,K] {
		def apply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]) = ApplyEither10(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
}

case class ApplyEither10[A,B,C,D,E,F,G,H,I,J,K](pred: Predicate10[A,B,C,D,E,F,G,H,I,J],
                                                thatTrue: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K],
                                                thatFalse: PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K])
	extends PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K]{

	def elif(pred0: Predicate10[A,B,C,D,E,F,G,H,I,J]) = new ClosedApplicationElif10[A,B,C,D,E,F,G,H,I,J,K] {
		def apply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
}

case class ApplyElse10[A,B,C,D,E,F,G,H,I,J,K](thatFalse: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K])
  extends PredicatedApplication10[A,B,C,D,E,F,G,H,I,J,K]{

	def elif(pred0: Predicate10[A,B,C,D,E,F,G,H,I,J]) = new ClosedApplicationElif10[A,B,C,D,E,F,G,H,I,J,K] {
		def apply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]) = ApplyEither10(pred0, func, ApplyElse10.this)
	}
	def elseApply(func: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]) = ApplyElse10(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
}
