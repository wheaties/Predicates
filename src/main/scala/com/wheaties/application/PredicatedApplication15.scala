package com.wheaties.application

import com.wheaties.predicate.Predicate15
import com.wheaties.function._

trait PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] extends ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
	def elif(pred0: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):ClosedApplicationElif15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
	def elseApply(func: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] = elseApply(WrappedFunction15(func))
	def elseApply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
}

trait ClosedApplicationElif15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
	def apply(func: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] = apply(WrappedFunction15(func))
	def apply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
}

trait UnclosedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] extends UnclosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
	def elif(pred0: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):UnclosedApplicationElif15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
	def elseApply(func: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] = elseApply(WrappedFunction15(func))
	def elseApply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
}

trait UnclosedApplicationElif15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
	def apply(func: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):UnclosedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] = apply(WrappedFunction15(func))
	def apply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):UnclosedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
}

case class ApplyIf15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], thatTrue: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends UnclosedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
	def elif(pred0: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) = new UnclosedApplicationElif15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] {
		def apply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P])= ApplyEitherIf15(pred, thatTrue, ApplyIf15(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) = ApplyEither15(pred, thatTrue, ApplyElse15(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) else None
}

case class ApplyEitherIf15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],
                                                            thatTrue: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],
                                                            thatFalse: UnclosedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P])
	extends UnclosedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{

	def elif(pred0: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) = new UnclosedApplicationElif15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] {
		def apply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) = ApplyEither15(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
}

case class ApplyEither15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],
                                                          thatTrue: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],
                                                          thatFalse: PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P])
	extends PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{

	def elif(pred0: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) = new ClosedApplicationElif15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] {
		def apply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
}

case class ApplyElse15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](thatFalse: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) 
  extends PredicatedApplication15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{

	def elif(pred0: Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) = new ClosedApplicationElif15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] {
		def apply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) = ApplyEither15(pred0, func, ApplyElse15.this)
	}
	def elseApply(func: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) = ApplyElse15(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)
}
