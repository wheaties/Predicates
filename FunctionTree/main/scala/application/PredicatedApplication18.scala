package com.wheaties.application

import com.wheaties.predicate.Predicate18
import com.wheaties.function._

trait PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] extends ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
	def elif(pred0: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):ClosedApplicationElif18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
	def elseApply(func: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] = elseApply(WrappedFunction18(func))
	def elseApply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
}

trait ClosedApplicationElif18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
	def apply(func: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] = apply(WrappedFunction18(func))
	def apply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
}

trait UnclosedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] extends UnclosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
	def elif(pred0: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):UnclosedApplicationElif18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
	def elseApply(func: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] = elseApply(WrappedFunction18(func))
	def elseApply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
}

trait UnclosedApplicationElif18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
	def apply(func: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):UnclosedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] = apply(WrappedFunction18(func))
	def apply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):UnclosedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
}

case class ApplyIf18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R],
                                                            thatTrue: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])
  extends UnclosedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{

	def elif(pred0: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = new UnclosedApplicationElif18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] {
		def apply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])= ApplyEitherIf18(pred, thatTrue, ApplyIf18(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = ApplyEither18(pred, thatTrue, ApplyElse18(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) else None
}

case class ApplyEitherIf18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R],
                                                                  thatTrue: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],
                                                                  thatFalse: UnclosedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])
	extends UnclosedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{

	def elif(pred0: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = new UnclosedApplicationElif18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] {
		def apply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = ApplyEither18(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)
}

case class ApplyEither18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R],
                                                                thatTrue: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],
                                                                thatFalse: PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])
	extends PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{

	def elif(pred0: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = new ClosedApplicationElif18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] {
		def apply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)
}

case class ApplyElse18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](thatFalse: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])
  extends PredicatedApplication18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{

	def elif(pred0: Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = new ClosedApplicationElif18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] {
		def apply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = ApplyEither18(pred0, func, ApplyElse18.this)
	}
	def elseApply(func: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = ApplyElse18(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17)
}
