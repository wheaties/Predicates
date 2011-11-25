package com.wheaties.application

import com.wheaties.predicate.Predicate17
import com.wheaties.function._

trait PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] extends ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
	def elif(pred0: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):ClosedApplicationElif17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
	def elseApply(func: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] = elseApply(WrappedFunction17(func))
	def elseApply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
}

trait ClosedApplicationElif17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
	def apply(func: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] = apply(WrappedFunction17(func))
	def apply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
}

trait UnclosedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] extends UnclosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
	def elif(pred0: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):UnclosedApplicationElif17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
	def elseApply(func: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] = elseApply(WrappedFunction17(func))
	def elseApply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
}

trait UnclosedApplicationElif17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
	def apply(func: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):UnclosedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] = apply(WrappedFunction17(func))
	def apply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]):UnclosedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
}

case class ApplyIf17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], thatTrue: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends UnclosedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
	def elif(pred0: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) = new UnclosedApplicationElif17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] {
		def apply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R])= ApplyEitherIf17(pred, thatTrue, ApplyIf17(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = ApplyEither17(pred, thatTrue, ApplyElse17(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) else None
}

case class ApplyEitherIf17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q],
                                                                thatTrue: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R],
                                                                thatFalse: UnclosedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R])
	extends UnclosedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{

	def elif(pred0: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) = new UnclosedApplicationElif17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] {
		def apply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = ApplyEither17(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
}

case class ApplyEither17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q],
                                                              thatTrue: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R],
                                                              thatFalse: PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R])
	extends PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{

	def elif(pred0: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) = new ClosedApplicationElif17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] {
		def apply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
}

case class ApplyElse17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](thatFalse: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R])
  extends PredicatedApplication17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{

	def elif(pred0: Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) = new ClosedApplicationElif17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] {
		def apply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = ApplyEither17(pred0, func, ApplyElse17.this)
	}
	def elseApply(func: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = ApplyElse17(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16)
}
