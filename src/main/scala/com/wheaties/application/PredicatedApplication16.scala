package com.wheaties.application

import com.wheaties.predicate.Predicate16
import com.wheaties.function._

trait PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] extends ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
	def elif(pred0: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):ClosedApplicationElif16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
	def elseApply(func: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] = elseApply(WrappedFunction16(func))
	def elseApply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
}

trait ClosedApplicationElif16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
	def apply(func: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] = apply(WrappedFunction16(func))
	def apply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
}

trait UnclosedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] extends UnclosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
	def elif(pred0: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]):UnclosedApplicationElif16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
	def elseApply(func: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] = elseApply(WrappedFunction16(func))
	def elseApply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
}

trait UnclosedApplicationElif16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
	def apply(func: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):UnclosedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] = apply(WrappedFunction16(func))
	def apply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]):UnclosedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
}

case class ApplyIf16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], thatTrue: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends UnclosedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
	def elif(pred0: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) = new UnclosedApplicationElif16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] {
		def apply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q])= ApplyEitherIf16(pred, thatTrue, ApplyIf16(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) = ApplyEither16(pred, thatTrue, ApplyElse16(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) else None
}

case class ApplyEitherIf16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],
                                                              thatTrue: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q],
                                                              thatFalse: UnclosedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q])
	extends UnclosedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{

	def elif(pred0: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) = new UnclosedApplicationElif16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] {
		def apply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) = ApplyEither16(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
}

case class ApplyEither16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],
                                                            thatTrue: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q],
                                                            thatFalse: PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q])
	extends PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{

	def elif(pred0: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) = new ClosedApplicationElif16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] {
		def apply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
}

case class ApplyElse16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](thatFalse: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends PredicatedApplication16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{

	def elif(pred0: Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) = new ClosedApplicationElif16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] {
		def apply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) = ApplyEither16(pred0, func, ApplyElse16.this)
	}
	def elseApply(func: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) = ApplyElse16(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15)
}
