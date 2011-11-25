package com.wheaties.application

import com.wheaties.predicate.Predicate14
import com.wheaties.function.{UnclosedFunctionFactory14, WrappedFunction14, ClosedFunctionFactory14}

trait PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] extends ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
	def elif(pred0: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):ClosedApplicationElif14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
	def elseApply(func: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] = elseApply(WrappedFunction14(func))
	def elseApply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
}

trait ClosedApplicationElif14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
	def apply(func: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] = apply(WrappedFunction14(func))
	def apply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
}

trait UnclosedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] extends UnclosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
	def elif(pred0: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):UnclosedApplicationElif14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
	def elseApply(func: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] = elseApply(WrappedFunction14(func))
	def elseApply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
}

trait UnclosedApplicationElif14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
	def apply(func: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):UnclosedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] = apply(WrappedFunction14(func))
	def apply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]):UnclosedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
}

case class ApplyIf14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N], thatTrue: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends UnclosedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
	def elif(pred0: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = new UnclosedApplicationElif14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] {
		def apply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O])= ApplyEitherIf14(pred, thatTrue, ApplyIf14(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) = ApplyEither14(pred, thatTrue, ApplyElse14(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) else None
}

case class ApplyEitherIf14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N],
                                                          thatTrue: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],
                                                          thatFalse: UnclosedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O])
	extends UnclosedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{

	def elif(pred0: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = new UnclosedApplicationElif14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] {
		def apply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) = ApplyEither14(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
}

case class ApplyEither14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N],
                                                        thatTrue: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],
                                                        thatFalse: PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O])
	extends PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{

	def elif(pred0: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = new ClosedApplicationElif14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] {
		def apply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
}

case class ApplyElse14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](thatFalse: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) 
  extends PredicatedApplication14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{

	def elif(pred0: Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = new ClosedApplicationElif14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] {
		def apply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) = ApplyEither14(pred0, func, ApplyElse14.this)
	}
	def elseApply(func: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) = ApplyElse14(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)
}
