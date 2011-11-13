package com.wheaties.application

import com.wheaties.predicate.Predicate13
import com.wheaties.function._

trait PredicatedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] extends ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
	def elif(pred0: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]):ClosedApplicationElif13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
	def elseApply(func: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):PredicatedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] = elseApply(WrappedFunction13(func))
	def elseApply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):PredicatedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
}

trait ClosedApplicationElif13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
	def apply(func: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):PredicatedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] = apply(WrappedFunction13(func))
	def apply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):PredicatedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
}

trait UnclosedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] extends UnclosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
	def elif(pred0: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]):UnclosedApplicationElif13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
	def elseApply(func: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):PredicatedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] = elseApply(WrappedFunction13(func))
	def elseApply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):PredicatedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
}

trait UnclosedApplicationElif13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
	def apply(func: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):UnclosedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] = apply(WrappedFunction13(func))
	def apply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]):UnclosedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
}

case class ApplyIf13[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M], 
                                                  thatTrue: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) 
  extends UnclosedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
  
	def elif(pred0: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) = new UnclosedApplicationElif13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] {
		def apply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N])= ApplyEitherIf13(pred, thatTrue, ApplyIf13(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = ApplyEither13(pred, thatTrue, ApplyElse13(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) else None
}

case class ApplyEitherIf13[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M],
                                                        thatTrue: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N],
                                                        thatFalse: UnclosedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N])
	extends UnclosedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{

	def elif(pred0: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) = new UnclosedApplicationElif13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] {
		def apply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = ApplyEither13(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
}

case class ApplyEither13[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M],
                                                      thatTrue: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N],
                                                      thatFalse: PredicatedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N])
	extends PredicatedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{

	def elif(pred0: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) = new ClosedApplicationElif13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] {
		def apply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
}

case class ApplyElse13[A,B,C,D,E,F,G,H,I,J,K,L,M,N](thatFalse: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N])
  extends PredicatedApplication13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{

	def elif(pred0: Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]) = new ClosedApplicationElif13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] {
		def apply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = ApplyEither13(pred0, func, ApplyElse13.this)
	}
	def elseApply(func: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = ApplyElse13(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
}
