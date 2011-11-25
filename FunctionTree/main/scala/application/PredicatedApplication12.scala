package com.wheaties.application

import com.wheaties.predicate.Predicate12
import com.wheaties.function._

trait PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M] extends ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]{
	def elif(pred0: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]):ClosedApplicationElif12[A,B,C,D,E,F,G,H,I,J,K,L,M]
	def elseApply(func: Function12[A,B,C,D,E,F,G,H,I,J,K,L,M]):PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M] = elseApply(WrappedFunction12(func))
	def elseApply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]):PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M]
}

trait ClosedApplicationElif12[A,B,C,D,E,F,G,H,I,J,K,L,M]{
	def apply(func: Function12[A,B,C,D,E,F,G,H,I,J,K,L,M]):PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M] = apply(WrappedFunction12(func))
	def apply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]):PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M]
}

trait UnclosedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M] extends UnclosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]{
	def elif(pred0: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]):UnclosedApplicationElif12[A,B,C,D,E,F,G,H,I,J,K,L,M]
	def elseApply(func: Function12[A,B,C,D,E,F,G,H,I,J,K,L,M]):PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M] = elseApply(WrappedFunction12(func))
	def elseApply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]):PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M]
}

trait UnclosedApplicationElif12[A,B,C,D,E,F,G,H,I,J,K,L,M]{
	def apply(func: Function12[A,B,C,D,E,F,G,H,I,J,K,L,M]):UnclosedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M] = apply(WrappedFunction12(func))
	def apply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]):UnclosedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M]
}

case class ApplyIf12[A,B,C,D,E,F,G,H,I,J,K,L,M](pred: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L], 
                                                thatTrue: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]) 
  extends UnclosedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M]{
  
	def elif(pred0: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) = new UnclosedApplicationElif12[A,B,C,D,E,F,G,H,I,J,K,L,M] {
		def apply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M])= ApplyEitherIf12(pred, thatTrue, ApplyIf12(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]) = ApplyEither12(pred, thatTrue, ApplyElse12(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) else None
}

case class ApplyEitherIf12[A,B,C,D,E,F,G,H,I,J,K,L,M](pred: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L],
                                                      thatTrue: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M],
                                                      thatFalse: UnclosedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M])
	extends UnclosedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M]{

	def elif(pred0: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) = new UnclosedApplicationElif12[A,B,C,D,E,F,G,H,I,J,K,L,M] {
		def apply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]) = ApplyEither12(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
}

case class ApplyEither12[A,B,C,D,E,F,G,H,I,J,K,L,M](pred: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L],
                                                    thatTrue: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M],
                                                    thatFalse: PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M])
	extends PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M]{

	def elif(pred0: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) = new ClosedApplicationElif12[A,B,C,D,E,F,G,H,I,J,K,L,M] {
		def apply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
}

case class ApplyElse12[A,B,C,D,E,F,G,H,I,J,K,L,M](thatFalse: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]) 
  extends PredicatedApplication12[A,B,C,D,E,F,G,H,I,J,K,L,M]{

	def elif(pred0: Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]) = new ClosedApplicationElif12[A,B,C,D,E,F,G,H,I,J,K,L,M] {
		def apply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]) = ApplyEither12(pred0, func, ApplyElse12.this)
	}
	def elseApply(func: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]) = ApplyElse12(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
}
