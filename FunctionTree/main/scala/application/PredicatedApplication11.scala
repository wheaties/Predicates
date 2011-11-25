package com.wheaties.application

import com.wheaties.predicate.Predicate11
import com.wheaties.function._

trait PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L] extends ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]{
	def elif(pred0: Predicate11[A,B,C,D,E,F,G,H,I,J,K]):ClosedApplicationElif11[A,B,C,D,E,F,G,H,I,J,K,L]
	def elseApply(func: Function11[A,B,C,D,E,F,G,H,I,J,K,L]):PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L] = elseApply(WrappedFunction11(func))
	def elseApply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]):PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L]
}

trait ClosedApplicationElif11[A,B,C,D,E,F,G,H,I,J,K,L]{
	def apply(func: Function11[A,B,C,D,E,F,G,H,I,J,K,L]):PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L] = apply(WrappedFunction11(func))
	def apply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]):PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L]
}

trait UnclosedApplication11[A,B,C,D,E,F,G,H,I,J,K,L] extends UnclosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]{
	def elif(pred0: Predicate11[A,B,C,D,E,F,G,H,I,J,K]):UnclosedApplicationElif11[A,B,C,D,E,F,G,H,I,J,K,L]
	def elseApply(func: Function11[A,B,C,D,E,F,G,H,I,J,K,L]):PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L] = elseApply(WrappedFunction11(func))
	def elseApply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]):PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L]
}

trait UnclosedApplicationElif11[A,B,C,D,E,F,G,H,I,J,K,L]{
	def apply(func: Function11[A,B,C,D,E,F,G,H,I,J,K,L]):UnclosedApplication11[A,B,C,D,E,F,G,H,I,J,K,L] = apply(WrappedFunction11(func))
	def apply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]):UnclosedApplication11[A,B,C,D,E,F,G,H,I,J,K,L]
}

case class ApplyIf11[A,B,C,D,E,F,G,H,I,J,K,L](pred: Predicate11[A,B,C,D,E,F,G,H,I,J,K], 
                                              thatTrue: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]) 
  extends UnclosedApplication11[A,B,C,D,E,F,G,H,I,J,K,L]{
  
	def elif(pred0: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) = new UnclosedApplicationElif11[A,B,C,D,E,F,G,H,I,J,K,L] {
		def apply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L])= ApplyEitherIf11(pred, thatTrue, ApplyIf11(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]) = ApplyEither11(pred, thatTrue, ApplyElse11(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) else None
}

case class ApplyEitherIf11[A,B,C,D,E,F,G,H,I,J,K,L](pred: Predicate11[A,B,C,D,E,F,G,H,I,J,K],
                                                    thatTrue: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L],
                                                    thatFalse: UnclosedApplication11[A,B,C,D,E,F,G,H,I,J,K,L])
	extends UnclosedApplication11[A,B,C,D,E,F,G,H,I,J,K,L]{

	def elif(pred0: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) = new UnclosedApplicationElif11[A,B,C,D,E,F,G,H,I,J,K,L] {
		def apply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]) = ApplyEither11(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
}

case class ApplyEither11[A,B,C,D,E,F,G,H,I,J,K,L](pred: Predicate11[A,B,C,D,E,F,G,H,I,J,K],
                                                  thatTrue: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L],
                                                  thatFalse: PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L])
	extends PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L]{

	def elif(pred0: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) = new ClosedApplicationElif11[A,B,C,D,E,F,G,H,I,J,K,L] {
		def apply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
}

case class ApplyElse11[A,B,C,D,E,F,G,H,I,J,K,L](thatFalse: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]) 
  extends PredicatedApplication11[A,B,C,D,E,F,G,H,I,J,K,L]{

	def elif(pred0: Predicate11[A,B,C,D,E,F,G,H,I,J,K]) = new ClosedApplicationElif11[A,B,C,D,E,F,G,H,I,J,K,L] {
		def apply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]) = ApplyEither11(pred0, func, ApplyElse11.this)
	}
	def elseApply(func: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]) = ApplyElse11(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
}
