package com.wheaties.application

import com.wheaties.predicate.Predicate19
import com.wheaties.function.{UnclosedFunctionFactory19, WrappedFunction19, ClosedFunctionFactory19}

trait PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] extends ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
	def elif(pred0: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):ClosedApplicationElif19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
	def elseApply(func: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] = elseApply(WrappedFunction19(func))
	def elseApply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
}

trait ClosedApplicationElif19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
	def apply(func: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] = apply(WrappedFunction19(func))
	def apply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
}

trait UnclosedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] extends UnclosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
	def elif(pred0: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]):UnclosedApplicationElif19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
	def elseApply(func: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] = elseApply(WrappedFunction19(func))
	def elseApply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
}

trait UnclosedApplicationElif19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
	def apply(func: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):UnclosedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] = apply(WrappedFunction19(func))
	def apply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):UnclosedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
}

case class ApplyIf19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],
                                                              thatTrue: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T])
  extends UnclosedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{

	def elif(pred0: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = new UnclosedApplicationElif19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] {
		def apply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T])= ApplyEitherIf19(pred, thatTrue, ApplyIf19(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = ApplyEither19(pred, thatTrue, ApplyElse19(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) else None
}

case class ApplyEitherIf19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],
                                                                    thatTrue: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T],
                                                                    thatFalse: UnclosedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T])
	extends UnclosedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{

	def elif(pred0: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = new UnclosedApplicationElif19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] {
		def apply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = ApplyEither19(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)
}

case class ApplyEither19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],
                                                                  thatTrue: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T],
                                                                  thatFalse: PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T])
	extends PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{

	def elif(pred0: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = new ClosedApplicationElif19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] {
		def apply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)
}

case class ApplyElse19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](thatFalse: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) 
  extends PredicatedApplication19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{

	def elif(pred0: Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = new ClosedApplicationElif19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] {
		def apply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = ApplyEither19(pred0, func, ApplyElse19.this)
	}
	def elseApply(func: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = ApplyElse19(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18)
}
