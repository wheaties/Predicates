package com.wheaties.application

import com.wheaties.predicate.Predicate20
import com.wheaties.function._

trait PredicatedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] extends ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
	def elif(pred0: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):ClosedApplicationElif20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]
	def elseApply(func: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]):PredicatedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] = elseApply(WrappedFunction20(func))
	def elseApply(func: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]):PredicatedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]
}

trait ClosedApplicationElif20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
	def apply(func: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]):PredicatedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] = apply(WrappedFunction20(func))
	def apply(func: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]):PredicatedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]
}

trait UnclosedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] extends UnclosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
	def elif(pred0: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]):UnclosedApplicationElif20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]
	def elseApply(func: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]):PredicatedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] = elseApply(WrappedFunction20(func))
	def elseApply(func: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]):PredicatedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]
}

trait UnclosedApplicationElif20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
	def apply(func: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]):UnclosedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] = apply(WrappedFunction20(func))
	def apply(func: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]):UnclosedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]
}

case class ApplyIf20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], thatTrue: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) extends UnclosedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
	def elif(pred0: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = new UnclosedApplicationElif20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] {
		def apply(func: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U])= ApplyEitherIf20(pred, thatTrue, ApplyIf20(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = ApplyEither20(pred, thatTrue, ApplyElse20(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) else None
}

case class ApplyEitherIf20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T],
                                                                      thatTrue: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U],
                                                                      thatFalse: UnclosedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U])
	extends UnclosedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{

	def elif(pred0: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = new UnclosedApplicationElif20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] {
		def apply(func: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = ApplyEither20(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)
}

case class ApplyEither20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T],
                                                                    thatTrue: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U],
                                                                    thatFalse: PredicatedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U])
	extends PredicatedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{

	def elif(pred0: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = new ClosedApplicationElif20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] {
		def apply(func: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)
}

case class ApplyElse20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](thatFalse: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) 
  extends PredicatedApplication20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{

	def elif(pred0: Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = new ClosedApplicationElif20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] {
		def apply(func: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = ApplyEither20(pred0, func, ApplyElse20.this)
	}
	def elseApply(func: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = ApplyElse20(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19)
}
