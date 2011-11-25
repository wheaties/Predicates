package com.wheaties.application

import com.wheaties.predicate.Predicate8
import com.wheaties.function.{UnclosedFunctionFactory8, WrappedFunction8, ClosedFunctionFactory8}

trait PredicatedApplication8[A,B,C,D,E,F,G,H,I] extends ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]{
	def elif(pred0: Predicate8[A,B,C,D,E,F,G,H]):ClosedApplicationElif8[A,B,C,D,E,F,G,H,I]
	def elseApply(func: Function8[A,B,C,D,E,F,G,H,I]):PredicatedApplication8[A,B,C,D,E,F,G,H,I] = elseApply(WrappedFunction8(func))
	def elseApply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]):PredicatedApplication8[A,B,C,D,E,F,G,H,I]
}

trait ClosedApplicationElif8[A,B,C,D,E,F,G,H,I]{
	def apply(func: Function8[A,B,C,D,E,F,G,H,I]):PredicatedApplication8[A,B,C,D,E,F,G,H,I] = apply(WrappedFunction8(func))
	def apply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]):PredicatedApplication8[A,B,C,D,E,F,G,H,I]
}

trait UnclosedApplication8[A,B,C,D,E,F,G,H,I] extends UnclosedFunctionFactory8[A,B,C,D,E,F,G,H,I]{
	def elif(pred0: Predicate8[A,B,C,D,E,F,G,H]):UnclosedApplicationElif8[A,B,C,D,E,F,G,H,I]
	def elseApply(func: Function8[A,B,C,D,E,F,G,H,I]):PredicatedApplication8[A,B,C,D,E,F,G,H,I] = elseApply(WrappedFunction8(func))
	def elseApply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]):PredicatedApplication8[A,B,C,D,E,F,G,H,I]
}

trait UnclosedApplicationElif8[A,B,C,D,E,F,G,H,I]{
	def apply(func: Function8[A,B,C,D,E,F,G,H,I]):UnclosedApplication8[A,B,C,D,E,F,G,H,I] = apply(WrappedFunction8(func))
	def apply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]):UnclosedApplication8[A,B,C,D,E,F,G,H,I]
}

case class ApplyIf8[A,B,C,D,E,F,G,H,I](pred: Predicate8[A,B,C,D,E,F,G,H], 
                                       thatTrue: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]) 
  extends UnclosedApplication8[A,B,C,D,E,F,G,H,I]{
  
	def elif(pred0: Predicate8[A,B,C,D,E,F,G,H]) = new UnclosedApplicationElif8[A,B,C,D,E,F,G,H,I] {
		def apply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I])= ApplyEitherIf8(pred, thatTrue, ApplyIf8(pred0, func))
	}
	def elseApply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]) = ApplyEither8(pred, thatTrue, ApplyElse8(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) else None
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = if(pred(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H)) Some(thatTrue.memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H)) else None

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) else None
}

case class ApplyEitherIf8[A,B,C,D,E,F,G,H,I](pred: Predicate8[A,B,C,D,E,F,G,H],
                                             thatTrue: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I],
                                             thatFalse: UnclosedApplication8[A,B,C,D,E,F,G,H,I])
	extends UnclosedApplication8[A,B,C,D,E,F,G,H,I]{

	def elif(pred0: Predicate8[A,B,C,D,E,F,G,H]) = new UnclosedApplicationElif8[A,B,C,D,E,F,G,H,I] {
		def apply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I])= copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]) = ApplyEither8(pred, thatTrue, thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) Some(thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) Some(thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) Some(thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
}

case class ApplyEither8[A,B,C,D,E,F,G,H,I](pred: Predicate8[A,B,C,D,E,F,G,H],
                                           thatTrue: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I],
                                           thatFalse: PredicatedApplication8[A,B,C,D,E,F,G,H,I])
	extends PredicatedApplication8[A,B,C,D,E,F,G,H,I]{

	def elif(pred0: Predicate8[A,B,C,D,E,F,G,H]) = new ClosedApplicationElif8[A,B,C,D,E,F,G,H,I] {
		def apply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]) = copy(thatFalse = thatFalse.elif(pred0)(func))
	}
	def elseApply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]) = copy(thatFalse = thatFalse.elseApply(func))

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) thatTrue.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) else thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) thatTrue.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) else thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = if(pred(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)) thatTrue(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7) else thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
}

case class ApplyElse8[A,B,C,D,E,F,G,H,I](thatFalse: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]) 
  extends PredicatedApplication8[A,B,C,D,E,F,G,H,I]{

	def elif(pred0: Predicate8[A,B,C,D,E,F,G,H]) = new ClosedApplicationElif8[A,B,C,D,E,F,G,H,I] {
		def apply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]) = ApplyEither8(pred0, func, ApplyElse8.this)
	}
	def elseApply(func: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]) = ApplyElse8(func)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = thatFalse.query(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = thatFalse.memoize(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)

	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = thatFalse(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
}
