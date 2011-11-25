package com.wheaties.function

trait ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] extends Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
  override def tupled = TupledFunction19(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S):ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S):(T,ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T])
}

case class WrappedFunction19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](that: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18)

	override def toString() = that.toString()
}

case class TupledFunction19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](that: ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends ClosedFunctionFactory1[Tuple19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],T]{
	def query(arg0: Tuple19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) = arg0
		TupledFunction19(that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18))
	}
	def memoize(arg0: Tuple19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18)
		(out, TupledFunction19(outFunc))
	}

	def apply(arg0: Tuple19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] extends Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Option[T]]{
	override def tupled = UnclosedTupledFunction19(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S):Option[ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S):Option[(T,ClosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T])]
}

case class UnclosedTupledFunction19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](that: UnclosedFunctionFactory19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) extends UnclosedFunctionFactory1[Tuple19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],T]{
	def query(arg0: Tuple19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18).map{result =>
			TupledFunction19(result)
		}
	}
	def memoize(arg0: Tuple19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction19(outFunc))
		}
}

	def apply(arg0: Tuple19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18)
	}

	override def toString() = "Tupled" + that.toString()
}
