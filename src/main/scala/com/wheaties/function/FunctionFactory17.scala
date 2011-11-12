package com.wheaties.function

trait ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] extends Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
  override def tupled = TupledFunction17(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q):ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q):(R,ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R])
}

case class WrappedFunction17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](that: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16)

	override def toString() = that.toString()
}

case class TupledFunction17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](that: ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends ClosedFunctionFactory1[Tuple17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q],R]{
	def query(arg0: Tuple17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) = arg0
		TupledFunction17(that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16))
	}
	def memoize(arg0: Tuple17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)
		(out, TupledFunction17(outFunc))
	}

	def apply(arg0: Tuple17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] extends Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Option[R]]{
	override def tupled = UnclosedTupledFunction17(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q):Option[ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q):Option[(R,ClosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R])]
}

case class UnclosedTupledFunction17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](that: UnclosedFunctionFactory17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) extends UnclosedFunctionFactory1[Tuple17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q],R]{
	def query(arg0: Tuple17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16).map{result =>
			TupledFunction17(result)
		}
	}
	def memoize(arg0: Tuple17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction17(outFunc))
		}
}

	def apply(arg0: Tuple17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)
	}

	override def toString() = "Tupled" + that.toString()
}