package com.wheaties.function

trait ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] extends Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
  override def tupled = TupledFunction18(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R):ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R):(S,ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])
}

case class WrappedFunction18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](that: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) extends ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17)

	override def toString() = that.toString()
}

case class TupledFunction18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](that: ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) extends ClosedFunctionFactory1[Tuple18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R],S]{
	def query(arg0: Tuple18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) = arg0
		TupledFunction18(that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17))
	}
	def memoize(arg0: Tuple18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17)
		(out, TupledFunction18(outFunc))
	}

	def apply(arg0: Tuple18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] extends Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Option[S]]{
	override def tupled = UnclosedTupledFunction18(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R):Option[ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R):Option[(S,ClosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S])]
}

case class UnclosedTupledFunction18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](that: UnclosedFunctionFactory18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) extends UnclosedFunctionFactory1[Tuple18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R],S]{
	def query(arg0: Tuple18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17).map{result =>
			TupledFunction18(result)
		}
	}
	def memoize(arg0: Tuple18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction18(outFunc))
		}
}

	def apply(arg0: Tuple18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17)
	}

	override def toString() = "Tupled" + that.toString()
}
