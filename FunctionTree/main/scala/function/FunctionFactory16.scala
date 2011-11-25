package com.wheaties.function

trait ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] extends Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
  override def tupled = TupledFunction16(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P):ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P):(Q,ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q])
}

case class WrappedFunction16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](that: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15)

	override def toString() = that.toString()
}

case class TupledFunction16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](that: ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends ClosedFunctionFactory1[Tuple16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],Q]{
	def query(arg0: Tuple16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = arg0
		TupledFunction16(that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15))
	}
	def memoize(arg0: Tuple16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
		(out, TupledFunction16(outFunc))
	}

	def apply(arg0: Tuple16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] extends Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Option[Q]]{
	override def tupled = UnclosedTupledFunction16(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P):Option[ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P):Option[(Q,ClosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q])]
}

case class UnclosedTupledFunction16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](that: UnclosedFunctionFactory16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) extends UnclosedFunctionFactory1[Tuple16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],Q]{
	def query(arg0: Tuple16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15).map{result =>
			TupledFunction16(result)
		}
	}
	def memoize(arg0: Tuple16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction16(outFunc))
		}
}

	def apply(arg0: Tuple16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
	}

	override def toString() = "Tupled" + that.toString()
}
