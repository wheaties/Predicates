package com.wheaties.function

trait ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] extends Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
  override def tupled = TupledFunction15(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O):ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O):(P,ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P])
}

case class WrappedFunction15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](that: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14)

	override def toString() = that.toString()
}

case class TupledFunction15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](that: ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends ClosedFunctionFactory1[Tuple15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],P]{
	def query(arg0: Tuple15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = arg0
		TupledFunction15(that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
	}
	def memoize(arg0: Tuple15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)
		(out, TupledFunction15(outFunc))
	}

	def apply(arg0: Tuple15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] extends Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Option[P]]{
	override def tupled = UnclosedTupledFunction15(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O):Option[ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O):Option[(P,ClosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P])]
}

case class UnclosedTupledFunction15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](that: UnclosedFunctionFactory15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) extends UnclosedFunctionFactory1[Tuple15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O],P]{
	def query(arg0: Tuple15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14).map{result =>
			TupledFunction15(result)
		}
	}
	def memoize(arg0: Tuple15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction15(outFunc))
		}
}

	def apply(arg0: Tuple15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)
	}

	override def toString() = "Tupled" + that.toString()
}
