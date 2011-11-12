package com.wheaties.function

trait ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] extends Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
  override def tupled = TupledFunction13(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M):ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M):(N,ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N])
}

case class WrappedFunction13[A,B,C,D,E,F,G,H,I,J,K,L,M,N](that: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) extends ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)

	override def toString() = that.toString()
}

case class TupledFunction13[A,B,C,D,E,F,G,H,I,J,K,L,M,N](that: ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) extends ClosedFunctionFactory1[Tuple13[A,B,C,D,E,F,G,H,I,J,K,L,M],N]{
	def query(arg0: Tuple13[A,B,C,D,E,F,G,H,I,J,K,L,M]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = arg0
		TupledFunction13(that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12))
	}
	def memoize(arg0: Tuple13[A,B,C,D,E,F,G,H,I,J,K,L,M]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
		(out, TupledFunction13(outFunc))
	}

	def apply(arg0: Tuple13[A,B,C,D,E,F,G,H,I,J,K,L,M])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N] extends Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,Option[N]]{
	override def tupled = UnclosedTupledFunction13(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M):Option[ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M):Option[(N,ClosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N])]
}

case class UnclosedTupledFunction13[A,B,C,D,E,F,G,H,I,J,K,L,M,N](that: UnclosedFunctionFactory13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) extends UnclosedFunctionFactory1[Tuple13[A,B,C,D,E,F,G,H,I,J,K,L,M],N]{
	def query(arg0: Tuple13[A,B,C,D,E,F,G,H,I,J,K,L,M]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12).map{result =>
			TupledFunction13(result)
		}
	}
	def memoize(arg0: Tuple13[A,B,C,D,E,F,G,H,I,J,K,L,M]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction13(outFunc))
		}
}

	def apply(arg0: Tuple13[A,B,C,D,E,F,G,H,I,J,K,L,M])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
	}

	override def toString() = "Tupled" + that.toString()
}
