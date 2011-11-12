package com.wheaties.function

trait ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] extends Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
  override def tupled = TupledFunction14(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N):ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N):(O,ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O])
}

case class WrappedFunction14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](that: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13)

	override def toString() = that.toString()
}

case class TupledFunction14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](that: ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends ClosedFunctionFactory1[Tuple14[A,B,C,D,E,F,G,H,I,J,K,L,M,N],O]{
	def query(arg0: Tuple14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = arg0
		TupledFunction14(that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13))
	}
	def memoize(arg0: Tuple14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
		(out, TupledFunction14(outFunc))
	}

	def apply(arg0: Tuple14[A,B,C,D,E,F,G,H,I,J,K,L,M,N])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] extends Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Option[O]]{
	override def tupled = UnclosedTupledFunction14(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N):Option[ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N):Option[(O,ClosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O])]
}

case class UnclosedTupledFunction14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](that: UnclosedFunctionFactory14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) extends UnclosedFunctionFactory1[Tuple14[A,B,C,D,E,F,G,H,I,J,K,L,M,N],O]{
	def query(arg0: Tuple14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13).map{result =>
			TupledFunction14(result)
		}
	}
	def memoize(arg0: Tuple14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction14(outFunc))
		}
}

	def apply(arg0: Tuple14[A,B,C,D,E,F,G,H,I,J,K,L,M,N])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
	}

	override def toString() = "Tupled" + that.toString()
}
