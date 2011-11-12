package com.wheaties.function

trait ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M] extends Function12[A,B,C,D,E,F,G,H,I,J,K,L,M]{
  override def tupled = TupledFunction12(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L):ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L):(M,ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M])
}

case class WrappedFunction12[A,B,C,D,E,F,G,H,I,J,K,L,M](that: Function12[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)

	override def toString() = that.toString()
}

case class TupledFunction12[A,B,C,D,E,F,G,H,I,J,K,L,M](that: ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends ClosedFunctionFactory1[Tuple12[A,B,C,D,E,F,G,H,I,J,K,L],M]{
	def query(arg0: Tuple12[A,B,C,D,E,F,G,H,I,J,K,L]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = arg0
		TupledFunction12(that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))
	}
	def memoize(arg0: Tuple12[A,B,C,D,E,F,G,H,I,J,K,L]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
		(out, TupledFunction12(outFunc))
	}

	def apply(arg0: Tuple12[A,B,C,D,E,F,G,H,I,J,K,L])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M] extends Function12[A,B,C,D,E,F,G,H,I,J,K,L,Option[M]]{
	override def tupled = UnclosedTupledFunction12(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L):Option[ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L):Option[(M,ClosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M])]
}

case class UnclosedTupledFunction12[A,B,C,D,E,F,G,H,I,J,K,L,M](that: UnclosedFunctionFactory12[A,B,C,D,E,F,G,H,I,J,K,L,M]) extends UnclosedFunctionFactory1[Tuple12[A,B,C,D,E,F,G,H,I,J,K,L],M]{
	def query(arg0: Tuple12[A,B,C,D,E,F,G,H,I,J,K,L]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11).map{result =>
			TupledFunction12(result)
		}
	}
	def memoize(arg0: Tuple12[A,B,C,D,E,F,G,H,I,J,K,L]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction12(outFunc))
		}
  }

	def apply(arg0: Tuple12[A,B,C,D,E,F,G,H,I,J,K,L])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
	}

	override def toString() = "Tupled" + that.toString()
}
