package com.wheaties.function

trait ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L] extends Function11[A,B,C,D,E,F,G,H,I,J,K,L]{
  override def tupled = TupledFunction11(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K):ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K):(L,ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L])
}

case class WrappedFunction11[A,B,C,D,E,F,G,H,I,J,K,L](that: Function11[A,B,C,D,E,F,G,H,I,J,K,L]) extends ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)

	override def toString() = that.toString()
}

case class TupledFunction11[A,B,C,D,E,F,G,H,I,J,K,L](that: ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]) extends ClosedFunctionFactory1[Tuple11[A,B,C,D,E,F,G,H,I,J,K],L]{
	def query(arg0: Tuple11[A,B,C,D,E,F,G,H,I,J,K]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = arg0
		TupledFunction11(that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))
	}
	def memoize(arg0: Tuple11[A,B,C,D,E,F,G,H,I,J,K]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
		(out, TupledFunction11(outFunc))
	}

	def apply(arg0: Tuple11[A,B,C,D,E,F,G,H,I,J,K])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L] extends Function11[A,B,C,D,E,F,G,H,I,J,K,Option[L]]{
	override def tupled = UnclosedTupledFunction11(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K):Option[ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K):Option[(L,ClosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L])]
}

case class UnclosedTupledFunction11[A,B,C,D,E,F,G,H,I,J,K,L](that: UnclosedFunctionFactory11[A,B,C,D,E,F,G,H,I,J,K,L]) extends UnclosedFunctionFactory1[Tuple11[A,B,C,D,E,F,G,H,I,J,K],L]{
	def query(arg0: Tuple11[A,B,C,D,E,F,G,H,I,J,K]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10).map{result =>
			TupledFunction11(result)
		}
	}
	def memoize(arg0: Tuple11[A,B,C,D,E,F,G,H,I,J,K]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction11(outFunc))
		}
}

	def apply(arg0: Tuple11[A,B,C,D,E,F,G,H,I,J,K])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
	}

	override def toString() = "Tupled" + that.toString()
}
