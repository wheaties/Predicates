package com.wheaties.function

trait ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K] extends Function10[A,B,C,D,E,F,G,H,I,J,K]{
  override def tupled = TupledFunction10(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J):ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J):(K,ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K])
}

case class WrappedFunction10[A,B,C,D,E,F,G,H,I,J,K](that: Function10[A,B,C,D,E,F,G,H,I,J,K]) extends ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)

	override def toString() = that.toString()
}

case class TupledFunction10[A,B,C,D,E,F,G,H,I,J,K](that: ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]) extends ClosedFunctionFactory1[Tuple10[A,B,C,D,E,F,G,H,I,J],K]{
	def query(arg0: Tuple10[A,B,C,D,E,F,G,H,I,J]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) = arg0
		TupledFunction10(that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9))
	}
	def memoize(arg0: Tuple10[A,B,C,D,E,F,G,H,I,J]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
		(out, TupledFunction10(outFunc))
	}

	def apply(arg0: Tuple10[A,B,C,D,E,F,G,H,I,J])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K] extends Function10[A,B,C,D,E,F,G,H,I,J,Option[K]]{
	override def tupled = UnclosedTupledFunction10(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J):Option[ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J):Option[(K,ClosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K])]
}

case class UnclosedTupledFunction10[A,B,C,D,E,F,G,H,I,J,K](that: UnclosedFunctionFactory10[A,B,C,D,E,F,G,H,I,J,K]) extends UnclosedFunctionFactory1[Tuple10[A,B,C,D,E,F,G,H,I,J],K]{
	def query(arg0: Tuple10[A,B,C,D,E,F,G,H,I,J]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9).map{result =>
			TupledFunction10(result)
		}
	}
	def memoize(arg0: Tuple10[A,B,C,D,E,F,G,H,I,J]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction10(outFunc))
		}
}

	def apply(arg0: Tuple10[A,B,C,D,E,F,G,H,I,J])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
	}

	override def toString() = "Tupled" + that.toString()
}
