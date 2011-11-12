package com.wheaties.function

trait ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J] extends Function9[A,B,C,D,E,F,G,H,I,J]{
  override def tupled = TupledFunction9(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I):ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I):(J,ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J])
}

case class WrappedFunction9[A,B,C,D,E,F,G,H,I,J](that: Function9[A,B,C,D,E,F,G,H,I,J]) extends ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)

	override def toString() = that.toString()
}

case class TupledFunction9[A,B,C,D,E,F,G,H,I,J](that: ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]) extends ClosedFunctionFactory1[Tuple9[A,B,C,D,E,F,G,H,I],J]{
	def query(arg0: Tuple9[A,B,C,D,E,F,G,H,I]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8) = arg0
		TupledFunction9(that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8))
	}
	def memoize(arg0: Tuple9[A,B,C,D,E,F,G,H,I]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8)
		(out, TupledFunction9(outFunc))
	}

	def apply(arg0: Tuple9[A,B,C,D,E,F,G,H,I])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J] extends Function9[A,B,C,D,E,F,G,H,I,Option[J]]{
	override def tupled = UnclosedTupledFunction9(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I):Option[ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I):Option[(J,ClosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J])]
}

case class UnclosedTupledFunction9[A,B,C,D,E,F,G,H,I,J](that: UnclosedFunctionFactory9[A,B,C,D,E,F,G,H,I,J]) extends UnclosedFunctionFactory1[Tuple9[A,B,C,D,E,F,G,H,I],J]{
	def query(arg0: Tuple9[A,B,C,D,E,F,G,H,I]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8).map{result =>
			TupledFunction9(result)
		}
	}
	def memoize(arg0: Tuple9[A,B,C,D,E,F,G,H,I]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction9(outFunc))
		}
}

	def apply(arg0: Tuple9[A,B,C,D,E,F,G,H,I])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8)
	}

	override def toString() = "Tupled" + that.toString()
}
