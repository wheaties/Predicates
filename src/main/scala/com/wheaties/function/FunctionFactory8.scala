package com.wheaties.function

trait ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I] extends Function8[A,B,C,D,E,F,G,H,I]{
  override def tupled = TupledFunction8(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H):ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H):(I,ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I])
}

case class WrappedFunction8[A,B,C,D,E,F,G,H,I](that: Function8[A,B,C,D,E,F,G,H,I]) extends ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7)

	override def toString() = that.toString()
}

case class TupledFunction8[A,B,C,D,E,F,G,H,I](that: ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]) extends ClosedFunctionFactory1[Tuple8[A,B,C,D,E,F,G,H],I]{
	def query(arg0: Tuple8[A,B,C,D,E,F,G,H]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7) = arg0
		TupledFunction8(that.query(x0, x1, x2, x3, x4, x5, x6, x7))
	}
	def memoize(arg0: Tuple8[A,B,C,D,E,F,G,H]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7)
		(out, TupledFunction8(outFunc))
	}

	def apply(arg0: Tuple8[A,B,C,D,E,F,G,H])={
		val (x0, x1, x2, x3, x4, x5, x6, x7) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory8[A,B,C,D,E,F,G,H,I] extends Function8[A,B,C,D,E,F,G,H,Option[I]]{
	override def tupled = UnclosedTupledFunction8(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H):Option[ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H):Option[(I,ClosedFunctionFactory8[A,B,C,D,E,F,G,H,I])]
}

case class UnclosedTupledFunction8[A,B,C,D,E,F,G,H,I](that: UnclosedFunctionFactory8[A,B,C,D,E,F,G,H,I]) extends UnclosedFunctionFactory1[Tuple8[A,B,C,D,E,F,G,H],I]{
	def query(arg0: Tuple8[A,B,C,D,E,F,G,H]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7).map{result =>
			TupledFunction8(result)
		}
	}
	def memoize(arg0: Tuple8[A,B,C,D,E,F,G,H]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction8(outFunc))
		}
}

	def apply(arg0: Tuple8[A,B,C,D,E,F,G,H])={
		val (x0, x1, x2, x3, x4, x5, x6, x7) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7)
	}

	override def toString() = "Tupled" + that.toString()
}
