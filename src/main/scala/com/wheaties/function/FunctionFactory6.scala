package com.wheaties.function

trait ClosedFunctionFactory6[A,B,C,D,E,F,G] extends Function6[A,B,C,D,E,F,G]{
  override def tupled = TupledFunction6(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F):ClosedFunctionFactory6[A,B,C,D,E,F,G]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F):(G,ClosedFunctionFactory6[A,B,C,D,E,F,G])
}

case class WrappedFunction6[A,B,C,D,E,F,G](that: Function6[A,B,C,D,E,F,G]) extends ClosedFunctionFactory6[A,B,C,D,E,F,G]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = (apply(arg0,arg1,arg2,arg3,arg4,arg5), query(arg0,arg1,arg2,arg3,arg4,arg5))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F) = that(arg0,arg1,arg2,arg3,arg4,arg5)

	override def toString() = that.toString()
}

case class TupledFunction6[A,B,C,D,E,F,G](that: ClosedFunctionFactory6[A,B,C,D,E,F,G]) extends ClosedFunctionFactory1[Tuple6[A,B,C,D,E,F],G]{
	def query(arg0: Tuple6[A,B,C,D,E,F]) ={
		val (x0, x1, x2, x3, x4, x5) = arg0
		TupledFunction6(that.query(x0, x1, x2, x3, x4, x5))
	}
	def memoize(arg0: Tuple6[A,B,C,D,E,F]) ={
		val (x0, x1, x2, x3, x4, x5) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5)
		(out, TupledFunction6(outFunc))
	}

	def apply(arg0: Tuple6[A,B,C,D,E,F])={
		val (x0, x1, x2, x3, x4, x5) = arg0
		that(x0, x1, x2, x3, x4, x5)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory6[A,B,C,D,E,F,G] extends Function6[A,B,C,D,E,F,Option[G]]{
	override def tupled = UnclosedTupledFunction6(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F):Option[ClosedFunctionFactory6[A,B,C,D,E,F,G]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F):Option[(G,ClosedFunctionFactory6[A,B,C,D,E,F,G])]
}

case class UnclosedTupledFunction6[A,B,C,D,E,F,G](that: UnclosedFunctionFactory6[A,B,C,D,E,F,G]) extends UnclosedFunctionFactory1[Tuple6[A,B,C,D,E,F],G]{
	def query(arg0: Tuple6[A,B,C,D,E,F]) ={
		val (x0, x1, x2, x3, x4, x5) = arg0
		that.query(x0, x1, x2, x3, x4, x5).map{result =>
			TupledFunction6(result)
		}
	}
	def memoize(arg0: Tuple6[A,B,C,D,E,F]) ={
		val (x0, x1, x2, x3, x4, x5) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction6(outFunc))
		}
}

	def apply(arg0: Tuple6[A,B,C,D,E,F])={
		val (x0, x1, x2, x3, x4, x5) = arg0
		that(x0, x1, x2, x3, x4, x5)
	}

	override def toString() = "Tupled" + that.toString()
}
