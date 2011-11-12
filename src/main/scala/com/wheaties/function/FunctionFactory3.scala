package com.wheaties.function

trait ClosedFunctionFactory3[A,B,C,D] extends Function3[A,B,C,D]{
  override def tupled = TupledFunction3(this)

	def query(arg0: A,arg1: B,arg2: C):ClosedFunctionFactory3[A,B,C,D]
	def memoize(arg0: A,arg1: B,arg2: C):(D,ClosedFunctionFactory3[A,B,C,D])
}

case class WrappedFunction3[A,B,C,D](that: Function3[A,B,C,D]) extends ClosedFunctionFactory3[A,B,C,D]{
	def query(arg0: A,arg1: B,arg2: C) = this
	def memoize(arg0: A,arg1: B,arg2: C) = (apply(arg0,arg1,arg2), query(arg0,arg1,arg2))

	def apply(arg0: A,arg1: B,arg2: C) = that(arg0,arg1,arg2)

	override def toString() = that.toString()
}

case class TupledFunction3[A,B,C,D](that: ClosedFunctionFactory3[A,B,C,D]) extends ClosedFunctionFactory1[Tuple3[A,B,C],D]{
	def query(arg0: Tuple3[A,B,C]) ={
		val (x0, x1, x2) = arg0
		TupledFunction3(that.query(x0, x1, x2))
	}
	def memoize(arg0: Tuple3[A,B,C]) ={
		val (x0, x1, x2) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2)
		(out, TupledFunction3(outFunc))
	}

	def apply(arg0: Tuple3[A,B,C])={
		val (x0, x1, x2) = arg0
		that(x0, x1, x2)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory3[A,B,C,D] extends Function3[A,B,C,Option[D]]{
	override def tupled = UnclosedTupledFunction3(this)

	def query(arg0: A, arg1: B, arg2: C):Option[ClosedFunctionFactory3[A,B,C,D]]
	def memoize(arg0: A, arg1: B, arg2: C):Option[(D,ClosedFunctionFactory3[A,B,C,D])]
}

case class UnclosedTupledFunction3[A,B,C,D](that: UnclosedFunctionFactory3[A,B,C,D]) extends UnclosedFunctionFactory1[Tuple3[A,B,C],D]{
	def query(arg0: Tuple3[A,B,C]) ={
		val (x0, x1, x2) = arg0
		that.query(x0, x1, x2).map{result =>
			TupledFunction3(result)
		}
	}
	def memoize(arg0: Tuple3[A,B,C]) ={
		val (x0, x1, x2) = arg0
		that.memoize(x0, x1, x2).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction3(outFunc))
		}
}

	def apply(arg0: Tuple3[A,B,C])={
		val (x0, x1, x2) = arg0
		that(x0, x1, x2)
	}

	override def toString() = "Tupled" + that.toString()
}
