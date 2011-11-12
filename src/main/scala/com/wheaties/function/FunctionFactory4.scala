package com.wheaties.function

trait ClosedFunctionFactory4[A,B,C,D,E] extends Function4[A,B,C,D,E]{
  override def tupled = TupledFunction4(this)
	def query(arg0: A,arg1: B,arg2: C,arg3: D):ClosedFunctionFactory4[A,B,C,D,E]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D):(E,ClosedFunctionFactory4[A,B,C,D,E])
}

case class WrappedFunction4[A,B,C,D,E](that: Function4[A,B,C,D,E]) extends ClosedFunctionFactory4[A,B,C,D,E]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D) = (apply(arg0,arg1,arg2,arg3), query(arg0,arg1,arg2,arg3))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D) = that(arg0,arg1,arg2,arg3)

	override def toString() = that.toString()
}

case class TupledFunction4[A,B,C,D,E](that: ClosedFunctionFactory4[A,B,C,D,E]) extends ClosedFunctionFactory1[Tuple4[A,B,C,D],E]{
	def query(arg0: Tuple4[A,B,C,D]) ={
		val (x0, x1, x2, x3) = arg0
		TupledFunction4(that.query(x0, x1, x2, x3))
	}
	def memoize(arg0: Tuple4[A,B,C,D]) ={
		val (x0, x1, x2, x3) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3)
		(out, TupledFunction4(outFunc))
	}

	def apply(arg0: Tuple4[A,B,C,D])={
		val (x0, x1, x2, x3) = arg0
		that(x0, x1, x2, x3)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory4[A,B,C,D,E] extends Function4[A,B,C,D,Option[E]]{
	override def tupled = UnclosedTupledFunction4(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D):Option[ClosedFunctionFactory4[A,B,C,D,E]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D):Option[(E,ClosedFunctionFactory4[A,B,C,D,E])]
}

case class UnclosedTupledFunction4[A,B,C,D,E](that: UnclosedFunctionFactory4[A,B,C,D,E]) extends UnclosedFunctionFactory1[Tuple4[A,B,C,D],E]{
	def query(arg0: Tuple4[A,B,C,D]) ={
		val (x0, x1, x2, x3) = arg0
		that.query(x0, x1, x2, x3).map{result =>
			TupledFunction4(result)
		}
	}
	def memoize(arg0: Tuple4[A,B,C,D]) ={
		val (x0, x1, x2, x3) = arg0
		that.memoize(x0, x1, x2, x3).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction4(outFunc))
		}
}

	def apply(arg0: Tuple4[A,B,C,D])={
		val (x0, x1, x2, x3) = arg0
		that(x0, x1, x2, x3)
	}

	override def toString() = "Tupled" + that.toString()
}
