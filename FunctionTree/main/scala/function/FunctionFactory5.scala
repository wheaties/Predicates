package com.wheaties.function

trait ClosedFunctionFactory5[A,B,C,D,E,F] extends Function5[A,B,C,D,E,F]{
  override def tupled = TupledFunction5(this)
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E):ClosedFunctionFactory5[A,B,C,D,E,F]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E):(F,ClosedFunctionFactory5[A,B,C,D,E,F])
}

case class WrappedFunction5[A,B,C,D,E,F](that: Function5[A,B,C,D,E,F]) extends ClosedFunctionFactory5[A,B,C,D,E,F]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E) = (apply(arg0,arg1,arg2,arg3,arg4), query(arg0,arg1,arg2,arg3,arg4))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E) = that(arg0,arg1,arg2,arg3,arg4)

	override def toString() = that.toString()
}

case class TupledFunction5[A,B,C,D,E,F](that: ClosedFunctionFactory5[A,B,C,D,E,F]) extends ClosedFunctionFactory1[Tuple5[A,B,C,D,E],F]{
	def query(arg0: Tuple5[A,B,C,D,E]) ={
		val (x0, x1, x2, x3, x4) = arg0
		TupledFunction5(that.query(x0, x1, x2, x3, x4))
	}
	def memoize(arg0: Tuple5[A,B,C,D,E]) ={
		val (x0, x1, x2, x3, x4) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4)
		(out, TupledFunction5(outFunc))
	}

	def apply(arg0: Tuple5[A,B,C,D,E])={
		val (x0, x1, x2, x3, x4) = arg0
		that(x0, x1, x2, x3, x4)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory5[A,B,C,D,E,F] extends Function5[A,B,C,D,E,Option[F]]{
	override def tupled = UnclosedTupledFunction5(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E):Option[ClosedFunctionFactory5[A,B,C,D,E,F]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E):Option[(F,ClosedFunctionFactory5[A,B,C,D,E,F])]
}

case class UnclosedTupledFunction5[A,B,C,D,E,F](that: UnclosedFunctionFactory5[A,B,C,D,E,F]) extends UnclosedFunctionFactory1[Tuple5[A,B,C,D,E],F]{
	def query(arg0: Tuple5[A,B,C,D,E]) ={
		val (x0, x1, x2, x3, x4) = arg0
		that.query(x0, x1, x2, x3, x4).map{result =>
			TupledFunction5(result)
		}
	}
	def memoize(arg0: Tuple5[A,B,C,D,E]) ={
		val (x0, x1, x2, x3, x4) = arg0
		that.memoize(x0, x1, x2, x3, x4).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction5(outFunc))
		}
}

	def apply(arg0: Tuple5[A,B,C,D,E])={
		val (x0, x1, x2, x3, x4) = arg0
		that(x0, x1, x2, x3, x4)
	}

	override def toString() = "Tupled" + that.toString()
}
