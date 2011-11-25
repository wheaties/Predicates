package com.wheaties.function

trait ClosedFunctionFactory2[A,B,C] extends Function2[A,B,C]{
  override def tupled = TupledFunction2(this)

	def query(arg0: A,arg1: B):ClosedFunctionFactory2[A,B,C]
	def memoize(arg0: A,arg1: B):(C,ClosedFunctionFactory2[A,B,C])
}

case class WrappedFunction2[A,B,C](that: Function2[A,B,C]) extends ClosedFunctionFactory2[A,B,C]{
	def query(arg0: A,arg1: B) = this
	def memoize(arg0: A,arg1: B) = (apply(arg0,arg1), query(arg0, arg1))

	def apply(arg0: A,arg1: B) = that(arg0,arg1)

	override def toString() = that.toString()
}

case class TupledFunction2[A,B,C](that: ClosedFunctionFactory2[A,B,C]) extends ClosedFunctionFactory1[Tuple2[A,B],C]{
  def query(arg0: Tuple2[A,B]) ={
    val (x0, x1) = arg0
    TupledFunction2(that.query(x0, x1))
  }
  def memoize(arg0: Tuple2[A,B]) ={
    val (x0, x1) = arg0
    val(out, outFunc) = that.memoize(x0, x1)
    (out, TupledFunction2(outFunc))
  }

  def apply(arg0: Tuple2[A,B])={
    val (x0, x1) = arg0
    that(x0, x1)
  }

  override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory2[A,B,C] extends Function2[A,B,Option[C]]{
	override def tupled = UnclosedTupledFunction2(this)

	def query(arg0: A, arg1: B):Option[ClosedFunctionFactory2[A,B,C]]
	def memoize(arg0: A, arg1: B):Option[(C,ClosedFunctionFactory2[A,B,C])]
}

case class UnclosedTupledFunction2[A,B,C](that: UnclosedFunctionFactory2[A,B,C]) extends UnclosedFunctionFactory1[Tuple2[A,B],C]{
	def query(arg0: Tuple2[A,B]) ={
		val (x0, x1) = arg0
		that.query(x0, x1).map{result =>
			TupledFunction2(result)
		}
	}
	def memoize(arg0: Tuple2[A,B]) ={
		val (x0, x1) = arg0
		that.memoize(x0, x1).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction2(outFunc))
		}
}

	def apply(arg0: Tuple2[A,B])={
		val (x0, x1) = arg0
		that(x0, x1)
	}

	override def toString() = "Tupled" + that.toString()
}
