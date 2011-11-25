package com.wheaties.function

trait ClosedFunctionFactory7[A,B,C,D,E,F,G,H] extends Function7[A,B,C,D,E,F,G,H]{
  override def tupled = TupledFunction7(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G):ClosedFunctionFactory7[A,B,C,D,E,F,G,H]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G):(H,ClosedFunctionFactory7[A,B,C,D,E,F,G,H])
}

case class WrappedFunction7[A,B,C,D,E,F,G,H](that: Function7[A,B,C,D,E,F,G,H]) extends ClosedFunctionFactory7[A,B,C,D,E,F,G,H]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6)

	override def toString() = that.toString()
}

case class TupledFunction7[A,B,C,D,E,F,G,H](that: ClosedFunctionFactory7[A,B,C,D,E,F,G,H]) extends ClosedFunctionFactory1[Tuple7[A,B,C,D,E,F,G],H]{
	def query(arg0: Tuple7[A,B,C,D,E,F,G]) ={
		val (x0, x1, x2, x3, x4, x5, x6) = arg0
		TupledFunction7(that.query(x0, x1, x2, x3, x4, x5, x6))
	}
	def memoize(arg0: Tuple7[A,B,C,D,E,F,G]) ={
		val (x0, x1, x2, x3, x4, x5, x6) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6)
		(out, TupledFunction7(outFunc))
	}

	def apply(arg0: Tuple7[A,B,C,D,E,F,G])={
		val (x0, x1, x2, x3, x4, x5, x6) = arg0
		that(x0, x1, x2, x3, x4, x5, x6)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory7[A,B,C,D,E,F,G,H] extends Function7[A,B,C,D,E,F,G,Option[H]]{
	override def tupled = UnclosedTupledFunction7(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G):Option[ClosedFunctionFactory7[A,B,C,D,E,F,G,H]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G):Option[(H,ClosedFunctionFactory7[A,B,C,D,E,F,G,H])]
}

case class UnclosedTupledFunction7[A,B,C,D,E,F,G,H](that: UnclosedFunctionFactory7[A,B,C,D,E,F,G,H]) extends UnclosedFunctionFactory1[Tuple7[A,B,C,D,E,F,G],H]{
	def query(arg0: Tuple7[A,B,C,D,E,F,G]) ={
		val (x0, x1, x2, x3, x4, x5, x6) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6).map{result =>
			TupledFunction7(result)
		}
	}
	def memoize(arg0: Tuple7[A,B,C,D,E,F,G]) ={
		val (x0, x1, x2, x3, x4, x5, x6) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction7(outFunc))
		}
}

	def apply(arg0: Tuple7[A,B,C,D,E,F,G])={
		val (x0, x1, x2, x3, x4, x5, x6) = arg0
		that(x0, x1, x2, x3, x4, x5, x6)
	}

	override def toString() = "Tupled" + that.toString()
}
