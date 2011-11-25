package com.wheaties.function

trait ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] extends Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
  override def tupled = TupledFunction20(this)

	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T):ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T):(U,ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U])
}

case class WrappedFunction20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](that: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) extends ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
	def query(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T) = this
	def memoize(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T) = (apply(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19), query(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19))

	def apply(arg0: A,arg1: B,arg2: C,arg3: D,arg4: E,arg5: F,arg6: G,arg7: H,arg8: I,arg9: J,arg10: K,arg11: L,arg12: M,arg13: N,arg14: O,arg15: P,arg16: Q,arg17: R,arg18: S,arg19: T) = that(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19)

	override def toString() = that.toString()
}

case class TupledFunction20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](that: ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) extends ClosedFunctionFactory1[Tuple20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T],U]{
	def query(arg0: Tuple20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) = arg0
		TupledFunction20(that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19))
	}
	def memoize(arg0: Tuple20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) = arg0
		val(out, outFunc) = that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)
		(out, TupledFunction20(outFunc))
	}

	def apply(arg0: Tuple20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)
	}

	override def toString() = "Tupled" + that.toString()
}

trait UnclosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] extends Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Option[U]]{
	override def tupled = UnclosedTupledFunction20(this)

	def query(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T):Option[ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]]
	def memoize(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T):Option[(U,ClosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U])]
}

case class UnclosedTupledFunction20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](that: UnclosedFunctionFactory20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) extends UnclosedFunctionFactory1[Tuple20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T],U]{
	def query(arg0: Tuple20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) = arg0
		that.query(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19).map{result =>
			TupledFunction20(result)
		}
	}
	def memoize(arg0: Tuple20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) ={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) = arg0
		that.memoize(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19).map{result =>
			val (out, outFunc) = result
			(out, TupledFunction20(outFunc))
		}
}

	def apply(arg0: Tuple20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T])={
		val (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) = arg0
		that(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)
	}

	override def toString() = "Tupled" + that.toString()
}
