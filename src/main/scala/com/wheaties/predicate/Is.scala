package com.wheaties.predicate

object Is{
  implicit def toIs[A,B](func: Function1[A,B]) = new { def is(pred: Predicate1[B]) = Is1(func, pred) }
  implicit def toIs[A,B,C](func: Function2[A,B,C]) = new { def is(pred: Predicate1[C]) = Is2(func, pred) }
  implicit def toIs[A,B,C,D](func: Function3[A,B,C,D]) = new { def is(pred: Predicate1[D]) = Is3(func, pred) }
  implicit def toIs[A,B,C,D,E](func: Function4[A,B,C,D,E]) = new { def is(pred: Predicate1[E]) = Is4(func, pred) }
  implicit def toIs[A,B,C,D,E,F](func: Function5[A,B,C,D,E,F]) = new { def is(pred: Predicate1[F]) = Is5(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G](func: Function6[A,B,C,D,E,F,G]) = new { def is(pred: Predicate1[G]) = Is6(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H](func: Function7[A,B,C,D,E,F,G,H]) = new { def is(pred: Predicate1[H]) = Is7(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I](func: Function8[A,B,C,D,E,F,G,H,I]) = new { def is(pred: Predicate1[I]) = Is8(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J](func: Function9[A,B,C,D,E,F,G,H,I,J]) = new { def is(pred: Predicate1[J]) = Is9(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J,K](func: Function10[A,B,C,D,E,F,G,H,I,J,K]) = new { def is(pred: Predicate1[K]) = Is10(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J,K,L](func: Function11[A,B,C,D,E,F,G,H,I,J,K,L]) = new { def is(pred: Predicate1[L]) = Is11(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J,K,L,M](func: Function12[A,B,C,D,E,F,G,H,I,J,K,L,M]) = new { def is(pred: Predicate1[M]) = Is12(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J,K,L,M,N](func: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N]) = new { def is(pred: Predicate1[N]) = Is13(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](func: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) = new { def is(pred: Predicate1[O]) = Is14(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](func: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]) = new { def is(pred: Predicate1[P]) = Is15(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](func: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]) = new { def is(pred: Predicate1[Q]) = Is16(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](func: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]) = new { def is(pred: Predicate1[R]) = Is17(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](func: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]) = new { def is(pred: Predicate1[S]) = Is18(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](func: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]) = new { def is(pred: Predicate1[T]) = Is19(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](func: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]) = new { def is(pred: Predicate1[U]) = Is20(func, pred) }
  implicit def toIs[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](func: Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V]) = new { def is(pred: Predicate1[V]) = Is21(func, pred) }
}

case class Is1[A,B](func: Function1[A,B], pred: Predicate1[B]) extends Predicate1[A]{
  def apply(arg0: A) = pred(func(arg0))
}

case class Is2[A,B,C](func: Function2[A,B,C], pred: Predicate1[C]) extends Predicate2[A,B]{
	def apply(arg0: A, arg1: B) = pred(func(arg0, arg1))
}

case class Is3[A,B,C,D](func: Function3[A,B,C,D], pred: Predicate1[D]) extends Predicate3[A,B,C]{
	def apply(arg0: A, arg1: B, arg2: C) = pred(func(arg0, arg1, arg2))
}

case class Is4[A,B,C,D,E](func: Function4[A,B,C,D,E], pred: Predicate1[E]) extends Predicate4[A,B,C,D]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D) = pred(func(arg0, arg1, arg2, arg3))
}

case class Is5[A,B,C,D,E,F](func: Function5[A,B,C,D,E,F], pred: Predicate1[F]) extends Predicate5[A,B,C,D,E]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E) = pred(func(arg0, arg1, arg2, arg3, arg4))
}

case class Is6[A,B,C,D,E,F,G](func: Function6[A,B,C,D,E,F,G], pred: Predicate1[G]) extends Predicate6[A,B,C,D,E,F]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5))
}

case class Is7[A,B,C,D,E,F,G,H](func: Function7[A,B,C,D,E,F,G,H], pred: Predicate1[H]) extends Predicate7[A,B,C,D,E,F,G]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6))
}

case class Is8[A,B,C,D,E,F,G,H,I](func: Function8[A,B,C,D,E,F,G,H,I], pred: Predicate1[I]) extends Predicate8[A,B,C,D,E,F,G,H]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7))
}

case class Is9[A,B,C,D,E,F,G,H,I,J](func: Function9[A,B,C,D,E,F,G,H,I,J], pred: Predicate1[J]) extends Predicate9[A,B,C,D,E,F,G,H,I]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
}

case class Is10[A,B,C,D,E,F,G,H,I,J,K](func: Function10[A,B,C,D,E,F,G,H,I,J,K], pred: Predicate1[K]) extends Predicate10[A,B,C,D,E,F,G,H,I,J]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9))
}

case class Is11[A,B,C,D,E,F,G,H,I,J,K,L](func: Function11[A,B,C,D,E,F,G,H,I,J,K,L], pred: Predicate1[L]) extends Predicate11[A,B,C,D,E,F,G,H,I,J,K]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10))
}

case class Is12[A,B,C,D,E,F,G,H,I,J,K,L,M](func: Function12[A,B,C,D,E,F,G,H,I,J,K,L,M], pred: Predicate1[M]) extends Predicate12[A,B,C,D,E,F,G,H,I,J,K,L]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11))
}

case class Is13[A,B,C,D,E,F,G,H,I,J,K,L,M,N](func: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,N], pred: Predicate1[N]) extends Predicate13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12))
}

case class Is14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](func: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], pred: Predicate1[O]) extends Predicate14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13))
}

case class Is15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](func: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], pred: Predicate1[P]) extends Predicate15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14))
}

case class Is16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](func: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], pred: Predicate1[Q]) extends Predicate16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15))
}

case class Is17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](func: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], pred: Predicate1[R]) extends Predicate17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16))
}

case class Is18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](func: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S], pred: Predicate1[S]) extends Predicate18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17))
}

case class Is19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](func: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], pred: Predicate1[T]) extends Predicate19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18))
}

case class Is20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](func: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U], pred: Predicate1[U]) extends Predicate20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19))
}

case class Is21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](func: Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V], pred: Predicate1[V]) extends Predicate21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
	def apply(arg0: A, arg1: B, arg2: C, arg3: D, arg4: E, arg5: F, arg6: G, arg7: H, arg8: I, arg9: J, arg10: K, arg11: L, arg12: M, arg13: N, arg14: O, arg15: P, arg16: Q, arg17: R, arg18: S, arg19: T, arg20: U) = pred(func(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20))
}