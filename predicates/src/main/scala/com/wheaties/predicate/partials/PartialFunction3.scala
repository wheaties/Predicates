package com.wheaties.predicate.partials

trait PartialFunction3[-T1, -T2, -T3, +R] extends ((T1, T2, T3) => R){
  self =>

  import PartialFunction3._

  override def tupled = new PartialFunction[(T1, T2, T3), R] {
    def isDefinedAt(v1: (T1, T2, T3)) = self isDefinedAt (v1._1, v1._2, v1._3)

    def apply(v1: (T1, T2, T3)) = self(v1._1, v1._2, v1._3)
  }

  def isDefinedAt(arg1: T1, arg2: T2, arg3: T3): Boolean

  def orElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, RR >: R](that: PartialFunction3[TT1, TT2, TT3, RR]): PartialFunction3[TT1, TT2, TT3, RR] =
    new orElse3(this, that)

  def lift: (T1, T2, T3) => Option[R] = new Lifted3(this)

  def applyOrElse[TT1 <: T1, TT2 <: T2, TT3 <: T3, RR >: R](arg1: TT1, arg2: TT2, arg3: TT3, default: (TT1, TT2, TT3) => RR): RR =
    if (isDefinedAt(arg1, arg2, arg3)) apply(arg1, arg2, arg3) else default(arg1, arg2, arg3)
}

object PartialFunction3{
  private class orElse3[-T1, -T2, -T3, +R](p: PartialFunction3[T1, T2, T3, R], q: PartialFunction3[T1, T2, T3, R])
    extends PartialFunction3[T1, T2, T3, R]{

    def isDefinedAt(arg1: T1, arg2: T2, arg3: T3) = p.isDefinedAt(arg1, arg2, arg3) || q.isDefinedAt(arg1, arg2, arg3)

    def apply(arg1: T1, arg2: T2, arg3: T3) = p applyOrElse (arg1, arg2, arg3, q)
  }

  private class Lifted3[-T1, -T2, -T3, +R](pf: PartialFunction3[T1, T2, T3, R]) extends ((T1, T2, T3) => Option[R]){
    def apply(arg1: T1, arg2: T2, arg3: T3): Option[R] = Option{
      pf applyOrElse (arg1, arg2, arg3, defaultFn.asInstanceOf[(T1, T2, T3) => R])
    }
  }

  private val defaultFn: ((Any, Any, Any) => Any) = (_, _, _) => null
}
