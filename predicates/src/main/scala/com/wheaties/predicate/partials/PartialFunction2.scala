package com.wheaties.partials

//TODO: Need AndThen just like PartialFunction has them
trait PartialFunction2[@specialized(Int,Long,Float,Double) -T1,
                       @specialized(Int,Long,Float,Double) -T2, +R] extends ((T1, T2) => R){
  self =>

  import PartialFunction2._

  override def tupled = new PartialFunction[(T1, T2), R] {
    def isDefinedAt(v1: (T1, T2)) = self isDefinedAt (v1._1, v1._2)

    def apply(v1: (T1, T2)) = self(v1._1, v1._2)
  }

  def isDefinedAt(arg1: T1, arg2: T2): Boolean

  def orElse[TT1 <: T1, TT2 <: T2, RR >: R](that: PartialFunction2[TT1, TT2, RR]): PartialFunction2[TT1, TT2, RR] =
    new orElse2(this, that)

  def lift: (T1, T2) => Option[R] = new Lifted2(this)

  def applyOrElse[TT1 <: T1, TT2 <: T2, RR >: R](arg1: TT1, arg2: TT2, default: (TT1, TT2) => RR): RR =
    if (isDefinedAt(arg1, arg2)) apply(arg1, arg2) else default(arg1, arg2)
}

object PartialFunction2{
  private class orElse2[@specialized(Int,Long,Float,Double) -T1,
                        @specialized(Int,Long,Float,Double) -T2, +R](p: PartialFunction2[T1, T2, R], q: PartialFunction2[T1, T2, R])
      extends PartialFunction2[T1, T2, R]{

    def isDefinedAt(arg1: T1, arg2: T2) = p.isDefinedAt(arg1, arg2) || q.isDefinedAt(arg1, arg2)

    def apply(arg1: T1, arg2: T2) = p applyOrElse (arg1, arg2, q)
  }

  private class Lifted2[@specialized(Int,Long,Float,Double) -T1, @specialized(Int,Long,Float,Double) -T2, +R](pf: PartialFunction2[T1, T2, R])
      extends ((T1, T2) => Option[R]){
    def apply(arg1: T1, arg2: T2): Option[R] = Option{
      pf applyOrElse (arg1, arg2, defaultFn.asInstanceOf[(T1, T2) => R])
    }
  }

  private val defaultFn: ((Any, Any) => Any) = (_, _) => null
}