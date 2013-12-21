package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate2[@specialized(Int,Long,Float,Double) -T1,
                 @specialized(Int,Long,Float,Double) -T2] extends Function2[T1, T2, Boolean]{
  self =>

  def or[TT1 <: T1, TT2 <: T2](that: Function2[TT1, TT2, Boolean]) = new Predicate2[TT1, TT2]{
    def apply(arg0: T1, arg1: T2) = self(arg0, arg1) || that(arg0, arg1)
  }

  def and[TT1 <: T1, TT2 <: T2](that: Function2[TT1, TT2, Boolean]) = new Predicate2[TT1, TT2]{
    def apply(arg0: T1, arg1: T2) = self(arg0, arg1) && that(arg0, arg1)
  }

  def xor[TT1 <: T1, TT2 <: T2](that: Function2[TT1, TT2, Boolean]) = new Predicate2[TT1, TT2]{
    def apply(arg0: T1, arg1: T2) = if(self(arg0, arg1)) !that(arg0, arg1) else that(arg0, arg1)
  }

  def nxor[TT1 <: T1, TT2 <: T2](that: Function2[TT1, TT2, Boolean]) = new Predicate2[TT1, TT2]{
    def apply(arg0: T1, arg1: T2) = if(self(arg0, arg1)) that(arg0, arg1) else !that(arg0, arg1)
  }

  def nand[TT1 <: T1, TT2 <: T2](that: Function2[TT1, TT2, Boolean]) = new Predicate2[TT1, TT2]{
    def apply(arg0: T1, arg1: T2) = !(self(arg0, arg1) && that(arg0, arg1))
  }

  def nor[TT1 <: T1, TT2 <: T2](that: Function2[TT1, TT2, Boolean]) = new Predicate2[TT1, TT2]{
    def apply(arg0: T1, arg1: T2) = !(self(arg0, arg1) || that(arg0, arg1))
  }
}

object Predicate2{

  implicit def not[T1,T2] = new Negation[Predicate2[T1,T2]]{
    def not(pred: Predicate2[T1,T2]) = new Predicate2[T1,T2]{
      def apply(arg0: T1, arg1: T2) = !pred(arg0, arg1)
    }
  }
}
case object Always2 extends Predicate2[Any, Any]{
def apply(arg0: Any, arg1: Any) = true
}

case object Never2 extends Predicate2[Any, Any]{
  def apply(arg0: Any, arg1: Any) = false
}