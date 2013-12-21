package com.wheaties.predicate

import com.wheaties.logical.Negation

trait Predicate3[@specialized(Int,Long,Float,Double) -T1,
                 @specialized(Int,Long,Float,Double) -T2,
                 @specialized(Int,Long,Float,Double) -T3] extends Function3[T1, T2, T3, Boolean] {
  self =>

	def or[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
    def apply(arg0: TT1, arg1: TT2, arg2: TT3) = self(arg0, arg1, arg2) || that(arg0, arg1, arg2)
  }
	def orNot[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
    def apply(arg0: TT1, arg1: TT2, arg2: TT3) = self(arg0, arg1, arg2) || !that(arg0, arg1, arg2)
  }
	def and[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
    def apply(arg0: TT1, arg1: TT2, arg2: TT3) = self(arg0, arg1, arg2) && that(arg0, arg1, arg2)
  }
	def andNot[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
    def apply(arg0: TT1, arg1: TT2, arg2: TT3) = self(arg0, arg1, arg2) && !that(arg0, arg1, arg2)
  }
	def xor[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
    def apply(arg0: TT1, arg1: TT2, arg2: TT3) = if(self(arg0, arg1, arg2)) !that(arg0, arg1, arg2) else that(arg0, arg1, arg2)
  }
	def nxor[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
    def apply(arg0: TT1, arg1: TT2, arg2: TT3) = if(self(arg0, arg1, arg2)) that(arg0, arg1, arg2) else !that(arg0, arg1, arg2)
  }
	def nand[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
    def apply(arg0: TT1, arg1: TT2, arg2: TT3) = !(self(arg0, arg1, arg2) && that(arg0, arg1, arg2))
  }
	def nor[TT1 <: T1, TT2 <: T2, TT3 <: T3](that: Function3[TT1, TT2, TT3, Boolean]) = new Predicate3[TT1, TT2, TT3]{
    def apply(arg0: TT1, arg1: TT2, arg2: TT3) = !(self(arg0, arg1, arg2) || that(arg0, arg1, arg2))
  }

  override def toString() = "<predicate3>"
}

object Predicate3{
  implicit def not[T1, T2, T3] = new Negation[Predicate3[T1, T2, T3]]{
    def not(pred: Predicate3[T1, T2, T3]) = new Predicate3[T1, T2, T3]{
      def apply(arg1: T1, arg2: T2, arg3: T3) = !pred(arg1, arg2, arg3)
    }
  }
}

object Always3 extends Predicate3[Any, Any, Any]{
  def apply(arg0: Any, arg1: Any, arg2: Any) = true
}

object Never3 extends Predicate3[Any, Any, Any]{
  def apply(arg0: Any, arg1: Any, arg2: Any) = false
}
