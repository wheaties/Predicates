package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate2[@specialized(Int,Long,Float,Double) -T1,
                 @specialized(Int,Long,Float,Double) -T2] extends Function2[T1, T2, Boolean] with PredicateLike[Predicate2[T1,T2]]{
  protected[predicate] implicit def conn[Q1 >: T1,Q2 >: T2] = new Connective[Predicate2[T1,T2],Predicate2[Q1,Q2],Predicate2[T1,T2]]{
    def or(p: Predicate2[T1,T2], q: Predicate2[Q1,Q2]) = new CompoundPredicate2(p, q){
      def apply(arg0: T1, arg1: T2) = p(arg0, arg1) || q(arg0, arg1)
    }

    def and(p: Predicate2[T1,T2], q: Predicate2[Q1,Q2]) = new CompoundPredicate2(p, q){
      def apply(arg0: T1, arg1: T2) = p(arg0, arg1) && q(arg0, arg1)
    }

    def xor(p: Predicate2[T1,T2], q: Predicate2[Q1,Q2]) = new CompoundPredicate2(p, q){
      def apply(arg0: T1, arg1: T2) = if(p(arg0, arg1)) !q(arg0, arg1) else q(arg0, arg1)
    }

    def nxor(p: Predicate2[T1,T2], q: Predicate2[Q1,Q2]) = new CompoundPredicate2(p, q){
      def apply(arg0: T1, arg1: T2) = if(p(arg0, arg1)) q(arg0, arg1) else !q(arg0, arg1)
    }

    def nand(p: Predicate2[T1,T2], q: Predicate2[Q1,Q2]) = new CompoundPredicate2(p, q){
      def apply(arg0: T1, arg1: T2) = !(p(arg0, arg1) && q(arg0, arg1))
    }

    def nor(p: Predicate2[T1,T2], q: Predicate2[Q1,Q2]) = new CompoundPredicate2(p, q){
      def apply(arg0: T1, arg1: T2) = !(p(arg0, arg1) || q(arg0, arg1))
    }
  }

  protected[predicate] implicit def not = new Negation[Predicate2[T1,T2]]{
    def not(pred: Predicate2[T1,T2]) = new Predicate2[T1,T2]{
      def apply(arg0: T1, arg1: T2) = !pred(arg0, arg1)
    }
  }
}

abstract class CompoundPredicate2[@specialized(Int,Long,Float,Double) T1,
                                  @specialized(Int,Long,Float,Double) T2,
                                  @specialized(Int,Long,Float,Double) Q1 >: T1,
                                  @specialized(Int,Long,Float,Double) Q2 >: T2](p: Predicate2[T1,T2], q: Predicate2[Q1,Q2])
    extends Predicate2[T1,T2]{}

case object Always2 extends Predicate2[Any, Any]{
def apply(arg0: Any, arg1: Any) = true
}

case object Never2 extends Predicate2[Any, Any]{
  def apply(arg0: Any, arg1: Any) = false
}