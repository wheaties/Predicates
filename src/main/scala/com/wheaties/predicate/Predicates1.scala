package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate1[@specialized(Int,Long,Float,Double) -T1] extends Function1[T1,Boolean] with PredicateLike[Predicate1[T1]]{
  protected[predicate] implicit def conn[T2 >: T1] = new Connective[Predicate1[T1],Predicate1[T2],Predicate1[T1]]{
    def or(p: Predicate1[T1], q: Predicate1[T2]) = new CompoundPredicate1(p, q){
      def apply(arg0: T1) = p(arg0) || q(arg0)
    }

    def and(p: Predicate1[T1], q: Predicate1[T2]) = new CompoundPredicate1(p, q){
      def apply(arg0: T1) = p(arg0) && q(arg0)
    }

    def xor(p: Predicate1[T1], q: Predicate1[T2]) = new CompoundPredicate1(p, q){
      def apply(arg0: T1) = if(p(arg0)) !q(arg0) else q(arg0)
    }

    def nxor(p: Predicate1[T1], q: Predicate1[T2]) = new CompoundPredicate1(p, q){
      def apply(arg0: T1) = if(p(arg0)) q(arg0) else !q(arg0)
    }

    def nand(p: Predicate1[T1], q: Predicate1[T2]) = new CompoundPredicate1(p, q){
      def apply(arg0: T1) = !(p(arg0) && q(arg0))
    }

    def nor(p: Predicate1[T1], q: Predicate1[T2]) = new CompoundPredicate1(p, q){
      def apply(arg0: T1) = !(p(arg0) || q(arg0))
    }
  }

  protected[predicate] implicit def not = new Negation[Predicate1[T1]]{
    def not(pred: Predicate1[T1]) = new Predicate1[T1]{
      def apply(arg0: T1) = !pred(arg0)
    }
  }
}

abstract class CompoundPredicate1[@specialized(Int,Long,Float,Double) T1,
                                  @specialized(Int,Long,Float,Double) Q1 >: T1](p: Predicate1[T1], q: Predicate1[Q1])
    extends Predicate1[T1]{}

object Always1 extends Predicate1[Any]{
  def apply(arg0: Any) = true
}

object Never1 extends Predicate1[Any]{
  def apply(arg0: Any) = false
}