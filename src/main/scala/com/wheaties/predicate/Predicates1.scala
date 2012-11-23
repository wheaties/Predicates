package com.wheaties.predicate

import com.wheaties.logical._

trait Predicate1[-T1] extends Function1[T1,Boolean] with PredicateLike[Predicate1[T1]]

//TODO: this needs to be an implicit
class Connective1[T1,T2 <: T1] extends Connective[Predicate1[T1],Predicate1[T2],Predicate1[T1]]{
  def or(p: Predicate1[T1], q: Predicate1[T2]) = new CompoundPredicate1(p, q){
    def apply(arg0: T1) = _1(arg0) || _2(arg0)
  }

  def and(p: Predicate1[T1], q: Predicate1[T2]) = new CompoundPredicate1(p, q){
    def apply(arg0: T1) = _1(arg0) && _2(arg0)
  }

  def xor(p: Predicate1[T1], q: Predicate1[T2]) = new CompoundPredicate1(p, q){
    def apply(arg0: T1) = if(_1(arg0)) !_2(arg0) else _2(arg0)
  }

  def nxor(p: Predicate1[T1], q: Predicate1[T2]) = new CompoundPredicate1(p, q){
    def apply(arg0: T1) = if(_1(arg0)) _2(arg0) else !_2(arg0)
  }

  def nand(p: Predicate1[T1], q: Predicate1[T2]) = new CompoundPredicate1(p, q){
    def apply(arg0: T1) = !(_1(arg0) && _2(arg0))
  }

  def nor(p: Predicate1[T1], q: Predicate1[T2]) = new CompoundPredicate1(p, q){
    def apply(arg0: T1) = !(_1(arg0) || _2(arg0))
  }
}

abstract class CompoundPredicate1[T1,T2 <: T1](val _1: Predicate1[T1], val _2: Predicate1[T2]) extends Predicate1[T1]
    with Product2[Predicate1[T1],Predicate1[T2]]{
  def hashCode = _1.hashCode * 31 + _2.hashCode

  def equals(that: Any) = that match{
    case x:CompoundPredicate1 => x._1 == _1 && x._2 == _2
    case _ => false
  }
}

class Not1[T1] extends Negation[Predicate1[T1]]{
  def not(pred: Predicate1[T1]) = new Predicate1[T1]{
    def apply(arg0: T1) = !pred(arg0)
  }
}

object Always1 extends Predicate1[Any]{
  def apply(arg0: Any) = true
}

object Never1 extends Predicate1[Any]{
  def apply(arg0: Any) = false
}