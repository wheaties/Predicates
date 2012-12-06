package com.wheaties.choice

import com.wheaties.choice.setter.Setter
import com.wheaties.choice.getter.Getter

//TODO: there's got to be a way to "partiion" things such that it composes...
//TODO: this.type bad idea I think
trait Choice[-Value]{
  self =>

  def get[A](collection: A)(implicit getter: Getter[A]): A
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]): A
  //def mod[A,B](collection: A, f: B)(implicit setter: Setter[A,B]): A

  def compose[V <: Value](that: Choice[V]): Choice[V] = that andThen this

  def andThen[V >: Value](that: Choice[V]): Choice[Value] = new Choice[Value]{
    def get[A](collection: A)(implicit getter: Getter[A]) = that get (self get (collection))
    def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = 
      self set(collection, that set (self get (collection), value))
    //def mod[A,B](collection: A, f: B)(implicit setter: Setter[A,B]) =
    //  self set(collection, that mod(self get (collection), f))
  }
}

//This is a choice that already has a satisfiability condition, thus can't have another
trait ChoiceS[-Value] extends Choice[Value]{
  def every[Out](n: Int)(implicit limit: Limit[Value,this.type,Out]) = limit every (this, n)
  def every[Out](f: Int => Int, init: Int)(implicit limit: Limit[Value,this.type,Out]) = limit every (this, f, init)
  def all[Out](implicit limit: Limit[Value,this.type,Out]) = limit all (this)
  def first[Out](n: Int)(implicit limit: Limit[Value,this.type,Out]) = limit first (this, n)
  //def last[Out](n: Int)(implicit limit: Limit[Value,this.type,Out]) = limit last (this, n)
  //def exactly[Out](n: Int)(implicit limit: Limit[Value,this.type,Out]) = limit exactly (this, n)
}

//This is a Choice that already is limited, thus can't be limited again.
trait ChoiceL extends Choice[Any]{
  def satisfying[B,Out](pred: B => Boolean)(implicit cond: Conditional[this.type,Out]) = cond.condition[B](this, pred)
  def until[B,Out](pred: B => Boolean)(implicit cond: Conditional[this.type,Out]) = cond.until[B](this, pred)
  def once[B,Out](pred: B => Boolean)(implicit cond: Conditional[this.type,Out]) = cond.once[B](this, pred)
}



////should this be able to handle List[A],A=>B,List[B] as well as List[A],List[A]=>B,B? No! Don't need F[A=>B] yet.
//trait Modder[In,Func,Out]{
//  def mod(collection: In, f: Func): Out
//}