package com.wheaties.choice

import com.wheaties.choice.iteration.IterationScheme

//TODO: there's got to be a way to "partiion" things
//TODO: this.type bad idea I think
trait Choice[-Value]{
  self =>

  def get[A](collection: A)(implicit getter: Getter[A]): A
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]): A
  //def mod[A,B,C](collection: A, f: B)(implicit modifer: Modder[A,B,C]): C

  def compose[V <: Value](that: Choice[V]): Choice[V] = that andThen this

  def andThen[V >: Value](that: Choice[V]): Choice[Value] = new Choice[Value]{
    def get[A](collection: A)(implicit getter: Getter[A]) = that get (self get (collection))
    def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = 
      self set(collection, that set (self get (collection), value))
    //def mod[A,B,C](collection: A, f: B)(implicit modifer: Modder[A,B,C]) =
    //  self set(collection, that mod(self get (collection), f))
  }
}

//This is a choice that already has a satisfiability condition, thus can't have another
trait ChoiceS[-Value] extends Choice[Value]{
  def every[Out](n: Int)(implicit limit: Limit[Value,this.type,Out]) = limit.every(this, n)
  def all[Out](implicit limit: Limit[Value,this.type,Out]) = limit.all(this)
  def first[Out](n: Int)(implicit limit: Limit[Value,this.type,Out]) = limit.first(this, n)
  //def last[Out](n: Int)(implicit limit: Limit[Value,this.type,Out]) = limit.last(this, n)
  def exactly[Out](n: Int)(implicit limit: Limit[Value,this.type,Out]) = limit.exactly(this, n)
}

//This is a Choice that already is limited, thus can't be limited again.
trait ChoiceL extends Choice[Any]{
  def satisfying[B,Out](pred: B => Boolean)(implicit cond: Conditional[this.type,Out]) = cond.condition[B](this, pred)
}

trait Getter[A]{
  def get(collection: A, scheme: IterationScheme): A
}

trait Setter[A,B]{
  def set(collection: A, value: B, scheme: IterationScheme): A
}

//trait Modder[In,Mapper,Out]{
//  def mod(collection: In, f: Mapper): Out
//}

trait Limit[V, C <: ChoiceS[V], Out <: Choice[V]]{
  def every(in: C, n: Int): Out
  def all(in: C): Out
  def first(in: C, n: Int): Out
  //def last(in: C, n: Int): Out
  def exactly(in: C, n: Int): Out
}

trait Conditional[C <: ChoiceL, Out <: Choice[_]]{
  def condition[B](in: C, pred: B => Boolean): Out[B]
}