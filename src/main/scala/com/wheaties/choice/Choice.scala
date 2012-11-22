package com.wheaties.choice

//TODO: there's got to be a way to "partiion" things
//TODO: this.type bad idea I think
trait Choice[-Value, L <: Lim, S <: Sat]{
  self =>

  def get[A](collection: A)(implicit getter: Getter[A]): A
  def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]): A
  //def mod[A,B,C](collection: A, f: B)(implicit modifer: Modder[A,B,C]): C

  def compose[V <: Value](that: Choice[V,_,_]): Choice[V,LimDef,SatDef] = that andThen this

  def andThen[V >: Value](that: Choice[V,_,_]): Choice[Value,LimDef,SatDef] = new Choice[Value,LimDef,SatDef]{
    def get[A](collection: A)(implicit getter: Getter[A]) = that get (self get (collection))
    def set[A,B](collection: A, value: B)(implicit setter: Setter[A,B]) = 
      self set(collection, that set (self get (collection), value))
    //def mod[A,B,C](collection: A, f: B)(implicit modifer: Modder[A,B,C]) =
    //  self set(collection, that mod(self get (collection), f))
  }

  def every(n: Int)(implicit limit: Limit[this.type]) = limit.every[Value,S](this, n)
  def all(implicit limit: Limit[this.type]) = limit.all[Value,S](this)
  def first(n: Int)(implicit limit: Limit[this.type]) = limit.first[Value,S](this, n)
  def last(n: Int)(implicit limit: Limit[this.type]) = limit.last[Value,S](this, n)
  def exactly(n: Int)(implicit limit: Limit[this.type]) = limit.exactly[Value,S](this, n)

  def satisfying[B](pred: B => Boolean)(implicit cond: Conditional[this.type]) = cond.condition[L,B](this, pred)
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

trait Limit[A[_,LimUndef,_]]{
  def every[Value,S <: Sat](in: A, n: Int): A[Value,LimDef,S]
  def all[Value,S <: Sat](in: A): A[Value,LimDef,S]
  def first[Value,S <: Sat](in: A, n: Int): A[Value,LimDef,S]
  def last[Value,S <: Sat](in: A, n: Int): A[Value,LimDef,S]
  def exactly[Value,S <: Sat](in: A, n: Int): A[Value,LimDef,S]
}

trait Conditional[A[_,_,SatUndef]]{
  def condition[L <: Lim,B](in: A, pred: B => Boolean): A[B,L,SatDef]
}