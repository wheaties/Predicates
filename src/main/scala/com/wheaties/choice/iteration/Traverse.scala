package com.wheaties.choice.iteration

trait Traverse[Elem,To,C[_]]
    extends ((C[Elem],Accum[To,C[To]],Accum[To,C[To]],Accept[Elem]) => (Accum[To,C[To]],Accum[To,C[To]])){
  def onTrue[A <: Elem](t: Elem => A): Traverse[Elem,A,C]
  def onFalse[A <: Elem](f: Elem => A): Traverse[Elem,A,C]
  def on[A](f: Elem => A, t: Elem => A): Traverse[Elem,A,C]
}

//Do nothing accumulator for get, same acc for set, two different for partition
case class TraverseIterable[Elem,C[_] <: Iterable[_]](t: Elem => Elem = identity[Elem], f: Elem => Elem = identity[Elem])
    extends Traverse[Elem,Elem,C]{
  def onTrue[To <: Elem](func: Elem => To) = copy(t = func)
  def onFalse[To <: Elem](func: Elem => To) = copy(f = func)
  def on[A](fFunc: Elem => A, tFunc: Elem => A): Traverse[Elem,A,C] = copy(t = tFunc, f = fFunc)

  def apply(collection: C[Elem], accTrue: Accum[Elem,C[Elem]], accFalse: Accum[Elem,C[Elem]], accept: Accept[Elem]) ={
    val iter = collection.toIterator
    while(iter hasNext){
      val next = iter next ()
      if(accept accept(next)) accTrue += t(next)
      else accFalse += f(next)
    }

    (accTrue, accFalse)
  }
}

