package com.wheaties.choice.iteration

trait Traverse[Elem,Collection] extends ((Collection,Accept[Elem]) => Collection){

  def onTrue(t: Elem => Elem): Traverse[Elem,Collection]
  def onFalse(f: Elem => Elem): Traverse[Elem,Collection]
  def on(f: Elem => Elem, t: Elem => Elem): Traverse[Elem,Collection]

  def apply(collection: Collection, accept: Accept[Elem]): Collection
}

trait TraverseImplicits{
  implicit def traverseIter[Elem,C[Elem] <: Iterable[Elem]] = TraverseIterable()
  implicit def traverseArray[Elem,A[Elem] <: Array[Elem]] = TraverseIterable()

  case class TraverseIterable[Elem,C[Elem] <: Iterable[Elem]](t: Elem => Elem = identity[Elem] _,
                                                              f: Elem => Elem = identity[Elem] _)
      extends Traverse[Elem,C[Elem]]{
    def onTrue(func: Elem => Elem) = copy(t = func)
    def onFalse(func: Elem => Elem) = copy(f = func)
    def on(fFunc: Elem => Elem, tFunc: Elem => Elem) = copy(t = tFunc, f = fFunc)

    def apply(collection: C[Elem], accept: Accept[Elem]) ={
      val acc = collection.companion.newBuilder[Elem]
      val iter = collection.iterator
      while(iter hasNext){
        val next = iter next ()
        if(accept accept(next)) acc += t(next)
        else acc += f(next)
      }

      acc result ()
    }
  }

  case class TraverseArray[Elem : ClassManifest,
                           A[Elem] <: Array[Elem]](t: Elem => Elem = identity[Elem] _,
                                                   f: Elem => Elem = identity[Elem] _)
    extends Traverse[Elem,A[Elem]]{
    def onTrue(func: Elem => Elem) = copy(t = func)
    def onFalse(func: Elem => Elem) = copy(f = func)
    def on(fFunc: Elem => Elem, tFunc: Elem => Elem) = copy(t = tFunc, f = fFunc)

    def apply(collection: A[Elem], accept: Accept[Elem]) ={
      val acc = collection.companion.newBuilder[Elem]
      val iter = collection.iterator
      while(iter hasNext){
        val next = iter next ()
        if(accept accept(next)) acc += t(next)
        else acc += f(next)
      }

      acc result ()
    }
  }
}