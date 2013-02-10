package com.wheaties.choice.iteration

import scala.collection.mutable.{Builder, ArrayBuffer}

//TODO: The one problem is Stream! Can't iterate over it. It could be infinite... Need to constraint the type bounds.

trait Traverse[Elem,Collection]{
  def prune(collection: Collection, accept: Accept[Elem]): Collection
  def replace(collection: Collection, f: Elem => Elem, accept: Accept[Elem]): Collection
  def part(collection: Collection, accept: Accept[Elem]): (Collection, Collection)
//  def fold[B](init: B)(collection: Collection, f: (B,Elem) => Elem, accept: Accept[Elem]): B
//  def reduce[B](collection: Collection, f: (B,Elem) => Elem, accept: Accept[Elem]): B
}

trait TraverseIterable[Elem,Collection <: Iterable[Elem]] extends Traverse[Elem,Collection]{

  def prune(collection: Collection, accept: Accept[Elem]) ={
    val acc = builder
    def onNext(next: Elem){ if(accept accept(next)) acc += t(next) }
    traverse(collection, onNext)

    acc result ()
  }

  def replace(collection: Collection, f: Elem => Elem, accept: Accept[Elem]) ={
    val acc = builder
    def onNext(next: Elem){ if(accept accept(next)) acc += f(next) else acc += next }
    traverse(collection, onNext)

    acc result ()
  }

  def part(collection: Collection, accept: Accept[Elem]) ={
    val accTrue = builder
    val accFalse = builder
    def onNext(next: Elem){ if(accept accept(next)) accTrue += next else accFalse += next }

    traverse(collection, onNext)

    (accTrue result (), accFalse result ())
  }

//    def fold[B](init: B)(collection: Collection, f: (B,Elem) => B, accept: Accept[Elem]) ={
//      var current = init
//      def onTrue(elem: Elem){ current = f(current, elem) }
//
//      current
//    }
//
//    @tailrec final def reduce[B](collection: Collection, f: (B,Elem) => Elem, accept: Accept[Elem]) ={
//      if(accept accept (collection.head)) fold(collection.head)(collection.tail, f, accept)
//      else reduce(collection.tail, f, accept)
//    }

  protected def traverse(collection: Collection, action: Elem => Unit) ={
    val iter = collection.iterator
    while(iter hasNext){
      action(iter next ())
    }
  }

  protected def builder: Builder[Elem,Collection]
}

trait TraverseImplicits{
  implicit def list[Elem,List[Elem]]: Traverse[Elem,List[Elem]] = new TraverseIterable[Elem,List[Elem]] {
    protected def builder = List.newBuilder[Elem]
  }

  implicit def set[Elem,Set[Elem]]: Traverse[Elem,Set[Elem]] = new TraverseIterable[Elem,Set[Elem]] {
    protected def builder = Set.newBuilder[Elem]
  }

  implicit def traverseArray[Elem : ClassManifest,Array[Elem]] = new Traverse[Elem,Array[Elem]]{
    def prune(collection: Array[Elem], accept: Accept[Elem]) ={
      val acc = Array.newBuilder[Elem]
      def onNext(next: Elem){ if(accept accept(next)) acc += t(next) }
      traverse(collection, onNext)

      acc result ()
    }

    def replace(collection: Array[Elem], f: Elem => Elem, accept: Accept[Elem]) ={
      val acc = Array.newBuilder[Elem]
      def onNext(next: Elem){ if(accept accept(next)) acc += f(next) else acc += next }
      traverse(collection, onNext)

      acc result ()
    }

    def part(collection: Array[Elem], accept: Accept[Elem]) ={
      val accTrue = Array.newBuilder[Elem]
      val accFalse = Array.newBuilder[Elem]
      def onNext(next: Elem){ if(accept accept(next)) accTrue += next else accFalse += next }
      traverse(collection, onNext)

      (accTrue result (), accFalse result ())
    }

    protected def traverse(collection: Array[Elem], action: Elem => Unit) ={
      var index = 0
      val count = collection.length
      while(index < count){
        action(collection(index))
      }
    }
  }
}