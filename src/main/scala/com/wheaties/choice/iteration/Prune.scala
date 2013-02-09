package com.wheaties.choice.iteration

trait Prune[Elem,Collection] extends ((Collection,Accept[Elem]) => Collection){
  def apply(collection: Collection, accept: Accept[Elem]): Collection
}

trait PruneImplicits{
  implicit def pruneIter[Elem,C[Elem] <: Iterable[Elem]] =
    new Prune[Elem,C[Elem]]{
      def apply(collection: C[Elem], accept: Accept[Elem]) ={
        val acc = implicitly[CanBuildFrom[C[Elem],Elem,C[Elem]]](collection)
        val iter = collection.iterator
        while(iter hasNext){
          val next = iter next ()
          if(accept accept(next)) acc += next
        }

        acc result ()
      }
    }

  implicit def pruneArray[Elem,A[Elem] <: Array[Elem]] =
    new Prune[Elem,A[Elem]]{
      def apply(collection: A[Elem], accept: Accept[Elem]) ={
        val acc = implicitly[CanBuildFrom[A[Elem],Elem,A[Elem]]](collection)
        val iter = collection.iterator
        while(iter hasNext){
          val next = iter next ()
          if(accept accept(next)) acc += next
        }

        acc result ()
      }
    }
}