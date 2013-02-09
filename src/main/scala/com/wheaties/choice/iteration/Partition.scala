package com.wheaties.choice.iteration

import scala.collection.generic.CanBuildFrom

trait Partition[Elem,Collection] extends ((Collection,Accept[Elem]) => (Collection,Collection)){
  def apply(collection: Collection, accept: Accept[Elem]): (Collection,Collection)
}

trait PartitionImplicits{
  implicit def partIterable[Elem,C[Elem] <: Iterable[Elem]](implicit cbf: CanBuildFrom[C[Elem],Elem,C[Elem]]) =
    new Partition[Elem,C[Elem]]{
      def apply(collection: C[Elem], accept: Accept[Elem]) ={
        val accTrue = cbf(collection)//implicitly[CanBuildFrom[C[Elem],Elem,C[Elem]]](collection)
        val accFalse = cbf(collection)
        val iter = collection.iterator
        while(iter hasNext){
          val next = iter next ()
          if(accept accept(next)) accTrue += next
          else accFalse += next
        }

        (accTrue result (), accFalse result ())
      }
    }

  implicit def partArray[Elem, A[Elem] <: Array[Elem]](implicit cbf: CanBuildFrom[A[Elem],Elem,A[Elem]]) =
    new Partition[Elem,A[Elem]]{
      def apply(collection: A[Elem], accept: Accept[Elem]) ={
        val accTrue = cbf(collection)
        val accFalse = cbf(collection)
        val iter = collection.iterator
        while(iter hasNext){
          val next = iter next ()
          if(accept accept(next)) accTrue += next
          else accFalse += next
        }

        (accTrue result (), accFalse result ())
      }
    }
}