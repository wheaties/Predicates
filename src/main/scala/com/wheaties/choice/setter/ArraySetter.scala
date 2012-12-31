package com.wheaties.choice.setter

import com.wheaties.choice.iteration.IterationScheme

abstract class ArraySetter[A : ClassManifest,B <: A : ClassManifest] extends Setter[Array[A], B]{
  def set(collection: Array[A], value: B, scheme: IterationScheme) ={
    val buffer = Array.newBuilder[A]
    for(indx <- 0 to collection.length){
      if(scheme accept (collection[indx])){
        buffer += value
      }
      else{
        buffer += collection[indx]
      }
    }

    buffer result ()
  }
}

abstract class ArraySetterR[A : ClassManifest,B <: A, C[_] <: Iterable[_]] extends Setter[Array[A],C[B]]{
  def set(collection: Array[A], value: C[B], scheme: IterationScheme) ={
    val buffer = Array.newBuilder[A]
    val subIter = value toIterator
    var indx = 0
    while(indx < collection.length && subIter.hasNext){
      if(scheme accept (collection[indx])){
        buffer += subIter next
      }
      else{
        buffer += collection[indx]
      }

      indx += 1
    }

    buffer result ()
  }
}