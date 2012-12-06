package com.wheaties.choice.getter

import com.wheaties.choice.iteration.IterationScheme

abstract class ArrayGetter[A : ClassManifest] extends Getter[Array[A]]{
  def get(collection: Array[A], scheme: IterationScheme) ={
    val buffer = Array.newBuilder[A]
    for(indx <- 0 to collection.length){
      if(scheme accept (collection[indx])) buffer += collection[indx]
    }

    buffer result ()
  }
}

abstract class WrappedArrayGetter[A] extends IterableGetter[A,WrappedArray]{
  def builder = WrappedArray.newBuilder[A]
}