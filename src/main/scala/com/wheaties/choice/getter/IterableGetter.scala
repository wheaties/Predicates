package com.wheaties.choice.getter

import com.wheaties.choice.iteration.IterationScheme
import com.scala.collection.mutable.Builder
import com.wheaties.choice.getter.IterableGetter

trait IterableGetter[BuildType, Collection <: Iterable[BuildType]] extends Getter[Collection]{
  def get(collection: Collection, scheme: IterationScheme) ={
    val buffer = builder
    val iter = collection toIterator
    var current = scheme
    var count = 1
    while(iter hasNext){
      val value = iter next ()
      if(current accept (value, count)) buffer += value
      current = scheme next (value, count)
      count += 1
    }

    buffer result ()
  }

  def builder: Builder[BuildType, Collection]
}

//trait IterableGetter[A,C[_] <: Iterable[_]] extends Getter[C[A]]{
//  def get(collection: C[A], scheme: IterationScheme) ={
//    val buffer = builder
//    val iter = collection toIterator
//    var current = scheme
//    var count = 1
//    while(iter hasNext){
//      val value = iter next ()
//      if(current accept (value, count)) buffer += value
//      current = scheme next (value, count)
//      count += 1
//    }
//
//    buffer result ()
//  }
//
//  def builder: Builder[A, C[A]]
//}
//
//trait IterableGetter2[A,B,C[_,_] <: Iterable[(_,_)]] extends Getter[C[A,B]]{
//  def get(collection: C[A,B], scheme: IterationScheme) ={
//    val buffer = builder
//    val iter = collection toIterator
//    var current = scheme
//    var count = 1
//    while(iter hasNext){
//      val value = iter next ()
//      if(current accept (value, count)) buffer += value
//      current = scheme next (value, count)
//      count += 1
//    }
//
//    buffer result ()
//  }
//
//  def builder: Builder[(A,B), C[A,B]]
//}