package com.wheaties.choice.getter

import com.wheaties.choice.iteration.IterationScheme
import scala.collection.mutable.MutableList

abstract class ListGetter[A] extends Getter[List[A]]{
  def get(collection: List[A], scheme: IterationScheme) ={
    val builder = new MultableList[A]
    val iter = collection toIterator
    while(iter hasNext){
      val next = iter next ()
      if(scheme accept (next)) builder += next
    }

    builder toList
  }
}

abstract class MutableListGetter[A] extends Getter[MutableList[A]]{
  def get(collection: MutableList[A], scheme: IterationScheme) ={
    val result = new MutableList[A]
    val iter = collection toIterator
    while(iter hasNext){
      val next = iter next ()
      if(scheme accept (next)) result += next
    }

    result
  }
}