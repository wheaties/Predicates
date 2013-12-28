package com.wheaties.choice.iteration

import scala.collection.{LinearSeq, TraversableLike}
import scala.collection.immutable.{ListSet, HashSet}
import scala.collection.mutable.{ArrayLike, ArrayBuffer, LinkedList, LinkedHashSet, ListBuffer, SortedSet, Set => MSet,
                                 MutableList, Stack, TreeSet, WrappedArray}

trait View[+Elem,Collection] extends ((Collection, Elem => Boolean) => Collection)

class TView[+Elem, Repr <: TraversableLike[Elem, Repr]] extends View[Elem, Repr]{
  def apply(coll: Repr, pred: Elem => Boolean): Repr = coll filter pred
}

trait ViewImplicits{
  implicit def abuffer[E] = new TView[E, ArrayBuffer[E]]
  implicit def hashset[E] = new TView[E, HashSet[E]]
  implicit def indexed[E] = new TView[E, IndexedSeq[E]]
  implicit def iter[E] = new TView[E, Iterable[E]]
  implicit def list[E] = new TView[E, List[E]]
  implicit def linseq[E] = new TView[E, LinearSeq[E]]
  implicit def linkhs[E] = new TView[E, LinkedHashSet[E]]
  implicit def linkl[E] = new TView[E, LinkedList[E]]
  implicit def listb[E] = new TView[E, ListBuffer[E]]
  implicit def listset[E] = new TView[E, ListSet[E]]
  implicit def mset[E] = new TView[E, MSet[E]]
  implicit def mlist[E] = new TView[E, MutableList[E]]
  implicit def seq[E] = new TView[E, Seq[E]]
  implicit def set[E] = new TView[E, Set[E]]
  implicit def sortedset[E] = new TView[E, SortedSet[E]]
  implicit def stack[E] = new TView[E, Stack[E]]
  implicit def stream[E] = new TView[E, Stream[E]]
  implicit def trav[E] = new TView[E, Traversable[E]]
  implicit def treeset[E] = new TView[E, TreeSet[E]]
  implicit def vector[E] = new TView[E, Vector[E]]

  implicit def iterator[E] = new View[E, Iterator[E]]{
    def apply(iter: Iterator[E], pred: E => Boolean) = iter filter pred
  }

  implicit def array[E] = new View[E, Array[E]]{
    def apply(a: Array[E], pred: E => Boolean) = a filter pred
  }
}