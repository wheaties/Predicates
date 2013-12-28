package com.wheaties.choice.iteration

import scala.collection.{LinearSeq, TraversableLike}
import collection.generic.CanBuildFrom
import scala.collection.immutable.{ListSet, HashSet}
import scala.collection.mutable.{ArrayLike, ArrayBuffer, LinkedList, LinkedHashSet, ListBuffer, SortedSet, Set => MSet,
                                 MutableList, Stack, TreeSet, WrappedArray}

trait Replace[+Elem,Collection,Substitute] extends ((Collection, Substitute, Elem => Boolean) => Collection)

class TVReplace[+Elem, Repr <: TraversableLike[Elem, Repr], V <: Elem](implicit cbf: CanBuildFrom[Repr, Elem, Repr])
    extends Replace[Elem, Repr, V]{

  def apply(repr: Repr, value: V, pred: Elem => Boolean): Repr ={
    def sub(elem: Elem) = if(pred(elem)) value else elem

    repr.map(sub)(cbf)
  }
}

class TTReplace[+Elem, Repr <: TraversableLike[Elem, Repr], TRepr <: TraversableLike[Elem, TRepr]](implicit cbf: CanBuildFrom[Repr, Elem, Repr])
  extends Replace[Elem, Repr, TRepr]{

  def apply(repr: Repr, value: TRepr, pred: Elem => Boolean): Repr ={
    val iter = value.toIterator
    def sub(elem: Elem) = if(pred(elem) && iter.hasNext) iter next () else elem

    repr.map(sub)(cbf)
  }
}

class TAReplace[+Elem, Repr <: TraversableLike[Elem, Repr], V <: Elem](implicit cbf: CanBuildFrom[Repr, Elem, Repr])
  extends Replace[Elem, Repr, Array[V]]{

  def apply(repr: Repr, value: Array[V], pred: Elem => Boolean): Repr ={
    val iter = value.toIterator
    def sub(elem: Elem) = if(pred(elem) && iter.hasNext) iter next () else elem

    repr.map(sub)(cbf)
  }
}

class TAlikeReplace[+Elem, Repr <: TraversableLike[Elem, Repr], V <: Elem, ARepr <: ArrayLike[V, ARepr]](implicit cbf: CanBuildFrom[Repr, Elem, Repr])
  extends Replace[Elem, Repr, ARepr]{

  def apply(repr: Repr, value: ARepr, pred: Elem => Boolean): Repr ={
    val iter = value.toIterator
    def sub(elem: Elem) = if(pred(elem) && iter.hasNext) iter next () else elem

    repr.map(sub)(cbf)
  }
}


trait ReplaceImplicits{
  implicit def abuffer[E, V <: E] = new TVReplace[E, ArrayBuffer[E], V]
  implicit def hashsetV[E, V <: E] = new TVReplace[E, HashSet[E], V]
  implicit def indexedV[E, V <: E] = new TVReplace[E, IndexedSeq[E], V]
  implicit def iterV[E, V <: E] = new TVReplace[E, Iterable[E], V]
  implicit def listV[E, V <: E] = new TVReplace[E, List[E], V]
  implicit def linseqV[E, V <: E] = new TVReplace[E, LinearSeq[E], V]
  implicit def linkhsV[E, V <: E] = new TVReplace[E, LinkedHashSet[E], V]
  implicit def linklV[E, V <: E] = new TVReplace[E, LinkedList[E], V]
  implicit def listbV[E, V <: E] = new TVReplace[E, ListBuffer[E], V]
  implicit def listsetV[E, V <: E] = new TVReplace[E, ListSet[E], V]
  implicit def msetV[E, V <: E] = new TVReplace[E, MSet[E], V]
  implicit def mlistV[E, V <: E] = new TVReplace[E, MutableList[E], V]
  implicit def seqV[E, V <: E] = new TVReplace[E, Seq[E], V]
  implicit def setV[E, V <: E] = new TVReplace[E, Set[E], V]
  implicit def sortedsetV[E, V <: E] = new TVReplace[E, SortedSet[E], V]
  implicit def stackV[E, V <: E] = new TVReplace[E, Stack[E], V]
  implicit def streamV[E, V <: E] = new TVReplace[E, Stream[E], V]
  implicit def travV[E, V <: E] = new TVReplace[E, Traversable[E], V]
  implicit def treesetV[E, V <: E] = new TVReplace[E, TreeSet[E], V]
  implicit def vectorV[E, V <: E] = new TVReplace[E, Vector[E], V]

  implicit def abuffer[E, Repr <: TraversableLike[E, Repr]] = new TVReplace[E, ArrayBuffer[E], Repr]
  implicit def hashsetT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, HashSet[E], Repr]
  implicit def indexedT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, IndexedSeq[E], Repr]
  implicit def iterT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, Iterable[E], Repr]
  implicit def listT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, List[E], Repr]
  implicit def linseqT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, LinearSeq[E], Repr]
  implicit def linkhsT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, LinkedHashSet[E], Repr]
  implicit def linklT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, LinkedList[E], Repr]
  implicit def listbT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, ListBuffer[E], Repr]
  implicit def listsetT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, ListSet[E], Repr]
  implicit def msetT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, MSet[E], Repr]
  implicit def mlistT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, MutableList[E], Repr]
  implicit def seqT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, Seq[E], Repr]
  implicit def setT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, Set[E], Repr]
  implicit def sortedsetT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, SortedSet[E], Repr]
  implicit def stackT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, Stack[E], Repr]
  implicit def streamT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, Stream[E], Repr]
  implicit def travT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, Traversable[E], Repr]
  implicit def treesetT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, TreeSet[E], Repr]
  implicit def vectorT[E, Repr <: TraversableLike[E, Repr]] = new TTReplace[E, Vector[E], Repr]

  implicit def listA[E, V <: E] = new TAReplace[E, List[E], V]
  implicit def setA[E, V <: E] = new TAReplace[E, Set[E], V]
  implicit def seqA[E, V <: E] = new TAReplace[E, Seq[E], V]

  implicit def repArray[Elem, Repr <: ArrayLike[Elem, Repr]](implicit cbf: CanBuildFrom[Repr, Elem, Repr]) =
    new Replace[Elem, Repr, Elem] {
      def apply(coll: Repr, value: Elem, pred: Elem => Boolean): Repr ={
        def sub(elem: Elem) = if(pred(elem)) value else elem

        coll.map(sub)(cbf)
      }
    }

  implicit def repTraversableSub[Elem, Repr <: TraversableLike[Elem, Repr], From <: TraversableLike[Elem, From]](implicit cbf: CanBuildFrom[Repr, Elem, Repr])=
    new Replace[Elem, Repr, From] {
      def apply(coll: Repr, value: From, pred: Elem => Boolean): Repr ={
        val iter = value.toIterator
        def sub(elem: Elem) = if(pred(elem) && iter.hasNext) iter next () else elem

        coll.map(sub)(cbf)
      }
    }

  implicit def repTraversableArray[Elem, Sub <: Elem, Repr <: TraversableLike[Elem, Repr], ARepr <: ArrayLike[Sub, ARepr]](implicit cbf: CanBuildFrom[Repr, Elem, Repr]) =
    new Replace[Elem, Repr, ARepr] {
      def apply(coll: Repr, value: ARepr, pred: Elem => Boolean): Repr ={
        val iter = value.toIterator
        def sub(elem: Elem) = if(pred(elem) && iter.hasNext) iter next () else elem

        coll.map(sub)(cbf)
      }
    }

  implicit def repArraySub[Elem, Sub <: Elem, Repr <: ArrayLike[Elem, Repr], ReprSub <: ArrayLike[Sub, ReprSub]](implicit cbf: CanBuildFrom[Repr, Elem, Repr]) =
    new Replace[Elem, Repr, ReprSub] {
      def apply(coll: Repr, value: ReprSub, pred: Elem => Boolean): Repr ={
        val iter = value.toIterator
        def sub(elem: Elem) = if(pred(elem) && iter.hasNext) iter next () else elem

        coll.map(sub)(cbf)
      }
  }
}