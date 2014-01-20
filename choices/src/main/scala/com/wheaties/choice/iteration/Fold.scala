package com.wheaties.choice.iteration

import scala.collection.TraversableLike
import scala.collection.mutable.ArrayLike

trait FoldL[Out, Elem, Collection] extends ((Out, Collection, (Out, Elem) => Out, Elem => Boolean) => Out)

trait FoldR[Out, Elem, Collection] extends ((Out, Collection, (Elem, Out) => Out, Elem => Boolean) => Out)

trait FoldImplicits{
  implicit def foldLTraversable[Out, Elem, Repr <: TraversableLike[Elem, Repr]] = new FoldL[Out, Elem, Repr] {
      def apply(init: Out, coll: Repr, f: (Out, Elem) => Out, pred: Elem => Boolean): Out =
        coll.foldLeft(init){ (x,y) => if(pred(y)) f(x,y) else x }
    }

  implicit def foldLArray[Out, Elem] = new FoldL[Out, Elem, Array[Elem]]{
    def apply(init: Out, coll: Array[Elem], f: (Out, Elem) => Out, pred: Elem => Boolean): Out =
      coll.foldLeft(init){ (x,y) => if(pred(y)) f(x,y) else x }
  }

  implicit def foldLArrayLike[Out, Elem, Repr <: ArrayLike[Elem, Repr]] = new FoldL[Out, Elem, Repr]{
    def apply(init: Out, coll: Repr, f: (Out, Elem) => Out, pred: Elem => Boolean): Out =
      coll.foldLeft(init){ (x,y) => if(pred(y)) f(x,y) else x }
  }

  implicit def foldRTraversable[Out, Elem, Repr <: TraversableLike[Elem, Repr]] = new FoldR[Out, Elem, Repr] {
    def apply(init: Out, coll: Repr, f: (Elem, Out) => Out, pred: Elem => Boolean): Out =
      coll.foldRight(init){ (x,y) => if(pred(x)) f(x,y) else y }
  }

  implicit def foldRArray[Out, Elem] = new FoldR[Out, Elem, Array[Elem]]{
    def apply(init: Out, coll: Array[Elem], f: (Elem, Out) => Out, pred: Elem => Boolean): Out =
      coll.foldRight(init){ (x,y) => if(pred(x)) f(x,y) else y }
  }

  implicit def foldArrayLike[Out, Elem, Repr <: ArrayLike[Elem, Repr]] = new FoldR[Out, Elem, Repr]{
    def apply(init: Out, coll: Repr, f: (Elem, Out) => Out, pred: Elem => Boolean): Out =
      coll.foldRight(init){ (x,y) => if(pred(x)) f(x,y) else y }
  }
}
