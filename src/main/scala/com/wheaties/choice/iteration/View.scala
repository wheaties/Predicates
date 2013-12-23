package com.wheaties.choice.iteration

import collection.TraversableLike
import collection.mutable.ArrayLike

trait View[Elem,Collection] extends ((Collection, Elem => Boolean) => Collection)

trait ViewImplicits{
  implicit def viewTraversable[Elem, Repr <: TraversableLike[Elem, Repr]] = new View[Elem, Repr] {
    def apply(coll: Repr, pred: Elem => Boolean): Repr = coll filter pred
  }

  implicit def viewArrayLike[Elem, Repr <: ArrayLike[Elem, Repr]] = new View[Elem, Repr] {
    def apply(array: Repr, pred: Elem => Boolean) = array filter pred
  }
}