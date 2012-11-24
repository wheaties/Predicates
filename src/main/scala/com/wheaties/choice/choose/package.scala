package com.wheaties.choice

import com.wheaties.choice.iteration._

package object choose {
  implicit def conCL[V, C1 <: Choose[V], C2 <: ChooseL] = new ChooseConCL[V]
  implicit def conCS[V1, V2 >: V1, C1 <: Choose[V1], C2 <: ChooseS[V2]] = new ChooseConCS[V1,V2]
  implicit def conCC[V1, V2 >: V1, C1 <: Choose[V1], C2 <: Choose[V2]] = new ChooseConCC[V1,V2]

  implicit def conSS[V1, V2 >: V1, C1 <: ChooseS[V1], C2 <: ChooseS[V2]] = new ChooseConSS[V1,V2]
  implicit def conSL[V, C1<: ChooseS[V], C2<: ChooseL] = new ChooseConLS[V]
  implicit def conSC[V1, V2 >: V1, C1 <: ChooseS[V1], C2 <: Choose[V2]] = new ChooseConSC[V1,V2]

  implicit def conLL[C1 <: ChooseL, C2 <: ChooseL] = ChooseConLL
  implicit def conLS[V, C1<: ChooseL, C2<: ChooseS[V]] = new ChooseConLS[V]
  implicit def conLC[V, C1 <: ChooseL, C2 <: Choose[V]] = new ChooseConLC[V]

  implicit def cond[C <: ChooseL] = new Conditional[ChooseL,Choose[_]]{
    def condition[B](c: ChooseL, pred: B => Boolean) = new Choose[B]{
      protected[choice] def scheme = (c scheme) andThen (new AcceptIf[B](pred))
    }
    def until[B](c: ChooseL, pred: B => Boolean) = new Choose[B] {
      protected[choice] def scheme = (c scheme) andThen (new AcceptUntil[B](pred))
    }
    def once[B](c: ChooseL, pred: B => Boolean) = new Choose[B] {
      protected[choice] def scheme = (c scheme) andThen (new AcceptOnce[B](pred))
    }
  }

  implicit def limit[V] = new Limit[V,ChooseS[V],Choose[V]]{
    def every(c: ChooseS[V], n: Int) = new Choose[V]{
      protected[choice] def scheme = (c scheme) andThen (new AcceptEvery(n))
    }

    def all(c: ChooseS[V]) = new Choose[V] {
      protected[choice] def scheme = c scheme
    }

    def first(c: ChooseS[V], n: Int) = new Choose[V] {
      protected[choice] def scheme = (c scheme) andThen (new AcceptFirst(n))
    }

    //def exactly(c: ChooseS[V], n: Int) = new Choose[V] {
    //  protected[choice] def scheme = (c scheme) andThen (new AcceptExactly(n))
    //}
  }
}