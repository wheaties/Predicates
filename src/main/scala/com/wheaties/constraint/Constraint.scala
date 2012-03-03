package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

package object constraint{
  implicit def pred2const1[A](pred: Predicate1[Function1[A,Boolean]]) = new Constraint1[A]{
    def apply(arg: Function1[A,Boolean]) = pred(arg)
  }
  implicit def func2const1[A](func: Function1[A,Boolean] => Boolean) = new Constraint1[A]{
    def apply(arg: Function1[A,Boolean]) = func(arg)
  }

  implicit def pred2const2[A,B](pred: Predicate1[Function2[A,B,Boolean]]) = new Constraint2[A,B]{
    def apply(arg: Function2[A,B,Boolean]) = pred(arg)
  }
  implicit def func2const2[A,B](func: Function2[A,B,Boolean] => Boolean) = new Constraint2[A,B]{
    def apply(arg: Function2[A,B,Boolean]) = func(arg)
  }

  implicit def pred2const3[A,B,C](pred: Predicate1[Function3[A,B,C,Boolean]]) = new Constraint3[A,B,C]{
    def apply(arg: Function3[A,B,C,Boolean]) = pred(arg)
  }
  implicit def func2const3[A,B,C](func: Function3[A,B,C,Boolean] => Boolean) = new Constraint3[A,B,C]{
    def apply(arg: Function3[A,B,C,Boolean]) = func(arg)
  }

  implicit def pred2const4[A,B,C,D](pred: Predicate1[Function4[A,B,C,D,Boolean]]) = new Constraint4[A,B,C,D]{
    def apply(arg: Function4[A,B,C,D,Boolean]) = pred(arg)
  }
  implicit def func2const4[A,B,C,D](func: Function4[A,B,C,D,Boolean] => Boolean) = new Constraint4[A,B,C,D]{
    def apply(arg: Function4[A,B,C,D,Boolean]) = func(arg)
  }

  implicit def pred2const5[A,B,C,D,E](pred: Predicate1[Function5[A,B,C,D,E,Boolean]]) = new Constraint5[A,B,C,D,E]{
    def apply(arg: Function5[A,B,C,D,E,Boolean]) = pred(arg)
  }
  implicit def func2const5[A,B,C,D,E](func: Function5[A,B,C,D,E,Boolean] => Boolean) = new Constraint5[A,B,C,D,E]{
    def apply(arg: Function5[A,B,C,D,E,Boolean]) = func(arg)
  }

  implicit def pred2const6[A,B,C,D,E,F](pred: Predicate1[Function6[A,B,C,D,E,F,Boolean]]) = new Constraint6[A,B,C,D,E,F]{
    def apply(arg: Function6[A,B,C,D,E,F,Boolean]) = pred(arg)
  }
  implicit def func2const6[A,B,C,D,E,F](func: Function6[A,B,C,D,E,F,Boolean] => Boolean) = new Constraint6[A,B,C,D,E,F]{
    def apply(arg: Function6[A,B,C,D,E,F,Boolean]) = func(arg)
  }

  implicit def pred2const7[A,B,C,D,E,F,G](pred: Predicate1[Function7[A,B,C,D,E,F,G,Boolean]]) = new Constraint7[A,B,C,D,E,F,G]{
    def apply(arg: Function7[A,B,C,D,E,F,G,Boolean]) = pred(arg)
  }
  implicit def func2const7[A,B,C,D,E,F,G](func: Function7[A,B,C,D,E,F,G,Boolean] => Boolean) = new Constraint7[A,B,C,D,E,F,G]{
    def apply(arg: Function7[A,B,C,D,E,F,G,Boolean]) = func(arg)
  }

  implicit def pred2const8[A,B,C,D,E,F,G,H](pred: Predicate1[Function8[A,B,C,D,E,F,G,H,Boolean]]) = new Constraint8[A,B,C,D,E,F,G,H]{
    def apply(arg: Function8[A,B,C,D,E,F,G,H,Boolean]) = pred(arg)
  }
  implicit def func2const8[A,B,C,D,E,F,G,H](func: Function8[A,B,C,D,E,F,G,H,Boolean] => Boolean) = new Constraint8[A,B,C,D,E,F,G,H]{
    def apply(arg: Function8[A,B,C,D,E,F,G,H,Boolean]) = func(arg)
  }

  implicit def pred2const9[A,B,C,D,E,F,G,H,I](pred: Predicate1[Function9[A,B,C,D,E,F,G,H,I,Boolean]]) = new Constraint9[A,B,C,D,E,F,G,H,I]{
    def apply(arg: Function9[A,B,C,D,E,F,G,H,I,Boolean]) = pred(arg)
  }
  implicit def func2const9[A,B,C,D,E,F,G,H,I](func: Function9[A,B,C,D,E,F,G,H,I,Boolean] => Boolean) = new Constraint9[A,B,C,D,E,F,G,H,I]{
    def apply(arg: Function9[A,B,C,D,E,F,G,H,I,Boolean]) = func(arg)
  }

  implicit def pred2const10[A,B,C,D,E,F,G,H,I,J](pred: Predicate1[Function10[A,B,C,D,E,F,G,H,I,J,Boolean]]) = new Constraint10[A,B,C,D,E,F,G,H,I,J]{
    def apply(arg: Function10[A,B,C,D,E,F,G,H,I,J,Boolean]) = pred(arg)
  }
  implicit def func2const10[A,B,C,D,E,F,G,H,I,J](func: Function10[A,B,C,D,E,F,G,H,I,J,Boolean] => Boolean) = new Constraint10[A,B,C,D,E,F,G,H,I,J]{
    def apply(arg: Function10[A,B,C,D,E,F,G,H,I,J,Boolean]) = func(arg)
  }

  implicit def pred2const11[A,B,C,D,E,F,G,H,I,J,K](pred: Predicate1[Function11[A,B,C,D,E,F,G,H,I,J,K,Boolean]]) = new Constraint11[A,B,C,D,E,F,G,H,I,J,K]{
    def apply(arg: Function11[A,B,C,D,E,F,G,H,I,J,K,Boolean]) = pred(arg)
  }
  implicit def func2const11[A,B,C,D,E,F,G,H,I,J,K](func: Function11[A,B,C,D,E,F,G,H,I,J,K,Boolean] => Boolean) = new Constraint11[A,B,C,D,E,F,G,H,I,J,K]{
    def apply(arg: Function11[A,B,C,D,E,F,G,H,I,J,K,Boolean]) = func(arg)
  }

  implicit def pred2const12[A,B,C,D,E,F,G,H,I,J,K,L](pred: Predicate1[Function12[A,B,C,D,E,F,G,H,I,J,K,L,Boolean]]) = new Constraint12[A,B,C,D,E,F,G,H,I,J,K,L]{
    def apply(arg: Function12[A,B,C,D,E,F,G,H,I,J,K,L,Boolean]) = pred(arg)
  }
  implicit def func2const12[A,B,C,D,E,F,G,H,I,J,K,L](func: Function12[A,B,C,D,E,F,G,H,I,J,K,L,Boolean] => Boolean) = new Constraint12[A,B,C,D,E,F,G,H,I,J,K,L]{
    def apply(arg: Function12[A,B,C,D,E,F,G,H,I,J,K,L,Boolean]) = func(arg)
  }

  implicit def pred2const13[A,B,C,D,E,F,G,H,I,J,K,L,M](pred: Predicate1[Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,Boolean]]) = new Constraint13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
    def apply(arg: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,Boolean]) = pred(arg)
  }
  implicit def func2const13[A,B,C,D,E,F,G,H,I,J,K,L,M](func: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,Boolean] => Boolean) = new Constraint13[A,B,C,D,E,F,G,H,I,J,K,L,M]{
    def apply(arg: Function13[A,B,C,D,E,F,G,H,I,J,K,L,M,Boolean]) = func(arg)
  }

  implicit def pred2const14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](pred: Predicate1[Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Boolean]]) = new Constraint14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
    def apply(arg: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Boolean]) = pred(arg)
  }
  implicit def func2const14[A,B,C,D,E,F,G,H,I,J,K,L,M,N](func: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Boolean] => Boolean) = new Constraint14[A,B,C,D,E,F,G,H,I,J,K,L,M,N]{
    def apply(arg: Function14[A,B,C,D,E,F,G,H,I,J,K,L,M,N,Boolean]) = func(arg)
  }

  implicit def pred2const15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](pred: Predicate1[Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Boolean]]) = new Constraint15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
    def apply(arg: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Boolean]) = pred(arg)
  }
  implicit def func2const15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O](func: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Boolean] => Boolean) = new Constraint15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]{
    def apply(arg: Function15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Boolean]) = func(arg)
  }

  implicit def pred2const16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](pred: Predicate1[Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Boolean]]) = new Constraint16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
    def apply(arg: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Boolean]) = pred(arg)
  }
  implicit def func2const16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P](func: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Boolean] => Boolean) = new Constraint16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P]{
    def apply(arg: Function16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Boolean]) = func(arg)
  }

  implicit def pred2const17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](pred: Predicate1[Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Boolean]]) = new Constraint17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
    def apply(arg: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Boolean]) = pred(arg)
  }
  implicit def func2const17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q](func: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Boolean] => Boolean) = new Constraint17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q]{
    def apply(arg: Function17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,Boolean]) = func(arg)
  }

  implicit def pred2const18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](pred: Predicate1[Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Boolean]]) = new Constraint18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
    def apply(arg: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Boolean]) = pred(arg)
  }
  implicit def func2const18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R](func: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Boolean] => Boolean) = new Constraint18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R]{
    def apply(arg: Function18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,Boolean]) = func(arg)
  }

  implicit def pred2const19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](pred: Predicate1[Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Boolean]]) = new Constraint19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
    def apply(arg: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Boolean]) = pred(arg)
  }
  implicit def func2const19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S](func: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Boolean] => Boolean) = new Constraint19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S]{
    def apply(arg: Function19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,Boolean]) = func(arg)
  }

  implicit def pred2const20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](pred: Predicate1[Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Boolean]]) = new Constraint20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
    def apply(arg: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Boolean]) = pred(arg)
  }
  implicit def func2const20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](func: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Boolean] => Boolean) = new Constraint20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T]{
    def apply(arg: Function20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,Boolean]) = func(arg)
  }

  implicit def pred2const21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](pred: Predicate1[Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Boolean]]) = new Constraint21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
    def apply(arg: Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Boolean]) = pred(arg)
  }
  implicit def func2const21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](func: Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Boolean] => Boolean) = new Constraint21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U]{
    def apply(arg: Function21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,Boolean]) = func(arg)
  }

  implicit def pred2const22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](pred: Predicate1[Function22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,Boolean]]) = new Constraint22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V]{
    def apply(arg: Function22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,Boolean]) = pred(arg)
  }
  implicit def func2const22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](func: Function22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,Boolean] => Boolean) = new Constraint22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V]{
    def apply(arg: Function22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,Boolean]) = func(arg)
  }
}