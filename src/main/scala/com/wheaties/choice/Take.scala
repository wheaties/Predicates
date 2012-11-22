package com.wheaties.choice

import com.wheaties.logical.{Connective, PredicateLike}

sealed trait Sat
sealed trait SatDef extends Sat
sealed trait SatUndef extends Sat

sealed trait Lim
sealed trait LimDef extends Lim
sealed trait LimUndef extends Lim

abstract class Choose[-V,L <: Lim,S <: Sat] extends Choice[V,L,S] with PredicateLike[Choose[V,L,S]]

object Choose{
  //TODO: looks like 9 connective definitions, if I'm not mistaken.


  def conUUUU[V1, V2 >: V1](choose1: Choose[V1,LimUndef,SatUndef], choose2: Choose[V2,LimUndef,SatUndef]) =
    new Connective[Choose[V1,LimUndef,SatUndef], Choose[V2,LimUndef,SatUndef], Choose[V1,LimUndef,SatUndef]]{

    }

  def conDUSU[V1, V2 >: V1, L2 <: Lim](choose1: Choose[V1,LimDef,SatUndef], choose2: Choose[V2,L2,SatUndef]) =
    new Connective[Choose[V1,LimUndef,SatUndef], Choose[V2,L2,SatUndef], Choose[V1,LimDef,SatUndef]]{

    }

  def conSUDU[V1, V2 >: V1, L1 <: Lim](choose1: Choose[V1,L1,SatUndef], choose2: Choose[V2,LimDef,SatUndef]) =
    new Connective[Choose[V1,L1,SatUndef], Choose[V2,LimDef,SatUndef], Choose[V1,LimDef,SatUndef]]{

    }

  def conUDUS[V1, V2 >: V1, S2 <: Sat](choose1: Choose[V1,LimUndef,SatDef], choose2: Choose[V2,LimUndef,S2]) =
    new Connective[Choose[V1,LimUndef,SatDef], Choose[V2,LimUndef,S2], Choose[V1,LimUndef,SatDef]]{

    }

  def conUSUD[V1, V2 >: V1, S1 <: Sat](choose1: Choose[V1,LimUndef,S1], choose2: Choose[V2,LimUndef,SatDef]) =
    new Connective[Choose[V1,LimUndef,S1], Choose[V2,LimUndef,SatDef], Choose[V1,LimUndef,SatDef]]{

    }
}

abstract class ChooseConUU[V1, V2 >: V1, C1 <: Choose[V1,LimUndef,SatUndef],C2 <: Choose[V2,LimUndef,SatUndef]]
    extends Connective[C1,C2,Choose[V1,LimUndef,SatUndef]]{
  def and(p: C1, q: C2) = ???

  def or(p: C1, q: C2) = ???

  def xor(p: C1, q: C2) = ???

  def nand(p: C1, q: C2) = ???

  def nor(p: C1, q: C2) = ???

  def nxor(p: C1, q: C2) = ???
}

abstract class ChooseConDU[V1, V2 >: V1, C1 <: Choose[V1,_,SatUndef],C2 <: Choose[V2,_,SatUndef]]
    extends Connective[C1,C2,Choose[V1,LimDef,SatUndef]]{
  def and(p: C1, q: C2) = ???

  def or(p: C1, q: C2) = ???

  def xor(p: C1, q: C2) = ???

  def nand(p: C1, q: C2) = ???

  def nor(p: C1, q: C2) = ???

  def nxor(p: C1, q: C2) = ???
}

abstract class ChooseConUD[V1, V2 >: V1, C1 <: Choose[V1,LimUndef,_],C2 <: Choose[V2,LimUndef,_]]
    extends Connective[C1,C2,Choose[V1,LimUndef,SatDef]]{
  def and(p: C1, q: C2) = ???

  def or(p: C1, q: C2) = ???

  def xor(p: C1, q: C2) = ???

  def nand(p: C1, q: C2) = ???

  def nor(p: C1, q: C2) = ???

  def nxor(p: C1, q: C2) = ???
}

abstract class ChooseConDD[V1, V2 >: V1, C1 <: Choose[V1,_,_],C2 <: Choose[V2,_,_]]
    extends Connective[C1,C2,Choose[V1,LimDef,SatDef]]{
  def and(p: C1, q: C2) = ???

  def or(p: C1, q: C2) = ???

  def xor(p: C1, q: C2) = ???

  def nand(p: C1, q: C2) = ???

  def nor(p: C1, q: C2) = ???

  def nxor(p: C1, q: C2) = ???
}