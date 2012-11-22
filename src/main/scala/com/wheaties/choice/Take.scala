package com.wheaties.choice

sealed trait Sat
sealed trait SatDef extends Sat
sealed trait SatUndef extends Sat

sealed trait Lim
sealed trait LimDef extends Lim
sealed trait LimUndef extends Lim



//TODO: fill in the nitty gritty
//object Choose extends Chooser[LimUndef,SatUndef]