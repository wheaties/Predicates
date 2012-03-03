package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint11[A,B,C,D,E,F,G,H,I,J,K]extends Predicate1[Function11[A,B,C,D,E,F,G,H,I,J,K,Boolean]]{
	def constrain[L](that: Constraint1[L]) = new Constraint12[A,B,C,D,E,F,G,H,I,J,K,L]{
		def apply(pred: Function12[A,B,C,D,E,F,G,H,I,J,K,L,Boolean]) = Constraint11.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,_)))
	}

	def suchThat(pred: Function11[A,B,C,D,E,F,G,H,I,J,K,Boolean]) = apply(pred)
}