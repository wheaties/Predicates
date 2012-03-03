package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint9[A,B,C,D,E,F,G,H,I]extends Predicate1[Function9[A,B,C,D,E,F,G,H,I,Boolean]]{
	def constrain[J](that: Constraint1[J]) = new Constraint10[A,B,C,D,E,F,G,H,I]{
		def apply(pred: Function9[A,B,C,D,E,F,G,H,I,Boolean]) = Constraint9.this((x0,x1,x2,x3,x4,x5,x6,x7,x8) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,_)))
	}

	def suchThat(pred: Function9[A,B,C,D,E,F,G,H,I,Boolean]) = apply(pred)
}