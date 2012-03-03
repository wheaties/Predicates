package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint8[A,B,C,D,E,F,G,H]extends Predicate1[Function8[A,B,C,D,E,F,G,H,Boolean]]{
	def constrain[I](that: Constraint1[I]) = new Constraint9[A,B,C,D,E,F,G,H,I]{
		def apply(pred: Function9[A,B,C,D,E,F,G,H,I,Boolean]) = Constraint8.this((x0,x1,x2,x3,x4,x5,x6,x7) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,_)))
	}

	def suchThat(pred: Function8[A,B,C,D,E,F,G,H,Boolean]) = apply(pred)
}