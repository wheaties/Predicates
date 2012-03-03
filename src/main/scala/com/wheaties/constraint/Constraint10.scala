package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint10[A,B,C,D,E,F,G,H,I,J]extends Predicate1[Function10[A,B,C,D,E,F,G,H,I,J,Boolean]]{
	def constrain[K](that: Constraint1[K]) = new Constraint11[A,B,C,D,E,F,G,H,I,J,K]{
		def apply(pred: Function11[A,B,C,D,E,F,G,H,I,J,K,Boolean]) = Constraint10.this((x0,x1,x2,x3,x4,x5,x6,x7,x8,x9) => that(pred(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,_)))
	}

	def suchThat(pred: Function10[A,B,C,D,E,F,G,H,I,J,Boolean]) = apply(pred)
}