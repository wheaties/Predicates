package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint7[A,B,C,D,E,F,G]extends Predicate1[Function7[A,B,C,D,E,F,G,Boolean]]{
	def constrain[H](that: Constraint1[H]) = new Constraint8[A,B,C,D,E,F,G]{
		def apply(pred: Function7[A,B,C,D,E,F,G,Boolean]) = Constraint7.this((x0,x1,x2,x3,x4,x5,x6) => that(pred(x0,x1,x2,x3,x4,x5,x6,_)))
	}

	def suchThat(pred: Function7[A,B,C,D,E,F,G,Boolean]) = apply(pred)
}