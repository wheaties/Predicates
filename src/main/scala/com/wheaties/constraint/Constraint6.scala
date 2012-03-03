package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint6[A,B,C,D,E,F]extends Predicate1[Function6[A,B,C,D,E,F,Boolean]]{
	def constrain[G](that: Constraint1[G]) = new Constraint7[A,B,C,D,E,F,G]{
		def apply(pred: Function7[A,B,C,D,E,F,G,Boolean]) = Constraint6.this((x0,x1,x2,x3,x4,x5) => that(pred(x0,x1,x2,x3,x4,x5,_)))
	}

	def suchThat(pred: Function6[A,B,C,D,E,F,Boolean]) = apply(pred)
}