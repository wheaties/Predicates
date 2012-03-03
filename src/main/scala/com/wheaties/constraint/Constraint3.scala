package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint3[A,B,C]extends Predicate1[Function3[A,B,C,Boolean]]{
	def constrain[D](that: Constraint1[D]) = new Constraint4[A,B,C]{
		def apply(pred: Function3[A,B,C,Boolean]) = Constraint3.this((x0,x1,x2) => that(pred(x0,x1,x2,_)))
	}

	def suchThat(pred: Function3[A,B,C,Boolean]) = apply(pred)
}