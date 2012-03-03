package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint2[A,B]extends Predicate1[Function2[A,B,Boolean]]{
	def constrain[C](that: Constraint1[C]) = new Constraint3[A,B,C]{
		def apply(pred: Function3[A,B,C,Boolean]) = Constraint2.this((x0,x1) => that(pred(x0,x1,_)))
	}

	def suchThat(pred: Function2[A,B,Boolean]) = apply(pred)
}