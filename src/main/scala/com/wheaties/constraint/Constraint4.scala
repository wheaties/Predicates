package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint4[A,B,C,D]extends Predicate1[Function4[A,B,C,D,Boolean]]{
	def constrain[E](that: Constraint1[E]) = new Constraint5[A,B,C,D,E]{
		def apply(pred: Function5[A,B,C,D,E,Boolean]) = Constraint4.this((x0,x1,x2,x3) => that(pred(x0,x1,x2,x3,_)))
	}

	def suchThat(pred: Function4[A,B,C,D,Boolean]) = apply(pred)
}