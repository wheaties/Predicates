package com.wheaties.constraint

import com.wheaties.predicate.Predicate1

trait Constraint1[A]extends Predicate1[Function1[A,Boolean]]{
	def constrain[B](that: Constraint1[B]) = new Constraint2[A]{
		def apply(pred: Function1[A,Boolean]) = Constraint1.this((x0) => that(pred(x0,_)))
	}

	def suchThat(pred: Function1[A,Boolean]) = apply(pred)
}