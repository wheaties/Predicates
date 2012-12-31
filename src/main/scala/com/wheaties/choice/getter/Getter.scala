package com.wheaties.choice.getter

import com.wheaties.choice.iteration.IterationScheme

trait Getter[A]{
  def get(collection: A, scheme: IterationScheme): A
  def partition(collection: A, scheme: IterationScheme): (A,A)
}