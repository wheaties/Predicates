package com.wheaties.predicate.defined

class NotDefinedForException(args: Any*) extends Exception{
  override def toString() = "Not Defined For %s" format (args mkString (","))
}
