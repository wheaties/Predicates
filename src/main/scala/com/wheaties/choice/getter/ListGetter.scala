package com.wheaties.choice.getter

import scala.collection.mutable.{MutableList, DoubleLinkedList, LinkedList, ListBuffer}

//TODO: Queues will be handled in a different file.

abstract class ListGetter[A] extends IterableGetter[A,List]{
  def builder = List.newBuilder[A]
}

abstract class MutableListGetter[A] extends IterableGetter[A,MutableList]{
  def builder = new MutableList[A]
}

abstract class DoubleLinkedListGetter[A] extends IterableGetter[A,DoubleLinkedList]{
  def builder = DoubleLinkedList.newBuilder[A]
}

abstract class LinkedListGetter[A] extends IterableGetter[A,LinkedList]{
  def builder = LinkedList.newBuilder[A]
}

abstract class ListBufferGetter[A] extends IterableGetter[A,ListBuffer]{
  def builder = ListBuffer.newBuilder[A]
}