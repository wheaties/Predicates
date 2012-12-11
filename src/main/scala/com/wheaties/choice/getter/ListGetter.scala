package com.wheaties.choice.getter

import scala.collection.mutable.{MutableList, DoubleLinkedList, LinkedList, ListBuffer}

//TODO: Queues will be handled in a different file.

abstract class ListGetter[A] extends IterableGetter[A,List[A]]{
  def builder = List.newBuilder[A]
}

abstract class MutableListGetter[A] extends IterableGetter[A,MutableList[A]]{
  def builder = MutableList.newBuilder[A]
}

abstract class DoubleLinkedListGetter[A] extends IterableGetter[A,DoubleLinkedList[A]]{
  def builder = DoubleLinkedList.newBuilder[A]
}

abstract class LinkedListGetter[A] extends IterableGetter[A,LinkedList[A]]{
  def builder = LinkedList.newBuilder[A]
}

abstract class ListBufferGetter[A] extends IterableGetter[A,ListBuffer[A]]{
  def builder = ListBuffer.newBuilder[A]
}