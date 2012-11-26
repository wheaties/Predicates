package com.wheaties.choice.getter

import scala.collection.mutable.{MutableList, DoubleLinkedList, LinkedList, ListBuffer}

//TODO: Queues will be handled in a different file.

abstract class ListGetter[A] extends BuildableGetter[A,List]{
  def toIter(collection: List[A]) = collection toIterator
  def builder = List.newBuilder[A]
}

abstract class MutableListGetter[A] extends BuildableGetter[A,MutableList]{
  def toIter(collection: MutableList[A]) = collection toIterator
  def builder = new MutableList[A]
}

abstract class DoubleLinkedListGetter[A] extends BuildableGetter[A,DoubleLinkedList]{
  def toIter(collection: DoubleLinkedList[A]) = collection toIterator
  def builder = DoubleLinkedList.newBuilder[A]
}

abstract class LinkedListGetter[A] extends BuildableGetter[A,LinkedList]{
  def toIter(collection: LinkedList[A]) = collection toIterator
  def builder = LinkedList.newBuilder[A]
}

abstract class ListBufferGetter[A] extends BuildableGetter[A,ListBuffer]{
  def toIter(collection: ListBuffer[A]) = collection toIterator
  def builder = ListBuffer.newBuilder[A]
}