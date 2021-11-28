package be.bruyere.romain.struct

import scala.collection.immutable.Queue

case class AggQueue[A,B](queue: Queue[() => A] = Queue[() => A]()) {

  def this(item: () => A) {
    this(Queue[() => A](item))
  }

  def enqueue(item: () => A): AggQueue[A,B] = {
    AggQueue(queue.enqueue(item))
  }

  def dequeue(): AggQueue[A,B] = {
    AggQueue(queue.dequeue._2)
  }

  def aggregate(init: B, trans: (B, A) => B): () => B = {
    val fn = (y: A) => trans.curried(init)(y)
    aggregateRec(() => fn, trans, queue)
  }

  def aggregateRec(transCurr: () => A => B, trans: (B, A) => B, queue: Queue[() => A]): () => B = {
    if(queue.size == 1) {
      () => transCurr()(queue.head())
    } else {
      aggregateRec(() => trans.curried(transCurr()(queue.head())), trans, queue.tail)
    }
  }

  def head: () => A = queue.head
  def size: Int = queue.size
}