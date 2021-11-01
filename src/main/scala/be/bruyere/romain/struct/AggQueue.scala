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
    var fn = (y: A) => trans.curried(init)(y)

    return aggregateRec(() => fn, trans, queue)


//    for(i <- 0 until queue.length-1) {
//      val elem = queue(i)
//      fn = (y: A) => {
//        val a = fn(elem())
//        trans.curried(a)(y)
//      }
//    }
//    val a = fn(queue.last())
//    () => fn(queue.last())
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