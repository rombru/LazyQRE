package be.bruyere.romain.eval

import be.bruyere.romain.struct.AggQueue

case class Window[In, ChildOut, Agg, Out, Fn] private(child: Eval[In, ChildOut, ((() => Agg) => Fn, AggQueue[ChildOut, Agg])], init: Agg, transformF: (Agg, ChildOut) => Agg, outputF: Agg => Out, output: Option[Fn], winSize: Int) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {

    val newChild = child.next(item)
    newChild.output match {
      case Some((out,queue)) =>
        if(queue.size == winSize) {
          val result = queue.aggregate(init, transformF)
          val newFn = (x: () => ChildOut) => (out,queue.dequeue().enqueue(x))
          Window(newChild.start(newFn), init, transformF, outputF, Some(out(() => result())), winSize)
        } else {
          val newFn = (x: () => ChildOut) => (out,queue.enqueue(x))
          Window(newChild.start(newFn), init, transformF, outputF, None, winSize)
        }
      case None => Window(newChild, init, transformF, outputF, None, winSize)
    }
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    val newFn = (x: () => ChildOut) => {
      val out = (y: () => Agg) => fn(() => outputF(y()))
      val queue = new AggQueue[ChildOut, Agg](x)
      (out,queue)
    }

    Window(child.start(newFn), init, transformF, outputF, None, winSize)
  }
}
