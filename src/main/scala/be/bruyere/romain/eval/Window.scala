package be.bruyere.romain.eval

import be.bruyere.romain.qre.WindowQRE
import be.bruyere.romain.struct.AggQueue

case class Window[In, ChildOut, Agg, Out, Fn] private(child: Eval[In, ChildOut, ((() => Agg) => Fn, AggQueue[ChildOut, Agg])], qre: WindowQRE[In, ChildOut, Agg, Out], output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {

    val newChild = child.next(item)
    newChild.output match {
      case Some((out,queue)) =>
        if(queue.size == qre.winSize) {
          val result = queue.aggregate(qre.init, qre.transformF)
          val newFn = qre.createNewF(out, queue.dequeue())
          Window(newChild.start(newFn), qre, Some(out(() => result())))
        } else {
          val newFn = qre.createNewF(out, queue)
          Window(newChild.start(newFn), qre, None)
        }
      case None => Window(newChild, qre, None)
    }
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    val out = qre.createOutF(fn)
    val newFn = qre.createNewF(out, new AggQueue[ChildOut, Agg]())
    Window(child.start(newFn), qre, None)
  }
}
