package be.bruyere.romain.eval

import be.bruyere.romain.qre.TumblingWindowQRE
import be.bruyere.romain.struct.AggQueue

case class TumblingWindow[In, ChildOut, Agg, Out, Fn] private(child: Eval[In, ChildOut, ((() => Agg) => Fn, AggQueue[ChildOut, Agg])], qre: TumblingWindowQRE[In, ChildOut, Agg, Out], output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {

    val newChild = child.next(item)
    newChild.output match {
      case Some((out,queue)) =>
        if(queue.size == qre.winSize) {
          val result = queue.aggregate(qre.init, qre.transformF)
          val newFn = qre.createNewF(out, new AggQueue[ChildOut, Agg]())
          TumblingWindow(newChild.start(newFn), qre, Some(out(() => result())))
        } else {
          val newFn = qre.createNewF(out, queue)
          TumblingWindow(newChild.start(newFn), qre, None)
        }
      case None => TumblingWindow(newChild, qre, None)
    }
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    val out = qre.createOutF(fn)
    val newFn = qre.createNewF(out, new AggQueue[ChildOut, Agg]())
    TumblingWindow(child.start(newFn), qre, None)
  }
}
