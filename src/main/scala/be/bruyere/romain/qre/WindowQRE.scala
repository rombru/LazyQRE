package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Eval, Window}
import be.bruyere.romain.struct.AggQueue

case class WindowQRE[In, Out, ChildOut, Agg](child: QRE[In, ChildOut], init: Agg, transformF: (Agg, ChildOut) => Agg, outputF: Agg => Out, winSize: Int) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Window[In, Out, ChildOut, Agg, Fn](child.create(), this, None)
  }

  def createNewF[Fn](out: (() => Agg) => Fn, queue: AggQueue[ChildOut, Agg]): (() => ChildOut) => ((() => Agg) => Fn, AggQueue[ChildOut, Agg]) = {
    (x: () => ChildOut) => (out,queue.enqueue(x))
  }

  def createOutF[Fn](fn: (() => Out) => Fn): (() => Agg) => Fn = {
    (y: () => Agg) => fn(() => outputF(y()))
  }
}
