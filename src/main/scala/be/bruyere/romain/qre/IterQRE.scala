package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Eval, Iter}

case class IterQRE[In, ChildOut, Agg, Out] (child: QRE[In, ChildOut], init: Agg, transformF: (Agg, ChildOut) => Agg, iterLimit: Long, outputF: Agg => Out) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Iter[In, ChildOut, Agg, Out, Fn](child.create(), this, None)
  }

  def createNewF[Fn](trans: Agg, out: (() => Agg) => Fn, max: Long): (() => ChildOut) => (() => Agg, (() => Agg) => Fn, Long) = createNewF(() => trans, out, max)

  def createNewF[Fn](trans: () => Agg, out: (() => Agg) => Fn, max: Long): (() => ChildOut) => (() => Agg, (() => Agg) => Fn, Long) = {
    (x: () => ChildOut) => {
      val newTrans = () => transformF.curried(trans())(x())
      (newTrans, out, max)
    }
  }

  def createOutF[Fn](fn: (() => Out) => Fn): (() => Agg) => Fn = {
    (y: () => Agg) => fn(() => outputF(y()))
  }
}
