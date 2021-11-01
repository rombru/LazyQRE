package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Eval, TumblingWindow}

case class TumblingWindowQRE[In, ChildOut, Agg, Out](child: QRE[In, ChildOut], init: Agg, transformF: (Agg, ChildOut) => Agg, outputF: Agg => Out, winSize: Int) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    TumblingWindow[In, ChildOut, Agg, Out, Fn](child.create(), init, transformF, outputF, None, winSize)
  }
}
