package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Eval, Iter}

case class IterQRE[In, ChildOut, Agg, Out] (child: QRE[In, ChildOut], init: Agg, transformF: (Agg, ChildOut) => Agg, iterLimit: Long, outputF: Agg => Out) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Iter[In, ChildOut, Agg, Out, Fn](child.create(), () => init, () => transformF, iterLimit, () => outputF, None)
  }
}
