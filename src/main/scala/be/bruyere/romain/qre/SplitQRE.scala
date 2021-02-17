package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Eval, Split}

case class SplitQRE[In, ChildLOut, ChildROut, Agg, Out] (childL: QRE[In, ChildLOut], childR: QRE[In, ChildROut], transformF: (ChildLOut, ChildROut) => Agg, outputF: Agg => Out) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Split[In, ChildLOut, ChildROut, Agg, Out, Fn](childL.create(), childR.create(), () => transformF, () => outputF, None)
  }
}
