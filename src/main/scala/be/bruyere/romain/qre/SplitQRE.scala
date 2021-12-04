package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Eval, Split}

case class SplitQRE[In, Out, ChildLOut, ChildROut, Agg] (childL: QRE[In, ChildLOut], childR: QRE[In, ChildROut], transformF: (ChildLOut, ChildROut) => Agg, outputF: Agg => Out) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Split[In, Out, ChildLOut, ChildROut, Agg, Fn](childL.create(), childR.create(), this, None)
  }

  def createNewF[Fn](fn: (() => Out) => Fn): (() => ChildLOut) => (() => ChildROut) => Fn = {
    (x: () => ChildLOut) => {
      (y: () => ChildROut) => fn(() => outputF(transformF.curried(x())(y())))
    }
  }

  def createOutputLeftF[Fn](outputL: (() => ChildROut) => Fn): (() => ChildROut) => Fn = {
    (x: () => ChildROut) => outputL(x)
  }
}
