package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Apply, Eval}

case class ApplyQRE[In, ChildOut, Out] (child: QRE[In, ChildOut], outputF: ChildOut => Out) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Apply[In, ChildOut, Out, Fn](child.create(), this, None)
  }

  def createNewF[Fn](fn: (() => Out) => Fn): (() => ChildOut) => Fn = {
    (x: () => ChildOut) => fn(() => outputF(x()))
  }
}
