package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Combine, Eval}

case class CombineQRE[In, Out, Child1Out, Child2Out] (child1: QRE[In, Child1Out], child2: QRE[In, Child2Out], transformF: (Child1Out,Child2Out) => Out) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Combine[In, Out, Child1Out, Child2Out, Fn](child1.create(), child2.create(), this, None)
  }

  def createNewF[Fn](fn: (() => Out) => Fn): (() => Child1Out) => (() => Child2Out) => Fn = {
    (x: () => Child1Out) => {
      (y: () => Child2Out) => fn(() => transformF.curried(x())(y()))
    }
  }
}
