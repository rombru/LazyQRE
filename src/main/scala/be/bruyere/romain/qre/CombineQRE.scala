package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Combine, Eval}

case class CombineQRE[In, Child1Out, Child2Out, Out] private(child1: QRE[In, Child1Out], child2: QRE[In, Child2Out], transformF: (Child1Out,Child2Out) => Out) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Combine[In, Child1Out, Child2Out, Out, Fn](child1.create(), child2.create(), transformF, None)
  }
}
