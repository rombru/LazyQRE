package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Else, Eval}

case class ElseQRE[In, Out] private(child1: QRE[In, Out], child2: QRE[In, Out]) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Else[In, Out, Fn](child1.create(), child2.create(), None)
  }
}
