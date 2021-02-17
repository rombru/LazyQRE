package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Eval, StreamingCompose}

case class StreamingCompositionQRE[In, Child1Out, Out] (child1: QRE[In, Child1Out], child2: QRE[Child1Out, Out]) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    StreamingCompose[In, Child1Out, Out, Fn](child1.create(), child2.create(), None)
  }
}
