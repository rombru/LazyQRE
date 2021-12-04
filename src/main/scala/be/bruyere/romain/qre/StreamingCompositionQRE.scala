package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Eval, StreamingCompose}

case class StreamingCompositionQRE[In, Out, ChildOut](child1: QRE[In, ChildOut], child2: QRE[ChildOut, Out]) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    StreamingCompose[In, Out, ChildOut, Fn](child1.create(), child2.create(), None)
  }
}
