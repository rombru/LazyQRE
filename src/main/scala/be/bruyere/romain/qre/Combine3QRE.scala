package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Combine3, Eval}

case class Combine3QRE[In, Child1Out, Child2Out, Child3Out, Out](child1: QRE[In, Child1Out], child2: QRE[In, Child2Out], child3: QRE[In, Child3Out], transformF: (Child1Out,Child2Out,Child3Out) => Out) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Combine3[In, Child1Out, Child2Out, Child3Out, Out, Fn](child1.create(), child2.create(), child3.create(), transformF, None)
  }
}
