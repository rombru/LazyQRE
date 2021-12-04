package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Combine4, Eval}

case class Combine4QRE[In, Out, Child1Out, Child2Out, Child3Out, Child4Out](child1: QRE[In, Child1Out], child2: QRE[In, Child2Out], child3: QRE[In, Child3Out], child4: QRE[In, Child4Out], transformF: (Child1Out,Child2Out,Child3Out,Child4Out) => Out) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Combine4[In, Out, Child1Out, Child2Out, Child3Out, Child4Out, Fn](child1.create(), child2.create(), child3.create(), child4.create(), this, None)
  }

  def createNewF[Fn](fn: (() => Out) => Fn): (() => Child1Out) => (() => Child2Out, () => Child3Out, () => Child4Out) => Fn = {
    (x: () => Child1Out) => {
      (y: () => Child2Out, z: () => Child3Out, w: () => Child4Out) =>   fn(() => transformF.curried(x())(y())(z())(w()));
    }
  }
}
