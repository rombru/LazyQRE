package be.bruyere.romain.qre

import be.bruyere.romain.eval.{Combine3, Eval}

case class Combine3QRE[In, Out, Child1Out, Child2Out, Child3Out](child1: QRE[In, Child1Out], child2: QRE[In, Child2Out], child3: QRE[In, Child3Out], transformF: (Child1Out,Child2Out,Child3Out) => Out) extends QRE[In, Out] {

  protected[qre] override def create[Fn](): Eval[In, Out, Fn] = {
    Combine3[In, Out, Child1Out, Child2Out, Child3Out, Fn](child1.create(), child2.create(), child3.create(), this, None)
  }

  def createNewF[Fn](fn: (() => Out) => Fn): (() => Child1Out) => (() => Child2Out, () => Child3Out) => Fn = {
    (x: () => Child1Out) => {
      (y: () => Child2Out, z: () => Child3Out) =>  fn(() => transformF.curried(x())(y())(z()))
    }
  }
}
