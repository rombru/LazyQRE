package be.bruyere.romain.eval

import be.bruyere.romain.qre.Combine3QRE

case class Combine3[In, Out, Child1Out, Child2Out, Child3Out, Fn] private(
                                                                child1: Eval[In, Child1Out, (() => Child2Out, () => Child3Out) => Fn],
                                                                child2: Eval[In, Child2Out, () => Child2Out],
                                                                child3: Eval[In, Child3Out, () => Child3Out],
                                                                qre: Combine3QRE[In, Out, Child1Out, Child2Out, Child3Out],
                                                                output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {
    val newChild1 = child1.next(item)
    val newChild2 = child2.next(item)
    val newChild3 = child3.next(item)
    Combine3(newChild1, newChild2, newChild3, qre, getOutput(newChild1, newChild2, newChild3))
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    val newFn = qre.createNewF(fn)
    val newChild1 = child1.start(newFn)
    val newChild2 = child2.start(identity)
    val newChild3 = child3.start(identity)
    Combine3(newChild1, newChild2, newChild3, qre, getOutput(newChild1, newChild2, newChild3))
  }

  private def getOutput(
                         child1: Eval[In, Child1Out, (() => Child2Out, () => Child3Out) => Fn],
                         child2: Eval[In, Child2Out, () => Child2Out],
                         child3: Eval[In, Child3Out, () => Child3Out],
                       ) = {
    for {out1 <- child1.output; out2 <- child2.output; out3 <- child3.output}
      yield out1.apply(out2, out3)
  }
}
