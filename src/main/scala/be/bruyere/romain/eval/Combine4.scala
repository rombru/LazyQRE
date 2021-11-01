package be.bruyere.romain.eval

case class Combine4[In, Child1Out, Child2Out, Child3Out, Child4Out, Out, Fn] private(
                                                                child1: Eval[In, Child1Out, (() => Child2Out, () => Child3Out, () => Child4Out) => Fn],
                                                                child2: Eval[In, Child2Out, () => Child2Out],
                                                                child3: Eval[In, Child3Out, () => Child3Out],
                                                                child4: Eval[In, Child4Out, () => Child4Out],
                                                                transformF: (Child1Out, Child2Out, Child3Out, Child4Out) => Out,
                                                                output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {
    val newChild1 = child1.next(item)
    val newChild2 = child2.next(item)
    val newChild3 = child3.next(item)
    val newChild4 = child4.next(item)
    Combine4(newChild1, newChild2, newChild3, newChild4, transformF, getOutput(newChild1, newChild2, newChild3, newChild4))
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    def newFn(x: () => Child1Out) = {
      (y: () => Child2Out, z: () => Child3Out, w: () => Child4Out) =>   fn(() => transformF.curried(x())(y())(z())(w()));
    }

    val newChild1 = child1.start(newFn)
    val newChild2 = child2.start(identity)
    val newChild3 = child3.start(identity)
    val newChild4 = child4.start(identity)
    Combine4(newChild1, newChild2, newChild3, newChild4, transformF, getOutput(newChild1, newChild2, newChild3, newChild4))
  }

  private def getOutput(
                         child1: Eval[In, Child1Out, (() => Child2Out, () => Child3Out, () => Child4Out) => Fn],
                         child2: Eval[In, Child2Out, () => Child2Out],
                         child3: Eval[In, Child3Out, () => Child3Out],
                         child4: Eval[In, Child4Out, () => Child4Out],
                       ) = {
    for {out1 <- child1.output; out2 <- child2.output; out3 <- child3.output; out4 <- child4.output}
      yield out1.apply(out2, out3, out4)
  }
}
