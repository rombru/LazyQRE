package be.bruyere.romain.eval

case class Combine3[In, Child1Out, Child2Out, Child3Out, Out, Fn] private(
                                                                child1: Eval[In, Child1Out, (() => Child2Out, () => Child3Out) => Fn],
                                                                child2: Eval[In, Child2Out, () => Child2Out],
                                                                child3: Eval[In, Child3Out, () => Child3Out],
                                                                transformF: (Child1Out, Child2Out, Child3Out) => Out,
                                                                output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {
    val newChild1 = child1.next(item)
    val newChild2 = child2.next(item)
    val newChild3 = child3.next(item)
    Combine3(newChild1, newChild2, newChild3, transformF, getOutput(newChild1, newChild2, newChild3))
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    def newFn(x: () => Child1Out) = {
      (y: () => Child2Out, z: () => Child3Out) =>   fn(() => transformF.curried(x())(y())(z()));
    }

    val newChild1 = child1.start(newFn)
    val newChild2 = child2.start(identity)
    val newChild3 = child3.start(identity)
    Combine3(newChild1, newChild2, newChild3, transformF, getOutput(newChild1, newChild2, newChild3))
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
