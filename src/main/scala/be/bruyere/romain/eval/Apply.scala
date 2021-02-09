package be.bruyere.romain.eval

case class Apply[In, ChildOut, Out, Fn] private(child: Eval[In, ChildOut, Fn], outputF: ChildOut => Out, output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {
    val newChild = child.next(item)
    Apply(newChild, outputF, newChild.output)
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    def newFn(x: () => ChildOut) = fn(() => outputF(x()))

    val newChild = child.start(newFn)
    Apply(newChild, outputF, newChild.output)
  }
}
