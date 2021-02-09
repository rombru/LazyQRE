package be.bruyere.romain.eval

case class Iter[In, ChildOut, Agg, Out, Fn] private(child: Eval[In, ChildOut, (() => Agg, (() => Agg) => Fn)], init: Agg, transformF: (Agg, ChildOut) => Agg, outputF: Agg => Out, output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {
    val newChild = child.next(item)
    newChild.output match {
      case Some((trans, out)) =>
        def newFn(x: () => ChildOut) = {
          def newTrans = () => transformF.curried(trans())(x())

          (newTrans, out)
        }

        Iter(newChild.start(newFn), init, transformF, outputF, Some(out(trans)))
      case None => Iter(newChild, init, transformF, outputF, None)
    }
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    def newFn(x: () => ChildOut) = {
      def trans = () => transformF.curried(init)(x())

      def out = (y: () => Agg) => fn(() => outputF(y()))

      (trans, out)
    }

    Iter(child.start(newFn), init, transformF, outputF, Some(fn(() => outputF(init))))
  }
}
