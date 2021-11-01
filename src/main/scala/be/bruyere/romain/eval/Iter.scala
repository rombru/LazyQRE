package be.bruyere.romain.eval

case class Iter[In, ChildOut, Agg, Out, Fn] private(child: Eval[In, ChildOut, (() => Agg, (() => Agg) => Fn, Long)], init: Agg, transformF: (Agg, ChildOut) => Agg, iterLimit: Long, outputF: Agg => Out, output: Option[Fn]) extends Eval[In, Out, Fn] {
  override def next(item: In): Eval[In, Out, Fn] = {

    val newChild = child.next(item)
    newChild.output match {
      case Some((trans, out, max)) =>
        max match {
          case 0 =>
            val t = trans()
            val newFn = (x: () => ChildOut) => {
              val newTrans = () => transformF.curried(t)(x())
              (newTrans, out, iterLimit)
            }
            Iter(newChild.start(newFn), init, transformF, iterLimit, outputF, Some(out(() => t)))
          case _ =>
            val newFn = (x: () => ChildOut) => {
              val newTrans = () => transformF.curried(trans())(x())
              (newTrans, out, max-1)
            }
            Iter(newChild.start(newFn), init, transformF, iterLimit, outputF, Some(out(trans)))
        }
      case None => Iter(newChild, init, transformF, iterLimit, outputF, None)
    }
  }

  override def start(fn: (() => Out) => Fn): Eval[In, Out, Fn] = {
    val newFn = (x: () => ChildOut) => {
      val trans = () => transformF.curried(init)(x())
      val out = (y: () => Agg) => fn(() => outputF(y()))

      (trans, out, iterLimit)
    }

    Iter(child.start(newFn), init, transformF, iterLimit, outputF, Some(fn(() => outputF(init))))
  }
}
