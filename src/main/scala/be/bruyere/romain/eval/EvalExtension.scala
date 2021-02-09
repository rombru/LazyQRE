package be.bruyere.romain.eval

object EvalExtension {
  implicit class StartEval[In, Out, Fn <: () => Out](eval: Eval[In, Out, Fn]) {
    def next(item: In): Eval[In, Out, Fn] = eval.next(item)

    def result(): Option[Out] = eval.output map (o => o())

    def resultFn(): Option[() => Out] = eval.output
  }
}
