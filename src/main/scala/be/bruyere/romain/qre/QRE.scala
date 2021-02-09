package be.bruyere.romain.qre

import be.bruyere.romain.eval.Eval
import be.bruyere.romain.eval.EvalExtension.StartEval

trait QRE[In, Out] {
  def start(): StartEval[In, Out, () => Out] = {
    create[() => Out]().start(identity)
  }

  protected[qre] def create[Fn](): Eval[In, Out, Fn]
}
