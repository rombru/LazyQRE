package be.bruyere.romain

object Main8 {


  def main(args: Array[String]): Unit = {

    val atom1 = AtomQRE[String, Int](x => x.length, x => x.nonEmpty)
    val atom2 = AtomQRE[String, Int](x => x.length, x => x.nonEmpty)
    val iter = IterQRE[String, Int, Int, Int](atom2, 20, (x, y) => x / y, x => x)
    val split = SplitQRE[String, Int, Int, Int, String](atom1, iter, (x, y) => x + y, x => x.toString)
    val atom3 = AtomQRE[String, Int](x => x.length, x => x.isEmpty)
    val split2 = SplitQRE[String, String, Int, String, String](split, atom3, (x, y) => x * y, x => x)
    val iter2 = IterQRE[String, String, String, String](split2, "10", (x, y) => x.concat(y) , x => x)

    var eval = iter2.start()
    eval = eval.next("aa")
    eval = eval.next("aa")
    eval = eval.next("aaaaa")
    eval = eval.next("aaaaaaaaaa")
    eval = eval.next("")
    println(eval.result())
  }

  trait QRE[Domain, Cost] {
    def start(): StartingCombinator[Domain,Cost,() => Cost] = {
      create[() => Cost]().start(identity)
    };

    protected[Main8] def create[Output](): Combinator[Domain,Cost,Output];
  }

  case class AtomQRE[Domain, OutFDomain] private(outputF: Domain => OutFDomain, predicate: Domain => Boolean) extends QRE[Domain, OutFDomain] {

    override def create[Output](): Combinator[Domain, OutFDomain, Output] = {
      Atom[Domain, OutFDomain, Output](outputF, predicate, None, None)
    }
  }

  case class IterQRE[Domain,TransOp,AggOp,OutF] private(child: QRE[Domain, TransOp], init: AggOp, transformF: (AggOp, TransOp) => AggOp, outputF: AggOp => OutF) extends QRE[Domain, OutF] {

    override def create[Output](): Combinator[Domain, OutF, Output] = {
      Iter[Domain,TransOp,AggOp,OutF,Output](child.create(), init, transformF, outputF, None)
    }
  }

  case class SplitQRE[Domain, TransOp1, TransOp2, OutF, Result] private(childL: QRE[Domain, TransOp1], childR: QRE[Domain, TransOp2], transformF: (TransOp1, TransOp2) => OutF, outputF: OutF => Result) extends QRE[Domain, Result] {

    override def create[Output](): Combinator[Domain, Result, Output] = {
      val left = childL.create[(() => TransOp2) => Output]()
      val right = childR.create[Output]()
      Split(left, right, transformF, outputF, None);
    }
  }

  implicit class StartingCombinator[Domain, FnDomain, Output <: () => FnDomain](combinator: Combinator[Domain, FnDomain, Output]) {
    def next(item: Domain): Combinator[Domain, FnDomain, Output] = combinator.next(item);
    def result(): Any = combinator.output map(o => o())
  }

  sealed trait Combinator[Domain, FnDomain, Output] {
    val output: Option[Output]

    def next(item: Domain): Combinator[Domain, FnDomain, Output]

    def start(current: (() => FnDomain) => Output): Combinator[Domain, FnDomain, Output]
  }

  case class Split[Domain, TransOp1, TransOp2, OutF, Result, Output] private
  (childL: Combinator[Domain, TransOp1, (() => TransOp2) => Output], childR: Combinator[Domain, TransOp2, Output], transformF: (TransOp1, TransOp2) => OutF, outputF: OutF => Result, output: Option[Output])
    extends Combinator[Domain, Result, Output] {

    def start(fn: (() => Result) => Output): Split[Domain, TransOp1, TransOp2, OutF, Result, Output] = {
      def newFn(x: () => TransOp1) = (y: () => TransOp2) => fn(() => outputF(transformF.curried(x())(y())))

      val newChildL = childL.start(newFn)
      newChildL.output match {
        case Some(childOutput) =>
          def fn(x: () => TransOp2) = childOutput(x)

          val newChildR = childR.start(fn)

          newChildR.output match {
            case Some(_) => Split[Domain, TransOp1, TransOp2, OutF, Result, Output](newChildL, newChildR, transformF, outputF, newChildR.output)
            case None => Split(newChildL, newChildR, transformF, outputF, None)
          }
        case None => Split(newChildL, childR, transformF, outputF, None)
      }
    }

    def next(item: Domain): Split[Domain, TransOp1, TransOp2, OutF, Result, Output] = {
      val newChildL = childL.next(item)
      val newChildR = childR.next(item)

      val (newChildR2, output) = newChildL.output match {
        case Some(childOutput) => restartRight(childOutput, newChildR)
        case None => (newChildR, newChildR.output)
      }
      Split(newChildL, newChildR2, transformF, outputF, output)

    }

    private def restartRight(outputL: (() => TransOp2) => Output, newChildR: Combinator[Domain, TransOp2, Output]) = {
      def fn(x: () => TransOp2) = outputL(x)
      val newChildR2 = newChildR.start(fn)

      newChildR2.output match {
        case Some(_) => (newChildR2, newChildR2.output)
        case None => (newChildR2, newChildR.output)
      }
    }
  }

  case class Atom[Domain, OutFDomain, Output] private(outputF: Domain => OutFDomain, predicate: Domain => Boolean, current: Option[Domain => Output], output: Option[Output])
    extends Combinator[Domain, OutFDomain, Output] {

    def start(fn: (() => OutFDomain) => Output): Atom[Domain, OutFDomain, Output] = {
      Atom(outputF, predicate, Some((x: Domain) => fn(() => outputF(x))), None)
    }

    def next(item: Domain): Atom[Domain, OutFDomain, Output] = {
      if (predicate(item) && current.isDefined) {
        Atom(outputF, predicate, None, current map (c => c(item)))
      } else {
        Atom(outputF, predicate, None, None)
      }
    }
  }

  case class Iter[Domain,TransOp,AggOp,OutF,Output] private(child: Combinator[Domain,TransOp,(() => AggOp, (() => AggOp) => Output)], init: AggOp, transformF: (AggOp,TransOp) => AggOp, outputF: AggOp => OutF, output: Option[Output]) extends Combinator[Domain,OutF,Output] {
    override def next(item: Domain): Combinator[Domain, OutF, Output] = {
      val newChild = child.next(item)
      newChild.output match {
        case Some((trans,out)) =>
          def newFn(x: () => TransOp) = {
            def newTrans = () => transformF.curried(trans())(x())
            (newTrans, out)
          }

          Iter(newChild.start(newFn), init, transformF, outputF, Some(out(trans)))
        case None => Iter(newChild, init, transformF, outputF, None)
      }
    }

    override def start(fn: (() => OutF) => Output): Combinator[Domain, OutF, Output] = {
      def newFn(x: () => TransOp) = {
        def trans = () => transformF.curried(init)(x())
        def out = (y: () => AggOp) => fn(() => outputF(y()))
        (trans, out)
      }
      Iter(child.start(newFn), init, transformF, outputF, Some(fn(() => outputF(init))))
    }
  }
}