package be.bruyere.romain

object Main8 {

  def main1(args: Array[String]): Unit = {

    // split multiplication
    def splitMultTransform = (x: Int, y: Int) => x * y
    val currentSplitMult = (x: Int) => splitMultTransform(x, _)

    // atom
    def atom1output = (x: Int) => x + 1
    val currentAtom1 = (x: Int) => currentSplitMult(atom1output(x))
    val resultAtom1 = () =>  currentAtom1(5)

    // split sub
    def splitSubTransform = (x: Int, y: Int) => x - y
    def currentSplitSub = (x: Int, y: Int) => resultAtom1()(x - y)

    // iter addition
    val i = 3
    def iterTransform = (x: Int, y: Int) => x + y

    val currentIter1 = (x: Int) => {
      currentSplitSub(iterTransform(3, x), _)
    }
    val resultIter1 = (x: Int) => currentSplitSub(3, x)

    //split division
    def splitDivide = (x: Int, y: Int) => {
      println("exec splitDivide")
      currentIter1((x / y))
    }

    //atom Y
    def atom2output = (x: Int) => x + 1
    def atom2 = (x: Int) => splitDivide(atom2output(x), _)
    val resultAtom2 = () => atom2(5)
    println("Result 2")

    //atom Z
    def atom3output = (x: Int) => x + 1
    def atom3 = (x: Int) => {
      val test = resultAtom2()
      test(atom3output(x))
    }
    var resultAtom3 = (x: Int) => {
      val res = atom3(5)
      res(x)
    }
    println("Result 3")

    // iter 2 eme fois
    val currentIter2 = (x: Int) => {
      currentSplitSub(iterTransform(3, x), _)
    }


    def atom4output = (x: Int) => x + 1
    def atom4 = (x: Int) => {
      resultAtom3(atom4output(x))
    }

    var resultAtom4 = () => atom4(6)
    println("Result 4")

    println(resultAtom4)
    println(resultAtom4())
  }


  def main(args: Array[String]): Unit = {
    // split multiplication
    //    def splitMultTransform = (x: Int, y: Int) => {
    //      println(this.toString)
    //      x * y
    //    }
    //    val currentSplitMult = splitMultTransform.curried
    //
    //    // atom
    //    def atom1output = (x: Int) => {
    //      println(this.toString)
    //      x + 1
    //    }
    //    val currentAtom1 = (x: Int) => currentSplitMult(atom1output(x))
    //    def resultAtom1 = (x: Int) => currentAtom1(5)(x)
    //
    //    def atom2output = (x: Int) => {
    //      println(this.toString)
    //      x + 2
    //    }
    //    val currentAtom2 = (x: Int) => resultAtom1(atom2output(x))
    //    def resultAtom2 = () => currentAtom2(5)
    //
    //    println(resultAtom2())
    //
//        val atom1 = AtomQRE[Int,Int](x => x + 1, x => x > 0)
//        val atom2 = AtomQRE[Int,Int](x => x + 1, x => x < 0)
//        val iter = IterQRE[Int,Int,Int](atom1, 5, (x, y) => x + y, x => x)
//        val split = SplitQRE[Int,Int,Int](iter, atom2, (x, y) => x + y)
    //
    //    split.accept(LazyQRECreator())

    // (2 + ((20 / 2) / 5)) * 10 = (2 + 2) * 10 = 40
    // -> 2, 2, 5, 10

    val atom1 = Atom[String, Int, (() => Int) => (() => Int) => () => String](x => {
      println("atom1"); x.length
    }, x => x.nonEmpty, None, None)
    val atom2 = Atom[String, Int, (() => Int, (() => Int) => (() => Int) => () => String)](x => {
      println("atom2"); x.length
    }, x => x.nonEmpty, None, None)
    val iter = Iter[String, Int, Int, Int, (() => Int) => () => String](atom2, 20, (x, y) => {
      println("iter"); x / y
    }, x => x, None)


    val split = Split[String, Int, Int, Int, String, (() => Int) => () => String](atom1, iter, (x, y) => {
      println("split"); x + y
    }, x => x.toString, None)

    val atom3 = Atom[String, Int, () => String](x => {
      println("atom3"); x.length
    }, x => x.nonEmpty, None, None)
    val split2 = Split[String, String, Int, String, String, () => String](split, atom3, (x, y) => {
      println("split2"); x * y
    }, x => x, None)

    split2.create()

    var eval = split2.start()
    eval = eval.next("aa")
    eval = eval.next("aa")
    eval = eval.next("aaaaa")
    eval = eval.next("aaaaaaaaaa")
    eval = eval.next("aa")
    println(eval.output.get())
  }

  trait QRE {
    def accept(e: QRECreator)
  }

  case class AtomQRE[D, C] private(outputF: D => C, predicate: D => Boolean) extends QRE {
    def accept(e: QRECreator): Unit = {
      e.visitAtom(this)
    }
  }

  case class IterQRE[A, B, C] private(child: QRE, init: B, transformF: (B, A) => B, outputF: B => C) extends QRE {
    def accept(e: QRECreator): Unit = {
      e.visitIter(this)
    }
  }

  case class SplitQRE[A, B, C] private(childL: QRE, childR: QRE, transformF: (A, B) => C) extends QRE {
    def accept(e: QRECreator): Unit = {
      e.visitSplit(this)
    }
  }

  trait QRECreator {
    def visitAtom[D, C](atom: AtomQRE[D, C])

    def visitIter[A, B, C](iter: IterQRE[A, B, C])

    def visitSplit[A, B, C](split: SplitQRE[A, B, C])
  }

  case class LazyQRECreator() extends QRECreator {
    override def visitAtom[D, C](atom: AtomQRE[D, C]): Unit = {
      println("atom")
    }

    override def visitIter[A, B, C](iter: IterQRE[A, B, C]): Unit = {
      println("iter")
    }

    override def visitSplit[A, B, C](split: SplitQRE[A, B, C]): Unit = {

    }
  }

  sealed trait Combinator[Domain, FnDomain, Output] {
    val output: Option[Output]

    def next(item: Domain): Combinator[Domain, FnDomain, Output]

    def start(current: (() => FnDomain) => Output): Combinator[Domain, FnDomain, Output]

    def start(): Combinator[Domain, FnDomain, () => FnDomain]

    def create[NewOutputType](): Combinator[Domain, FnDomain, NewOutputType]
  }

  case class Split[Domain, TransOp1, TransOp2, OutF, Result, Output] private
  (childL: Combinator[Domain, TransOp1, (() => TransOp2) => Output], childR: Combinator[Domain, TransOp2, Output], transformF: (TransOp1, TransOp2) => OutF, outputF: OutF => Result, output: Option[Output])
    extends Combinator[Domain, Result, Output] {

    def start(): Split[Domain, TransOp1, TransOp2, OutF, Result, () => Result] = {
      val newChildL = childL.create[(() => TransOp2) => () => Result]()
      val newChildR = childR.create[() => Result]()
      Split[Domain, TransOp1, TransOp2, OutF, Result, () => Result](newChildL, newChildR, transformF, outputF, None).start(identity)
    }

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

    def create[NewOutputType](): Split[Domain, TransOp1, TransOp2, OutF, Result, NewOutputType] = {
      Split[Domain, TransOp1, TransOp2, OutF, Result, NewOutputType](childL.create[(() => TransOp2) => NewOutputType](), childR.create[NewOutputType](), transformF, outputF, None)
    }
  }

  case class Atom[Domain, OutFDomain, Output] private(outputF: Domain => OutFDomain, predicate: Domain => Boolean, current: Option[Domain => Output], output: Option[Output])
    extends Combinator[Domain, OutFDomain, Output] {

    def start(): Atom[Domain, OutFDomain, () => OutFDomain] = {
      Atom(outputF, predicate, Some((x: Domain) => () => outputF(x)), None)
    }

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

    def create[NewOutputType](): Atom[Domain, OutFDomain, NewOutputType] = {
      Atom[Domain, OutFDomain, NewOutputType](outputF, predicate, None, None)
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

          Iter(child.start(newFn), init, transformF, outputF, Some(out(trans)))
        case None => Iter(child, init, transformF, outputF, None)
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

    override def start(): Combinator[Domain, OutF, () => OutF] = {
      val newChild = child.create[(() => AggOp, (() => AggOp) => () => OutF)]()
      Iter[Domain,TransOp,AggOp,OutF,()=>OutF](newChild, init, transformF, outputF, None).start(identity)
    }

    override def create[NewOutputType](): Combinator[Domain, OutF, NewOutputType] = {
      Iter[Domain,TransOp,AggOp,OutF,NewOutputType](child.create(), init, transformF, outputF, None)
    }
  }

  def iteration1(args: Array[String]): Unit = {
    //--------------iter
    val init = 20
    val trans = (x: Int, y: Int) => x / y
    val output = (x: Int) => x

    //---------------atom
    val atomOutput = (x: Int) => x + 1


    //---------------EXEC
    // 20 + (5 + 1) + (6 + 1) = 33
    def a = (x: Int) => x
    //start
    def b = () => a(output(init))

    def c = (x: Int) => {
      def h = () => trans(init, x)
      def i = (y: Int) => a(output(y))
      (h, i)
    }

    def d = (x: Int) => c(atomOutput(x))

    //next 5
    val (pTransf, out) = d(3)

    //iter
    def e = () => out(pTransf())
    def f = (x: Int) => {
      def g = () => trans(pTransf(), x)
      def h = out

      (g, h)
    }

    def g = (x: Int) => f(atomOutput(x))
    val (pTransf2, out2) = g(1)

    def h = () => out2(pTransf2())

    println(h())
  }
}