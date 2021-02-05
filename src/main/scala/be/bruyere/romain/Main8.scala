package be.bruyere.romain

object Main8 {

  def main1(args: Array[String]): Unit = {

    // split multiplication
    def splitMultTransform = (x: Int, y: Int) => x * y
    val currentSplitMult = (x: Int) => splitMultTransform(x, _);

    // atom
    def atom1output = (x: Int) => x + 1;
    val currentAtom1 = (x: Int) => currentSplitMult(atom1output(x));
    val resultAtom1 = () =>  currentAtom1(5);

    // split sub
    def splitSubTransform = (x: Int, y: Int) => x - y
    def currentSplitSub = (x: Int, y: Int) => resultAtom1()(x - y);

    // iter addition
    val i = 3;
    def iterTransform = (x: Int, y: Int) => x + y
    val currentIter1 = (x: Int) => {
      currentSplitSub(iterTransform(3, x), _)
    }
    val resultIter1 = (x: Int) => currentSplitSub(3,x);

    //split division
    def splitDivide = (x: Int, y: Int) => {
      println("exec splitDivide");
      currentIter1((x / y))
    };

    //atom Y
    def atom2output = (x: Int) => x + 1;
    def atom2 = (x: Int) => splitDivide(atom2output(x),_);
    val resultAtom2 = () => atom2(5);
    println("Result 2")

    //atom Z
    def atom3output = (x: Int) => x + 1;
    def atom3 = (x: Int) => {
      val test = resultAtom2();
      test(atom3output(x))
    };
    var resultAtom3 = (x : Int) => {
      val res = atom3(5);
      res(x)
    }
    println("Result 3")

    // iter 2 eme fois
    val currentIter2 = (x: Int) => {
      currentSplitSub(iterTransform(3, x), _)
    }


    def atom4output = (x: Int) => x + 1;
    def atom4 = (x: Int) => {
      resultAtom3(atom4output(x))
    };

    var resultAtom4 = () => atom4(6);
    println("Result 4")

    println(resultAtom4)
    println(resultAtom4())
  }

  trait QRE {
    def accept(e:QRECreator);
  }
  case class AtomQRE[D,C] private(outputF: D => C, predicate: D => Boolean) extends QRE {
    def accept(e:QRECreator): Unit = {
      e.visitAtom(this);
    }
  };

  case class IterQRE[A,B,C] private(child: QRE, init: B, transformF: (B,A) => B, outputF: B => C) extends QRE {
    def accept(e:QRECreator): Unit = {
      child.accept(e);
      e.visitIter(this);
    }
  };

  case class SplitQRE[A,B,C] private(childL: QRE, childR: QRE, transformF: (A,B) => C) extends QRE {
    def accept(e:QRECreator): Unit = {
      childL.accept(e);
      childR.accept(e);
      e.visitSplit(this);
    }
  }

  trait QRECreator {
    def visitAtom[D,C](atom: AtomQRE[D,C]);
    def visitIter[A,B,C](iter: IterQRE[A,B,C]);
    def visitSplit[A,B,C](split: SplitQRE[A,B,C]);
  }
  case class LazyQRECreator() extends QRECreator {
    override def visitAtom[D,C](atom: AtomQRE[D,C]): Unit = {
      println("atom")
    }

    override def visitIter[A,B,C](iter: IterQRE[A,B,C]): Unit = {
      println("iter")
    }

    override def visitSplit[A,B,C](split: SplitQRE[A,B,C]): Unit = {
      println("split")
    }
  }

  def main3(args: Array[String]): Unit = {
    val atom1 = AtomQRE[Int,Int](x => x + 1, x => x > 0);
    val atom2 = AtomQRE[Int,Int](x => x + 1, x => x > 0);
    val iter = IterQRE[Int,Int,Int](atom1, 5, (x, y) => x + y, x => x);
    val split = SplitQRE[Int,Int,Int](iter, atom2, (x, y) => x + y);

    atom1.accept(LazyQRECreator())
  }

  sealed trait Combinator[D,C,T] {
    val maybeOutput: Option[() => T];
    def next(item: D): Combinator[D,C,T];
    def start(current: C => T): Combinator[D,C,T];
    def start(): Combinator[D,C,C];
  }

  case class Atom[D,C,T] private(outputF: D => C, predicate: D => Boolean, current: Option[C => T], maybeOutput: Option[() => T]) extends Combinator[D,C,T] {

    def this(outputF: D => C, predicate: D => Boolean) = this(outputF, predicate, None, None);

    def next(item: D): Atom[D,C,T] = {
      if(predicate(item) && current.isDefined) {
        Atom(outputF, predicate, None, Some(() => current.get(outputF(item))));
      } else {
        Atom(outputF, predicate, None, None);
      }
    }

    def start(current: C => T): Atom[D,C,T] = {
      Atom[D,C,T](outputF, predicate, Some(current), None);
    }

    def start(): Atom[D,C,C] = {
      Atom[D,C,C](outputF,predicate,Some(identity),None)
    }
  }


  def main(args: Array[String]): Unit = {
    //--------------iter
    val init = 20;
    val trans = (x: Int, y: Int) => x + y;
    val output = (x: Int) => x;

    //---------------atom
    val atomOutput = (x: Int) => x + 1;


    //---------------EXEC
    // 20 + (5 + 1) + (6 + 1) = 33
    def a = (x: Int) => x;
    //start
    def b = () => a(output(init))
    def c = (x: Int) => {
      def h = () => trans(init, x)
      def i = (y: Int) => a(output(y));
      (h,i)
    }

    def d = (x: Int) => c(atomOutput(x));

    //next 5
    val (pTransf, out) = d(5);

    //iter
    def e = () => out(pTransf());
    def f = (x: Int) => {
      def g = () => trans(pTransf(), x);
      def h = out
      (g,h)
    }

    def g = (x: Int) => f(atomOutput(x));
    val (pTransf2, out2) = g(6);

    def h = () => out2(pTransf2());

    println(h());
  }

}