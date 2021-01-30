package be.bruyere.romain

object Main3 {

  sealed trait Combinator[A] {
    val maybeOutput: Option[() => A];
    def next(item: A): Combinator[A];
    def start(current: A => A): Combinator[A];
    def start(): Combinator[A];
    def map[B](f: Combinator[A] => Combinator[B]): Combinator[B] = f(this)
  }

  case class Atom[A] private (outputF: A => A, predicate: A => Boolean, current: Option[A => A], maybeOutput: Option[() => A]) extends Combinator[A] {

    def this(outputF: A => A, predicate: A => Boolean) = this(outputF, predicate, None, None);

    def next(item: A): Atom[A] = {
      if(predicate(item) && current.isDefined) {
        Atom(outputF, predicate, None, Some(() => current.get(item)));
      } else {
        Atom(outputF, predicate, None, None);
      }
    }

    def start(current: A => A): Atom[A] = {
      //andThen : x -> current(output(x))
      Atom(outputF, predicate, Some(outputF andThen current), None);
    }

    def start(): Atom[A] = {
      start(identity);
    }
  }

  case class Iter[A] private (child: Combinator[A], init: A, transformF: (A,A) => A, outputF: A => A, maybeOutput: Option[() => A]) extends Combinator[A] {

    def this(child: Combinator[A], init: A, transformF: (A,A) => A, outputF: A => A) = this(child, init, transformF, outputF, None)

    def next(item: A): Iter[A] = {
      val combinator = child.next(item);

      combinator.maybeOutput match {
        case Some(output) => {
          def h(x:A) : A = transformF.curried(output())(x);
          Iter(child.start(h), init, transformF, outputF, combinator.maybeOutput);
        }
        case None => Iter(combinator, init, transformF, outputF, None);
      }
    }

    def start(current: A => A): Iter[A] = {
      def h(x:A) : A = transformF.curried(init)(current(x));
      Iter(child.start(h), init, transformF, outputF, Some(() => init));
    }

    def start(): Iter[A] = {
      start(identity);
    }
  }

  case class Split[A] private (childL: Combinator[A], childR: Combinator[A], transformF: (A,A) => A, maybeOutput: Option[() => A]) extends Combinator[A] {

    def this(childL: Combinator[A], childR: Combinator[A], transformF: (A,A) => A) = this(childL, childR, transformF, None)

    def next(item: A): Split[A] = {
      val combinatorL = childL.next(item);

      combinatorL.maybeOutput match {
        case Some(outputL) => {
          def h(x:A) : A = transformF.curried(outputL())(x);
          val combinatorR = childR.start(h);
          Split(combinatorL, combinatorR, transformF, combinatorR.maybeOutput)
        }
        case None =>  {
          val combinatorR = childR.next(item);
          Split(combinatorL, combinatorR, transformF, combinatorR.maybeOutput)
        }
      }
    }

    def start(current: A => A): Split[A] = {
      val combinatorL = childL.start(current);

      combinatorL.maybeOutput match {
        case Some(outputL) => {
          def h(x:A) : A = transformF.curried(outputL())(x);
          val combinatorR = childR.start(h);

          combinatorR.maybeOutput match {
            case Some(_) => Split(combinatorL, combinatorR, transformF, combinatorR.maybeOutput)
            case None => Split(combinatorL, combinatorR, transformF, None)
          }
        }
        case None => Split(combinatorL, childR, transformF, None)
      }
    }

    def start(): Split[A] = {
      start(identity);
    }
  }

//  def startIter[A](init: A, f: (A,A) => A): A => A = {
//    def h(x:A) : A = f.curried(init)(x);
//    h;
//  }
//
//  def startIter[A](init: A, f: (A,A) => A, g: A => A): A => A = {
//    def h(x:A) : A = f.curried(init)(g(x));
//    h;
//  }
//
//  def nextIter[A](f: (A,A) => A, g: () => A): A => A = {
//    def h(x:A) : A = f.curried(g())(x);
//    h;
//  }
//
//  def nextSplit[A](f: (A,A) => A, g: () => A): A => A = {
//    def h(x:A) : A = f.curried(g())(x);
//    h;
//  }
//
//  def startAtom[A](f: A => A, current: A => A): A => A = {
//    f andThen current // x -> current(f(x))
//  }
//
//  def nextAtom[A](f: A => A, item: A): () => A = {
//    () => f(item);
//  }

  def main(args: Array[String]): Unit = {

    val atomPos = new Atom[Int](x => x, x => x > 0);
    val iterPos = new Iter[Int](atomPos, 0, (x, y) => x + y, x => x);
    val atomNeg = new Atom[Int](x => x, x => x < 0);
    val split1 = new Split[Int](iterPos, atomNeg, (x,y) => x * y);

    val atomPos1 = new Atom[Int](x => x, x => x > 0);
    val atomPos2 = new Atom[Int](x => x, x => x > 0);
    val split2 = new Split[Int](atomPos1, atomPos2, (x,y) => x + y);

    val atomNeg1 = new Atom[Int](x => x, x => x < 0);
    val split3 = new Split[Int](split2, atomNeg1, (x,y) => x - y);

    val split = new Split[Int](split1, split3, (x,y) => x * y);

    var eval = split.start();
    List(1, 1, 3, 4, -1, 10, 11, -3).foreach(item => {
      eval = eval.next(item);
      eval.maybeOutput match {
        case Some(output) => println(output());
        case None => println("Undefined")
      }
    })

    // 0* 1 0 0 2
    // 000 1 00 2
    // f1 : 0 + 0 + 0 -> 3
    // f2 : 1 -> 2
    // f3 : f1 + f2

    //atom
//    val atom = (x: Int) => x;
//
//    //iter
//    val iter = (x: Int, y: Int) => x + 1
//    val init = 0;
//
//    //split
//    val splitMult = (x: Int, y: Int) => x * y
//
//    //split
//    val splitDiv = (x: Int, y: Int) => x / y

    // 1 1 1 0 1 1 0
//    val fn1 = (x: Int) => 0;
//    val fn2 = createIter(1, iter, fn1)
//    val fn3 = createIter(1, iter, fn2)
//    val fn4 = createSplit(splitMult, createAtom(fn3,1), atom)
//    val atom6 = createSplit(splitDiv, createAtom(fn4, 0), atom)

    // O*
    //---------- start
    //iter
//    val iterFn1 = startIter(init, iter)
//    //atom
//    var current = startAtom(iterFn1, atom);
//    var out: () => Int = null;
//
//    //---------- next 1
//    //atom
//    out = nextAtom(current, 1)
//    println(out.apply());
//    //iter
//    val iterFn2 = nextIter(iter, out);
//    current = startAtom(iterFn2, atom);
//
//    //---------- next 1
//    //atom
//    out = nextAtom(current, 1)
//    println(out.apply());
//    //iter
//    val iterFn3 = nextIter(iter, out);
//    current = startAtom(iterFn3, atom);
//
//    //---------- next 1
//    //atom
//    out = nextAtom(current, 1)
//    println(out.apply());
//    //iter
//    val iterFn4 = nextIter(iter, out);
//    current = startAtom(iterFn4, atom);
//    //split
//    var splitFn1 = nextSplit(splitMult, out)
//    current = startAtom(splitFn1, atom);
//
//    //---------- next 0
//    out = nextAtom(current, 0)
//    println(out.apply());
//    //split
//    var splitFn2 = nextSplit(splitDiv, out)
//    current = startAtom(splitFn2, atom);
  }
}