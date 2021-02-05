package be.bruyere.romain

object Main6 {

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
      val combinatorR = childR.next(item);

      val (newCombinatorR, output) = combinatorL.maybeOutput match {
        case Some(outputL) => restartRight(outputL, combinatorR)
        case None => (combinatorR, combinatorR.maybeOutput)
      }
      Split(combinatorL, newCombinatorR, transformF, output)
    }

    private def restartRight(outputL: () => A, cc: Combinator[A]) = {
      def h(x: A): A = transformF.curried(outputL())(x);
      val combinatorR = childR.start(h);
      combinatorR.maybeOutput match {
        case Some(_) => (combinatorR, combinatorR.maybeOutput)
        case None => (combinatorR, cc.maybeOutput)
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


  def main(args: Array[String]): Unit = {

    val atomPos = new Atom[Int](x => x, x => x != 0);
    val iterPos = new Iter[Int](atomPos, 0, (x, y) => x + y, x => x);
    val atomNeg = new Atom[Int](x => x, x => x < 0);
    val split1 = new Split[Int](iterPos, atomNeg, (x,y) => x - y);

    val atomPos1 = new Atom[Int](x => x, x => x != 0);
    val atomPos2 = new Atom[Int](x => x, x => x != 0);
    val split2 = new Split[Int](atomPos1, atomPos2, (x,y) => x - y);

    val atomNeg1 = new Atom[Int](x => x, x => x < 0);
    val split3 = new Split[Int](split2, atomNeg1, (x,y) => x + y);

    val split = new Split[Int](split1, split3, (x,y) => x * y);

    var eval = split.start();
    List(2, 2, -1, 10, 5, -4).foreach(item => {
      eval = eval.next(item);
      eval.maybeOutput match {
        case Some(output) => println(output());
        case None => println("Undefined")
      }
    })
  }
}