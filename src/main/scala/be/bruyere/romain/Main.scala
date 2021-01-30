//package be.bruyere.romain
//
//object Main {
//
//  sealed trait Combinator[A] {
//
//    def map[B](f: Combinator[A] => Combinator[B]): Combinator[B] = f(this)
//
//    def next(f: A): Combinator[A]
//
//    def get(): Option[A]
//
//    def reset(): Combinator[A]
//  }
//
//  case class Atom[A] private (value: Option[A], predicate: A => Boolean, output: A => A) extends Combinator[A] {
//    def this(predicate: A => Boolean, output: A => A) = this(None, predicate, output)
//
//    def next(a: A): Combinator[A] = if (predicate(a)) Atom(Some(a), predicate, output) else Atom(None, predicate, output)
//
//    def get(): Option[A] = value.map(x => output(x))
//
//    def reset(): Combinator[A] = Atom(None, predicate, output)
//  }
//
//  case class Apply[A] private (value: Option[A], child: Combinator[A], transform: A => A) extends Combinator[A] {
//    def this(child: Combinator[A], transform: A => A) = this(None, child, transform)
//
//    def next(a: A): Combinator[A] = child
//      .next(a) map (newChild => Apply(newChild.get().map(transform), newChild, transform))
//
//    def get(): Option[A] = value
//
//    def reset(): Combinator[A] = Apply(None, child.reset(), transform)
//  }
//
//  case class Or[A] private (value: Option[A], child1: Combinator[A], child2: Combinator[A]) extends Combinator[A] {
//    def this(child1: Combinator[A], child2: Combinator[A]) = this(None, child1, child2)
//
//    def next(a: A): Combinator[A] = child1
//      .next(a) map (newChild1 => child2
//      .next(a) map (newChild2 =>
//        Or(newChild1.get() orElse newChild2.get(), newChild1, newChild2)
//      ))
//
//    def get(): Option[A] = value
//
//    def reset(): Combinator[A] = Or(None, child1.reset(), child2.reset())
//  }
//
//  case class Iter[A] private (value: Option[A], aggValue: A, init: A, transform: (A, A) => A, output: A => A, child: Combinator[A]) extends Combinator[A] {
//    def this(init: A, transform: (A, A) => A, output: A => A, child: Combinator[A]) = this(None, init, init, transform, output, child)
//
//    def next(a: A): Combinator[A] =
//      child.next(a) map (newChild => newChild.get() map (childValue => transform(aggValue, childValue)) match {
//      case Some(transformedValue) => Iter(Some(transformedValue), transformedValue, init, transform, output, newChild)
//      case None => Iter(None, aggValue, init, transform, output, newChild)
//    })
//
//    def get(): Option[A] = value.map(x => output(x))
//
//    def reset(): Combinator[A] = Iter(None, init, init, transform, output, child.reset())
//  }
//
//  implicit def TupleMapper[T, X, Y, Z, A <: T, B <: X, C <: Y, D <: Z](t: (A, B, C, D)): Object {
//    def map[R](f: (T, X, Y, Z) => R): R
//  } = new {
//    def map[R](f: (T, X, Y, Z) => R): R = (f(t._1, t._2, t._3, t._4))
//  }
//
//  case class Split[A] private (value: Option[A], prevValue: Option[A], childNb: Int, child1: Combinator[A], child2: Combinator[A], transform: (A, A) => A) extends Combinator[A] {
//    def this(child1: Combinator[A], child2: Combinator[A], transform: (A, A) => A)
//    = this(None, None, 1, child1, child2, transform)
//
//    def createSplitWithChilds(c1: Combinator[A], c2: Combinator[A])(value: Option[A], prevValue: Option[A], childNb: Int, transform: (A, A) => A): Split[A]
//    = Split(value, prevValue, childNb, c1, c2, transform)
//
//    def next(a: A): Combinator[A] = {
//      childNb match {
//        case 1 => child1.next(a) map (newChild => nextByChild(child1, newChild, a, createSplitWithChilds(newChild, child2)))
//        case 2 => child2.next(a) map (newChild => nextByChild(child2, newChild, a, createSplitWithChilds(child1, newChild)))
////        case _ => this.reset().next(a)
//      }
//    }
//
//    private def nextByChild(prevChild: Combinator[A], newChild: Combinator[A], a: A, factory: (Option[A], Option[A], Int, (A, A) => A) => Combinator[A]): Combinator[A] = {
//      newChild.get() match {
////        case Some(output) => onMatch(newChild, output) map { (value, aggValue, childNb, child) => factory(value, aggValue, childNb, transform) }
//        case Some(output) => {
//          val (value, aggValue, childNb, child) = onMatch(newChild, output)
//          return factory(value, aggValue, childNb, transform)
//        }
//        case None => newChild match {
//          case Iter(_, _, _, _, _, _) => onIterMiss(prevChild, newChild) map { (value, aggValue, childNb, child) => factory(value, aggValue, childNb, transform).next(a) }
//          case _ => this.reset() // Undefined ?
//        }
//      }
//    }
//
//    private def onIterMiss(prevChild: Combinator[A], newChild: Combinator[A]): (Option[A], Option[A], Int, Combinator[A]) = {
//      newChild match {
//        case Iter(_, _, init, _, _, _) =>
//          prevValue
//            .map(p => prevChild.get()
//              .map(v => (Some(transform(p, v)), Some(v), childNb + 1, newChild))
//              .getOrElse((None, None, childNb + 1, newChild))
//            )
//            .getOrElse(
//              prevChild.get()
//                .map(v => (None, Some(v), childNb + 1, newChild))
//                .getOrElse((None, Some(init), childNb + 1, newChild))
//            )
//        case _ => throw new RuntimeException("Iter error") // impossible
//      }
//    }
//
//    private def onMatch(child: Combinator[A], output: A): (Option[A], Option[A], Int, Combinator[A]) = {
//      child match {
//        case Iter(_, _, _, _, _, _) => prevValue
//          .map(p => (Some(transform(p, output)), prevValue, childNb, child))
//          .getOrElse((None, None, childNb, child))
//        case _ => prevValue
//          .map(p => (Some(transform(p, output)), Some(output), childNb + 1, child))
//          .getOrElse((None, Some(output), childNb + 1, child))
//      }
//    }
//
//    def get(): Option[A] = value
//
//    def reset(): Combinator[A] = Split(None, None, 1, child1.reset(), child2.reset(), transform)
//  }
//
//  case class Undefined[A]() extends Combinator[A] {
//    def next(a: A): Combinator[A] = new Undefined
//
//    def get(): Option[A] = None
//
//    def reset(): Combinator[A] = this
//  }
//
//  def main(args: Array[String]): Unit = {
//
//    /**
//     * Multiplication de la somme des nombres positifs consécutifs
//     * 1 2 3 4 5 -1 4 5 6 -2
//     */
//
//    // ((>0)* (<=0))*
//    val atomP = new Atom[Int](x => x > 0, x => x);
//    val iterP = new Iter[Int](0, (a, b) => a + b, a => a, atomP);
//    val atomN = new Atom[Int](x => x <= 0, x => x);
//    val split = new Split[Int](iterP, atomN, (x, y) => x + y);
//    val iterSplit = new Iter[Int](1, (a, b) => a * b, a => a, split);
//
//    // ((<=0)* (>0))*
//    val atom2N = new Atom[Int](x => x <= 0, x => x);
//    val iter2N = new Iter[Int](0, (a, b) => a + b, a => a, atom2N);
//    val atom2P = new Atom[Int](x => x > 0, x => x);
//    val split2 = new Split[Int](iter2N, atom2P, (x, y) => x + y);
//    val iterSplit2 = new Iter[Int](1, (a, b) => a * b, a => a, split2);
//
//    // ((>0)* (<=0))* || ((<=0)* (>0))*
//    val or = new Or[Int](iterSplit, iterSplit2);
//    val apply = new Apply[Int](or, x => x * 10);
//
//    val a = Apply[Int](Some(10), or, x => x * 1)
//
//
//    // (>0)* (>0) (>0) (<0)
//    val atom31P = new Atom[Int](x => x > 0, x => x);
//    val atom32P = new Atom[Int](x => x > 0, x => x);
//    val atom33P = new Atom[Int](x => x > 0, x => x);
//    val atom34N = new Atom[Int](x => x < 0, x => x);
//    val iter3P = new Iter[Int](0, (a, b) => a + b, a => a, atom31P);
//    val split3c = new Split[Int](atom33P, atom34N, (x, y) => x + y);
//    val split3d = new Split[Int](atom32P, split3c, (x, y) => x + y);
//    val split3a = new Split[Int](iter3P, split3d, (x, y) => x + y);
//
//    println(split3d.next(8).next(2).next(9).get());
//
////    List(8, 9)
////      .map(split3c.next)
////      .map(_.get())
////      .foreach(println)
//  }
//}