//package be.bruyere.romain
//
//
//object Main2 {
//
//  sealed trait Combinator[A] {
//
//    val out: Option[() => A]
//
//    def start(func: A => A): Combinator[A]
//
//    def next(item: A): Combinator[A]
//  }
//
//  case class Split[A] private(left: Combinator[A], right: Combinator[A], transform: (A, A) => A, partialTransform: A => A, value: Option[A]) extends Combinator[A]{
//
//    def this(left: Combinator[A], right: Combinator[A], transform: (A, A) => A) = this(left, right, transform, null, None);
//
//    def start() : Split[A] = {
//      val newLeft = left.start();
//
//      transform.curried
//
//      newLeft.value match {
//        case Some(v) => {
//          val newPartialTransform = transform.curried(v)
//          val newRight = right.start()
//
//          newRight.value match {
//            case Some(rightResult) => Split(newLeft, newRight, transform, newPartialTransform, Some(newPartialTransform.apply(rightResult)))
//            case None => Split(newLeft, newRight, transform, newPartialTransform, None)
//          }
//        }
//        case None => {
//          Split(newLeft, right, transform, null, None)
//        }
//      }
//    }
//
//    def next(item: A): Split[A] = {
//      val newLeft = left.next(item);
//
//      newLeft.value match {
//        case Some(v) => {
//          val newPartialTransform = transform.curried(v)
//          val newRight = right.start()
//
//          newRight.value match {
//            case Some(rightResult) => Split(newLeft, newRight, transform, newPartialTransform, Some(newPartialTransform.apply(rightResult)))
//            case None => Split(newLeft, newRight, transform, newPartialTransform, None)
//          }
//        }
//        case None => {
//          val newRight = right.next(item);
//          newRight.value match {
//            case Some(rightResult) => Split(newLeft, newRight, transform, partialTransform, Some(partialTransform.apply(rightResult)))
//            case None => Split(newLeft, newRight, transform, partialTransform, None)
//          }
//        }
//      }
//    }
//  }
//
//
//  case class Iter[A] private(out: Option[() => A], child: Combinator[A], init: A, transform: (A, A) => A) extends Combinator[A] {
//
//    def this(child: Combinator[A], init: A, transform: (A, A) => A) = this(Some(() => init), child, init, transform);
//
//    def start(): Iter[A] = {
//      Iter(Some(() => init), child.start(transform.curried(init)), init, transform);
//    }
//
//    def next(item: A): Iter[A] = {
//      val newChild = child.next(item)
//
//      newChild.out match {
//        case Some(v) => {
//
//
//
//
//          Iter(newChild, init, transform, transform.curried(v), Some(v))
//        }
//        case None => Iter(newChild, init, transform, partialTransform, None)
//      }
//    }
//  }
//
//  case class Atom[A] private(out: Option[() => A], func: A => A, output: A => A, predicate: A => Boolean) extends Combinator[A] {
//
//    def this(output: A => A, predicate: A => Boolean) = this(null, null, output, predicate);
//
//    def start(func: A => A): Atom[A] = {
//      Atom(None, func, output, predicate);
//    }
//
//    def next(item: A): Atom[A] = {
//      if (predicate(item)) {
//        Atom(Some(() => func(output(item))), null, output, predicate);
//      } else {
//        Atom(None, null, output, predicate);
//      }
//    }
//  }
//
//  def main(args: Array[String]): Unit = {
//
//    /**
//     * Multiplication de la somme des nombres positifs consécutifs
//     * 1 2 3 4 5 -1 4 5 6 -2
//     */
//
//    val splitFn = (x: Int,y: Int) => x + y;
//    val splitFunCurr = splitFn.curried
//
//    val split2Fn = (x:Int, y:Int) => x - y
//    val split2FnCurr = split2Fn.curried
//
//    val iterFn = (x:Int, y:Int) => x * y
//    val iterFnCurr = iterFn.curried
//
//    val atomFn = (x:Int) => x + 1
//
//    val test =  iterFnCurr compose atomFn
//    val test2 = split2FnCurr
//
//    val splitComb = splitFunCurr compose split2FnCurr
//
////    val split2 = ((x:Int, y:Int) => x - y)
//
////    val atomPos = new Atom[Int](x => x * 2, x => x > 0);
////    val iterPos = new Iter[Int](atomPos, 1, (x, y) => x + y);
////
////    val atomNeg = new Atom[Int](x => x * 2, x => x < 0);
////
////    val split = new Split[Int](iterPos, atomNeg, (x,y) => x - y);
////
////    var iterS = new Iter[Int](split, 0, (x, y) => x + y);
//
//    val atomPos = new Atom[Int](x => x, x => x > 0);
//    val atomNeg = new Atom[Int](x => x, x => x < 0);
//    val atomZer = new Atom[Int](x => x, x => x == 0);
//
//    val split = new Split[Int](atomPos, atomNeg, (x,y) => x + y);
//    val splitZer = new Split[Int](split, atomZer, (x,y) => x + 10);
//
//    var iterZer = new Iter[Int](splitZer, 0, (x, y) => x + y);
//
//
//    iterZer = iterZer.start();
//    List(1, -2, 0, 4, -1, 0).foreach(v => {
//      iterZer = iterZer.next(v);
//      println(iterZer.value);
//    })
//
//    // ((>0)* (<=0))*
//    //    val atomP = new Atom[Int](x => x > 0, x => x);
//
//    //    List(8, 9)
//    //      .map(iter.next)
//    //      .map(_.get())
//    //      .foreach(println)
//  }
//}