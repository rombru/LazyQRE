//class Iter[A] {
//
//  def start(value: A): A = {
//    value;
//  }
//
//  def next(value: A): Unit = {
//
//  }
//}
//
//type PFRule = PartialFunction[Int, String]
//
//def numberRule(f: Int => Boolean, result: String): PFRule = {
//  case n: Int if f(n) => result
//}
//
//val GreaterThanFiveRule: PFRule   = numberRule(_ > 5, "Greater than five")
//val LessThanFiveRule: PFRule      = numberRule(_ < 5, "Less than five")
//def defaultValue(n: Int): String = "This must be magic"
//
//val NumberRulesFn: PFRule =
//  GreaterThanFiveRule orElse LessThanFiveRule
//
//def NumberRules(n: Int): String =  NumberRulesFn.applyOrElse(n, defaultValue)
//
//NumberRules(7)
////res0: String = Greater than five
//NumberRules(1)
////res1: String = Less than five
//NumberRules(5)
////res2: String = This must be magic
