import day1.case7._
import day1.case7.CanTrueOps._
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.language.higherKinds

5 ?
// DO NOT WORK ? 5

List("a").?
List().?
List(1).?
List(1, 2, 3).?
Nil ?

?: (1 :: 2 :: Nil) ("true") ("false")
?: (Nil) ("true") ("false")