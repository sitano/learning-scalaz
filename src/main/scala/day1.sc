import scalaz._
import scalaz.Scalaz._

// Equal
1 === 2
1 === 1
1.some === 1.some
1.some === 2.some
1.some =/= 2.some
// 1 =/= "w" error
// 1 assert_=== 2

// Order
1 > 2.0
1 gt 2
1 ?|? 2 === scalaz.Ordering.LT
1.0 max 2.0
// Show
3.show
// Read

// Enum
'z' to 'a' // num range inclusive
'a' to 'z' // num range inclusive
1 to 2 // range inclusive
2 |-> 5 // list
3 |=> 5 // stream
// Bound
// implicitly[scalaz.Enum[Char]].max
// implicitly[scalaz.Enum[Char]].min
// implicitly[Enum[Double]].max

case class TrafficLight(name: String)
val red = TrafficLight("red")
val yellow = TrafficLight("yellow")
val green = TrafficLight("green")
implicit val TrafficLightEqual: Equal[TrafficLight] = Equal.equal(_ == _)
red === TrafficLight("red")