package exchange

import scala.annotation.tailrec
import scala.io.Source

case class Order(owner:String, operation:String, itemName:String, price:Int, amount:Int)

object Orders {
  val BuyCode = "b"
  val SellCode = "s"
  val FieldsSep = "\t"

  def loadFrom(filePath:String): Orders = {
    val ordersList = Source.fromFile(filePath).getLines
    .map(_.split(FieldsSep).toList match {
      case List(owner:String, op:String, item:String, price:String, amount:String) =>
        Order(owner, op, item, price.toInt, amount.toInt)
    }).toList
    new Orders(ordersList)
  }
}

/**
  * Created by Idon on 25.07.2017.
  */
class Orders(val list:List[Order]) {

  def findMatches():Iterable[(Order, Order)] = {
    list
      .groupBy(_.itemName)
      .flatMap {
        case (_, l) => l.groupBy(o => (o.price, o.amount))
          .map {
            case (_, orders) => (matchOrders _).tupled(orders.partition(_.operation == Orders.BuyCode))
          }
      }.flatten
  }

  private def matchOrders(buyOrders:List[Order], sellOrders:List[Order]):List[(Order, Order)] = {
    @tailrec
    def matchRecursively(b:List[Order], s:List[Order], shelf:List[Order], pairs:List[(Order, Order)]): List[(Order, Order)] = {
      if (b.isEmpty || s.isEmpty)
        if (b.nonEmpty && shelf.nonEmpty) //no possible sell orders left for first buy order, except self sell orders - drop buy order
          matchRecursively(b.tail, shelf, Nil, pairs)
        else
          pairs
      else if (b.head.owner != s.head.owner)
        matchRecursively(b.tail, shelf ::: s.tail, Nil, (b.head, s.head) :: pairs)
      else
        matchRecursively(b, s.tail, s.head :: shelf, pairs)
    }
    matchRecursively(buyOrders, sellOrders, Nil, Nil).reverse
  }

}
