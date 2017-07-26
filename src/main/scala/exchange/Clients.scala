package exchange

import java.io.{File, PrintWriter}

import scala.collection.breakOut
import scala.io.Source
import scala.util.Properties

case class Client(name:String, var balance:Map[String, Int]) {
  def processOrder(order:Order): Unit = {
    if (order.owner == name) {
      order match {
        case Order(_, Orders.BuyCode, itemName, price, amount) =>
          changeBalance(StockItem.USD, -price)
          changeBalance(itemName, amount)
        case Order(_, Orders.SellCode, itemName, price, amount) =>
          changeBalance(StockItem.USD, price)
          changeBalance(itemName, -amount)
        case _ =>
      }
    }
  }

  def changeBalance(itemName:String, diff:Int): Unit = {
    balance += itemName -> (balance(itemName) + diff)
  }
}

object Clients {
  import StockItem._
  val SupportedItems = List(USD, A, B, C, D)
  val FieldsSep = "\t"
  val LineSep:String = Properties.lineSeparator

  def loadFrom(filePath:String):Clients = {
    val clientsList = Source.fromFile(filePath).getLines
      .map(rawString => {
        val name::rawBalanceData = rawString.split(FieldsSep).toList
        Client(name, SupportedItems.zip(rawBalanceData.map(_.toInt))(breakOut))
      }).toList
    new Clients(clientsList)
  }

  def save(clients:Clients, resultFileName:String): Unit = {
    val pw = new PrintWriter(new File(resultFileName))
    pw.write(Clients.stringify(clients))
    pw.close()
  }

  def stringify(clients:Clients):String = {
    clients.getAll.map(client => {
      (client.name :: SupportedItems.map(client.balance.getOrElse(_, 0))).mkString(FieldsSep)
    }).mkString(LineSep)
  }
}

/**
  * Created by Idon on 25.07.2017.
  */
class Clients(clients:List[Client]) {
  private val nameToClient:Map[String, Client] = clients.map(c => c.name -> c)(breakOut)

  //NOTE: Prone to errors, but writing error handling is not subject of this test
  def getBy(name:String):Client = nameToClient(name)

  def getAll:List[Client] = clients

  //For same reason as stated above ^^ there are no checks if orders are truly matching
  def processMatchedOrders(matchedOrders:Iterable[(Order, Order)]): Unit = {
    matchedOrders.foreach(ordersPair => {
      val (buyOrder, sellOrder) = ordersPair
      val (buyer, seller) = (getBy(buyOrder.owner), getBy(sellOrder.owner))
      buyer.processOrder(buyOrder)
      seller.processOrder(sellOrder)
    })
  }
}
