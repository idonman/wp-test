import exchange.{Clients, Orders}

object Main {
  def main(args: Array[String]): Unit = {

    val clients = Clients.loadFrom(getClass.getResource("clients.txt").getPath)
    val matchedOrders = Orders.loadFrom(getClass.getResource("orders.txt").getPath).findMatches()

    clients.processMatchedOrders(matchedOrders)

    Clients.save(clients, "result.txt")
  }

}




