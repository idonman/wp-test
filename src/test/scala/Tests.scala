import java.io.File

import exchange._
import exchange.StockItem._
import exchange.Orders._
import org.scalatest.{FunSpec, Matchers}

/**
  * Created by Idon on 25.07.2017.
  */
class Tests extends FunSpec with Matchers {

  describe("Clients") {
    it("should load from file") {
      val clients = Clients.loadFrom(getClass.getResource("clients.txt").getPath)
      clients.getAll should be(List(
        Client("TC1", Map(USD -> 10, A -> 11, B -> 12, C -> 13, D -> 14)),
        Client("TC2", Map(USD -> 20, A -> 21, B -> 22, C -> 23, D -> 24)),
        Client("TC3", Map(USD -> 30, A -> 31, B -> 32, C -> 33, D -> 34))
      ))
    }

    it("should save to file") {
      val clientsToSave = List(
        Client("TC4", Map(USD -> 40, A -> 41, B -> 42, C -> 43, D -> 44)),
        Client("TC5", Map(USD -> 50, A -> 51, B -> 52, C -> 53, D -> 54))
      )
      val clients = new Clients(clientsToSave)
      val testResultPath = new File(getClass.getResource(".").getPath, "test_result.txt").getCanonicalPath
      Clients.save(clients, testResultPath)

      Clients.loadFrom(testResultPath).getAll should be(clientsToSave)
    }

    it("should process buy order, decrease usd balance and increase stock item balance") {
      val client = Client("TC1", Map(USD -> 100, A -> 10, B -> 10, C -> 10, D -> 10))
      val order = Order("TC1", BuyCode, A, 10, 10)
      client.processOrder(order)
      client.balance should be (Map(USD -> 90, A -> 20, B -> 10, C -> 10, D -> 10))
    }

    it("should process sell order, increase usd balance and decrease stock item balance") {
      val client = Client("TC1", Map(USD -> 100, A -> 10, B -> 10, C -> 10, D -> 10))
      val order = Order("TC1", SellCode, A, 10, 10)
      client.processOrder(order)
      client.balance should be (Map(USD -> 110, A -> 0, B -> 10, C -> 10, D -> 10))
    }

    it("should not process other's orders") {
      val client = Client("TC1", Map(USD -> 100, A -> 10, B -> 10, C -> 10, D -> 10))
      val order = Order("TC2", SellCode, A, 10, 10)
      client.processOrder(order)
      client.balance should be (Map(USD -> 100, A -> 10, B -> 10, C -> 10, D -> 10))
    }

    it("should not process orders for unknown operations") {
      val client = Client("TC1", Map(USD -> 100, A -> 10, B -> 10, C -> 10, D -> 10))
      val order = Order("TC1", "someOperation", A, 10, 10)
      client.processOrder(order)
      client.balance should be (Map(USD -> 100, A -> 10, B -> 10, C -> 10, D -> 10))
    }

    it("should not check if items balance is negative") {
      val client = Client("TC1", Map(USD -> 100, A -> 10, B -> 10, C -> 10, D -> 10))
      val order = Order("TC1", SellCode, A, 10, 20)
      client.processOrder(order)
      client.balance should be (Map(USD -> 110, A -> -10, B -> 10, C -> 10, D -> 10))
    }

    it("should not check if usd balance is negative") {
      val client = Client("TC1", Map(USD -> 100, A -> 10, B -> 10, C -> 10, D -> 10))
      val order = Order("TC1", BuyCode, A, 200, 10)
      client.processOrder(order)
      client.balance should be (Map(USD -> -100, A -> 20, B -> 10, C -> 10, D -> 10))
    }

    it("should process multiple matched orders") {
      val clientsList = List(
        Client("TC1", Map(USD -> 10, A -> 10, B -> 10, C -> 10, D -> 10)),
        Client("TC2", Map(USD -> 10, A -> 10, B -> 10, C -> 10, D -> 10))
      )

      val matchedOrders = List(
        (Order("TC1", BuyCode, A, 1, 1), Order("TC2", SellCode, A, 1, 1)),
        (Order("TC2", BuyCode, B, 1, 1), Order("TC1", SellCode, B, 1, 1))
      )

      val clients = new Clients(clientsList)
      clients.processMatchedOrders(matchedOrders)
      clients.getAll should be(List(
        Client("TC1", Map(USD -> 10, A -> 11, B -> 9, C -> 10, D -> 10)),
        Client("TC2", Map(USD -> 10, A -> 9, B -> 11, C -> 10, D -> 10))
      ))
    }
  }

  describe("Orders") {
    it("should load from file") {
      val orders = Orders.loadFrom(getClass.getResource("orders.txt").getPath)
      orders.list should be(List(
        Order("TC1", "b", "A", 1, 2),
        Order("TC2", "s", "B", 3, 4),
        Order("TC3", "s", "C", 5, 6)
      ))
    }

    it("should match buy<->sell orders by item, price and amount") {
      val b1 = Order("TC1", BuyCode, A, 1, 1)
      val s1 = Order("TC2", SellCode, A, 1, 1)
      new Orders(List(b1,s1)).findMatches().toList should be(List((b1,s1)))
    }

    it("should not match orders with same owner") {
      val b1 = Order("TC1", BuyCode, A, 1, 1)
      val s1 = Order("TC1", SellCode, A, 1, 1)
      new Orders(List(b1,s1)).findMatches().toList should be(empty)
    }

    it("should not match orders if price is not equal") {
      val b1 = Order("TC1", BuyCode, A, 1, 1)
      val s1 = Order("TC2", SellCode, A, 2, 1)
      new Orders(List(b1,s1)).findMatches().toList should be(empty)
    }

    it("should not match orders if amount is not equal") {
      val b1 = Order("TC1", BuyCode, A, 1, 1)
      val s1 = Order("TC2", SellCode, A, 1, 2)
      new Orders(List(b1,s1)).findMatches().toList should be(empty)
    }

    it("should not match orders with same operation") {
      var b1 = Order("TC1", BuyCode, A, 1, 1)
      var s1 = Order("TC2", BuyCode, A, 1, 1)
      new Orders(List(b1,s1)).findMatches().toList should be(empty)
      b1 = Order("TC1", SellCode, A, 1, 1)
      s1 = Order("TC2", SellCode, A, 1, 1)
      new Orders(List(b1,s1)).findMatches().toList should be(empty)
    }

    it("should find all matches") {
      val b1 = Order("TC1", BuyCode, A, 1, 1)
      val b2 = Order("TC2", BuyCode, A, 1, 1)
      val b3 = Order("TC3", BuyCode, A, 1, 1)

      val s1 = Order("TC1", SellCode, A, 1, 1)
      val s2 = Order("TC1", SellCode, A, 1, 1)
      val s3 = Order("TC2", SellCode, A, 1, 1)
      val s4 = Order("TC3", SellCode, A, 1, 1)

      new Orders(List(b1,b2,b3,s1,s2,s3,s4)).findMatches().toList should be(List(
        (b1,s3),
        (b2,s1),
        (b3,s2)
      ))
    }

    it("should find all order matches in order of creation") {
      val b1 = Order("TC1", BuyCode, A, 1, 1)
      val b2 = Order("TC2", BuyCode, A, 1, 1)
      val b3 = Order("TC3", BuyCode, A, 1, 1)

      val s1 = Order("TC2", SellCode, A, 1, 1)
      val s2 = Order("TC1", SellCode, A, 1, 1)
      val s3 = Order("TC3", SellCode, A, 1, 1)

      new Orders(List(b1,b2,b3,s1,s2,s3)).findMatches().toList should be(List(
        (b1,s1),
        (b2,s2)
      ))
    }

  }

}
