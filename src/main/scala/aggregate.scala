package net.debasishg.snippet.domainpatterns

import java.util.Date
import scalaz._
import Scalaz._
import \/._
import PLens._

object aggregate {
  type ValidationStatus[S] = \/[String, S]
  type ProcessingStatus[S] = \/[String, S]

  type ReaderTStatus[A, S] = ReaderT[ValidationStatus, A, S]

  object ReaderTStatus extends KleisliInstances with KleisliFunctions {
    def apply[A, S](f: A => ValidationStatus[S]): ReaderTStatus[A, S] = kleisli(f)
  }

  sealed trait Item {
    def itemCode: String
  }
  case class ItemA(itemCode: String, desc: Option[String], minPurchaseUnit: Int) extends Item
  case class ItemB(itemCode: String, desc: Option[String], nutritionInfo: String) extends Item

  case class LineItem(item: Item, quantity: BigDecimal, value: Option[BigDecimal] = None, discount: Option[BigDecimal] = None)

  case class Customer(custId: String, name: String, category: Int)

  sealed trait OrderStatus
  case object Placed extends OrderStatus
  case object Validated extends OrderStatus

  case class Address(number: String, street: String, city: String, zip: String)
  case class ShipTo(name: String, address: Address)

  case class Order(orderNo: String, orderDate: Date, customer: Customer,
    lineItems: Vector[LineItem], shipTo: ShipTo, netOrderValue: Option[BigDecimal] = None, status: OrderStatus = Placed)

  /**
   * Specifications
   */
  def isReadyForFulfilment(order: Order) = {
    val s = for {

      _ <- validate
      _ <- approve
      _ <- checkCustomerStatus(order.customer)
      c <- checkInventory

    } yield c
    s(order)
  }

  private def validate = ReaderTStatus[Order, Boolean] { order =>
    if (order.lineItems isEmpty) left(s"Validation failed for order $order") else right(true)
  }

  private def approve = ReaderTStatus[Order, Boolean] { order =>
    println("approved")
    right(true)
  }

  private def checkCustomerStatus(customer: Customer) = ReaderTStatus[Order, Boolean] { order =>
    right(true)
  }

  private def checkInventory = ReaderTStatus[Order, Boolean] { order =>
    println("inventory checked")
    right(true)
  }

  /**
   * lens definitions for update of aggregate root
   */

  val orderShipTo = Lens.lensu[Order, ShipTo](
    (o, sh) => o.copy(shipTo = sh),
    _.shipTo)

  val shipToAddress = Lens.lensu[ShipTo, Address](
    (sh, add) => sh.copy(address = add),
    _.address)

  val addressToCity = Lens.lensu[Address, String](
    (add, c) => add.copy(city = c),
    _.city)

  def orderShipToCity = orderShipTo andThen shipToAddress andThen addressToCity

  def valueOrder = Kleisli[ProcessingStatus, Order, Order] { order =>
    // No advantage of using lens
    val o = order.copy(lineItems = setLineItemValues(order.lineItems))

    // Extracted method
    fieldNotEmptyInLineItems(o, _.value, "Missing value for items")
  }

  private def setLineItemValues(lis: Vector[LineItem]) =
    // Disadvantage of using lens, code was more complicated
    lis.map { li =>
      li.copy(value = unitPrice(li.item).map(_ * li.quantity))
    }

  def applyDiscounts = Kleisli[ProcessingStatus, Order, Order] { order =>
    // No advantage of using lens
    val o = order.copy(
      lineItems = setLineItemDiscounts(order.lineItems, order.customer))

    // Extracted method
    fieldNotEmptyInLineItems(o, _.discount, "Missing discount for items")
  }

  private def setLineItemDiscounts(lis: Vector[LineItem], customer: Customer) =
    // Disadvantage of using lens, code was more complicated
    lis.map { li =>
      li.copy(discount = discount(li.item, customer))
    }

  def checkOut = Kleisli[ProcessingStatus, Order, Order] { order =>

    val netOrderValue = order.lineItems.foldLeft(BigDecimal(0).some) { (total, item) =>
      // Created variable to ease reading
      // Removed overcomplicated use of append in combination with tags
      val actualValue = item.value |+| item.discount.map(_ * -1)
      total |+| actualValue
    }

    // No advantage of using lens
    right(order.copy(netOrderValue = netOrderValue))
  }

  private def unitPrice(item: Item) =
    BigDecimal(12).some

  private def discount(item: Item, customer: Customer) =
    BigDecimal(5).some

  def process(order: Order): ProcessingStatus[Order] = {
    (valueOrder andThen applyDiscounts andThen checkOut) =<<
      // No advantage of using lens
      right(order.copy(status = Validated))
  }

  private def fieldNotEmptyInLineItems(o: Order, field: LineItem => Option[_], message: => String): ProcessingStatus[Order] =
    /*
      // If extracted (using the field function) the original would
      // be like this:

      o.lineItems.map(field(_)).sequenceU match {
        case Some(_) => right(o)
        case _ => left(message)
      }

      I think the code below conveys the intention of the code more clearly:
      "Create a message for the first empty field you find"
     */
    o.lineItems
      .find(field(_).isEmpty)
      .map(_ => message)
      .toLeft(o)
      .disjunction
}
