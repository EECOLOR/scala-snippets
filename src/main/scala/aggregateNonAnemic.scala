package org.qirx.snippet.domainpatterns

import java.util.Date
import scalaz._
import Scalaz._
import \/._

object aggregateNonAnemic {
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

  case class LineItem private (
    item: Item,
    quantity: BigDecimal,
    value: Option[BigDecimal] = None,
    discount: Option[BigDecimal] = None) {

    def withValue(value: Item => Option[BigDecimal]) =
      copy(value = value(item).map(_ * quantity))

    def withDiscount(discount: Item => Option[BigDecimal]) =
      copy(discount = discount(item))

    lazy val actualValue = value.map(_ - discount.getOrElse(0))
  }

  case class Customer(custId: String, name: String, category: Int)

  sealed trait OrderStatus
  case object Placed extends OrderStatus
  case object Validated extends OrderStatus

  case class Address(number: String, street: String, city: String, zip: String) {
    def withCity(city: String) = copy(city = city)
  }
  case class ShipTo(name: String, address: Address) {
    def withCity(city: String) = copy(address = address withCity city)
  }

  case class Order(orderNo: String, orderDate: Date, customer: Customer,
    lineItems: Vector[LineItem], shipTo: ShipTo, status: OrderStatus = Placed) {

    def shipToCity(city: String) = copy(shipTo = shipTo withCity city)

    lazy val netValue =
      lineItems.foldLeft(BigDecimal(0).some) { (total, lineItem) =>
        total |+| lineItem.actualValue
      }

    def process(
      values: Item => Option[BigDecimal],
      discounts: (Item, Customer) => Option[BigDecimal]): ProcessingStatus[Order] = {

      this
        .withLineItemValues(values)
        .withLineItemDiscounts(discounts(_: Item, customer))
        .validated
    }

    def validated =
      for {
        _ <- allLineItemsHaveValues
        _ <- allLineItemsHaveDiscounts
      } yield copy(status = Validated)

    val withLineItemValues = updateLineItems(_.withValue)
    val withLineItemDiscounts = updateLineItems(_.withDiscount)

    private def updateLineItems[F](update: LineItem => F => LineItem) =
      (f: F) => copy(lineItems = lineItems.map(update(_) apply f))

    private def allLineItemsHaveValues =
      fieldNotEmptyInLineItems(_.value, "Missing value for items")

    private def allLineItemsHaveDiscounts =
      fieldNotEmptyInLineItems(_.discount, "Missing discount for items")

    private def fieldNotEmptyInLineItems(field: LineItem => Option[_], message: => String): ProcessingStatus[Order] =
      lineItems
        .find(field(_).isEmpty)
        .map(_ => message)
        .toLeft(this)
        .disjunction
  }

  def isReadyForFulfilment(order: Order): ValidationStatus[Boolean] = {
    val isReady =
      for {
        _ <- validate
        _ <- approve
        _ <- checkCustomerStatus(order.customer)
        c <- checkInventory
      } yield c

    isReady(order)
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

  private def unitPrice(item: Item) =
    BigDecimal(12).some

  private def discount(item: Item, customer: Customer) =
    BigDecimal(5).some

  def process(order: Order) =
    order.process(unitPrice, discount)
}
