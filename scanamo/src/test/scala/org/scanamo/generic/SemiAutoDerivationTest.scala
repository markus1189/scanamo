package org.scanamo.generic

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scanamo.{ DynamoArray, DynamoFormat, DynamoObject, DynamoValue }

class SemiAutoDerivationTest extends AnyFunSuite with Matchers {
  ignore("Derivation should fail if no derived format or automatic derivation") {
    """write(Person("Alice", 65))""" shouldNot compile
  }

  ignore("Derivation should succeed if derived format in scope") {
    """
      |import org.scanamo._
      |import org.scanamo.generic.semiauto._
      |implicit val formatLocationInfo: DynamoFormat[LocationInfo] = deriveDynamoFormat[LocationInfo]
      |implicit val formatUser: DynamoFormat[User] = deriveDynamoFormat[User]
      |
      |write(User(Some(1), true, "Bob", "Geldorf", "pink", "1234", None, Some(LocationInfo(Some("UK"), None, None, None))))
      |""".stripMargin should compile
  }

  test("Derivation should correctly encode Option with Map inside") {
    // this test fails, because it incorrectly uses the derived ADT DynamoFormat for Option
    import org.scanamo.generic.auto._

    case class OptionWrapper(theOption: Option[InnerWrapper])
    case class InnerWrapper(innerMap: Map[String, String])

    val value = OptionWrapper(Some(InnerWrapper(Map())))

    val result = DynamoFormat[OptionWrapper].write(value)

    result should ===(
      DynamoValue.fromDynamoObject(
        DynamoObject(
          "theOption" -> DynamoValue.fromDynamoObject(
            DynamoObject("innerMap" -> DynamoValue.fromDynamoObject(DynamoObject.empty))
          )
        )
      )
    )
  }

  test("Derivation should correctly encode Option with Map inside using semiauto derivation") {
    // this test WORKS, it's using the semiauto derivation and picks the correct instance for Option
    import org.scanamo.generic.semiauto._

    case class OptionWrapper(theOption: Option[InnerWrapper])
    object OptionWrapper {
      implicit val optionWrapperFormat: DynamoFormat[OptionWrapper] = deriveDynamoFormat[OptionWrapper]
    }

    case class InnerWrapper(innerMap: Map[String, String])
    object InnerWrapper {
      implicit val innerWrapperFormat: DynamoFormat[InnerWrapper] = deriveDynamoFormat[InnerWrapper]
    }

    val value = OptionWrapper(Some(InnerWrapper(Map())))

    val result = DynamoFormat[OptionWrapper].write(value)

    result should ===(
      DynamoValue.fromDynamoObject(
        DynamoObject(
          "theOption" -> DynamoValue.fromDynamoObject(
            DynamoObject("innerMap" -> DynamoValue.fromDynamoObject(DynamoObject.empty))
          )
        )
      )
    )
  }

  test("Derivation should correctly encode Option with List inside") {
    // this test WORKS and is somehow picking up the correct DynamoFormat for Option, the inner type was changed from Map to List
    import org.scanamo.generic.auto._

    case class OptionWrapper(theOption: Option[InnerWrapper])
    case class InnerWrapper(innerList: List[String])

    val value = OptionWrapper(Some(InnerWrapper(List())))

    val result = DynamoFormat[OptionWrapper].write(value)

    result should ===(
      DynamoValue.fromDynamoObject(
        DynamoObject(
          "theOption" -> DynamoValue.fromDynamoObject(
            DynamoObject("innerList" -> DynamoValue.fromDynamoArray(DynamoArray(List())))
          )
        )
      )
    )
  }

  def write[T](t: T)(implicit f: DynamoFormat[T]) = f.write(t)
}

case class Person(name: String, age: Int)

trait UserShape {
  val id: Option[Long]
  val isActiveUser: Boolean
  val firstName: String
  val lastName: String
  val userSlug: String
  val hashedPassword: String
  val phone: Option[String]
  val locationInfo: Option[LocationInfo]
}

case class User(
  override val id: Option[Long],
  override val isActiveUser: Boolean,
  override val firstName: String,
  override val lastName: String,
  override val userSlug: String,
  override val hashedPassword: String,
  override val phone: Option[String],
  override val locationInfo: Option[LocationInfo]
) extends UserShape

case class LocationInfo(nation: Option[String],
                        provState: Option[String],
                        postalCode: Option[String],
                        preferredLocale: Option[String])
