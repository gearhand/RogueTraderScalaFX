package com.github.gearhand.roguetraderscalafx

import enumeratum._

case class Essential(category: EssentialCategory
                , hulls  : Set[HullType]
                , power  : Int
                , space  : Int
                , name   : String
                , price  : Option[Int]
                , traits : Option[String]
)

sealed trait EssentialCategory extends EnumEntry
object EssentialCategory extends Enum[EssentialCategory] {
  val values = findValues
  case object Drive extends EssentialCategory
  case object WarpDrive extends EssentialCategory
  case object GellarField extends EssentialCategory
  case object VoidShield extends EssentialCategory
  case object Bridge extends EssentialCategory
  case object LifeSupport extends EssentialCategory
  case object Quarters extends EssentialCategory
  case object Sensors extends EssentialCategory
}
// deriving (Enum, Bounded, Eq, Ord)
