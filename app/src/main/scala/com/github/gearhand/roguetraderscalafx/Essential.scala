package com.github.gearhand.roguetraderscalafx

import enumeratum._

case class Essential(category: EssentialCategory)
                ( val hulls  : Set[HullType]
                , val power  : Int
                , val space  : Int
                , val name   : String
                , val price  : Option[Int]
                , val traits : Option[String]
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
