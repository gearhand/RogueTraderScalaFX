package com.github.gearhand.roguetraderscalafx

import enumeratum._

class Essential(val category: EssentialCategory
                , hulls  : Set[HullType]
                , power  : Int
                , space  : Int
                , price  : Int
                , traits : String
                , name   : String
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
