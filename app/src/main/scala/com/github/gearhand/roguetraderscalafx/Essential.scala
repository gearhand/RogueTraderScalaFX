package com.github.gearhand.roguetraderscalafx

import enumeratum._

class Essential(category: EssentialCategory
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
  case object Drive
  case object WarpDrive
  case object GellarField
  case object VoidShield
  case object Bridge
  case object LifeSupport
  case object Quarters
  case object Sensors
}
// deriving (Enum, Bounded, Eq, Ord)
