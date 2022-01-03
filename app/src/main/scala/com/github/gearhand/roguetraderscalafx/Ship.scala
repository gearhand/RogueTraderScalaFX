package com.github.gearhand.roguetraderscalafx

import com.github.gearhand.roguetraderscalafx.Artillery.createArtCells
import com.github.gearhand.roguetraderscalafx.ArtillerySlot.{Forward, LeftBoard, RightBoard}
import com.github.gearhand.roguetraderscalafx.HullType.Transport
import com.github.gearhand.roguetraderscalafx.MyYamlProtocol.Catalog
import enumeratum._

class Ship(
  var hull: Hull,
  var essentials: Set[EssentialStats],
  var supplementals: List[Supplemental],
)

sealed trait Race extends EnumEntry
object Race extends Enum[Race] {
  val values: IndexedSeq[Race] = findValues
  case object Imperium extends Race
  case object Orkz extends Race
  case object Aeldar extends Race
} // deriving (Show, Enum, Bounded)


case class Supplemental (hulls: Set[HullType], power: Int, space: Int, cost: Int, name: String, traits: Option[String])

object Examples {
  // "Jerico" pilgrim vessel
  val exampleHull = new Hull("Jerico"
    , Transport
    , 3
    , -10
    , 5
    , 50
    , 12
    , 1
    , 45
    , 20
    , createArtCells(List((Forward, 1), (LeftBoard, 1), (RightBoard, 1)))
    , "Грузовое судно")
  //val foo = Supp
}