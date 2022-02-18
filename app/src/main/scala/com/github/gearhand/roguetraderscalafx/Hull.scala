package com.github.gearhand.roguetraderscalafx

import enumeratum._

import scala.collection.mutable

case class Hull(
  hullName: String,
  hullType: HullType,
  speed: Int,
  maneuverability: Int,
  awareness: Int,
  durability: Int,
  armor: Int,
  turretsClass: Int,
  space: Int,
  cost: Int,
  artilleryCells: mutable.IndexedSeq[ArtilleryCell],
  traits: String,
) {
  def addArt(component: Artillery): Either[String, String] = {
    val found = artilleryCells.find(arg => component.validateConstraint(hullType, arg.slot) && arg.cell.isEmpty)
    found match {
      case Some(value) =>
        value.cell = Some(component)
        Right("OK")
      case None => Left("Cannot add")
    }
  }
}

sealed trait HullType extends EnumEntry

object HullType extends Enum[HullType] {
  val values: IndexedSeq[HullType] = findValues

  case object Transport extends HullType
  case object Raider extends HullType
  case object Frigate extends HullType
  case object LightCruiser extends HullType {
    override def entryName: String = "Light Cruiser"
  }
  case object Cruiser extends HullType
  case object GrandCruiser extends HullType // Battle Fleet Koronus hull type
  case object LinearCruiser extends HullType // Battle Fleet Koronus hull type
}

sealed trait ArtillerySlot extends EnumEntry


object ArtillerySlot extends Enum[ArtillerySlot] {
  val values = findValues

  case object LeftBoard extends ArtillerySlot {
    override def entryName: String = "Left Board"
  }
  case object RightBoard extends ArtillerySlot {
    override def entryName: String = "Right Board"
  }
  case object Deck extends ArtillerySlot
  case object Forward extends ArtillerySlot
  case object Kile extends ArtillerySlot
}