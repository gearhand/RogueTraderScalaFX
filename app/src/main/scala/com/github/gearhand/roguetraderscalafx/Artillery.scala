package com.github.gearhand.roguetraderscalafx

import com.github.gearhand.roguetraderscalafx.HullType.{Frigate, Raider, Transport}
import enumeratum.{Enum, EnumEntry}

import scala.collection.mutable


sealed trait Artillery extends EnumEntry {
  val stats : ArtilleryStat

  // Какие вообще бывают ограничения на артиллерию? Есть ограничение компонента по корпусу,
  // есть ограничение для боковых батарей (только боковой слот) и есть ограничение для лэнсов
  // (только носовой слот для транспортов, фрегатов и рейдеров)
  def validateConstraint(hull: HullType, slot: ArtillerySlot): Boolean = {
    stats.slotConstraint.map( _.contains(slot) ).getOrElse(default = true) &&
      stats.constraint.contains(hull)
  }

  def score: (Int, Int, Int) = (stats.power, stats.space, stats.cost)
}

case class ArtilleryCell(slot: ArtillerySlot, var cell: Option[Artillery])
case class ArtilleryStat(
  name: String,
  constraint: Set[HullType],
  energy: Int,
  space: Int,
  cost: Int,
  power: Int,
  damage: String,
  crit: Int,
  range: Int,
  slotConstraint: Option[Set[ArtillerySlot]],
)

object Artillery extends Enum[Artillery] {
  val values = findValues

  case class Battery(
                      stats: ArtilleryStat
                    ) extends Artillery

  case class Lance(
                    stats: ArtilleryStat
                  ) extends Artillery {
    override def validateConstraint(hull: HullType, slot: ArtillerySlot): Boolean = {
      hull match  {
        case Transport | Raider | Frigate => slot == ArtillerySlot.Forward
        case _ => super.validateConstraint(hull, slot)
      }
    }
  }
  case class Unique(
    stats: ArtilleryStat
  ) extends Artillery

  def createArtCells (slotList: IterableOnce[(ArtillerySlot, Int)]): mutable.IndexedSeq[ArtilleryCell] = {
    mutable.IndexedSeq.from(slotList).flatMap{
      case (slot: ArtillerySlot, quantity: Int) => mutable.ArraySeq.fill(quantity) (ArtilleryCell(slot, None))
    }
  }

  val createBattery = ArtilleryStat.tupled andThen Battery
  val createLance = ArtilleryStat.tupled andThen Lance

}
