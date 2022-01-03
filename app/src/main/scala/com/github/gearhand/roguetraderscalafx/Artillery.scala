package com.github.gearhand.roguetraderscalafx

import com.github.gearhand.roguetraderscalafx.Artillery.findValues
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
}

case class ArtilleryCell(slot: ArtillerySlot, var cell: Option[Artillery])
case class ArtilleryStat(
                          name: String,
                          constraint: Set[HullType],
                          power: Int,
                          space: Int,
                          cost: Int,
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
  def createArtCells (slotList: Seq[(ArtillerySlot, Int)]): mutable.Seq[ArtilleryCell] = slotList.flatMap{
    case (slot: ArtillerySlot, quantity: Int) => Array.fill(quantity) (ArtilleryCell(slot, None))
  }.toBuffer

  def createBattery = ArtilleryStat.tupled andThen Battery
  def createLance = ArtilleryStat.tupled andThen Lance

}
