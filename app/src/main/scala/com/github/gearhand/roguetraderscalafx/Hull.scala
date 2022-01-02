package com.github.gearhand.roguetraderscalafx

import com.github.gearhand.roguetraderscalafx.HullType.{Frigate, Raider, Transport}
import scala.Function
import scala.Function0
import scala.Function1
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
  artilleryCells: mutable.Seq[ArtilleryCell],
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

  case object Any extends HullType
  case object Transport extends HullType
  case object Raider extends HullType
  case object Frigate extends HullType
  case object LightCruiser extends HullType
  case object Cruiser extends HullType
  case object GrandCruiser extends HullType // Battle Fleet Koronus hull type
  case object LinearCruiser extends HullType // Battle Fleet Koronus hull type
}

sealed trait ArtillerySlot extends EnumEntry

object ArtillerySlot extends Enum[ArtillerySlot] {
  val values = findValues

  case object LeftBoard extends ArtillerySlot
  case object RightBoard extends ArtillerySlot
  case object Deck extends ArtillerySlot
  case object Forward extends ArtillerySlot
  case object Kile extends ArtillerySlot
}

sealed trait Artillery extends EnumEntry {
  val stats : ArtilleryStat

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

  // Какие вообще бывают ограничения на артиллерию? Есть ограничение компонента по корпусу,
  // есть ограничение для боковых батарей (только боковой слот) и есть ограничение для лэнсов
  // (только носовой слот для транспортов, фрегатов и рейдеров)
}

//createArtCells : [(ArtillerySlot, Natural)] -> ArtilleryCells
//createArtCells = fromList . fmap (\(k,v) -> (k, array (1, v) $ fmap (, Nothing) [1..v]))
