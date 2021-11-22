package com.github.gearhand.roguetraderscalafx

import com.github.gearhand.roguetraderscalafx.HullType.{Frigate, Raider, Transport}
import enumeratum._

import scala.collection.mutable

class Hull ( val hullName: String
            , val hullType        : HullType
            , val speed           : Int
            , val maneuverability : Int
            , val awareness       : Int
            , val durability      : Int
            , val armor           : Int
            , val turretsClass    : Int
            , val space           : Int
            , val cost            : Int
            , val artilleryCells  : mutable.Seq[ArtilleryCell]
            , val traits          : String
          )
{
  def addArt(component: Artillery) = {
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
  val values = findValues

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
  val  name : String
  val constraint : Set[HullType]
  val power : Int
  val space : Int
  val cost  : Int
  val damage   : String
  val crit     : Int
  val range    : Int
  val slotConstraint: Option[Set[ArtillerySlot]]

  def validateConstraint(hull: HullType, slot: ArtillerySlot): Boolean = {
    slotConstraint.map( _.contains(slot) ).getOrElse(default = true) &&
      constraint.contains(hull)
  }
}

case class ArtilleryCell(slot: ArtillerySlot, var cell: Option[Artillery])

object Artillery extends Enum[Artillery] {
  val values = findValues
  case class Battery ( name : String
                 , constraint : Set[HullType]
                 , power : Int
                 , space : Int
                 , cost  : Int
                 , damage   : String
                 , crit     : Int
                 , range    : Int
                 , slotConstraint: Option[Set[ArtillerySlot]]
               ) extends Artillery
  case class Lance ( name : String
               , constraint : Set[HullType]
               , power : Int
               , space : Int
               , cost  : Int
               , damage   : String
               , crit     : Int
               , range    : Int
               , slotConstraint: Option[Set[ArtillerySlot]]
             ) extends Artillery
  {
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


  // Какие вообще бывают ограничения на артиллерию? Есть ограничение компонента по корпусу,
  // есть ограничение для боковых батарей (только боковой слот) и есть ограничение для лэнсов
  // (только носовой слот для транспортов, фрегатов и рейдеров)
}

//createArtCells : [(ArtillerySlot, Natural)] -> ArtilleryCells
//createArtCells = fromList . fmap (\(k,v) -> (k, array (1, v) $ fmap (, Nothing) [1..v]))
