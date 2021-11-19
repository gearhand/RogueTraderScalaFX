package com.github.gearhand.roguetraderscalafx

import enumeratum._

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
            , val artilleryCells  : Map[ArtillerySlot, Array[Option[Artillery]]]
            , val traits          : String
          )

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

sealed trait Artillery extends EnumEntry
object Artillery extends Enum[Artillery] {
  val values = findValues
  case class Battery ( name : String
                 , constraint : Hull => Boolean
                 , artPower : Int
                 , artSpace : Int
                 , artCost  : Int
                 , damage   : String
                 , crit     : Int
                 , range    : Int
               )
  case class Lance ( name : String
               , constraint : Hull => Boolean
               , artPower : Int
               , artSpace : Int
               , artCost  : Int
               , damage   : String
               , crit     : Int
               , range    : Int
             )
  type ArtilleryCells = Map[ArtillerySlot, Array[Option[Artillery]]]
  val createArtCells: List[(ArtillerySlot, Int)] => ArtilleryCells =
    (slotList: List[(ArtillerySlot, Int)]) => {
      slotList.map{ case (slot: ArtillerySlot, quantity: Int) => (slot: ArtillerySlot, Array.fill(quantity) (Option.empty[Artillery])) }.toMap
    }
}

//createArtCells : [(ArtillerySlot, Natural)] -> ArtilleryCells
//createArtCells = fromList . fmap (\(k,v) -> (k, array (1, v) $ fmap (, Nothing) [1..v]))
