package com.github.gearhand.roguetraderscalafx

import enumeratum._
import Algorithms.Int3

case class EssentialStats(
  hulls: Set[HullType],
  power: Int,
  space: Int,
  name: String,
  cost: Option[Int],
  traits: Option[String],
)

sealed trait Essential extends EnumEntry {
  val stats: EssentialStats

  def score: (Int, Int, Int) = (stats.power, stats.space, stats.cost.getOrElse(0))
}

object Essential extends Enum[Essential] {
  val values = findValues
  case class Drive(stats: EssentialStats) extends Essential
  case class WarpDrive(stats: EssentialStats) extends Essential
  case class GellarField(stats: EssentialStats) extends Essential
  case class VoidShield(stats: EssentialStats) extends Essential
  case class Bridge(stats: EssentialStats) extends Essential
  case class LifeSupport(stats: EssentialStats) extends Essential
  case class Quarters(stats: EssentialStats) extends Essential
  case class Sensors(stats: EssentialStats) extends Essential

  case class EssentialSet(
    drive: Drive,
    warpDrive: WarpDrive,
    gellarField: GellarField,
    voidShield: VoidShield,
    bridge: Bridge,
    lifeSupport: LifeSupport,
    quarters: Quarters,
    sensors: Sensors
  ) {
    def score: (Int, Int, Int) = {
      drive.score - (
        warpDrive.score +
        gellarField.score +
        voidShield.score +
        bridge.score +
        lifeSupport.score +
        quarters.score +
        sensors.score
      )
    }
  }
}

