package com.github.gearhand.roguetraderscalafx
import com.github.gearhand.roguetraderscalafx.Artillery.{Battery, Lance, Unique, createArtCells}
import com.github.gearhand.roguetraderscalafx.Essential._
import net.jcazevedo.moultingyaml._

import scala.collection.{Iterable, mutable}

object MyYamlProtocol extends DefaultYamlProtocol {
  case class Catalog(
    drive: List[Drive],
    warpDrive: List[WarpDrive],
    gellarField: List[GellarField],
    voidShield: List[VoidShield],
    bridge: List[Bridge],
    lifeSupport: List[LifeSupport],
    quarters: List[Quarters],
    sensors: List[Sensors],
  )

  implicit object HullTypeFormat extends YamlFormat[HullType] {
    override def read(yaml: YamlValue): HullType = yaml match {
      case YamlString(value) => HullType.withName(value)
      case _ => deserializationError("Hull type deserialization failure")
    }

    override def write(obj: HullType): YamlValue = YamlString(obj.entryName)
  }

  implicit object RaceFormat extends YamlFormat[Race] {
    override def read(yaml: YamlValue): Race = yaml match {
      case YamlString(value) => Race.withName(value)
      case _ => deserializationError("Race type deserialization failure")
    }

    override def write(obj: Race): YamlValue = ???
  }

  implicit object HullTypeSetFormat extends YamlFormat[Set[HullType]] {
    def write(set: Set[HullType]) = YamlSet(set.map(_.toYaml))
    def read(value: YamlValue): Set[HullType] = value match {
      case YamlSet(elements) =>
        elements.map(_.convertTo[HullType])
      case YamlNull => Set.from(HullType.values)
      case x =>
        deserializationError("Expected Set as YamlSet, but got " + x)
    }
  }

  implicit val essentialStatsFormat = yamlFormat6(EssentialStats)

  class EssentialFormat[A <: Essential] extends YamlFormat[A] {
    override def write(obj: A): YamlValue = YamlObject(
      obj.entryName.toYaml -> obj.stats.toYaml
    )

    override def read(yaml: YamlValue): A = ???
  }

  implicit val driveFormat = new EssentialFormat[Drive]
  implicit val warpFormat = new EssentialFormat[WarpDrive]
  implicit val gellarFormat = new EssentialFormat[GellarField]
  implicit val voidShieldFormat = new EssentialFormat[VoidShield]
  implicit val bridgeFormat = new EssentialFormat[Bridge]
  implicit val lifeSupportFormat = new EssentialFormat[LifeSupport]
  implicit val quartersFormat = new EssentialFormat[Quarters]
  implicit val sensorsFormat = new EssentialFormat[Sensors]
  implicit val essentialSetFormat = yamlFormat8(EssentialSet)
  implicit val driveListFormat = listFormat(driveFormat)

  implicit object EssentialsCatalogFormat extends YamlFormat[Catalog] {

    override def write(obj: Catalog): YamlValue = ???

    override def read(value: YamlValue): Catalog = {
      val fieldMap = value.asYamlObject.fields
      Catalog(
        fieldMap("Drive".toYaml).convertTo[List[EssentialStats]].map(Drive),
        fieldMap("WarpDrive".toYaml).convertTo[List[EssentialStats]].map(WarpDrive),
        fieldMap("GellarField".toYaml).convertTo[List[EssentialStats]].map(GellarField),
        fieldMap("VoidShield".toYaml).convertTo[List[EssentialStats]].map(VoidShield),
        fieldMap("Bridge".toYaml).convertTo[List[EssentialStats]].map(Bridge),
        fieldMap("LifeSupport".toYaml).convertTo[List[EssentialStats]].map(LifeSupport),
        fieldMap("Quarters".toYaml).convertTo[List[EssentialStats]].map(Quarters),
        fieldMap("Sensors".toYaml).convertTo[List[EssentialStats]].map(Sensors),
      )
    }
  }


  implicit object ArtillerySlotFormat extends YamlFormat[ArtillerySlot] {
    override def read(yaml: YamlValue): ArtillerySlot = yaml match {
      case YamlString(name) => ArtillerySlot.withName(name)
      case _ => deserializationError("Expecting string for artillery slot")
    }

    override def write(obj: ArtillerySlot): YamlValue = YamlString(obj.entryName)
  }


  implicit val artStatFormat = yamlFormat10(ArtilleryStat)
  implicit object ArtilleryFormat extends YamlFormat[Artillery] {

    override def write(obj: Artillery): YamlValue = {
      val intermediate = obj.stats.toYaml.asYamlObject.fields +
        (YamlString("type") -> YamlString(obj.entryName))
      YamlObject(intermediate)
    }

    override def read(yaml: YamlValue): Artillery = yaml match {
      case YamlObject(fields) =>
        fields(YamlString("type")).convertTo[String] match {
          case "Battery" => Battery(yaml.convertTo[ArtilleryStat])
          case "Lance" => Lance(yaml.convertTo[ArtilleryStat])
          case "Unique" => Unique(yaml.convertTo[ArtilleryStat])
          case _ => deserializationError("Unknown artillery type!")
        }
      case _ => deserializationError("Expected yaml object")
    }
  }

  implicit val artilleryCellFormat = yamlFormat2(ArtilleryCell)

  val artilleryEquipped = viaSeq[mutable.IndexedSeq[ArtilleryCell], ArtilleryCell](mutable.ArraySeq.from(_))

  def hullFormat(implicit format: YamlFormat[mutable.IndexedSeq[ArtilleryCell]]): YamlFormat[Hull] = {
    yamlFormat12(Hull)
  }

  object ArtilleryCellStructure extends YamlFormat[mutable.IndexedSeq[ArtilleryCell]] {
    override def read(yaml: YamlValue): mutable.IndexedSeq[ArtilleryCell] = yaml match {
      case YamlObject(fields) => createArtCells(fields.map {
        case (YamlString(key), YamlNumber(value)) => (ArtillerySlot.withName(key), value.toInt)
        case _ => deserializationError("Expected String: Number mapping")
      }.iterator)
      case _ => deserializationError("Expected yaml object for cells")
    }

    override def write(obj: mutable.IndexedSeq[ArtilleryCell]): YamlValue = YamlArray(obj.map(_.toYaml).toVector)

  }

  implicit val supplementalFormat = yamlFormat7(Supplemental)

  implicit object ShipFormat extends YamlFormat[Ship] {
    override def read(yaml: YamlValue): Ship = ???

    override def write(obj: Ship): YamlValue = YamlObject(
      YamlString("hull") -> (hullFormat(artilleryEquipped) write obj.hull),
      YamlString("essentials") -> obj.essentials.toYaml,
      YamlString("supplementals") -> obj.supplementals.toYaml,

    )
  }
}
