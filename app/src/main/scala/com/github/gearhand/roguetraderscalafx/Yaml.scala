package com.github.gearhand.roguetraderscalafx
import com.github.gearhand.roguetraderscalafx.Essential._
import com.github.gearhand.roguetraderscalafx.HullType.AnyHull
import net.jcazevedo.moultingyaml._

import scala.collection.mutable

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

  implicit object HullTypeSetFormat extends YamlFormat[Set[HullType]] {
    def write(set: Set[HullType]) = YamlSet(set.map(_.toYaml))
    def read(value: YamlValue): Set[HullType] = value match {
      case YamlSet(elements) =>
        val converted = elements.map(_.convertTo[HullType])
        if (!converted.contains(AnyHull)) converted
        else Set.from(HullType.values).excl(AnyHull)
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
    override def read(yaml: YamlValue): ArtillerySlot = ???

    override def write(obj: ArtillerySlot): YamlValue = YamlString(obj.entryName)
  }


  implicit val artStatFormat = yamlFormat9(ArtilleryStat)
  implicit object Artillery extends YamlFormat[Artillery] {

    override def write(obj: Artillery): YamlValue = {
      YamlArray(
        YamlString(obj.entryName),
        obj.stats.toYaml
      )
    }

    override def read(yaml: YamlValue): Artillery = ???
  }

  implicit val artilleryCellFormat = yamlFormat2(ArtilleryCell)
  implicit val artCellSeqFormat = new MutableSeqFormat[ArtilleryCell]
  implicit val hullFormat = yamlFormat12(Hull)

  class MutableSeqFormat[A: YamlFormat] extends YamlFormat[mutable.Seq[A]] {

    override def read(yaml: YamlValue): mutable.Seq[A] = ???

    override def write(obj: mutable.Seq[A]): YamlValue = obj.toSeq.toYaml
  }


  implicit val supplementalFormat = yamlFormat6(Supplemental)

  implicit object ShipFormat extends YamlFormat[Ship] {
    override def read(yaml: YamlValue): Ship = ???

    override def write(obj: Ship): YamlValue = YamlObject(
      YamlString("hull") -> obj.hull.toYaml,
      YamlString("essentials") -> obj.essentials.toYaml,
      YamlString("supplementals") -> obj.supplementals.toYaml,

    )
  }
}
