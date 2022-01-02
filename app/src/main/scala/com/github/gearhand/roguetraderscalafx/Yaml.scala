package com.github.gearhand.roguetraderscalafx
import com.github.gearhand.roguetraderscalafx.Artillery.{Battery, Lance}
import io.estatico.newtype.macros.newtype
import net.jcazevedo.moultingyaml._

import scala.collection.mutable




object MyYamlProtocol extends DefaultYamlProtocol {
  @newtype case class Catalog (toMap: Map[EssentialCategory, List[Essential]])

  implicit object HullTypeFormat extends YamlFormat[HullType] {
    override def read(yaml: YamlValue): HullType = yaml match {
      case YamlString(value) => HullType.withName(value)
      case _ => deserializationError("Hull type deserialization failure")
    }

    override def write(obj: HullType): YamlValue = YamlString(obj.entryName)
  }

  implicit object EssentialsCatalogFormat extends YamlFormat[Catalog] {

    override def write(obj: Catalog): YamlValue = ???

    override def read(value: YamlValue): Catalog = Catalog {
      value.asYamlObject.fields.map {
        case (a_category: YamlString,  others: YamlArray) =>
          val category = EssentialCategory.withName(a_category.value)
          category -> others.elements.map { (elem: YamlValue) =>
            val fieldMap = elem.asYamlObject.fields
            Essential(
              category,
              fieldMap(YamlString("hull")).convertTo[Set[HullType]],
              fieldMap(YamlString("power")).convertTo[Int],
              fieldMap(YamlString("space")).convertTo[Int],
              fieldMap(YamlString("name")).convertTo[String],
              fieldMap.get(YamlString("price")).map(_.convertTo[Int]),
              fieldMap.get(YamlString("traits")).map(_.convertTo[String]),
            )
          }.toList
        case _ => deserializationError("")
      }
    }
  }


  implicit object ArtillerySlotFormat extends YamlFormat[ArtillerySlot] {
    override def read(yaml: YamlValue): ArtillerySlot = ???

    override def write(obj: ArtillerySlot): YamlValue = YamlString(obj.entryName)
  }


  implicit val artStatFormat = yamlFormat9(ArtilleryStat)
  //implicit val batteryFormat = yamlFormat1(Battery.apply)
  //implicit val lanceFormat = yamlFormat1(Lance.apply)
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
}
