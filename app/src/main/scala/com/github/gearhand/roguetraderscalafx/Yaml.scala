package com.github.gearhand.roguetraderscalafx
import net.jcazevedo.moultingyaml._

import scala.collection.immutable.{AbstractSeq, LinearSeq}


object MyYamlProtocol extends DefaultYamlProtocol {
  type Catalog = Map[EssentialCategory, List[Essential]]

  implicit object HullTypeFormat extends YamlFormat[HullType] {
    override def read(yaml: YamlValue): HullType = yaml match {
      case YamlString(value) => HullType.withName(value)
      case _ => deserializationError("Hull type deserialization failure")
    }

    override def write(obj: HullType): YamlValue = ???
  }

  implicit object EssentialsFormat extends YamlFormat[Catalog] {

    override def write(obj: Catalog): YamlValue = ???

    override def read(value: YamlValue): Catalog = {
      value.asYamlObject.fields.map {
        case (a_category: YamlString,  others: YamlArray) =>
          val category = EssentialCategory.withName(a_category.value)
          val essentialPart = Essential(category) _
          category -> others.elements.map { (elem: YamlValue) =>
            val fieldMap = elem.asYamlObject.fields
            essentialPart(
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
}
