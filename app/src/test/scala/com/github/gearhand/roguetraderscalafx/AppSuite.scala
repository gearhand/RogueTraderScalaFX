/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
package com.github.gearhand.roguetraderscalafx

import com.github.gearhand.roguetraderscalafx.Artillery.{Battery, Lance, createArtCells, createBattery, createLance}
import com.github.gearhand.roguetraderscalafx.ArtillerySlot.{Forward, LeftBoard, RightBoard}
import com.github.gearhand.roguetraderscalafx.EssentialCategory.Drive
import com.github.gearhand.roguetraderscalafx.HullType.Transport
import com.github.gearhand.roguetraderscalafx.MyYamlProtocol._
import com.github.gearhand.roguetraderscalafx.Simple
import net.jcazevedo.moultingyaml._
import org.junit.Before
import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.yaml.snakeyaml.nodes.Tag
import org.yaml.snakeyaml.{TypeDescription, Yaml}

import scala.io._
import scala.jdk.CollectionConverters.{CollectionHasAsScala, MapHasAsScala}

@RunWith(classOf[JUnitRunner])
class AppSuite extends AnyFunSuite {
  def exampleHull = {
    new Hull("Jerico"
      , Transport
      , 3
      , -10
      , 5
      , 50
      , 12
      , 1
      , 45
      , 20
      , createArtCells(List((Forward, 1), (LeftBoard, 1), (RightBoard, 1)))
      , "Грузовое судно")
   }

  test("App has a greeting") {
    assert(App.greeting() != null)
  }

  test("Components deserialization") {

    println(System.getProperty("user.dir"))
    val source = Source.fromFile("src/test/resources/test.yml", "utf-8")
    val yamlAst = try
      EssentialsCatalogFormat.read(source.getLines().mkString("\n").parseYaml).toMap
      finally source.close()

    assert(yamlAst.get(Drive).map { list =>
      val head = list.head
      head.name.equals("Jovian Pattern Class 1 Drive") && head.hulls.contains(HullType.Transport)
    }.get)
  }

  //test("SnakeYaml deserialization with tags") {
  //  println(System.getProperty("user.dir"))
  //  //val source = Source.fromFile("src/test/resources/test.yml", "utf-8")
  //  val source = Source.fromFile("src/test/resources/test_simple.yml", "utf-8")
  //  val yaml = new Yaml()
  //  val description = new TypeDescription(classOf[Simple], new Tag("!simple"))
  //  //val description = new TypeDescription(Essential.getClass, new Tag("!essential"))
  //  //val description = new TypeDescription(classOf[java.util.Map[String, java.util.Collection[Essential]]], new Tag("!essential"))
  //  val input = source.getLines().mkString("\n")
  //  yaml.addTypeDescription(description)
  //  val result: Simple = yaml.load(input)
  //  (result.first.asScala concat result.second.asScala).foreach(println)
  //  /*val result: java.util.Map[String, java.util.Collection[Essential]] = yaml.load(input)
  //  val scalaResult: Map[String, List[Essential]] = result.asScala.view.mapValues(_.asScala.toList).toMap
  //  scalaResult("Drive").map(_.space).foreach(println(_))*/
  //  //scalaResult.get("Drive").map(_.space).forEach(x => println(x))
  //  source.close()
  //}

  test("Artillery addition") {

    val hull = exampleHull
    val lance = createLance("TestLance", Set(Transport), 1, 1, 1, "1k10", 1, 1, None)
    val boardGun = createBattery("TestBoard", Set(Transport), 1, 1, 1, "1k10", 1, 1, Some(Set(LeftBoard, RightBoard)))
    val regular = createBattery("TestRegular", Set(Transport), 1, 1, 1, "1k10", 1, 1, None)

    assertResult(Right("OK")) (hull.addArt(boardGun))
    assertResult(Right("OK")) (hull.addArt(regular))
    assertResult(Left("Cannot add")) (hull.addArt(lance))
    assertResult(ArtilleryCell(Forward, Some(regular)))(hull.artilleryCells.head)
    assertResult(ArtilleryCell(LeftBoard, Some(boardGun)))(hull.artilleryCells(1))
    assertResult(ArtilleryCell(RightBoard, None))(hull.artilleryCells(2))
  }

  test("Artillery addition -- positive lance") {

    val hull = exampleHull
    val lance = createLance("TestLance", Set(Transport), 1, 1, 1, "1k10", 1, 1, None)
    val boardGun = createBattery("TestBoard", Set(Transport), 1, 1, 1, "1k10", 1, 1, Some(Set(LeftBoard, RightBoard)))
    val regular = createBattery("TestRegular", Set(Transport), 1, 1, 1, "1k10", 1, 1, None)

    assertResult(Right("OK")) (hull.addArt(boardGun))
    assertResult(Right("OK")) (hull.addArt(lance))
    assertResult(Right("OK")) (hull.addArt(regular))
    assertResult(ArtilleryCell(Forward, Some(lance)))(hull.artilleryCells.head)
    assertResult(ArtilleryCell(LeftBoard, Some(boardGun)))(hull.artilleryCells(1))
    assertResult(ArtilleryCell(RightBoard, Some(regular)))(hull.artilleryCells(2))
  }

  test("Random 1") {
    val ship = Algorithms.generate(exampleHull)
    println(ship.hull.toYaml.prettyPrint)
  }
}
