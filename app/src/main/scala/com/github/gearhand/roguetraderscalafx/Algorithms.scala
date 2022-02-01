package com.github.gearhand.roguetraderscalafx

import com.github.gearhand.roguetraderscalafx.Essential.EssentialSet
import com.github.gearhand.roguetraderscalafx.MyYamlProtocol.Catalog

import scala.collection.mutable
import scala.util.Random

object Algorithms {

  implicit class PimpledSeq[A](toSeq: Seq[A]) {
    def getRandom: A = toSeq(random.nextInt(toSeq.length))
  }

  implicit class Int3(tuple3: (Int, Int, Int)) {
    def +(rhs: (Int, Int, Int)): (Int, Int, Int) = {
      (tuple3._1 + rhs._1, tuple3._2 + rhs._2, tuple3._3 + rhs._3)
    }
    def -(rhs: (Int, Int, Int)): (Int, Int, Int) = {
      (tuple3._1 - rhs._1, tuple3._2 - rhs._2, tuple3._3 - rhs._3)
    }

    def allPositive: Boolean = tuple3._1 > 0 && tuple3._2 > 0 && tuple3._3 > 0
  }
  // стратегия по умолчанию -- забивать все артиллерийские слоты, а потом уже добирать компоненты
  // вообще мы не можем набирать артиллерию без учёта энергии
  // другой вариант -- выбираем случайный вектор необходимых компонент, затем добираем
  // артиллерию с учётом энергии, и затем случайным образом добираем доп. компоненты
  val random: Random = new Random

  def randomElement[A](seq: Seq[A]): A = seq(random.nextInt(seq.length))

  def generate(hull: Hull, essentialCatalog: Catalog, artilleryCatalog: Seq[Artillery]) = {
    val essentials = randomEssentialVector(hull.hullType, essentialCatalog)
    val essentialScore = essentials.score
    fillArtillery(hull, artilleryCatalog)
    val artScore = hull.artilleryCells.map(_.cell.get.score).fold (0,0,0) (_ + _)
  }

  def fillArtillery(hull: Hull, artilleryCatalog: Seq[Artillery]): Unit = {
    val filtered = artilleryCatalog.filter(_.stats.constraint.contains(hull.hullType))
    hull.artilleryCells.mapInPlace { cell =>
      cell.cell = Some(filtered.filter(_.validateConstraint(hull.hullType, cell.slot)).getRandom)
      cell
    }
  }
  def fillSupplementals(score: (Int, Int, Int), hullType: HullType, suppCatalog: Seq[Supplemental]) = {
    var currentScore = score
    var suggestion = suppCatalog.filter { item =>
      item.hulls.contains(hullType) && (currentScore - item.score).allPositive
    }
    val result = mutable.ArrayBuffer.empty[Supplemental]
    while (suggestion.nonEmpty) {
      val item = suggestion.getRandom
      if (!item.unique.getOrElse(false) || !result.contains(item)) {
        result += item
        currentScore -= item.score
      }
      suggestion = suggestion.filter(item => (currentScore - item.score).allPositive)
    }
    result
  }

  def filterRandom[A <: Essential]: HullType => List[A] => A = (hullType: HullType) => (list: List[A]) => {
    list.filter(_.stats.hulls(hullType)).getRandom
  }

  def randomEssentialVector (hullType: HullType, catalog: Catalog): EssentialSet = {
    def _filterRandom[A <: Essential] = filterRandom[A](hullType)
    EssentialSet(
      _filterRandom (catalog.drive),
      _filterRandom (catalog.warpDrive),
      _filterRandom (catalog.gellarField),
      _filterRandom (catalog.voidShield),
      _filterRandom (catalog.bridge),
      _filterRandom (catalog.lifeSupport),
      _filterRandom (catalog.quarters),
      _filterRandom (catalog.sensors),
    )
  }

}
