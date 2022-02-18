package com.github.gearhand.roguetraderscalafx

import com.github.gearhand.roguetraderscalafx.Essential.EssentialSet
import com.github.gearhand.roguetraderscalafx.MyYamlProtocol.Catalog

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import cats.data.State

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

  type Score = (Int, Int, Int)
  // стратегия по умолчанию -- забивать все артиллерийские слоты, а потом уже добирать компоненты
  // вообще мы не можем набирать артиллерию без учёта энергии
  // другой вариант -- выбираем случайный вектор необходимых компонент, затем добираем
  // артиллерию с учётом энергии, и затем случайным образом добираем доп. компоненты
  val random: Random = new Random

  def generate(
    hullType: HullType,
    points: Int,
    hulls: Seq[Hull],
    essentialCatalog: Catalog,
    artilleryCatalog: Seq[Artillery],
    supplementalPool: Seq[Supplemental],
    randomizer: Random
  ): Ship = {
    val hull = randomElement(hulls)
    val limits = (0, hull.space, points - hull.cost)
    val generator = for {
      ess <- generateEssentials(hullType, essentialCatalog)
      a_hull <- generateArtillery(hull, artilleryCatalog)
      supp <- generateSupplementals(limits, supplementalPool)
    } yield new Ship(a_hull, ess, supp.toList)
    generator.runA((0,0,0)).value
  }

  def randomElement[A](seq: Seq[A]): A = seq(random.nextInt(seq.length))

  def fillArtillery(hull: Hull, artilleryCatalog: Seq[Artillery]): Unit = {
    val filtered = artilleryCatalog.filter(_.stats.constraint.contains(hull.hullType))
    hull.artilleryCells.mapInPlace { cell =>
      cell.cell = Some(filtered.filter(_.validateConstraint(hull.hullType, cell.slot)).getRandom)
      cell
    }
  }

  /**
   * Алгоритм: есть начальный пул, есть ограничение, есть функция случайного выбора
   * Хорошо бы сделать бесконечный итератор, где next будет выдавать нам рандомный элемент
   * Получив элемент, мы хотим узнать, превысим ли мы ограничение, добавив его
   * Либо же мы можем изначально фильтровать превышающие элементы (сейчас так и сделано, но как будто
   * это не имеет большого смысла, так как скорее всего ограничение будет стоять высоко)
   * Условие останова: не осталось элементов, которые мы можем добавить к ограничению
   * Сейчас оно реализовано через фильтр начального пула. Как будто мы можем просто бежать по циклу,
   * пока не упрёмся в ограничение первый раз, затем уже добирать с фильтом пула
   * @param score
   * @param suppCatalog уже должен быть отфильтрован по hullType
   * @return
   */
  def fillSupplementals(score: (Int, Int, Int), pool: Seq[Supplemental]): ArrayBuffer[Supplemental] = {
    var currentScore = score
    var suggestion = pool.filter { item =>
      (currentScore - item.score).allPositive
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

  def fillSupplementals2(score: (Int, Int, Int), pool: Seq[Supplemental]): ArrayBuffer[Supplemental] = {
    type Result = ArrayBuffer[Supplemental]
    type Score = (Int, Int, Int)
    @tailrec
    def _firstStep(limit: Score, accum: Result): (Score, Result) = {
      val item = pool.getRandom
      if (!(limit - item.score).allPositive) {
        (limit, accum)
      } else if (item.unique.getOrElse(false) && accum.contains(item)) {
        _firstStep(limit, accum)
      } else {
        _firstStep(limit - item.score, accum += item)
      }
    }

    val result = mutable.ArrayBuffer.empty[Supplemental]
    @tailrec
    def _fill(limit: Score, accum: Result, pool: Seq[Supplemental]): Result = {
      if (pool.nonEmpty) {
        val (new_limit, temp_acc) = _firstStep(limit, accum)
        val new_pool = pool.filter(item => (new_limit - item.score).allPositive)
        _fill(new_limit, temp_acc, new_pool)
      } else {
        accum
      }
    }

    _fill(score, result, pool)

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

  private def generateEssentials (hullType: HullType, catalog: Catalog) = State { _ : Score =>
    val vec = randomEssentialVector(hullType, catalog)
    (vec.score, vec)
  }

  private def generateArtillery (hull: Hull, artilleryCatalog: Seq[Artillery]) = State { score: Score =>
    fillArtillery(hull, artilleryCatalog)
    val artScore = hull.artilleryCells.map(_.cell.get.score).fold (score) (_ + _)
    (artScore, hull)
  }

  private def generateSupplementals(limits: (Int, Int, Int), pool: Seq[Supplemental]) = State { score: Score =>
    val vec = fillSupplementals(limits - score, pool)
    val totalScore = vec.map(_.score).fold (score) (_ + _)
    (totalScore, vec)
  }

}
