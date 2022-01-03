package com.github.gearhand.roguetraderscalafx

import com.github.gearhand.roguetraderscalafx.Essential.EssentialSet
import com.github.gearhand.roguetraderscalafx.MyYamlProtocol.Catalog
import scala.util.Random

object Algorithms {

  implicit class PimpledSeq[A](toSeq: Seq[A]) {
    def getRandom(): A = toSeq(random.nextInt(toSeq.length))
  }
  // стратегия по умолчанию -- забивать все артиллерийские слоты, а потом уже добирать компоненты
  // вообще мы не можем набирать артиллерию без учёта энергии
  // другой вариант -- выбираем случайный вектор необходимых компонент, затем добираем
  // артиллерию с учётом энергии, и затем случайным образом добираем доп. компоненты
  val random: Random = new Random

  def randomElement[A](seq: Seq[A]): A = seq(random.nextInt(seq.length))

  def generate(hull: Hull, essentialCatalog: Catalog) = new Ship(
    hull,
    Set.empty /*randomElement(generateEssentials(hull) (essentialCatalog)).values.toSet*/,
    List.empty
  )
  //def fillArtillery
  //def fillEssentials(hullType) = allEssentials.filter(hullType).
  //def fillSupplementals

  def filterRandom[A <: Essential]: Hull => List[A] => A = (hull: Hull) => (list: List[A]) => {
    list.filter(_.stats.hulls(hull.hullType)).getRandom()
  }

  def randomEssentialVector (hull: Hull, catalog: Catalog) = {
    EssentialSet(
      filterRandom (hull) (catalog.drive),
      filterRandom (hull) (catalog.warpDrive),
      filterRandom (hull) (catalog.gellarField),
      filterRandom (hull) (catalog.voidShield),
      filterRandom (hull) (catalog.bridge),
      filterRandom (hull) (catalog.lifeSupport),
      filterRandom (hull) (catalog.quarters),
      filterRandom (hull) (catalog.sensors),
    )
  }

}
