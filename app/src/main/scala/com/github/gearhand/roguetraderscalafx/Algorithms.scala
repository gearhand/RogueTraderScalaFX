package com.github.gearhand.roguetraderscalafx

import com.github.gearhand.roguetraderscalafx.Functions.generateEssentials

import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.util.Random


class Simple(var first: java.util.List[Int], var second: java.util.List[Int]) {
  def this() = this(List.empty.asJava, List.empty.asJava)
}
object Algorithms {
  // стратегия по умолчанию -- забивать все артиллерийские слоты, а потом уже добирать компоненты
  // вообще мы не можем набирать артиллерию без учёта энергии
  // другой вариант -- выбираем случайный вектор необходимых компонент, затем добираем
  // артиллерию с учётом энергии, и затем случайным образом добираем доп. компоненты
  val random: Random = new Random

  def randomElement[A](seq: Seq[A]): A = seq(random.nextInt(seq.length))

  def generate(hull: Hull) = new Ship(
    hull,
    randomElement(generateEssentials(hull) (Map.empty)).values.toSet,
    List.empty
  )
  //def fillArtillery
  //def fillEssentials(hullType) = allEssentials.filter(hullType).
  //def fillSupplementals
}