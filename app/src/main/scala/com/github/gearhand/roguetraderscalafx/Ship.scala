package com.github.gearhand.roguetraderscalafx

import com.github.gearhand.roguetraderscalafx.Artillery.createArtCells
import com.github.gearhand.roguetraderscalafx.ArtillerySlot.{Forward, LeftBoard, RightBoard}
import com.github.gearhand.roguetraderscalafx.HullType.Transport
import enumeratum._

import scala.List
class Ship ( var hull: Hull
           , var essentials: Set[Essential]
           , var supplementals: List[Supplemental]
           )

sealed trait Race extends EnumEntry
object Race extends Enum[Race] {
  val values = findValues
  case object Imperium
  case object Orkz
  case object Aeldar
} // deriving (Show, Enum, Bounded)


case class Supplemental (hulls: Set[HullType], power: Int, space: Int, cost: Int, name: String, traits: Option[String])

object Examples {
  // "Jerico" pilgrim vessel
  val exampleHull = new Hull("Jerico"
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
  //val foo = Supp
}

object Functions {
  // generate = generateSupp . generateArt . generateEssentials
  def dekart[A,B,C](op: (A,B) => C) (c1: Iterable[A]) (c2: Iterable[B]): Iterable[C] = {
    c1.flatMap { a => c2.map { b => op(a, b) }}
  }
  def dekartSpecific(essentialVariant: List[EssentialVariant], essentials: List[Essential]): List[EssentialVariant] = {
    essentialVariant.flatMap { variant =>
      essentials.map { ess => variant.updated(ess.category, ess)}
    }
  }
  type EssentialVariant = Map[EssentialCategory,Essential]
  type Catalog = Map[EssentialCategory, List[Essential]]
  // Хотим собрать список наборов
  def generateEssentials (hull: Hull) (catalog: Catalog): List[EssentialVariant] = {
    val init: List[EssentialVariant] = List(Map.empty)
    catalog.foldLeft(init) { case (foobar, (_, essentials: List[Essential])) =>
      dekartSpecific(foobar, essentials)
    }
  }
}

//  -- Проблема артиллерии!
//    -- Артиллерия, с одной стороны, является дополнительным компонентом.
//    -- С другой стороны, она занимает артиллерийский слот
//    -- Отдельные экземпляры артиллерии имеют ограничения по слотам/размерам кораблей
//  -- Думаю, что нужно считать артиллерию не как доп. компонент, а как отдельный тип
//
//  -- мы хотим оценить размер графа состояний для одного корабля
//    -- для этого нам (к сожалению) понадобится каталог компонент
//
//  --generate = generateSupp . generateArt . generateEssentials
//
//  type EssentialVariant = Map EssentialCategory Essential
//  generateEssentials :: Hull -> Map EssentialCategory [Essential] -> [EssentialVariant]
//  generateEssentials hull comps =
//    let allCats = [minBound..maxBound] :: [EssentialCategory]
//  allCats2 = keys comps :: [EssentialCategory]
//  in List.foldl' (\listMap key -> dekartSpecific key (comps ! key) listMap ) [Map.empty] allCats2
//    -- Imperative code
//  -- for each 'key' in 'comps' do
//    --     let oneCat = 'comps' filter by 'key'
//  --     dekartSpecific 'key' 'result' 'oneCat'
//
//  dekart :: [a] -> [b] -> [(a,b)]
//    dekart (x:xs) y = ((x,) <$> y) <> dekart xs y
//    dekart [] _ = []
//
//  dekartGeneric1 :: (Applicative fa) => fa a -> fa b -> fa (a,b)
//  dekartGeneric1 xs ys = (,) <$> xs <*> ys
//
//  dekartSpecific :: EssentialCategory -> [Essential] -> [EssentialVariant] -> [EssentialVariant]
//    dekartSpecific category variants  =  (<*>) (Map.insert category <$> variants)
