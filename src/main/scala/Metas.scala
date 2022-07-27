import Value.*
import Common.*
import Core.*
import Errors.*
import scala.collection.mutable.ArrayBuffer

object Metas:
  private val tmetas: ArrayBuffer[TMetaEntry] = ArrayBuffer.empty
  private val kmetas: ArrayBuffer[KMetaEntry] = ArrayBuffer.empty

  def resetMetas(): Unit =
    tmetas.clear()
    kmetas.clear()

  enum TMetaEntry:
    case Unsolved(lvl: Lvl, kind: Kind)
    case Solved(vty: VTy, ty: Ty)
  import TMetaEntry.*

  def unsolvedTMetas(): List[MetaId] =
    tmetas.zipWithIndex.collect { case (Unsolved(_, _), i) => i }.toList

  def freshTMeta(lvl: Lvl, kind: Kind): MetaId =
    val id = tmetas.size
    tmetas.addOne(Unsolved(lvl, kind))
    id

  def getTMeta(id: MetaId): TMetaEntry = tmetas(id)
  def getTMetaUnsolved(id: MetaId): Unsolved = getTMeta(id) match
    case s @ Unsolved(_, _) => s
    case _                  => throw Impossible
  def getTMetaSolved(id: MetaId): Solved = getTMeta(id) match
    case s @ Solved(_, _) => s
    case _                => throw Impossible

  def solveTMeta(id: MetaId, vty: VTy, ty: Ty): Unit =
    tmetas(id) = Solved(vty, ty)

  enum KMetaEntry:
    case KUnsolved
    case KSolved(kind: Kind)
  import KMetaEntry.*

  def freshKMeta(): MetaId =
    val id = kmetas.size
    kmetas.addOne(KUnsolved)
    id

  def getKMeta(id: MetaId): KMetaEntry = kmetas(id)
  def getKMetaSolved(id: MetaId): KSolved = getKMeta(id) match
    case s @ KSolved(_) => s
    case _              => throw Impossible

  def solveKMeta(id: MetaId, kind: Kind): Unit =
    kmetas(id) = KSolved(kind)

  def unsolvedKMetas(): List[MetaId] =
    kmetas.zipWithIndex.collect { case (KUnsolved, i) => i }.toList
