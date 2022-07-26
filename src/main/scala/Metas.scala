import Value.*
import Common.*
import Core.*
import Errors.*
import scala.collection.mutable.ArrayBuffer

object Metas:
  private val tmetas: ArrayBuffer[TMetaEntry] = ArrayBuffer.empty

  enum TMetaEntry:
    case Unsolved(lvl: Lvl)
    case Solved(vty: VTy, ty: Ty)
  import TMetaEntry.*

  def freshTMeta(lvl: Lvl): MetaId =
    val id = tmetas.size
    tmetas.addOne(Unsolved(lvl))
    id

  def resetMetas(): Unit =
    tmetas.clear()

  def getTMeta(id: MetaId): TMetaEntry = tmetas(id)
  def getTMetaUnsolved(id: MetaId): Unsolved = getTMeta(id) match
    case s @ Unsolved(_) => s
    case _               => throw Impossible
  def getTMetaSolved(id: MetaId): Solved = getTMeta(id) match
    case s @ Solved(_, _) => s
    case _                => throw Impossible

  def solveTMeta(id: MetaId, vty: VTy, ty: Ty): Unit =
    tmetas(id) = Solved(vty, ty)
