import Common.*
import Core.Tm
import Core.Ty
import Core.Kind
import Value.VTy

import scala.collection.immutable.ListMap

object Globals:
  private var globals: ListMap[Name, GlobalEntry] = ListMap.empty
  private var tglobals: ListMap[Name, TGlobalEntry] = ListMap.empty

  final case class GlobalEntry(
      val name: Name,
      val ty: Ty,
      val vty: VTy,
      val value: Tm
  )

  def addGlobal(g: GlobalEntry): Unit = globals = globals + (g.name -> g)
  def getGlobal(x: Name): Option[GlobalEntry] = globals.get(x)

  def getGlobals(): List[(Name, GlobalEntry)] = globals.toList

  final case class TGlobalEntry(
      val name: Name,
      val params: List[(Name, Kind)],
      val kind: Kind,
      val fullkind: Kind,
      val vvalue: List[VTy] => VTy,
      val value: Ty
  )

  def addTGlobal(g: TGlobalEntry): Unit = tglobals = tglobals + (g.name -> g)
  def getTGlobal(x: Name): Option[TGlobalEntry] = tglobals.get(x)

  def getTGlobals(): List[(Name, TGlobalEntry)] = tglobals.toList
