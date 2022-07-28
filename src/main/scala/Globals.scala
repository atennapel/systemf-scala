import Common.*
import Core.Tm
import Core.Ty
import Value.VTy

import scala.collection.immutable.ListMap

object Globals:
  private var globals: ListMap[Name, GlobalEntry] = ListMap.empty

  final case class GlobalEntry(
      val name: Name,
      val ty: Ty,
      val vty: VTy,
      val value: Tm
  )

  def addGlobal(g: GlobalEntry): Unit = globals = globals + (g.name -> g)
  def getGlobal(x: Name): Option[GlobalEntry] = globals.get(x)

  def getGlobals(): List[(Name, GlobalEntry)] = globals.toList
