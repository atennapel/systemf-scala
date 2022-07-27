import Common.*
import Value.*
import Core.*
import Evaluation.{eval as veval, quote as vquote}
import Pretty.{pretty as prTm, prettyTy as prTy, prettyKind as prKind}

import scala.annotation.tailrec
import scala.util.parsing.input.{Position, NoPosition}

final case class Ctx(
    val env: Env,
    val lvl: Lvl,
    val types: List[(Name, VTy)],
    val kinds: List[(Name, Boolean, Kind)],
    val pos: Position
):
  def names: List[Name] = types.map(_._1)
  def namesTy: List[Name] = kinds.map(_._1)

  def enter(pos: Position): Ctx = copy(pos = pos)

  def bind(x: Name, ty: VTy): Ctx = copy(types = (x, ty) :: types)

  def bindTy(x: Name, ki: Kind, inserted: Boolean = false): Ctx =
    copy(
      env = VVar(lvl) :: env,
      lvl = lvl + 1,
      kinds = (x, inserted, ki) :: kinds
    )

  def clos(ty: Ty): Clos = Clos(env, ty)
  def closeVal(v: VTy): Clos = Clos(env, vquote(lvl + 1, v))

  def eval(ty: Ty): VTy = veval(env, ty)
  def quote(v: VTy): Ty = vquote(lvl, v)

  def pretty(tm: Tm): String = prTm(tm, names, namesTy)
  def pretty(ty: Ty): String = prTy(ty, namesTy)
  def pretty(ki: Kind): String = prKind(ki)
  def pretty(v: VTy): String = prTy(quote(v), namesTy)

  def lookup(name: Name): Option[(Ix, VTy)] =
    @tailrec
    def go(ts: List[(Name, VTy)], ix: Ix): Option[(Ix, VTy)] = ts match
      case Nil                       => None
      case (x, ty) :: _ if x == name => Some((ix, ty))
      case _ :: rest                 => go(rest, ix + 1)
    go(types, 0)

  def lookupTy(name: Name): Option[(Ix, Kind)] =
    @tailrec
    def go(ts: List[(Name, Boolean, Kind)], ix: Ix): Option[(Ix, Kind)] =
      ts match
        case Nil                              => None
        case (x, false, ki) :: _ if x == name => Some((ix, ki))
        case _ :: rest                        => go(rest, ix + 1)
    go(kinds, 0)

object Ctx:
  def empty(pos: Position = NoPosition): Ctx =
    Ctx(Nil, 0, Nil, Nil, pos)
