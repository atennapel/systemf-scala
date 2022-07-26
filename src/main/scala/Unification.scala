import Common.*
import Value.*
import Value.VTy.*
import Debug.*
import Evaluation.*
import Errors.*
import Core.*

object Unification:
  def unifyKind(a: Kind, b: Kind): Unit =
    if a == b then () else throw UnifyError("failed to unify kinds")

  private def unifySp(l: Lvl, sp1: Spine, sp2: Spine): Unit = (sp1, sp2) match
    case (Nil, Nil) => ()
    case (a1 :: sp1, a2 :: sp2) =>
      unifySp(l, sp1, sp2)
      unify(l, a1, a2)
    case _ => throw UnifyError("spine mismatch")

  def unify(l: Lvl, t: VTy, u: VTy): Unit =
    debug(s"unify: ${quote(l, t)} ~ ${quote(l, u)}")
    (t, u) match
      case (VForall(_, k1, b1), VForall(_, k2, b2)) if k1 == k2 =>
        val v = VVar(l)
        unify(l + 1, vinst(b1, v), vinst(b2, v))
      case (VFun(l1, r1), VFun(l2, r2)) =>
        unify(l, l1, l2)
        unify(l, r1, r2)
      case (VNe(h1, sp1), VNe(h2, sp2)) if h1 == h2 => unifySp(l, sp1, sp2)
      case (v1, v2) => throw UnifyError("failed to unify")
