import Common.*
import Value.*
import Value.VTy.*
import Value.Head.*
import Debug.*
import Evaluation.*
import Errors.*
import Metas.*
import Core.*
import Core.Ty.*

object Unification:
  def unifyKind(a: Kind, b: Kind): Unit =
    if a == b then () else throw UnifyError("failed to unify kinds")

  private def checkSolution(l: Lvl, ml: Lvl, id: MetaId, ty: Ty): Unit =
    ty match
      case TVar(ix) if l - ix >= ml =>
        throw UnifyError(s"out of scope variable: ?$id")
      case TVar(_) => ()
      case TMeta(id2) if id == id2 =>
        throw UnifyError(s"occurs check failed: ?$id")
      case TMeta(_) => ()
      case TFun(left, right) =>
        checkSolution(l, ml, id, left)
        checkSolution(l, ml, id, right)
      case TApp(left, right) =>
        checkSolution(l, ml, id, left)
        checkSolution(l, ml, id, right)
      case TForall(name, kind, body) =>
        checkSolution(l + 1, ml, id, body)

  private def solve(l: Lvl, id: MetaId, v: VTy): Unit =
    val ml = getTMetaUnsolved(id).lvl
    val ty = quote(l, v)
    checkSolution(l, ml, id, ty)
    solveTMeta(id, v, quote(l, v))

  private def unifySp(l: Lvl, sp1: Spine, sp2: Spine): Unit = (sp1, sp2) match
    case (Nil, Nil) => ()
    case (a1 :: sp1, a2 :: sp2) =>
      unifySp(l, sp1, sp2)
      unify(l, a1, a2)
    case _ => throw UnifyError("spine mismatch")

  def unify(l: Lvl, t: VTy, u: VTy): Unit =
    debug(s"unify: ${quote(l, t)} ~ ${quote(l, u)}")
    (force(t), force(u)) match
      case (VForall(_, k1, b1), VForall(_, k2, b2)) if k1 == k2 =>
        val v = VVar(l)
        unify(l + 1, vinst(b1, v), vinst(b2, v))
      case (VFun(l1, r1), VFun(l2, r2)) =>
        unify(l, l1, l2)
        unify(l, r1, r2)
      case (VNe(h1, sp1), VNe(h2, sp2)) if h1 == h2 => unifySp(l, sp1, sp2)
      case (VNe(HMeta(id), Nil), v)                 => solve(l, id, v)
      case (v, VNe(HMeta(id), Nil))                 => solve(l, id, v)
      case (v1, v2) => throw UnifyError("failed to unify")
