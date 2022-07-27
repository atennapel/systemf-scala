import Common.*
import Core.*
import Core.Ty.*
import Core.Kind.*
import Value.*
import Value.Head.*
import Value.VTy.*
import Errors.*
import Metas.*
import Metas.TMetaEntry.*
import Metas.KMetaEntry.*

object Evaluation:
  def vinst(cl: Clos, v: VTy): VTy = eval(v :: cl.env, cl.ty)

  def force(v: VTy): VTy = v match
    case ne @ VNe(HMeta(id), sp) =>
      getTMeta(id) match
        case Unsolved(_, _) => ne
        case Solved(sol, _) => force(sp.foldRight(sol)(vapp))
    case v => v

  def forceKind(k: Kind): Kind = k match
    case KMeta(id) =>
      getKMeta(id) match
        case KUnsolved   => k
        case KSolved(k2) => forceKind(k2)
    case KType      => KType
    case KFun(l, r) => KFun(forceKind(l), forceKind(r))

  def vapp(fn: VTy, arg: VTy): VTy = fn match
    case VNe(hd, spine) => VNe(hd, arg :: spine)
    case _              => throw Impossible

  def vmeta(id: MetaId): VTy = getTMeta(id) match
    case Unsolved(_, _) => VMeta(id)
    case Solved(v, _)   => v

  def eval(env: Env, ty: Ty): VTy = ty match
    case TVar(ix)             => env(ix)
    case TMeta(id)            => vmeta(id)
    case TApp(l, r)           => vapp(eval(env, l), eval(env, r))
    case TFun(l, r)           => VFun(eval(env, l), eval(env, r))
    case TForall(x, ki, body) => VForall(x, ki, Clos(env, body))

  private def quoteSp(lvl: Lvl, ty: Ty, sp: Spine): Ty =
    sp match
      case Nil => ty
      case arg :: sp =>
        TApp(
          quoteSp(lvl, ty, sp),
          quote(lvl, arg)
        )

  private def quoteHead(lvl: Lvl, head: Head): Ty = head match
    case HVar(head) => TVar(lvl2ix(lvl, head))
    case HMeta(id)  => TMeta(id)

  def quote(lvl: Lvl, ty: VTy): Ty =
    force(ty) match
      case VNe(head, sp) => quoteSp(lvl, quoteHead(lvl, head), sp)
      case VFun(l, r) =>
        TFun(quote(lvl, l), quote(lvl, r))
      case VForall(x, ki, b) =>
        TForall(x, forceKind(ki), quote(lvl + 1, vinst(b, VVar(lvl))))
