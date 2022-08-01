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
import Globals.*

object Evaluation:
  def vinst(cl: Clos, v: VTy): VTy = eval(v :: cl.env, cl.ty)

  def force(v: VTy, forceGlobals: Boolean = true): VTy = v match
    case VGlobal(_, _, _, Right(v)) if forceGlobals => force(v(), true)
    case ne @ VNe(HMeta(id), sp) =>
      getTMeta(id) match
        case Unsolved(_, _) => ne
        case Solved(sol, _) => force(sp.foldRight(sol)(vapp), forceGlobals)
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
    case VGlobal(x, arity, spine, Right(v)) =>
      VGlobal(x, arity, arg :: spine, Right(() => vapp(v(), arg)))
    case VGlobal(x, arity, spine, Left(v)) if spine.size + 1 == arity =>
      VGlobal(x, arity, arg :: spine, Right(() => v(arg :: spine)))
    case VGlobal(x, arity, spine, v) =>
      VGlobal(x, arity, arg :: spine, v)
    case _ => throw Impossible

  def vmeta(id: MetaId): VTy = getTMeta(id) match
    case Unsolved(_, _) => VMeta(id)
    case Solved(v, _)   => v

  def eval(env: Env, ty: Ty): VTy = ty match
    case TVar(ix) => env(ix)
    case TGlobal(x) =>
      val e = getTGlobal(x).getOrElse(throw Impossible)
      val v =
        if e.params.size == 0 then Right(() => e.vvalue(Nil))
        else Left(e.vvalue)
      VGlobal(x, e.params.size, Nil, v)
    case TMeta(id)            => vmeta(id)
    case TApp(l, r)           => vapp(eval(env, l), eval(env, r))
    case TFun(l, r)           => VFun(eval(env, l), eval(env, r))
    case TForall(x, ki, body) => VForall(x, ki, Clos(env, body))

  private def quoteSp(lvl: Lvl, ty: Ty, sp: Spine, forceGlobals: Boolean): Ty =
    sp match
      case Nil => ty
      case arg :: sp =>
        TApp(
          quoteSp(lvl, ty, sp, forceGlobals),
          quote(lvl, arg, forceGlobals)
        )

  private def quoteHead(lvl: Lvl, head: Head): Ty = head match
    case HVar(head) => TVar(lvl2ix(lvl, head))
    case HMeta(id)  => TMeta(id)

  def quote(lvl: Lvl, ty: VTy, forceGlobals: Boolean = false): Ty =
    force(ty, forceGlobals) match
      case VNe(head, sp) => quoteSp(lvl, quoteHead(lvl, head), sp, forceGlobals)
      case VGlobal(x, _, sp, _) => quoteSp(lvl, TGlobal(x), sp, forceGlobals)
      case VFun(l, r) =>
        TFun(quote(lvl, l, forceGlobals), quote(lvl, r, forceGlobals))
      case VForall(x, ki, b) =>
        TForall(
          x,
          forceKind(ki),
          quote(lvl + 1, vinst(b, VVar(lvl)), forceGlobals)
        )
