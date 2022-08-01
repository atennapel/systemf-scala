import Common.*
import Core.*
import Core.Tm.*
import Core.Ty.*
import Core.Kind.*
import Metas.*
import Metas.TMetaEntry.*
import Metas.KMetaEntry.*

object Zonking:
  def zonk(k: Kind): Kind = k match
    case KType => KType
    case KMeta(id) =>
      getKMeta(id) match
        case KUnsolved   => k
        case KSolved(k2) => zonk(k2)
    case KFun(l, r) => KFun(zonk(l), zonk(r))

  def zonk(t: Ty): Ty = t match
    case TVar(_)    => t
    case TGlobal(_) => t
    case TMeta(id) =>
      getTMeta(id) match
        case Unsolved(_, _) => t
        case Solved(_, c)   => zonk(c)
    case TFun(left, right)      => TFun(zonk(left), zonk(right))
    case TApp(left, right)      => TApp(zonk(left), zonk(right))
    case TForall(x, kind, body) => TForall(x, zonk(kind), zonk(body))

  def zonk(t: Tm): Tm = t match
    case Var(_)                  => t
    case Global(_)               => t
    case Let(x, ty, value, body) => Let(x, zonk(ty), zonk(value), zonk(body))
    case App(fn, arg)            => App(zonk(fn), zonk(arg))
    case Lam(x, ty, body)        => Lam(x, zonk(ty), zonk(body))
    case AppTy(fn, arg)          => AppTy(zonk(fn), zonk(arg))
    case LamTy(x, kind, body)    => LamTy(x, zonk(kind), zonk(body))
