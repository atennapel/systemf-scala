import Common.*
import Core.*
import Core.Tm.*
import Core.Ty.*
import Metas.*
import Metas.TMetaEntry.*

object Zonking:
  def zonkKind(k: Kind): Kind = k

  def zonkTy(t: Ty): Ty = t match
    case TVar(ix) => TVar(ix)
    case TMeta(id) =>
      getTMeta(id) match
        case Unsolved(_)  => t
        case Solved(_, c) => zonkTy(c)
    case TFun(left, right)      => TFun(zonkTy(left), zonkTy(right))
    case TApp(left, right)      => TApp(zonkTy(left), zonkTy(right))
    case TForall(x, kind, body) => TForall(x, zonkKind(kind), zonkTy(body))

  def zonk(t: Tm): Tm = t match
    case Var(ix)                 => Var(ix)
    case Let(x, ty, value, body) => Let(x, zonkTy(ty), zonk(value), zonk(body))
    case App(fn, arg)            => App(zonk(fn), zonk(arg))
    case Lam(x, ty, body)        => Lam(x, zonkTy(ty), zonk(body))
    case AppTy(fn, arg)          => AppTy(zonk(fn), zonkTy(arg))
    case LamTy(x, kind, body)    => LamTy(x, zonkKind(kind), zonk(body))
