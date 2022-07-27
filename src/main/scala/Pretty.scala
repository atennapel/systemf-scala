import Core.{Tm as CTm, Ty as CTy, Kind as CKind}
import Surface.*
import Surface.Tm.*
import Surface.Ty.*
import Surface.Kind.*
import Common.*

object Pretty:
  private def kindS(k: CKind): Kind = k match
    case CKind.KType      => KType
    case CKind.KMeta(id)  => KMeta(id)
    case CKind.KFun(l, r) => KFun(kindS(l), kindS(r))

  private def tyS(ty: CTy, ts: List[Name]): Ty = ty match
    case CTy.TVar(ix)   => TVar(ts(ix))
    case CTy.TMeta(id)  => TVar(s"?$id")
    case CTy.TFun(l, r) => TFun(tyS(l, ts), tyS(r, ts))
    case CTy.TApp(l, r) => TApp(tyS(l, ts), tyS(r, ts))
    case CTy.TForall(x0, k, b) =>
      val x = freshName(x0, ts)
      TForall(x, Some(kindS(k)), tyS(b, x :: ts))

  private def tmS(tm: CTm, ns: List[Name], ts: List[Name]): Tm = tm match
    case CTm.Var(ix)     => Var(ns(ix))
    case CTm.App(l, r)   => App(tmS(l, ns, ts), tmS(r, ns, ts))
    case CTm.AppTy(l, r) => AppTy(tmS(l, ns, ts), tyS(r, ts))
    case CTm.Let(x0, t, v, b) =>
      val x = freshName(x0, ts)
      Let(x, Some(tyS(t, ts)), tmS(v, ns, ts), tmS(b, x :: ns, ts))
    case CTm.Lam(x0, t, b) =>
      val x = freshName(x0, ts)
      Lam(x, Some(tyS(t, ts)), tmS(b, x :: ns, ts))
    case CTm.LamTy(x0, k, b) =>
      val x = freshName(x0, ts)
      LamTy(x, Some(kindS(k)), tmS(b, ns, x :: ts))

  def pretty(tm: CTm, ns: List[Name] = Nil, ts: List[Name] = Nil): String =
    tmS(tm, ns, ts).toString
  def prettyTy(tm: CTy, ts: List[Name] = Nil): String =
    tyS(tm, ts).toString
  def prettyKind(tm: CKind): String =
    kindS(tm).toString
