import Common.*

import scala.util.parsing.input.Positional

object Surface:
  enum Kind extends Positional:
    case KType
    case KFun(left: Kind, right: Kind)

    override def toString: String = this match
      case KType      => "Type"
      case KFun(l, r) => s"($l -> $r)"

  enum Ty extends Positional:
    case TVar(name: Name)
    case TFun(left: Ty, right: Ty)
    case TApp(left: Ty, right: Ty)
    case TForall(name: Name, kind: Option[Kind], body: Ty)

    override def toString: String = this match
      case TVar(ix)         => s"'$ix"
      case TFun(l, r)       => s"($l -> $r)"
      case TApp(l, r)       => s"($l $r)"
      case TForall(x, k, b) => s"(forall ($x : $k). $b)"

  enum Tm extends Positional:
    case Var(name: Name)
    case Let(name: Name, ty: Option[Ty], value: Tm, body: Tm)
    case App(fn: Tm, arg: Tm)
    case Lam(name: Name, ty: Option[Ty], body: Tm)
    case AppTy(fn: Tm, arg: Ty)
    case LamTy(name: Name, kind: Option[Kind], body: Tm)

    override def toString: String = this match
      case Var(x)                => s"$x"
      case Let(x, Some(t), v, b) => s"(let $x : $t = $v; $b)"
      case Let(x, None, v, b)    => s"(let $x = $v; $b)"
      case App(l, r)             => s"($l $r)"
      case Lam(x, Some(t), b)    => s"(\\($x : $t). $b)"
      case Lam(x, None, b)       => s"(\\$x. $b)"
      case AppTy(l, r)           => s"($l @$r)"
      case LamTy(x, Some(k), b)  => s"(\\(@$x : $k). $b)"
      case LamTy(x, None, b)     => s"(\\@$x. $b)"
