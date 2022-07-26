import Common.*

import scala.util.parsing.input.Positional

object Surface:
  enum Kind extends Positional:
    case KType
    case KFun(left: Kind, right: Kind)

  enum Ty extends Positional:
    case TVar(name: Name)
    case TFun(left: Ty, right: Ty)
    case TApp(left: Ty, right: Ty)
    case TForall(name: Name, kind: Option[Kind], body: Ty)

  enum Tm extends Positional:
    case Var(name: Name)
    case Let(name: Name, ty: Option[Ty], value: Tm, body: Tm)
    case App(fn: Tm, arg: Tm)
    case Lam(name: Name, ty: Option[Ty], body: Tm)
    case AppTy(fn: Tm, arg: Ty)
    case LamTy(name: Name, kind: Option[Kind], body: Tm)
