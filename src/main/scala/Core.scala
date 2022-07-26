import Common.*

object Core:
  enum Kind:
    case KType
    case KFun(left: Kind, right: Kind)

  enum Ty:
    case TVar(ix: Ix)
    case TFun(left: Ty, right: Ty)
    case TApp(left: Ty, right: Ty)
    case TForall(name: Name, kind: Kind, body: Ty)

  enum Tm:
    case Var(ix: Ix)
    case Let(name: Name, ty: Ty, value: Tm, body: Tm)
    case App(fn: Tm, arg: Tm)
    case Lam(name: Name, ty: Ty, body: Tm)
    case AppTy(fn: Tm, arg: Ty)
    case LamTy(name: Name, kind: Kind, body: Tm)
