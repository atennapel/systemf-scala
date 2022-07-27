import Common.*

object Core:
  enum Kind:
    case KType
    case KMeta(id: MetaId)
    case KFun(left: Kind, right: Kind)

    override def toString: String = this match
      case KType      => "Type"
      case KMeta(id)  => s"?$id"
      case KFun(l, r) => s"($l -> $r)"

  enum Ty:
    case TVar(ix: Ix)
    case TMeta(id: MetaId)
    case TFun(left: Ty, right: Ty)
    case TApp(left: Ty, right: Ty)
    case TForall(name: Name, kind: Kind, body: Ty)

    override def toString: String = this match
      case TVar(ix)         => s"'$ix"
      case TMeta(id)        => s"?$id"
      case TFun(l, r)       => s"($l -> $r)"
      case TApp(l, r)       => s"($l $r)"
      case TForall(x, k, b) => s"(forall ($x : $k). $b)"

  enum Tm:
    case Var(ix: Ix)
    case Let(name: Name, ty: Ty, value: Tm, body: Tm)
    case App(fn: Tm, arg: Tm)
    case Lam(name: Name, ty: Ty, body: Tm)
    case AppTy(fn: Tm, arg: Ty)
    case LamTy(name: Name, kind: Kind, body: Tm)

    override def toString: String = this match
      case Var(ix)         => s"'$ix"
      case Let(x, t, v, b) => s"(let $x : $t = $v; $b)"
      case App(l, r)       => s"($l $r)"
      case Lam(x, t, b)    => s"(\\($x : $t). $b)"
      case AppTy(l, r)     => s"($l @$r)"
      case LamTy(x, k, b)  => s"(\\(@$x : $k). $b)"
