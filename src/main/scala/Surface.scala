import Common.*

import scala.util.parsing.input.Positional

object Surface:
  enum Kind extends Positional:
    case KType
    case KMeta(id: MetaId)
    case KFun(left: Kind, right: Kind)

    override def toString: String = this match
      case KType     => "Type"
      case KMeta(id) => s"?$id"
      case k @ KFun(_, _) =>
        k.flattenKFun
          .map {
            case k @ KFun(_, _) => s"($k)"
            case k              => k.toString
          }
          .mkString(" -> ")

    protected def flattenKFun: List[Kind] = this match
      case KFun(l, r) => List(l) ++ r.flattenKFun
      case k          => List(k)

  enum Ty extends Positional:
    case TVar(name: Name)
    case TFun(left: Ty, right: Ty)
    case TApp(left: Ty, right: Ty)
    case TForall(name: Name, kind: Option[Kind], body: Ty)

    override def toString: String = this match
      case TVar(x)         => s"$x"
      case ty @ TFun(_, _) => ty.flattenTFun.map(_.showS()).mkString(" -> ")
      case ty @ TApp(_, _) => ty.flattenTApp.map(_.showS(false)).mkString(" ")
      case ty @ TForall(_, _, _) =>
        val (xs, b) = ty.flattenTForall
        s"forall ${xs.map(showTForallParam).mkString(" ")}. $b"

    private def showTForallParam(p: (Name, Option[Kind])): String = p match
      case (x, None)    => s"$x"
      case (x, Some(k)) => s"($x : $k)"

    protected def isSimple(appIsSimple: Boolean = true): Boolean = this match
      case TVar(_)    => true
      case TApp(_, _) => appIsSimple
      case _          => false

    def showS(appIsSimple: Boolean = true): String =
      if this.isSimple(appIsSimple) then toString() else s"(${toString()})"

    protected def flattenTFun: List[Ty] = this match
      case TFun(l, r) => List(l) ++ r.flattenTFun
      case ty         => List(ty)

    protected def flattenTApp: List[Ty] = this match
      case TApp(l, r) => l.flattenTApp ++ List(r)
      case ty         => List(ty)

    protected def flattenTForall: (List[(Name, Option[Kind])], Ty) = this match
      case TForall(x, k, b) =>
        val (xs, ty) = b.flattenTForall
        ((x, k) :: xs, ty)
      case ty => (Nil, ty)

  enum Tm extends Positional:
    case Var(name: Name)
    case Let(name: Name, ty: Option[Ty], value: Tm, body: Tm)
    case App(fn: Tm, arg: Tm)
    case Lam(name: Name, ty: Option[Ty], body: Tm)
    case AppTy(fn: Tm, arg: Ty)
    case LamTy(name: Name, kind: Option[Kind], body: Tm)

    override def toString: String = this match
      case Var(x)                => s"$x"
      case Let(x, Some(t), v, b) => s"let $x : $t = $v; $b"
      case Let(x, None, v, b)    => s"let $x = $v; $b"
      case tm @ App(_, _) =>
        val (t, as) = tm.flattenApp
        s"${t.showS()} ${as.map(showArg).mkString(" ")}"
      case tm @ AppTy(_, _) =>
        val (t, as) = tm.flattenApp
        s"${t.showS()} ${as.map(showArg).mkString(" ")}"
      case tm @ Lam(_, _, _) =>
        val (xs, b) = tm.flattenLam
        s"\\${xs.map(showParam).mkString(" ")}. $b"
      case tm @ LamTy(_, _, _) =>
        val (xs, b) = tm.flattenLam
        s"\\${xs.map(showParam).mkString(" ")}. $b"

    private def showArg(arg: Either[Ty, Tm]) = arg match
      case Left(ty)  => s"@${ty.showS(false)}"
      case Right(tm) => tm.showS(false)

    private def showParam(p: (Name, Either[Option[Kind], Option[Ty]])) = p match
      case (x, Left(None))     => s"@$x"
      case (x, Right(None))    => s"$x"
      case (x, Left(Some(k)))  => s"@($x : $k)"
      case (x, Right(Some(t))) => s"($x : $t)"

    protected def isSimple(appIsSimple: Boolean = true): Boolean = this match
      case Var(_)      => true
      case App(_, _)   => appIsSimple
      case AppTy(_, _) => appIsSimple
      case _           => false

    def showS(appIsSimple: Boolean = true): String =
      if this.isSimple(appIsSimple) then toString() else s"(${toString()})"

    protected def flattenApp: (Tm, List[Either[Ty, Tm]]) = this match
      case App(l, r) =>
        val (t, as) = l.flattenApp
        (t, as ++ List(Right(r)))
      case AppTy(l, r) =>
        val (t, as) = l.flattenApp
        (t, as ++ List(Left(r)))
      case tm => (tm, Nil)

    protected def flattenLam
        : (List[(Name, Either[Option[Kind], Option[Ty]])], Tm) =
      this match
        case Lam(x, t, b) =>
          val (xs, b2) = b.flattenLam
          ((x, Right(t)) :: xs, b2)
        case LamTy(x, k, b) =>
          val (xs, b2) = b.flattenLam
          ((x, Left(k)) :: xs, b2)
        case tm => (Nil, tm)
