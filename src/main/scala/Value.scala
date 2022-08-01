import Common.*
import Core.*

object Value:
  type Env = List[VTy]

  final case class Clos(env: Env, ty: Ty)

  enum Head:
    case HVar(lvl: Lvl)
    case HMeta(id: MetaId)

  type Spine = List[VTy]

  enum VTy:
    case VNe(head: Head, spine: Spine)
    case VGlobal(
        name: Name,
        arity: Int,
        spine: Spine,
        value: Either[List[VTy] => VTy, () => VTy]
    )
    case VFun(left: VTy, right: VTy)
    case VForall(name: Name, kind: Kind, body: Clos)

  object VVar:
    import VTy.VNe
    import Head.HVar
    def apply(lvl: Lvl) = VNe(HVar(lvl), Nil)
    def unapply(value: VTy): Option[Lvl] = value match
      case VNe(HVar(head), Nil) => Some(head)
      case _                    => None

  object VMeta:
    import VTy.VNe
    import Head.HMeta
    def apply(id: MetaId) = VNe(HMeta(id), Nil)
    def unapply(value: VTy): Option[MetaId] = value match
      case VNe(HMeta(head), Nil) => Some(head)
      case _                     => None
