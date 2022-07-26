import Common.*
import Core.*
import Core.Ty.*
import Value.*
import Value.Head.*
import Value.VTy.*
import Errors.*

object Evaluation:
  def vinst(cl: Clos, v: VTy): VTy = eval(v :: cl.env, cl.ty)

  def vapp(fn: VTy, arg: VTy): VTy = fn match
    case VNe(hd, spine) => VNe(hd, arg :: spine)
    case _              => throw Impossible

  def eval(env: Env, ty: Ty): VTy = ty match
    case TVar(ix)             => env(ix)
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

  def quote(lvl: Lvl, ty: VTy, forceGlobals: Boolean = false): Ty =
    ty match
      case VNe(head, sp) => quoteSp(lvl, quoteHead(lvl, head), sp, forceGlobals)
      case VFun(l, r) =>
        TFun(quote(lvl, l, forceGlobals), quote(lvl, l, forceGlobals))
      case VForall(x, ki, b) =>
        TForall(x, ki, quote(lvl + 1, vinst(b, VVar(lvl)), forceGlobals))
