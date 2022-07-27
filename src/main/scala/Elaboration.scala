import Common.*
import Surface.Tm as S
import Surface.Tm as STm
import Surface.Ty as STy
import Surface.Kind as SKind
import Core.*
import Core.Tm.*
import Core.Ty.*
import Core.Kind.*
import Ctx.*
import Value.*
import Value.VTy.*
import Evaluation.*
import Unification.*
import Errors.*
import Metas.*
import Metas.TMetaEntry.*
import Zonking.*
import Debug.debug

import scala.util.parsing.input.{Position, NoPosition}
import scala.annotation.tailrec

object Elaboration:
  private def newMeta(ctx: Ctx, kind: Kind): Ty = TMeta(
    freshTMeta(ctx.lvl, kind)
  )
  private def newKMeta(): Kind = KMeta(freshKMeta())

  private def insertForall(ctx: Ctx, inp: (Tm, VTy)): (Tm, VTy) =
    val (tm, ty) = inp
    force(ty) match
      case VForall(x, a, b) =>
        val m = newMeta(ctx, a)
        val mv = ctx.eval(m)
        insertForall(ctx, (AppTy(tm, m), vinst(b, mv)))
      case _ => (tm, ty)

  private def insert(ctx: Ctx, inp: (Tm, VTy)): (Tm, VTy) =
    val (tm, ty) = inp
    tm match
      case LamTy(_, _, _) => (tm, ty)
      case _              => insertForall(ctx, (tm, ty))

  private def unifyKindCatch(k1: Kind, k2: Kind): Unit =
    try unifyKind(k1, k2)
    catch case e: UnifyError => throw KindMismatchError(s"$k1 ~ $k2: $e")

  private def checkKind(ki: SKind): Kind =
    ki match
      case SKind.KType      => KType
      case SKind.KFun(l, r) => KFun(checkKind(l), checkKind(r))

  private def inferType(ctx0: Ctx, ty: STy): (Ty, Kind) =
    val ctx = ctx0.enter(ty.pos)
    debug(s"inferType: $ty")
    ty match
      case STy.TVar(x) =>
        ctx.lookupTy(x) match
          case Some((i, ki)) => (TVar(i), ki)
          case None => throw VarError(s"tvar $x\n${ctx.pos.longString}")
      case STy.TFun(l, r) =>
        val el = checkType(ctx, l, KType)
        val er = checkType(ctx, r, KType)
        (TFun(el, er), KType)
      case STy.TApp(fn, arg) =>
        val (efn, kfn) = inferType(ctx, fn)
        kfn match
          case KFun(pki, rki) =>
            val earg = checkType(ctx, arg, pki)
            (TApp(efn, earg), rki)
          case _ => throw NotAKFunError(ty.toString)
      case STy.TForall(x, oki, body) =>
        val eki = oki.map(checkKind(_)).getOrElse(newKMeta())
        val ebody = checkType(ctx.bindTy(x, eki), body, KType)
        (TForall(x, eki, ebody), KType)

  private def checkType(ctx: Ctx, ty: STy, ki: Kind): Ty =
    debug(s"checkType: $ty : $ki")
    val (ety, ki2) = inferType(ctx, ty)
    unifyKindCatch(ki2, ki)
    ety

  private def checkOptionalType(
      ctx: Ctx,
      oty: Option[STy],
      value: STm
  ): (Tm, Ty, VTy) =
    oty match
      case None =>
        val (evalue, ty) = infer(ctx, value)
        (evalue, ctx.quote(ty), ty)
      case Some(ty) =>
        val ety = checkType(ctx, ty, KType)
        val vty = ctx.eval(ety)
        val evalue = check(ctx, value, vty)
        (evalue, ety, vty)

  private def coe(ctx: Ctx, tm: Tm, ty1: VTy, ty2: VTy): Tm =
    debug(s"coe: $tm : ${ctx.quote(ty1)} ~> ${ctx.quote(ty2)}")
    unify(ctx.lvl, ty1, ty2)
    tm

  private def check(ctx0: Ctx, tm: STm, ty: VTy): Tm =
    val ctx = ctx0.enter(tm.pos)
    debug(s"check: $tm : ${ctx.quote(ty)}")
    (tm, force(ty)) match
      case (S.Lam(x, oty, body), VFun(pty, rty)) =>
        oty.foreach { ty =>
          val ety = checkType(ctx, ty, KType)
          val vty = ctx.eval(ety)
          unify(ctx.lvl, vty, pty)
        }
        val ebody = check(ctx.bind(x, pty), body, rty)
        Lam(x, ctx.quote(pty), ebody)
      case (S.LamTy(x, oki, body), VForall(_, ki, rty)) =>
        oki.foreach { ki2 => unifyKindCatch(checkKind(ki2), ki) }
        val ebody = check(ctx.bindTy(x, ki), body, vinst(rty, VVar(ctx.lvl)))
        LamTy(x, ki, ebody)
      case (tm, VForall(x, ki, rty)) =>
        LamTy(
          x,
          ki,
          check(ctx.bindTy(x, ki, true), tm, vinst(rty, VVar(ctx.lvl)))
        )
      case (S.Let(x, oty, value, body), _) =>
        val (evalue, ety, vty) = checkOptionalType(ctx, oty, value)
        val ebody = check(ctx.bind(x, vty), body, ty)
        Let(x, ety, evalue, ebody)
      case (tm, _) =>
        val (etm, tyActual) = insert(ctx, infer(ctx, tm))
        coe(ctx, etm, tyActual, ty)

  private def infer(ctx0: Ctx, tm: STm): (Tm, VTy) =
    debug(s"infer: $tm")
    val ctx = ctx0.enter(tm.pos)
    tm match
      case S.Var(name) =>
        ctx.lookup(name) match
          case Some((ix, ty)) => (Var(ix), ty)
          case None           => throw VarError(s"$name\n${ctx.pos.longString}")
      case S.Let(x, oty, value, body) =>
        val (evalue, ety, vty) = checkOptionalType(ctx, oty, value)
        val (ebody, rty) = infer(ctx.bind(x, vty), body)
        (Let(x, ety, evalue, ebody), rty)
      case S.App(fn, arg) =>
        val (efn, fnty) = insertForall(ctx, infer(ctx, fn))
        force(fnty) match
          case VFun(pty, rty) =>
            val earg = check(ctx, arg, pty)
            (App(efn, earg), rty)
          case _ => throw NotAFunError(tm.toString)
      case S.Lam(x, oty, body) =>
        val (ety, vty) = oty match
          case Some(ty) =>
            val ety = checkType(ctx, ty, KType)
            val vty = ctx.eval(ety)
            (ety, vty)
          case None =>
            val ety = newMeta(ctx, KType)
            val vty = ctx.eval(ety)
            (ety, vty)
        val (ebody, rty) = insert(ctx, infer(ctx.bind(x, vty), body))
        (Lam(x, ety, ebody), VFun(vty, rty))
      case S.AppTy(fn, arg) =>
        val (efn, fnty) = infer(ctx, fn)
        force(fnty) match
          case VForall(x, ki, body) =>
            val earg = checkType(ctx, arg, ki)
            (AppTy(efn, earg), vinst(body, ctx.eval(earg)))
          case _ => throw NotAForallError(tm.toString)
      case S.LamTy(x, oski, body) =>
        val ki = oski.map(checkKind(_)).getOrElse(newKMeta())
        val (ebody, rty) = infer(ctx.bindTy(x, ki), body)
        (LamTy(x, ki, ebody), VForall(x, ki, ctx.closeVal(rty)))

  private def unsolvedMetasInType(
      ty: Ty,
      k: Lvl = 0,
      ms: List[(MetaId, Lvl)] = Nil
  ): List[(MetaId, Lvl)] =
    ty match
      case TVar(_) => ms
      case TFun(l, r) =>
        unsolvedMetasInType(r, k, unsolvedMetasInType(l, k, ms))
      case TApp(l, r) =>
        unsolvedMetasInType(r, k, unsolvedMetasInType(l, k, ms))
      case TForall(_, _, b) => unsolvedMetasInType(b, k + 1, ms)
      case TMeta(id) if !ms.exists { case (id2, _) => id == id2 } =>
        ms ++ List((id, k))
      case TMeta(_) => ms

  private def generalizeMetas(
      ms: List[(MetaId, Lvl)],
      total: Int,
      ix: Int = 0
  ): List[Kind] =
    ms match
      case Nil => Nil
      case (id, k) :: ms =>
        val ki = getTMetaUnsolved(id).kind
        solveTMeta(id, VVar(ix), TVar(k + (total - ix - 1)))
        ki :: generalizeMetas(ms, total, ix + 1)

  private def generalizeTy(ks: List[Kind], ty: Ty, i: Int = 0): Ty = ks match
    case Nil     => ty
    case k :: ks => TForall(s"t$i", k, generalizeTy(ks, ty, i + 1))

  private def generalizeTm(ks: List[Kind], tm: Tm, i: Int = 0): Tm = ks match
    case Nil     => tm
    case k :: ks => LamTy(s"t$i", k, generalizeTm(ks, tm, i + 1))

  private def generalize(ctx: Ctx, inp: (Tm, VTy)): (Tm, Ty) =
    val (etm, vty) = inp
    val qty = zonkTy(ctx.quote(vty))
    val ms = unsolvedMetasInType(qty)
    val ks = generalizeMetas(ms, ms.size)
    val zty = zonkTy(generalizeTy(ks, qty))
    val ztm = zonk(generalizeTm(ks, etm))
    (ztm, zty)

  def elaborate(tm: STm, pos: Position = NoPosition): (Tm, Ty) =
    resetMetas()
    val ctx = Ctx.empty(pos)
    val (ztm, zty) = generalize(ctx, infer(ctx, tm))
    debug(s"elaboration done: $ztm : $zty")
    val utms = unsolvedTMetas()
    val ukms = unsolvedKMetas()
    if utms.nonEmpty || ukms.nonEmpty then
      val t =
        if utms.nonEmpty then
          s"in types: ${utms.map(i => s"?$i").mkString(", ")}"
        else ""
      val k =
        if ukms.nonEmpty then
          s"in kinds: ${ukms.map(i => s"?$i").mkString(", ")}"
        else ""
      throw UnsolvedMetasError(
        s"$t${if t.nonEmpty && k.nonEmpty then "; " else ""}$k\n$ztm : $zty"
      )
    (ztm, zty)
