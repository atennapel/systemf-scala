import Surface.*
import Surface.Tm.*
import Surface.Ty.*
import Surface.Kind.*
import Surface.Decl.*
import Common.*

import parsley.Parsley, Parsley._
import scala.language.implicitConversions

object Parser:
  object LangLexer:
    import parsley.token.{LanguageDef, Lexer, Predicate, Parser}
    import parsley.character.{alphaNum, isWhitespace}
    import parsley.combinator.eof

    val lang = LanguageDef.plain.copy(
      commentLine = "--",
      commentStart = "{-",
      commentEnd = "-}",
      keywords = Set("Type", "let", "forall", "def", "type"),
      operators = Set("=", ":", ";", "\\", ".", "->", "_"),
      identStart = Predicate(_.isLetter),
      identLetter = Predicate(_.isLetterOrDigit),
      space = Predicate(isWhitespace)
    )
    val lexer = new Lexer(lang)

    def fully[A](p: => Parsley[A]): Parsley[A] = lexer.whiteSpace *> p <* eof

    val ident: Parsley[String] = lexer.identifier
    def keyword(s: String): Parsley[Unit] = lexer.keyword(s)
    def symbol(s: String): Parsley[Unit] = void(lexer.symbol_(s))

    object Implicits:
      given Conversion[String, Parsley[Unit]] with
        def apply(s: String): Parsley[Unit] =
          if lang.keywords(s) then lexer.keyword(s)
          else if lang.operators(s) then lexer.maxOp(s)
          else void(lexer.symbol_(s))

  object KindParser:
    import parsley.expr.{precedence, Ops, InfixR}
    import parsley.combinator.{many, option}

    import LangLexer.{fully, ident}
    import LangLexer.Implicits.given

    lazy val atom: Parsley[Kind] =
      "(" *> kind <* ")" <|> "Type" #> KType <|> "_" #> KHole

    lazy val kind: Parsley[Kind] =
      precedence[Kind](atom)(Ops(InfixR)("->" #> KFun.apply))

  object TyParser:
    import parsley.expr.{precedence, Ops, InfixR}
    import parsley.combinator.{many, option, some}

    import LangLexer.{fully, ident}
    import LangLexer.Implicits.given

    lazy val atom: Parsley[Ty] =
      "(" *> ty <* ")" <|> "_" #> THole <|> ident.map(TVar.apply)

    lazy val ty: Parsley[Ty] =
      forall <|> precedence[Ty](tapp)(Ops(InfixR)("->" #> TFun.apply))

    private lazy val forall: Parsley[Ty] =
      ("forall" *> many(param) <~> "." *> ty).map { case (xs, b) =>
        xs.foldRight(b) { case ((xs, ki), b) =>
          xs.foldRight(b)(TForall(_, ki, _))
        }
      }

    lazy val param: Parsley[(List[Name], Option[Kind])] =
      ("(" *> some(ident) <~> ":" *> KindParser.kind <* ")").map((xs, ki) =>
        (xs, Some(ki))
      )
        <|> ident.map(x => (List(x), None))

    private lazy val tapp: Parsley[Ty] =
      (atom <~> many(atom) <~> option(forall)).map { case ((fn, args), opt) =>
        (args ++ opt).foldLeft(fn)(TApp.apply)
      }

  object TmParser:
    import parsley.combinator.{many, some, option}

    import LangLexer.{fully, ident}
    import LangLexer.Implicits.given

    private lazy val atom: Parsley[Tm] =
      "(" *> tm <* ")" <|> "_" #> Hole <|> ident.map(Var.apply)

    lazy val tm: Parsley[Tm] = let <|> lam <|> app

    private lazy val let: Parsley[Tm] =
      ("let" *> ident <~> many(param) <~> option(
        ":" *> TyParser.ty
      ) <~> "=" *> tm <~> ";" *> tm).map { case ((((x, ps), ty), v), b) =>
        Let(
          x,
          ty.map(typeFromParams(ps, _)),
          lamFromParams(ps, v, ty.isEmpty),
          b
        )
      }

    private type Param =
      Either[(List[Name], Option[Kind]), (List[Name], Option[Ty])]

    def typeFromParams(xs: List[Param], rt: Ty): Ty =
      xs.foldRight(rt)((x, b) =>
        x.fold(
          (xs, ki) => xs.foldRight(b)(TForall(_, ki, _)),
          (xs, ty) => xs.foldRight(b)((_, b) => TFun(ty.getOrElse(THole), b))
        )
      )

    def lamFromParams(xs: List[Param], b: Tm, useTypes: Boolean): Tm =
      xs.foldRight(b)((x, b) =>
        x.fold(
          (xs, ki) =>
            xs.foldRight(b)(LamTy(_, if useTypes then ki else None, _)),
          (xs, ty) => xs.foldRight(b)(Lam(_, if useTypes then ty else None, _))
        )
      )

    private lazy val lam: Parsley[Tm] =
      ("\\" *> many(param) <~> "." *> tm).map(lamFromParams(_, _, true))

    lazy val param: Parsley[Param] =
      ("(" *> some(ident) <~> ":" *> TyParser.ty <* ")").map((xs, ty) =>
        Right((xs, Some(ty)))
      )
        <|> ("@" *> (("(" *> some(ident) <~> ":" *> KindParser.kind <* ")").map(
          (xs, ki) => Left((xs, Some(ki)))
        ) <|> ident.map(x => Left((List(x), None)))))
        <|> ident.map(x => Right((List(x), None)))

    private lazy val app: Parsley[Tm] =
      (atom <~> many(arg) <~> option(let <|> lam)).map {
        case ((fn, args), opt) =>
          (args ++ opt.map(Right.apply)).foldLeft(fn)((f, a) =>
            a.fold(AppTy(f, _), App(f, _))
          )
      }

    private lazy val arg: Parsley[Either[Ty, Tm]] =
      ("@" *> TyParser.atom).map(Left.apply) <|> atom.map(Right.apply)

    val parser = fully(tm)

  object DeclsParser:
    import parsley.combinator.{sepEndBy, many, some, option}

    import LangLexer.{fully, ident}
    import LangLexer.Implicits.given

    private lazy val ddef: Parsley[Decl] =
      (ident <~> many(TmParser.param) <~> option(
        ":" *> TyParser.ty
      ) <~> "=" *> TmParser.tm).map { case (((x, ps), ty), v) =>
        DDef(
          x,
          ty.map(TmParser.typeFromParams(ps, _)),
          TmParser.lamFromParams(ps, v, ty.isEmpty)
        )
      }

    private lazy val dtype: Parsley[Decl] =
      ("type" *> ident <~> many(
        TyParser.param
      ) <~> option(
        ":" *> KindParser.kind
      ) <~> "=" *> TyParser.ty).map { case (((x, ps), ki), ty) =>
        val psfl = ps.flatMap((xs, ki) => xs.map(x => (x, ki)))
        DType(x, psfl, ki, ty)
      }

    private lazy val decl: Parsley[Decl] =
      dtype <|> ddef

    private lazy val decls: Parsley[Decls] =
      sepEndBy(decl, ";").map(Decls.apply)

    val parser = fully(decls)
