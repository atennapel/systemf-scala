import Elaboration.*
import Surface.*
import Surface.Tm.*
import Surface.Ty.*
import Surface.Kind.*
import Surface.Decl.*
import Debug.*
import Pretty.*
import Common.*
import Globals.*
import Parser.DeclsParser.parser

import java.io.File
import parsley.io.given

@main def cli(filename: String): Unit =
  setDebug(true)
  val tm = parser.parseFromFile(new File(filename)).flatMap(_.toTry).get
  debug(tm.toString)
  elaborateDecls(tm, Pos(0, 0))
  getTGlobals().foreach { (x, e) =>
    val ctx = e.params.foldLeft(Ctx.empty(Pos(0, 0))) { case (ctx, (x, k)) =>
      ctx.bindTy(x, k)
    }
    println(
      s"type $x ${e.params.map((x, k) => s"($x : ${Ctx.pretty(k)})").mkString(" ")}${
          if e.params.isEmpty then "" else " "
        }: ${Ctx.pretty(e.kind)} = ${ctx.pretty(e.value)}"
    )
  }
  getGlobals().foreach { (x, e) =>
    println(s"$x : ${Ctx.pretty(e.ty)} = ${Ctx.pretty(e.value)}")
  }
