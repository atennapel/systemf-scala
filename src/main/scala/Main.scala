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
  val tm = parser.parseFromFile(new File(filename)).flatMap(_.toTry).get
  debug(tm.toString)
  elaborateDecls(tm, Pos(0, 0))
  getGlobals().foreach { (x, e) =>
    println(s"$x : ${Ctx.pretty(e.ty)} = ${Ctx.pretty(e.value)}")
  }
