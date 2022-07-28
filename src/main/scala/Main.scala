import Elaboration.*
import Surface.*
import Surface.Tm.*
import Surface.Ty.*
import Surface.Kind.*
import Surface.Decl.*
import Debug.*
import Pretty.*

object Main:
  val ds = Decls(
    List(
      DDef("id", Some(TFun(TVar("t"), TVar("t"))), Lam("x", None, Var("x"))),
      DDef("x", None, Var("id"))
    )
  )

  @main def run(): Unit =
    setDebug(true)
    println(ds.toString)
    elaborateDecls(ds)
