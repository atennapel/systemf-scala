import Elaboration.*
import Surface.*
import Surface.Tm.*
import Surface.Ty.*
import Surface.Kind.*
import Debug.*
import Pretty.*

object Main:
  val test = Let(
    "id",
    Some(TForall("A", Some(KType), TFun(TVar("A"), TVar("A")))),
    Lam("x", None, Var("x")),
    App(Var("id"), Var("id"))
  )

  @main def run(): Unit =
    setDebug(false)
    println(test.toString)
    val (tm, ty) = elaborate(test)
    println(pretty(tm))
    println(prettyTy(ty))
