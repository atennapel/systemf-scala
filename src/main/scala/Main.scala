import Elaboration.*
import Surface.*
import Surface.Tm.*
import Surface.Ty.*
import Surface.Kind.*
import Debug.*

object Main:
  val test = Lam("x", None, Lam("y", None, Var("x")))

  @main def run(): Unit =
    setDebug(false)
    println(test.toString)
    val (tm, ty) = elaborate(test)
    println(tm.toString)
    println(ty.toString)
