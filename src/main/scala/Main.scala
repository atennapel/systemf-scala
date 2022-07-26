import Elaboration.*
import Surface.*
import Surface.Tm.*
import Surface.Ty.*
import Surface.Kind.*

object Main:
  val test = LamTy("A", Some(KType), Lam("x", Some(TVar("A")), Var("x")))

  @main def run(): Unit =
    println(test.toString)
    val (tm, ty) = elaborate(test)
    println(tm.toString)
    println(ty.toString)
