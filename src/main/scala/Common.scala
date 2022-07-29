import scala.annotation.tailrec

object Common:
  final case class Pos(line: Int, col: Int):
    override def toString: String = s"L$line:$col"

  type Ix = Int
  type Lvl = Int
  def lvl2ix(l: Lvl, x: Lvl): Ix = l - x - 1

  type Name = String

  @tailrec
  def freshName(x: Name, ns: Seq[Name]): Name =
    if x == "_" then x
    else if ns.contains(x) then freshName(nextName(x), ns)
    else x

  // TODO: better name generation
  def nextName(x: Name): Name =
    if x == "_" then x
    else s"$x'"

  def chooseName(x: Name, y: Name): Name =
    if y == "_" then x else y

  type MetaId = Int
