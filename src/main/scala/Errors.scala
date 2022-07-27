object Errors:
  object Impossible extends Exception(s"impossible")
  final case class UnifyError(msg: String)
      extends Exception(s"unify error: $msg")
  final case class VarError(msg: String)
      extends Exception(s"undefined variable: $msg")
  final case class CannotInferError(msg: String)
      extends Exception(s"cannot infer: $msg")
  final case class NotAFunError(msg: String)
      extends Exception(s"expected function type: $msg")
  final case class NotAForallError(msg: String)
      extends Exception(s"expected forall type: $msg")
  final case class NotAKFunError(msg: String)
      extends Exception(s"expected kind function: $msg")
  final case class KindMismatchError(msg: String)
      extends Exception(s"kind mismatch: $msg")
  final case class UnsolvedMetasError(msg: String)
      extends Exception(s"unsolved metas: $msg")
