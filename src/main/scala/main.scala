object Hello {

  def main(args: Array[String]) =
    println("Hello")

  sealed trait Bool {
    type If[T <: Up, F <: Up, Up] <: Up
  }
  sealed trait True extends Bool {
    type If[T <: Up, F <: Up, Up] = T
  }

  sealed trait False extends Bool {
    type If[T <: Up, F <: Up, Up] = F
  }

  object Bool {
    type &&[A <: Bool, B <: Bool] = A#If[B, False, Bool]
    type || [A <: Bool, B <: Bool] = A#If[True, B, Bool]
    type Not[A <: Bool] = A#If[False, True, Bool]
  }

  sealed trait Nat {
    type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] <: Up
  }
  sealed trait _0 extends Nat {
    type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] = IfZero
  }
  sealed trait Succ[N <: Nat] extends Nat {
    type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] = NonZero[N]
  }
  type Is0[A <: Nat] = A#Match[ConstFalse, True, Bool]
  type ConstFalse[A] = False

}
