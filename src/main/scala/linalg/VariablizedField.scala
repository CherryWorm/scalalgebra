package linalg

sealed trait Variablized[A]
case class Plus[A](left: Variablized[A], right: Variablized[A]) extends Variablized[A] {
	override def toString: String = s"$left + $right"
}
case class PlusInverse[A](ast: Variablized[A]) extends Variablized[A] {
	override def toString: String = s"\\left(-$ast\\right)"
}
case class Mult[A](left: Variablized[A], right: Variablized[A]) extends Variablized[A] {
	override def toString: String = s"$left \\cdot $right"
}
case class MultInverse[A](ast: Variablized[A]) extends Variablized[A] {
	override def toString: String = s"\\frac{1}{$ast}"
}
case class Val[A](a: A) extends Variablized[A] {
	override def toString: String = a.toString
}
case class Var[A](name: String)(implicit field: Field[A]) extends Variablized[A] {
	override def toString: String = name
}

case class VariablizedField[A](implicit f: Field[A]) extends Field[Variablized[A]] {
	override def one: Variablized[A] = Val(f.one)
	override def zero: Variablized[A] = Val(f.zero)
	override def invert(a: Variablized[A]): Variablized[A] = MultInverse(a)
	override def negate(a: Variablized[A]): Variablized[A] = PlusInverse(a)
	override def plus(a: Variablized[A], b: Variablized[A]): Variablized[A] = Plus(a, b)
	override def times(a: Variablized[A], b: Variablized[A]): Variablized[A] = Mult(a, b)
}