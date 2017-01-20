package linalg

sealed trait VariablizedAST[A] {
	def simplified(implicit field: Field[A]): VariablizedAST[A] = VariablizedAST.simplify(this)
}
case class Plus[A](left: VariablizedAST[A], right: VariablizedAST[A])(implicit field: Field[A]) extends VariablizedAST[A]
case class PlusInverse[A](ast: VariablizedAST[A])(implicit field: Field[A]) extends VariablizedAST[A]
case class Mult[A](left: VariablizedAST[A], right: VariablizedAST[A])(implicit field: Field[A]) extends VariablizedAST[A]
case class MultInverse[A](ast: VariablizedAST[A])(implicit field: Field[A]) extends VariablizedAST[A]
case class Value[A](a: A)(implicit field: Field[A]) extends VariablizedAST[A]
case class Var[A](name: String)(implicit field: Field[A]) extends VariablizedAST[A]

object VariablizedAST {
	def simplify[A](ast: VariablizedAST[A])(implicit field: Field[A]): VariablizedAST[A] = ast match {
		case Plus(Value(a), Value(b)) => Value(field.plus(a, b))
		case Plus(a, Value(field.zero)) => a
		case Plus(Value(field.zero), a) => a
		case PlusInverse(Value(a)) => Value(field.additiveInverse(a))
		case Mult(Value(a), Value(b)) => Value(field.times(a, b))
		case Mult(Value(field.zero), _) => Value(field.zero)
		case Mult(_, Value(field.zero)) => Value(field.zero)
		case Mult(a, Value(field.one)) => a
		case Mult(Value(field.one), a) => a
		case MultInverse(Value(a)) => Value(field.multiplicativeInverse(a))
		case _ => ast
	}
}

case class VariablizedASTField[A](implicit field: Field[A]) extends Field[VariablizedAST[A]] {
	override def plus(a: VariablizedAST[A], b: VariablizedAST[A]): VariablizedAST[A] = Plus(a, b)
	override def times(a: VariablizedAST[A], b: VariablizedAST[A]): VariablizedAST[A] = Mult(a, b)
	override def additiveInverse(a: VariablizedAST[A]): VariablizedAST[A] = PlusInverse(a)
	override def multiplicativeInverse(a: VariablizedAST[A]): VariablizedAST[A] = MultInverse(a)
	
	override val zero: VariablizedAST[A] = Value(field.zero)
	override val one: VariablizedAST[A] = Value(field.one)
}