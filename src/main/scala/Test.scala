import linalg.Field.ops._
import linalg._
import linalg.Rational._

object Test extends App {
	val lambda = Var[Rational]("\\lambda")
	val mu = Var[Rational]("\\mu")
	val x = Plus(Val(Rational(5)), lambda)
	val y = Plus(Val(Rational(3)), mu)
	val z = Mult(x, MultInverse(y))
	
	val m = Matrix[Variablized[Rational]](Vector(
		Vector(Val(Rational(2)), Val(Rational(3)), Val(Rational(1))),
		Vector(Val(Rational(3)), Val(Rational(-1)), lambda),
		Vector(Val(Rational(1)), Val(Rational(7)), Val(Rational(-6)))
	))(VariablizedField[Rational]())
	val l = Vector(Val(Rational(5)), Val(Rational(2)), lambda)
	println(m + " solved for " + l + ": \\\\" + m.solve(l) + "\\\\")
	
	val a = Matrix[Variablized[Rational]](Vector(
		Vector(Var[Rational]("a"), Var[Rational]("b")),
		Vector(Var[Rational]("c"), Var[Rational]("d"))
	))(VariablizedField[Rational]())
	println(a + "^{-1} = " + a.inverse)
}
