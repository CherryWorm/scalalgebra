import linalg._

object Test extends App {
	implicit val astField = VariablizedASTField[Rational]()(RationalField)
	
	val T = Matrix[VariablizedAST[Rational]](Vector(
		Vector(Var("1,1")(RationalField), Var("1,2")(RationalField), Var("1,3")(RationalField), Var("1,4")(RationalField)),
		Vector(Var("2,1")(RationalField), Var("2,2")(RationalField), Var("2,3")(RationalField), Var("2,4")(RationalField)),
		Vector(Var("3,1")(RationalField), Var("3,2")(RationalField), Var("3,3")(RationalField), Var("3,4")(RationalField)),
		Vector(Var("4,1")(RationalField), Var("4,2")(RationalField), Var("4,3")(RationalField), Var("4,4")(RationalField))
	))
	
	val R = Matrix.zero(4, 5).set(0, 0, astField.one).set(1, 1, astField.one).set(2, 2, astField.one)
	
	val rightSide = T * R
	val simplified = Matrix(rightSide.rows, rightSide.columns, (i: Int, j: Int) => rightSide(i, j).simplified(RationalField))
	
	println(rightSide)
}
