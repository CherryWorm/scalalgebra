package linalg

case class Rational(num: Int, denom: Int) {
	
	lazy val simplified: Rational = {
		val div = Math.gcd(num, denom)
		Rational(num / div, denom / div)
	}
	
	override def toString: String = if(num == 0) "0" else if(denom == 1) num.toString else (if(num * denom < 0) "-" else "") + s"\\frac{${num.abs}}{${denom.abs}}"
}
object Rational {
	implicit object RationalField extends Field[Rational] {
		override def one: Rational = Rational(1, 1)
		override def zero: Rational = Rational(0, 1)
		override def invert(a: Rational): Rational = Rational(a.denom, a.num).simplified
		override def negate(a: Rational): Rational = Rational(-a.num, a.denom).simplified
		override def plus(a: Rational, b: Rational): Rational = Rational(a.num * b.denom + b.num * a.denom, a.denom * b.denom).simplified
		override def times(a: Rational, b: Rational): Rational = Rational(a.num * b.num, a.denom * b.denom).simplified
	}
	
	def apply(n: Int): Rational = Rational(n, 1)
}
