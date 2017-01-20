package linalg

case class Rational(num: Int, denom: Int) {
	
	lazy val simplified: Rational = {
		def gcd(a: Int, b: Int): Int = {
			if(b == a) a
			else if(a > b) gcd(a - b, b)
			else gcd(a, b - a)
		}
		val g = gcd(num.abs, denom.abs)
		if(num < 0 && denom < 0)
			Rational(-num / g, -denom / g)
		else if(num > 0 && denom > 0)
			Rational(num / g, denom / g)
		else
			Rational(-num.abs / g, denom.abs / g)
	}
	
	lazy val neg: Rational = Rational(-num, denom).simplified
	lazy val inverse: Rational = Rational(denom, num).simplified
	
	def *(a: Rational) = {
		Rational(num * a.num, denom * a.denom).simplified
	}
	def +(a: Rational) = {
		Rational(num * a.denom + a.num * denom, denom * a.denom).simplified
	}
	
	override def toString: String = if(num == 0) "0" else if(denom == 1) num.toString else s"\\frac{$num}{$denom}"
}

object RationalField extends Field[Rational] {
	override def plus(a: Rational, b: Rational): Rational = a + b
	override def times(a: Rational, b: Rational): Rational = a * b
	override def additiveInverse(a: Rational): Rational = a.neg
	override def multiplicativeInverse(a: Rational): Rational = a.inverse
	override val zero: Rational = Rational(0, 1)
	override val one: Rational = Rational(1, 1)
}