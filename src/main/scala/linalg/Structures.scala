package linalg

import simulacrum._

@typeclass trait Semigroup[A] {
	@op("|+|") def append(a: A, b: A): A
}

@typeclass trait Monoid[A] extends Semigroup[A] {
	def id: A
}

@typeclass trait Group[A] extends Monoid[A] {
	@op("unary_-") def inverse(a: A): A
}

@typeclass trait Semiring[A] {
	@op("+") def plus(a: A, b: A): A
	@op("*") def times(a: A, b: A): A
}

@typeclass trait Ring[A] extends Semiring[A] {
	def one: A
	def zero: A
	@op("unary_-") def negate(a: A): A
	@op("-") def subtract(a: A, b: A): A = plus(a, negate(b))
}

@typeclass trait Field[A] extends Ring[A] {
	@op("inverted") def invert(a: A): A
	@op("/") def divide(a: A, b: A): A = times(a, invert(b))
}
