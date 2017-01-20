package linalg

trait Field[A] {
	def plus(a: A, b: A): A
	def times(a: A, b : A): A
	def additiveInverse(a: A): A
	def multiplicativeInverse(a: A): A
	val zero: A
	val one: A
}
