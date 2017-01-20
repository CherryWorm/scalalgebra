package linalg

case class Matrix[A](matrix: Vector[Vector[A]])(implicit field: Field[A]) {
	
	lazy val columns: Int = matrix.length
	lazy val rows: Int = matrix(0).length
	lazy val rank: Int = {
		0
	}
	lazy val inverse: Matrix[A] = {
		this
	}
	
	def get(row: Int, column: Int): A = matrix(row)(column)
	def apply(row: Int, column: Int): A = get(row, column)
	def set(row: Int, column: Int, e: A): Matrix[A] = Matrix[A](matrix.updated(row, matrix(row).updated(column, e)))
	
	def +(o: Matrix[A]): Matrix[A] = Matrix[A](columns, rows, (i: Int, j: Int) => field.plus(get(i, j), o.get(i, j)))
	
	def *(scalar: A): Matrix[A] = Matrix[A](columns, rows, (i: Int, j: Int) => field.times(get(i, j), scalar))
	
	def *(o: Matrix[A]): Matrix[A] = {
		def calc(i: Int, j: Int) = (for(k <- 0 until columns) yield field.times(get(i, k), o(k, j))).fold(field.zero)(field.plus)
		Matrix[A](rows, o.columns, calc(_, _))
	}
}

object Matrix {
	def apply[A](rows: Int, columns: Int, f: (Int, Int) => A)(implicit field: Field[A]): Matrix[A] = {
		Matrix[A](((0 until rows) map (i => ((0 until columns) map (j => f(i, j))).toVector)).toVector)
	}
	
	def id[A](d: Int)(implicit field: Field[A]): Matrix[A] = {
		Matrix[A](d, d, (i: Int, j: Int) => if(i == j) field.one else field.zero)
	}
	
	def zero[A](rows: Int, columns: Int)(implicit field: Field[A]): Matrix[A] = {
		Matrix[A](rows, columns, (i: Int, j: Int) => field.zero)
	}
}
