package linalg

import scalaz._
import Scalaz._
import util._

case class Matrix[A](matrix: Vector[Vector[A]])(implicit field: Field[A]) {
	
	lazy val columns: Int = matrix.length
	lazy val rows: Int = matrix(0).length
	lazy val rank: Int = {
		0
	}
	lazy val inverse: Matrix[A] = complement * field.invert(det)
	lazy val det: A = {
		if(rows == 1) get(1, 1)
		else (for(j <- 1 to rows) yield {
			val prod = field.times(get(1, j), develop(1, j).det)
			if(j % 2 == 0)
				field.negate(prod)
			else
				prod
		}) reduce field.plus
	}
	lazy val transpose: Matrix[A] = Matrix[A](columns, rows, (i: Int, j: Int) => get(j, i))
	lazy val complement: Matrix[A] = Matrix[A](columns, rows, { (i: Int, j: Int) =>
		val d = develop(j, i).det
		if((i + j) % 2 == 0)
			d
		else
			field.negate(d)
	})
	
	def get(row: Int, column: Int): A = matrix(row - 1)(column - 1)
	def apply(row: Int, column: Int): A = get(row, column)
	def set(row: Int, column: Int, e: A): Matrix[A] = Matrix[A](matrix.updated(row - 1, matrix(row).updated(column - 1, e)))
	
	def +(o: Matrix[A]): Matrix[A] = Matrix[A](columns, rows, (i: Int, j: Int) => field.plus(get(i, j), o.get(i, j)))
	
	def *(scalar: A): Matrix[A] = Matrix[A](columns, rows, (i: Int, j: Int) => field.times(get(i, j), scalar))
	
	def *(o: Matrix[A]): Matrix[A] = {
		def calc(i: Int, j: Int) = (for(k <- 1 to columns) yield field.times(get(i, k), o(k, j))).fold(field.zero)(field.plus)
		Matrix[A](rows, o.columns, calc(_, _))
	}
	
	def develop(x: Int, y: Int): Matrix[A] = {
		def g(j: Int, x: Int) = if(j >= x) j + 1 else j
		Matrix[A](rows - 1, columns - 1, (i: Int, j: Int) => get(g(i, x), g(j, y)))
	}
	
	def solve(rs: Vector[A]): Vector[A] = {
		def insert(n: Int) = Matrix[A](rows, columns, (i: Int, j: Int) => if(j == n) rs(i - 1) else get(i, j))
		val d = field.invert(det)
		(for(i <-  1 to rows) yield field.times(insert(i).det, d)).toVector
	}
	
	override def toString: String =
		"\\begin{pmatrix}" + join(matrix map ( row => join(row map (_.toString), "&")), "\\\\") + "\\end{pmatrix}"
	
}

object Matrix {
	def apply[A](rows: Int, columns: Int, f: (Int, Int) => A)(implicit field: Field[A]): Matrix[A] = {
		Matrix[A](((1 to rows) map (i => ((1 to columns) map (j => f(i, j))).toVector)).toVector)
	}
	
	def id[A](d: Int)(implicit field: Field[A]): Matrix[A] = {
		Matrix[A](d, d, (i: Int, j: Int) => if(i == j) field.one else field.zero)
	}
	
	def zero[A](rows: Int, columns: Int)(implicit field: Field[A]): Matrix[A] = {
		Matrix[A](rows, columns, (i: Int, j: Int) => field.zero)
	}
}
