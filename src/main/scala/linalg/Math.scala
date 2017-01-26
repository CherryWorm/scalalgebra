package linalg

import scala.annotation._

object Math {
	@tailrec def gcd(a: Int, b: Int): Int =
		if(b == 0) a
		else gcd(b, a % b)
}
