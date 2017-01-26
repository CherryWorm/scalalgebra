package object util {
	
	def join(traversable: Traversable[String], s: String) = traversable.reduce(_ + s + _)
	
}
