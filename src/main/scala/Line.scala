class Line(ps:Array[Point]) {

	// read only values a and b
	val a = Util.cov(ps.map(p => p.x), ps.map(p => p.y)) / Util.variance(ps.map(p => p.x))
	val b = (ps.map(p => p.y).sum / ps.size) - a * ((ps.map(p => p.x).sum) / ps.size)

	// f
	def f(x: Double): Double = {
		return a * x + b
	}

	// dist
	def dist(p: Point): Double = {
		return Math.abs(f(p.x) - p.y)
	}
}
