package week1

object fixedPoint {

    val eps = 0.00001

    def averageF(f: Double => Double)(x: Double): Double = {
        (x + f(x)) / 2
    }
    def isGoodEnough(guess: Double, next: Double): Boolean = {
        math.abs((guess - next) / guess) / guess < eps
    }
    def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
        def loop(guess: Double): Double = {
            val next = f(guess)
            if (isGoodEnough(guess, next)) next
            else
                loop(next)
        }
        loop(firstGuess)
    }

    def square(x: Double) = fixedPoint(averageF(y => x / y))(1.0)
    
    def main(args: Array[String]) {
        println(square(2.0))
    }
}