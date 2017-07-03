package week1

object function {
    def product(f: (Int, Int) => Int, a: Int, b: Int): Int = {
        if (a > b) 1
        else
            f(a, product(f, a + 1, b))
    }

    def sum(f: Int => Int): (Int, Int) => Int = {
        def sumF(a: Int, b: Int): Int = {
            if (a > b) 0
            else
                f(a) + sumF(a + 1, b)
        }
        sumF
    }
    // This style of function is currying
    def product_(f: Int => Int)(a: Int, b: Int): Int = {
        if (a > b) 1
        else f(a) * product_(f)(a + 1, b)
    }
    // This style of function is currying
    def sum_(f: Int => Int)(a: Int, b: Int): Int = {
        if (a > b) 0
        else f(a) + sum_(f)(a + 1, b)
    }

    def productInt(a: Int, b: Int) = product_(x => x*x)(a, b)
    def sumInts = sum(x => x)
    def sumCubes = sum(x => x * x * x)

    def main(args: Array[String]) {
        println(productInt(1, 3))
        println(sumInts(1, 10))
        println(product_(x => x * x)(1, 3))
        println(productInt(1, 3))
    }
}