package week1

object PascalTriangle {
    def pascal(c: Int, r: Int): Int = {
        if ((c == 0) || (r == c)) 1
        else
            pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
    
    def main(args: Array[String]) {
        println(pascal(0, 3))
    }
}