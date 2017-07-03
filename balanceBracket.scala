package week1

object balanceBracket {
    def    balance(chars: List[Char]) : Boolean = {
        def    loop(acc: Int, c: List[Char]): Int = {
            if (c.isEmpty || acc < 0) acc 
            else {
                if (c.head == ')') loop(acc - 1, c.tail)
                else
                if (c.head == '(') loop(acc + 1, c.tail)
                else
                    loop(acc, c.tail)

            }
        }
        loop(0, chars) == 0
    }
    
    def    main(args: Array[String]) {
        val s = "I told him (that it’s not (yet) done). (But he wasn’t listening)"
        val chars = s.toList
        println(balance(chars))
    }
}