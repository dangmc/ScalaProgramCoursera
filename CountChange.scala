package week1

object CountChange {
    def countChange(money: Int, coins: List[Int]): Int = {
        def loop(remain: Int, c: List[Int]): Int = {
            if (c.isEmpty) 0 else
                
            if (remain - c.head == 0) 1
            else {
                
                if (remain > c.head) {
                    loop(remain - c.head, c) + loop(remain - c.head, c.tail)
                } else
                    loop(remain, c.tail)
            }
        }
        def count(acc: Int, remain: Int, c: List[Int]) : Int = {
            if (c.isEmpty) acc
            else {
                count(acc + loop(remain, c), remain, c.tail)
            }
        }
        count(0, money, coins)
    }
    
    def    main(args: Array[String]) {
        val coins = List(1, 2, 3)
        val money = 6
        println(countChange(money, coins))
    }
}