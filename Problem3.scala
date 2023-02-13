object Problem3 {

    def luhnDouble(n: Int): Int = {
        val newVal:Int = n * 2
        if (newVal > 9) {
            val minus9:Int = newVal- 9
            minus9
        }
        else {
            val newnewval:Int = newVal
            newnewval
        }
    }

    def altMap[A, B](l1: List[A])(f1: List[A => B]): List[B] = l1 match {
        case Nil =>
            Nil

        case l1 => 
            val listElement = l1(0)
            val func = f1(0)
            val newVal = func(listElement)
            val d = f1.drop(1)
            val e = d ::: (func :: Nil)
            (newVal :: Nil) ::: altMap(l1.drop(1))(e)
    }

    def luhn(l1: List[Int]): Boolean = {
        val f1 = List(luhnDouble _, luhnDouble _, luhnDouble _)
        val l2 = altMap(l1)(f1)

        def go(nums: List[Int]): Int = nums match {
            case Nil => 0
            case (x::xs) => x + go(xs)
        }

        val totalVal = go(l1)

        if (totalVal % 10 == 0) {
            true
        }
        else {
            false
        }
    }


    def main(args: Array[String]): Unit = {
        println("Testing Problem 3")

        /* Testing Part A */
        println("")
        println("Testing Part A")
        println("Luhn double of 6 should be 3, answer returned from luhnDouble(): "+luhnDouble(6))
        if (luhnDouble(6)!=3) println("error in luhnDouble()")

        println("Luhn double of 3 should be 6, answer returned from luhnDouble(): "+luhnDouble(3))
        if (luhnDouble(6)!=3) println("error in luhnDouble()")



        /* Testing Part B */
        println("")
        println("Testing Part B")
        val list1 = List(1, 2, 3, 4, 5)
        val functionList1 = List(luhnDouble _, luhnDouble _, luhnDouble _)
        println("altmap() function given a List of (1, 2, 3, 4, 5) and a function list" +
          "of (luhnDouble_, luhnDouble_, luhnDouble_) should return a list of (2, 4, 6, 8, 1), " +
          "answer returned from altMap(): "+altMap(list1)(functionList1))
        val list4 = List(6, 7, 8, 9)
        println("altmap() function given a List of (6, 7, 8, 9) and a function list" +
          "of (luhnDouble_, luhnDouble_, luhnDouble_) should return a list of (3, 5, 7, 9), " +
          "answer returned from altMap(): "+altMap(list4)(functionList1))

        
        /* Testing Part C */
        println("")
        println("Testing Part C")
        val list2 = List(1, 2, 3, 4)
        println("luhn() function given a List of (1, 2, 3, 4) should return true, answer returned form luhn(): " + luhn(list2))
        val list3 = List(3, 3, 3)
        println("luhn() function given a List of (3, 3, 3) should return false, answer returned form luhn(): " + luhn(list3))
    }
}