object Problem1 {

    /* Problem 1 Part A */
    def shuffle[A](l1: List[A], l2: List[A]): List[A] = (l1,l2) match {
        // case // both lists empty
        case (Nil,Nil) => Nil

        // case // if list 1 is empty
        case (Nil,l2) => l2

        // case // if list 2 is empty
        case (l1,Nil) => l1

        // case // choose which list to take from
        case (x::xs, y::ys) =>
            if (xs.length == ys.length) x:: shuffle(xs, l2)
            else y:: shuffle(l1, ys)
    }

    /* Problem 1 Part B */
    def split[A](l1: List[A], n: Int): List[List[A]] = (l1,n) match {
        // case // list is empty
        case (Nil,_) => Nil

        // case //
        case (l1, _) =>
            List(l1.take(n), l1.drop(n))
    }

    /* Problem 1 Part C */
    // def outshuffle[A](l1: List[A], f:A => List[List[A]], h:A => List[A]): List[A] = (l1, f, h) match {
    /*  */
    def outshuffle[A](l1: List[A]): List[A] = l1 match {
        // case //
        case _ =>
            val lists = split(l1, l1.length/2)
            shuffle(lists(0), lists(1))
            // result
    }

    /* In shuffle results in the top and bottom cards of the stack becoming the second and second last cards */
    def inshuffle[A](l1: List[A]): List[A] = l1 match {
        case _ =>

            val lists = split(l1, l1.length/2)
            shuffle(lists(1), lists(0))
    }

    /* Problem 1 Part D */
    // @tailrec
    def nshuffle[A] (l1: List[A], n: Int)(f:List[A] => List[A]): List[A] = (l1, n, f) match {
        case (_, _, _) =>
            // f(l1)
            if (n == 0) l1
            // else nshuffle(l1, n-1)(f)
            else {
                // val l2 = nshuffle(l1, n-1)(f)
                // val l2 = outshuffle(l1)
                nshuffle(f(l1), n-1)(f)
            }
    }

    /* Problem 1 Part E */
    def howManyShuffles[A] (l1: List[A], l2: List[A])(f:List[A] => List[A]): Int = {
        // case (_, _, _) => 2

        // val shuffle_count = 0
        def go(first: List[A], count: Int): Int  =  {
            // case (_, _) => 
                if (first.equals(l2)) count
                else {
                    go(f(first), count + 1)
                }
        }
        go(f(l1), 1)
    }

    def main(args: Array[String]): Unit = {
        println("Tesing Problem 1")

        /* Testing Part A */
        println("")
        println("Testing Part A")
        val l1 = List(0, 2, 4, 6)
        val l2 = List (1, 3, 5, 7)
        println("shuffle() given a first list of (0, 2, 4, 6) and given a second list of (1, 3, 5, 7) should" +
          " be (0, 1, 2, 3, 4, 5, 6, 7), answer returned: "+shuffle(l1, l2))
        println(shuffle(l1, l2))
        if (shuffle(l1, l2) != List(0, 1, 2, 3, 4, 5, 6, 7)) println("error")


        /* Test Part B */
        println("")
        println("Testing Part B")
        val l3 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        println("split() given a first list of (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) and given an index of 5 should" +
          " return two lists (1, 2, 3, 4, 5) and (6, 7, 8, 9, 10), answer returned: "+split(l3, 5))


        /* Test Problem 1 Part C */
        println("")
        println("Testing Part C")

        val l4 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        println("outshuffle() given a list of (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) should return a list of the first element" +
          "1 on top and last element 10 on bottom, and inshuffle actually returns "+outshuffle(l4))

        println("inshuffle() given a list of (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) should return a list of the first element" +
          "1 on second and last element 10 on second to bottom, and outshuffle actually returns "+inshuffle(l4))


        /* Test Problem 1 Part D */
        println("")
        println("Testing Part D")

        val l8 = List(1, 2, 3, 4, 5, 6)
        val l9 = List(1, 3, 5, 2, 4, 6)
        val l10 = List(2, 4, 6, 1, 3, 5)

        println("nshuffle() given "+l8+ " and 3 and outshuffle(), should give a list of (1, 3, 5, 2, 4, 6) and actually returns: " + (nshuffle(l8, 3)(outshuffle)))
        println("")
        println("nshuffle() given "+l8+ " and 2 and inshuffle(), should give a list of (2, 4, 6, 1, 3, 5) and actually returns: " + (nshuffle(l8, 2)(inshuffle)))
        

        /* Test Problem 1 Part E */
        println("")
        println("Testing Part E")

        val l5 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 
        27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52)

        val l6 = List(1, 33, 14, 46, 27, 8, 40, 21, 2, 34, 15, 47, 28, 9, 41, 22, 3, 35, 16, 48, 29, 10, 42, 23, 4, 36, 17, 49, 30, 11, 43, 24, 5, 37, 18, 50, 31, 12, 44, 25, 6, 38, 19, 51, 32, 13, 45, 26, 7, 39, 20, 52)
        val l7 = List(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 2, 7, 12, 17, 22, 27, 32, 37, 42, 47, 52, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 1, 6, 11, 16, 21, 26, 31, 36, 41, 46, 51, 3, 8, 13, 18, 23, 28, 33, 38, 43, 48)

        // 3
        println("howManyShuffles() of "+l5+ "and" +l6+ " when given outshuffle() should return 3 and actually returns " + howManyShuffles(l5, l6)(outshuffle))
        println("")

        // 5
        println("howManyShuffles() of "+l5+ "and" +l7+ " when given outshuffle() should return 5 and actually returns " + howManyShuffles(l5, l7)(inshuffle))
    }
}
