object Problem1 {

    /* Problem 1 Part A */
    def shuffle[A](l1: List[A], l2: List[A]): List[A] = (l1,l2) match {
        // case // both lists empty
        case (Nil,Nil) => Nil

        // case // if list 1 is empty
        case (Nil,l2) => l2

        // case // if list 2 is empty
        case (l1,Nil) => l1

        // case // 
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
        // case (_, _, _) => Nil

        // case //
        case _ =>
            val lists = split(l1, l1.length/2)
            shuffle(lists(0), lists(1))
            // result
    }

    /* In shuffle results in the top and bottom cards of the stack becoming the second and second last cards */
    def inshuffle[A](l1: List[A]): List[A] = l1 match {
        case _ =>



            Nil
    }

    /* Problem 1 Part D */


        


    def main(args: Array[String]): Unit = {
        println("Begin Testing Assignment 2")

        /* Test Problem 1 Part A */
        val l1 = List(0, 2, 4, 6)
        val l2 = List (1, 3, 5, 7)
        // println(shuffle(l1, l2))
        if (shuffle(l1, l2) != List(0, 1, 2, 3, 4, 5, 6, 7)) println("error")


        /* Test Problem 1 Part B */
        val l3 = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        // println(split(l3, 5))

        val l4 = split(l3, 5)
        // println(l4(0))


        /* Test Problem 1 Part C */
        val prob1_partc_Outshuffle = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        println(outshuffle(prob1_partc_Outshuffle))

        val prob1_partc_Inshuffle = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)











    }
}
