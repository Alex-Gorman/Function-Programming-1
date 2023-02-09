object Problem1 {

    /* Problem 1 Part A */
    def shuffle[A](l1: List[A], l2: List[A]): List[A] = (l1,l2) match{
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
    // def split[A](l1: List[A], n: Int) = {
    //     // case Nil => Nil
    //     def inOrder[A](l1: List)



    // }
    def inOrder[A](l1: List[A]): List[A] = l1 match {
        case Nil => Nil
        case x::y::xs => l1
    }



    def main(args: Array[String]): Unit = {
        println("Begin Testing Assignment 2")

        /* Test Problem 1 Part A */
        val l1 = List(0, 2, 4, 6)
        val l2 = List (1, 3, 5, 7)
        // println(shuffle(l1, l2))
        if (shuffle(l1, l2) != List(0, 1, 2, 3, 4, 5, 6, 7)) println("error")

        /* Test Problem 1 Part B */
        val l3 = List(1, 3, 5, 8, 2, 6, 8, 9, 0, 4, 7)








    }
}
