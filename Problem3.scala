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

        true
    }


    def main(args: Array[String]): Unit = {
        // println(luhnDouble(6))
        // println(luhnDouble(3))

        // val l1 = List(1, 2, 3, 4)
        // val f1 = List(luhnDouble _, luhnDouble _, luhnDouble _, luhnDouble _)
        // println(altMap(l1)(f1))

        val l1 = List(1, 2, 3, 4)
        val f1 = List(luhnDouble _, luhnDouble _, luhnDouble _)
        println(altMap(l1)(f1))

        val l2 =  altMap(l1)(f1)
    }
}