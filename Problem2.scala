sealed trait Tree[+A]
case object None extends Tree[Nothing]
case class Node[+A] (value: A, leftSubtree: Tree[A], rightSubtree: Tree[A]) extends Tree[A]

object Problem2 {

    def inOrder[A](t1: Tree[A]): List[A] = t1 match {
        case None => Nil

        case Node(v, lt, rt) => 
            inOrder(lt)::: (v :: Nil) :::inOrder(rt)    
    }

    def preOrder[A](t1: Tree[A]): List[A] = t1 match {
        case None => Nil

        case Node(v, lt, rt) =>
            (v :: Nil) ::: preOrder(lt) ::: preOrder(rt)
    }

    def postOrder[A](t1: Tree[A]): List[A] = t1 match {
        case None => Nil

        case Node(v, lt, rt) =>
            postOrder(lt) ::: postOrder(rt) ::: (v :: Nil)
    }


    def main(args: Array[String]): Unit = {
        // val t1 = 
        val t1: Tree[Int] = 
            Node(7, 
                Node(1, 
                    Node(2, None, None), 
                    Node(3, 
                        Node(4, None, None), 
                        Node(5, None, None))), 
                Node(6, None, None))

        println(inOrder(t1))
        println(preOrder(t1))
        println(postOrder(t1))


    }

}