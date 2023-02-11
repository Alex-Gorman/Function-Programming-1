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

    def search[A](t1: Tree[A], key:A): Boolean = {
        val lst = inOrder(t1)
        def loop(n: Int): Int = {
            if (n >= lst.length) -1
            else if (lst(n) == key) n
            else loop(n+1)
        }
        if (loop(0) == -1) false
        else true
    }

    def replace[A](t1: Tree[A], before:A, after:A): Tree[A] = t1 match {
        case None => None

        case Node(v, lt, rt) =>
            if (v.equals(before)) {
                Node(after, replace(lt, before, after), replace(rt, before, after))
            }
            else {
                Node(v, replace(lt, before, after), replace(rt, before, after))
            }
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

        // println(inOrder(t1))
        // println(preOrder(t1))
        // println(postOrder(t1))

        // println(search(t1, 8))
        // println(search(t1, 1))

        println(replace(t1, 2, 100))


    }

}