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

        def somefunction[A](l1: List[A]): List[A] = l1 match {
            case Nil => Nil
            case (x:: xs) => {
                if (x.equals(key)) {
                    (x :: Nil)
                } else {
                     somefunction(xs)
                }
            }
        }

        val l1 = somefunction(lst)
        if (l1.equals(Nil)) {
            false
        }
        else {
            true
        }
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
        val t1: Tree[Int] = 
            Node(7, 
                Node(1, 
                    Node(2, None, None), 
                    Node(3, 
                        Node(4, None, None), 
                        Node(5, None, None))), 
                Node(6, None, None))

        println("Testing Problem 2")

        /* Testing Part A */
        println("")
        println("Testing Part A")

        /* Testing inOrder() */
        println("inOrder() given a tree: "+ t1 +" should visit the nodes in the order (2, 1, 4, 3, 5, 7, 6), " +
          "answer of nodes visited by inOrder():" + inOrder(t1))
        println("")

        /* Testing preOrder() */
        println("preOrder() given a tree: "+ t1 +" should visit the nodes in the order (7, 1, 2, 3, 4, 5, 6), " +
          "answer of nodes visited by inOrder():" + preOrder(t1))
        println("")

        /* Testing postOrder() */
        println("postOrder() given a tree: "+ t1 +" should visit the nodes in the order (2, 4, 5, 3, 1, 6, 7), " +
          "answer of nodes visited by inOrder():" + postOrder(t1))
        println("")

        /* Testing search() */
        println("search() given a tree: "+ t1 +" and a value of 8 to search for should return false, the answer " +
          "search() returns: " + search(t1, 8))
        println("")

        println("search() given a tree: "+ t1 +" and a value of 8 to search for should return true, the answer " +
          "search() returns: " + search(t1, 1))
        println("")

        /* Testing replace() */
        println("replace() given a tree: " + t1 + "and a value to replace of 2 with a value of 100 should return the tree" +
          "with 2 replaced by 100: "+replace(t1, 2, 100))
    }
}