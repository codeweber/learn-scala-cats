package com.rockthejvm.cats.abstractMath

import scala.annotation.tailrec

object CustomMonads:

    //Create a monad for the identity type
    type Identity = [T] =>> T

    import cats.Monad

    given identityMonad: Monad[Identity] with
        override def pure[A](x: A): Identity[A] = x 
        override def flatMap[A,B](x: Identity[A])(f: A => Identity[B]): Identity[B] = f(x)
        
        @annotation.tailrec
        override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = 
            f(a) match 
                case Left(nextA) => tailRecM(nextA)(f)
                case Right(b) => b


    //Ex 2: define a monad for a binary tree
    enum Tree[+A]:
        case Leaf(value: A)
        case Branch(left: Tree[A], right: Tree[A])


    given treeMonad: Monad[Tree] with
        override def pure[A](x: A): Tree[A] = Tree.Leaf(x)

        override def flatMap[A,B](t: Tree[A])(f: A => Tree[B]): Tree[B] = 
            t match
                case Tree.Leaf(a) => f(a)
                case Tree.Branch(l,r) => Tree.Branch(flatMap(l)(f), flatMap(r)(f)) 

        override def tailRecM[A,B](a: A)(f: A => Tree[Either[A,B]]): Tree[B] = 

            /* def sequence(t: Tree[Either[A,B]]): Either[A, Tree[B]] =
                t match
                    case Tree.Leaf(Left(a)) => Left(a)
                    case Tree.Leaf(Right(v)) => Right(Tree.Leaf(v))
                    case Tree.Branch(left, right) => 
                        for 
                            l <- sequence(left)
                            r <- sequence(right)
                        yield Tree.Branch(l,r)

            sequence(f(a)) match 
                case Left(nextA) => tailRecM(nextA)(f)
                case Right(t) => t */

            @tailrec
            def tailRec(todo: List[Tree[Either[A,B]]], explored: List[Tree[Either[A,B]]], done: List[Tree[B]]): Tree[B] = 

                todo match
                    case Nil => done.head 
                    case t :: ts =>
                        t match 
                            case Tree.Leaf(Left(a)) => tailRec(f(a) :: ts, explored, done)
                            case Tree.Leaf(Right(b)) =>tailRec(ts, explored, Tree.Leaf(b) :: done)
                            case node @ Tree.Branch(l, r) =>
                                if explored.head != node then
                                    tailRec(r :: l :: todo, node :: explored, done)
                                else
                                    val newLeft = done.head 
                                    val newRight = done.tail.head 
                                    tailRec(ts, explored.tail, Tree.Branch(newLeft, newRight) :: done.drop(2))
            
            
            tailRec(List(f(a)), List.empty[Tree[Either[A,B]]], List.empty[Tree[B]])