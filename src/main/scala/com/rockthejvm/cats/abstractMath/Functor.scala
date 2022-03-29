package com.rockthejvm.cats.abstractMath

import scala.util.Try

object Functor:

    import cats.Functor

    def doMap[F[_]:Functor,A,B](init: F[A])(g: A => B): F[B] =
        val f = summon[Functor[F]]
        f.map(init)(g)


    @main def TryDoMap() =

        val g: Int => Int = _ + 10

        val aList = List(1,2,3,4)
        println(doMap(aList)(g))

        val anOption = Option(4)
        println(doMap(anOption)(g))

        val aTry = Try(5)
        println(doMap(aTry)(g))


    // Exercise 1: Define a functor for a binary tree
    enum Tree[+T]:
        case Leaf(value: T)
        case Branch(value: T, left: Tree[T], right: Tree[T])

    given Functor[Tree] with 
        import Tree.*
        def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
            fa match
                case Leaf(a) => Leaf(f(a))
                case Branch(a, lt, rt) => Branch(f(a), map(lt)(f), map(rt)(f))

    @main def TryDoMapTree() =

        import Tree.*
        val g: Int => Int = _ + 100
        val tree = Branch(1, Branch(2, Leaf(3), Branch(4, Leaf(6), Leaf(7))), Leaf(5))
        println(tree)
        println(doMap(tree)(g))


    // extension methods
    import cats.syntax.functor.* 
    import Tree.*
    val testTree = Branch(1, Leaf(2), Branch(3, Leaf(4), Leaf(5)))

    def doMapShort[F[_]:Functor,A,B](init: F[A])(g: A => B): F[B] =
        init.map(g)



    @main def TryDoMapTreeShort() =

        import Tree.*
        val g: Int => Int = _ * -1 
        val tree = Branch(1, Branch(2, Leaf(3), Branch(4, Leaf(6), Leaf(7))), Leaf(5))
        println(tree)
        println(doMapShort(tree)(g))

    