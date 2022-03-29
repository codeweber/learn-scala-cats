package com.rockthejvm.cats.abstractMath

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

object Monads:

    // Exercise 1: Create all combinations of (number, char)
    val numbersList = List(1,2,3)
    val charsList = List('a', 'b', 'c')

    val allCombinationsList =
        for
            n <- numbersList
            c <- charsList
        yield (n,c)

    val allCombinationsListAlt =
        numbersList.flatMap(n => charsList.map(c => (n,c)))


    val numbersOption = Option(0)
    val charsOption = Option('a')

    val allCombinationsOption = 
        for
            n <- numbersOption
            c <- charsOption
        yield (n,c)

    @main def TryAllCombinations() = 

        println(numbersList)
        println(charsList)
        println(allCombinationsList)
        println(allCombinationsListAlt)

        println(allCombinationsOption)


    import cats.Monad 

    given ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(3))

    val monadFuture = summon[Monad[Future]]

    val f1 = monadFuture.pure(42)
    val f3 = monadFuture.flatMap(f1)(x => monadFuture.pure(x+1))


    // Example of generalized API
    def getPairs[F[_]:Monad,A,B](fa: F[A], fb: F[B]): F[(A,B)] =
        val m = summon[Monad[F]]
        m.flatMap(fa)(a => m.map(fb)(b => (a,b)))


    @main def TryGetPairs() =

        val x = Option(4)
        val y = Option('a')

        println(getPairs(x,y))

    // extension methods of moands are: pure, flatMap
    // pure is in the applicative package; since all monads are applicatives
    import cats.syntax.applicative.* 
    val oneOption = 1.pure[Option]
    val oneList = 1.pure[List]

    // flatMap is in the flatMap....
    import cats.syntax.flatMap.* 

    // A monad is an applicative is a functor....
    trait RockMonad[M[_]]:

        def pure[A](value: A): M[A]
        def flatMap[A,B](mA: M[A])(f: A => M[B]): M[B]
        def map[A,B](mA: M[A])(f: A => B): M[B] = flatMap(mA)( a => pure(f(a)) )   


    import cats.syntax.functor.*

    def getPairsWithForComp[F[_]:Monad,A,B](fa: F[A], fb: F[B]): F[(A,B)] =

        for
            a <- fa 
            b <- fb 
        yield (a,b)
