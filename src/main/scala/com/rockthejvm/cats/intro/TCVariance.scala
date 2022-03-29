package com.rockthejvm.cats.intro

object TCVariance:

    import cats.Eq
    import cats.syntax.eq.* 


    val aComparison = Option(2) === Option(3)

    //val anInvalidComparison = Some(2) === None 
    // The above will not compile, as Eq[Some[Int]] is not found
    // this is the result of the fact that Eq is not covariant, i.e. Eq[Some[Int]] is not a subtype of Eq[Option[Int]]


    // Cats uses INVARIANT typeclasses! Be aware of this!