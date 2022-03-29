package com.rockthejvm.cats.intro

object CatsIntro:

    // Import the desired type class
    import cats.Eq 

    // Import a given instance catsKernelStdOrderForInt, which is a subtype of Eq[Int]
    // Note that we can use *, but could also use the given keyword
    //import cats.instances.int.{given Eq[Int]}
    // Actually this is not necessary at all. The Eq class contains an implicit def that 
    // is used by the compile to bring Eq[Int] into scope.

    // Below uses apply method, which makes use of the 
    val intEquality = Eq[Int]
    val aTypeSafeComparison = intEquality.eqv(2,4)
    // val unsafeComparison = intEquality.eqv(2, "a string") // does not compile

    // Import extension methods
    import cats.syntax.eq.* 
    val anotherTypeSafeComp = 2 === 3
    val neqComparison = 2 =!= 3
    //val invalidComparison = 2 === "a string"

    // Extending TC operations to composite types
    //import cats.instances.list.*
    // Note that is not required to bring an Eq[List] explicitly into scope, as the Eq class
    // contains an implicit function that converts Eq[Int] into Eq[List]
    val aListComparison = List(2,3) === List(3,4)
    val neqListComparison = List(2,3) =!= List(3,4)

    val eqDoubleTypeSafe = 3.14 === 5.12
    val neqDoubleTypeSage = 3.14 =!= 5.12


    // Some new class
    case class ToyCar(model: String, price: Double)
    val eqToyCar = ToyCar("foo", 1.0) === ToyCar("bar", 0.5) // will not compile without the given below
    given Eq[ToyCar] = Eq.instance[ToyCar] {
        (car1, car2) => car1.model == car2.model
    }

    val neqToyCar = ToyCar("foo", 1.0) =!= ToyCar("bar", 0.5)

    @main def TryCatsIntro() = 
        println(aListComparison)
        println(neqListComparison)

        println(s"eqToyCar: $eqToyCar")
        println(s"neqToyCar: $neqToyCar")

