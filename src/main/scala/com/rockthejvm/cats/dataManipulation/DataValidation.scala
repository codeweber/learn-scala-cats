package com.rockthejvm.cats.dataManipulation

import scala.annotation.tailrec
import cats.kernel.Semigroup
import cats.syntax.validated
import scala.util.Try

object DataValidation:


    // Cats has a Validated class, that has the same type signature as an Either
    import cats.data.Validated

    // A 'right' type
    val validValue: Validated[String, Int] = Validated.valid(42)
    // A 'left' type
    val invalidValue: Validated[String, Int] = Validated.invalid("Error!")

    val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "Meaning of life is too small")

    // Exercise 1 
    val testsToRun: List[(Int => Boolean, String)] = 
        List(
            ((x: Int) => x % 2 == 0) -> "n must be even",
            ((x: Int) => x <= 100) -> "n must be less than 100",
            ((x: Int) => x >= 0) -> "n must be non-negative",
            (testPrime) -> "n must be prime"
        )

    def testPrime(n: Int): Boolean =

        @tailrec
        def trTestPrime(d: Int): Boolean =
            if d <= 1 then
                true 
            else 
                (n % d == 0) && trTestPrime(d - 1)
        
        if (n == 0 || n == 1 || n == -1) then
            false 
        else
            trTestPrime((n/2).abs)


    def testNumber(n: Int): Either[List[String], Int] = 

        val testErrors = 
            for
                (test, message) <- testsToRun
                result = test(n)
                if !result 
            yield message

        if testErrors.isEmpty then
            Right(n)
        else
            Left(testErrors)


    def validateNumber(n: Int): Validated[List[String], Int] =

        // Define a given Semigroup, used to 'combine' different values in the Validation.combine method
        // If this wasn't supplied, the default sum semigroup would be used
        given combineIntMax: Semigroup[Int] = Semigroup.instance( (a,b) => a max b)
        
        val vs = 
            for
                (test, message) <- testsToRun 
            yield Validated.cond(test(n), n, List(message))

        vs.reduce( _ combine _ )

    @main def TryDataValidationEx1() =

        println(testNumber(2))
        println(testNumber(301))

        println("Using Validation")
        println(validateNumber(2))
        println(validateNumber(301))


    
    
    val someValidValue: Validated[List[String], Int] = Validated.valid(42)
    val someInvalidValue: Validated[List[String], Int ] = Validated.invalid(List("Error!"))
    val f: Int => Validated[List[String], String] = i => Validated.valid(s"This was the value: $i")

    @main def TryValidatedChaining() =

        // Chaining
        // Use andThen to chain validations; note this is not a true flatMap operation as monad laws are not obeyed.
        println(someValidValue.andThen(f))
        println(someInvalidValue.andThen(f))

        // ensure provides a way of also chaining; note that nothing is done on invalid values
        println(someValidValue.ensure(List("Number is too small"))( _ > 100 ))
        println(someInvalidValue.ensure(List("Number is too small"))( _ > 100 ))

        // Transformations
        println("Mapping examples")
        println(someValidValue.map(_ * 10))
        println(someValidValue.leftMap(_.length))
        println(someInvalidValue.leftMap(_.length))
        println(someValidValue.bimap(_.length, _ + 1))
        println(someInvalidValue.bimap(_.length, _ + 1))
        

        // Interplay with scala collections
        val validatedFromEither: Validated[String, Int] = Validated.fromEither(Left("There was an error!"))
        println(validatedFromEither)

        val validatedFromTry = Validated.fromTry(Try("something".toInt))
        println(validatedFromTry)


    // Exercise 2 - form vlidation
    object FormValidation:
        type FormValidation = [T] =>> Validated[List[String], T]

        def getValue(form: Map[String, String], field: String): FormValidation[(String, String)] =
            Validated.fromOption(form.get(field).map((field, _)), List(s"$field must be specified"))

        def checkLength(minLength: Int)(keyVal: (String, String)): FormValidation[(String, String)] =
            Validated.cond(keyVal._2.length >= minLength, keyVal, List(s"${keyVal._1} must have length at least $minLength") )

        def checkContains(c: Char)(keyVal: (String,String)): FormValidation[(String, String)] =
            Validated.cond(keyVal._2.contains(c), keyVal, List(s"${keyVal._1} must contain the character $c"))

        def validateForm(form: Map[String, String]): FormValidation[String] = 
            
            getValue(form, "name").andThen(checkLength(1))
            .combine(getValue(form, "email").andThen(checkContains('@')))
            .combine(getValue(form, "password").andThen(checkLength(10)))

            .map(_ => "Success")


    @main def TryFormValidation() =

        val testFormOK = Map(
            "name" -> "Dean",
            "email" -> "dean.horton@tailbounce",
            "password" -> "ihopethisislongenough"
        )

        val testFormNOK1 = Map(
            "email" -> "dean.horton@tailbounce",
            "password" -> "ihopethisislongenough"
        )

        val testFormNOK2 = Map(
            "name" -> "",
            "email" -> "dean.horton@tailbounce",
            "password" -> "ihopethisislongenough"
        )

        val testFormNOK3 = Map(
            "name" -> "",
            "email" -> "dean.horton%tailbounce",
            "password" -> "ihopethisislongenough"
        )
                
        val testFormNOK4 = Map(
            "name" -> "",
            "email" -> "dean.horton%tailbounce",
            "password" -> "tooshort?"
        )

        import FormValidation.validateForm
        println(validateForm(testFormOK))
        println(validateForm(testFormNOK1))
        println(validateForm(testFormNOK2))
        println(validateForm(testFormNOK3))
        println(validateForm(testFormNOK4))


    // Some additional syntax
    import cats.syntax.validated.*
    val somethingValid: Validated[List[String], Int] = 42.valid[List[String]]
    val somethingInvalid: Validated[List[String], Int] = List("Some ERROR!").invalid[Int]