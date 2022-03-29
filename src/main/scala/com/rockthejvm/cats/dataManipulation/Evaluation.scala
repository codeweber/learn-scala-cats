package com.rockthejvm.cats.dataManipulation

import java.lang.Thread

object Evaluation:

    import cats.Eval 

    // Evaluate instantly, store output
    val instantEval: Eval[Long] = Eval.now {
        println("Computing now!")
        System.currentTimeMillis()
    }

    // Evaluate each time required; like def
    val redoEval = Eval.always {
        println("Computing again...")
        System.currentTimeMillis()
    }

    // Evaluate when required and memoize; like lazy val
    val delayedEval = Eval.later {
        println("Computing later...")
        System.currentTimeMillis()
    }

    val composedEval =
        for
            v1 <- instantEval
            v2 <- delayedEval
        yield v1 + v2

    val composedEval2 =
        for
            v1 <- instantEval
            v2 <- redoEval
        yield v1 + v2


    @main def TryEval() =

        //Note that instantEval is executed BEFORE it is referenced in main
        println("Running TryEval...")

        println(s"instantEval call 1: ${instantEval.value}")
        println(s"instantEval call 2: ${instantEval.value}")
        
        println(s"redoEval call 1: ${redoEval.value}")
        Thread.sleep(100)
        println(s"redoEval call 2: ${redoEval.value}")
        
        Thread.sleep(100)

        println(s"delayedEval call 1: ${delayedEval.value}")
        Thread.sleep(100)
        println(s"delayedEval call 2: ${delayedEval.value}")

        Thread.sleep(100)

        println(s"composedEval call 1: ${composedEval.value}")
        Thread.sleep(100)
        println(s"composedEval call 2: ${composedEval.value}")

        Thread.sleep(100)

        println(s"composedEval2 call 1: ${composedEval2.value}")
        Thread.sleep(100)
        println(s"composedEval2 call 2: ${composedEval2.value}")

        println("...done")

    // Evals can be memoized to prevent recomputation, e.g.
    val dontRecompute = redoEval.memoize
    // another exampple
    val playGuitar = Eval
        .always { println("Step 1"); "Put in lap" }
        .map { s => println("Step 2"); s + " and then hold neck" }
        .memoize // save at this point
        .map { s => println("Step 3"); s + " and strike the strings"}

    @main def TryMemoize() =
        // Note that the first two steps are run ONLY once, due to the memoize
        // Last step is ALWAYS run
        println(playGuitar.value)
        println(playGuitar.value)
    

    // Ex2: Implement a defer method
    def defer[T](eval: => Eval[T]): Eval[T] =
        Eval
        .later( () )
        .flatMap(_ => eval)

    @main def TryEvalEx2() =
        val deferred = defer(
            Eval.now {
                println("Now!")
                42
            }
        )

        println(deferred.value)

    // Ex3: reverse a List with Evals
    def reverseList[T](list: List[T]): Eval[List[T]] =
        list match
            case Nil => Eval.later(list)
            case x :: xs => reverseList(xs).map( _ :+ x )

    // Note that the above is NOT stack safe. But it can be made tail recursive as follows:
    def reverseListStackSafe[T](list: List[T]): Eval[List[T]] =
        list match
            case Nil => Eval.later(list)
            case x :: xs => Eval.defer(reverseListStackSafe(xs).map( _ :+ x ))

    @main def TryReverseList() =

        val aList = (1 to 5).toList 
        println(aList)
        println(reverseList(aList).value)

        // Following causs stack overflow
        //println(reverseList((1 to 18000).toList).value)
        // Following works
        println(reverseListStackSafe((1 to 18000).toList).value)