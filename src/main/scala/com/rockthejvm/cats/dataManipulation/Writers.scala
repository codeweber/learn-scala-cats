package com.rockthejvm.cats.dataManipulation

object Writers:

    import cats.data.Writer 

    // T =>> Writer[A,T]  is a monad...

    // 1. Define a Writer
    val aWriter: Writer[List[String], Int] = Writer(List("Started..."), 45)

    // 2. Modify
    // a. Using map (Functor)
    val anIncreasedWriter = aWriter.map(_ + 1)
    val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting")
    val aWriterWithBiMap = aWriter.bimap(_ :+ "changing both log and number", _ + 1)
    val aWriterWithMapBoth = aWriter.mapBoth {
        (logs, value) =>
            (logs :+ "using mapBoth", value + 10)
    }

    // b. Using flatMap (Monad)
    val writerA = Writer(Vector("Log A1", "Log A2"), 1)
    val writerB = Writer(Vector("Log B1"), 10)

    val writerC =
        for
            va <- writerA 
            vb <- writerB 
        yield va + vb
    // this is equivalent to:
    // writerA.flatMap( va => writerB.map( vb => vb + va) )
    // the flatMap combines the logs, contain in Vector, using the Semigroup instance for Vector, which has the
    // combine method for concatentation.

    // c. Resetting the logs
    val anEmptyWriter = aWriter.reset // uses the monoid empty value for the L type

    // 3. Dump logs and/or value
    val valueOut = aWriter.value 
    val log = aWriter.written
    val (logs, value) = aWriter.run 

    @main def TryWriter() =

        println(writerC.written)
        println(writerC.value)

        println(anEmptyWriter.run)


    // TODO 1: rewrite a function which "prints" with Writers
    def countAndSay(n: Int): Unit =
        if n <= 0 then
            println("Starting!")
        else
            countAndSay(n-1)
            println(n)


    def countAndLog(n: Int): Writer[Vector[String], Int] =
        
        if n <= 0 then
            Writer(Vector("Starting!"), n)
        else 
            countAndLog(n-1).flatMap(_ => Writer(Vector(s"$n"), n))

    def countAndLogTR(n: Int): Writer[Vector[String], Int] =

        def getWriter(n: Int): Writer[Vector[String], Int] =
            if n <= 0 then
                Writer(Vector("Starting!"), 0)
            else
                Writer(Vector(s"$n"), n)


        @annotation.tailrec
        def helper(n: Int, w: Writer[Vector[String], Int]): Writer[Vector[String], Int] = 

            if n <= 0 then
                getWriter(0).flatMap(_ => w)
            else
                helper(n-1, getWriter(n).flatMap(_ => w))

        
        if n <= 0 then
            getWriter(0)
        else
            helper(n-1, getWriter(n))

    @main def TryWriterEx1() =

        val n = 10
        countAndSay(n)

        println(countAndLog(n).run)

        println(countAndLogTR(n).run)

    // Exercise 2
    // Rewrite the function using Writer
    def naiveSum(n: Int): Int =

        if n <= 0 then
            0
        else 
            println(s"Now at $n")
            val lowerSum = naiveSum(n-1)
            println(s"Computed sum(${n-1}) = $lowerSum")
            lowerSum + n

    def naiveSumWriter(n: Int): Writer[Vector[String], Int] =

        def helper(n: Int, w: Writer[Vector[String], Int]): Writer[Vector[String], Int] = 

            if n <= 0 then 
                w 
            else
                helper(n-1, w.mapWritten( _ :+ s"Now at $n") ).flatMap( ls => Writer(Vector(s"Calculated sum(${n-1}) = $ls"), ls+n))


        helper(n, Writer(Vector.empty[String], 0))


    def naiveSumWriterFor(n: Int): Writer[Vector[String], Int] =

        if n <= 0 then
            Writer(Vector.empty[String], 0)
        else
            for 
                _ <- Writer(Vector(s"Now at $n"), 0)
                l <- naiveSumWriterFor(n-1)
                _ <- Writer(Vector(s"Calculated sum(${n-1}) = $l"), 0)
            yield l + n

    @main def TryWriterEx2() =

        println(s"naiveSum: ${naiveSum(5)}")

        println(s"naiveSumWriter: ${naiveSumWriter(5).run}")

        println(s"naiveSumeWriterFor: ${naiveSumWriterFor(5).run}")