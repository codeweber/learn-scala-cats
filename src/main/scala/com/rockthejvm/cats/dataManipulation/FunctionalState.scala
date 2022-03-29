package com.rockthejvm.cats.dataManipulation

object FunctionalState:

    type MyState = [S] =>> [A] =>> S => (S,A)

    import cats.data.State 
    val countAndSay: State[Int,String] = State(currentCount => ( currentCount+1, s"Current count $currentCount"))

    @main def TryState() =

        val (s, v) = countAndSay.run(10).value
        println(s"State: $s, Value: $v")

        // state is used for interative computations
        var a = 10
        a += 1
        val firstComputation = s"Added 1 to 10, obtained $a"
        a *= 5
        val secondComputation = s"Multiplied with 5, obtains $a"
        println((firstComputation, secondComputation))


        // pure FP version with states
        val firstST  = State((s: Int) => (s + 1, s"Added 1 to $s, obtained ${s+1}"))
        val secondST = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s*5}")) 
        val composedST =
            for
                m1 <- firstST
                m2 <- secondST
            yield (m1, m2)

        println(composedST.run(10).value)


    // Exercise 1
    case class Cart(items: List[String], total: Double)
    def addToCart(item: String, price: Double): State[Cart, Double] =
        State {
            cart =>
                val newTotal = price + cart.total
                val newItems = item :: cart.items 
                (Cart(newItems, newTotal), newTotal)
        }

    @main def TryStateEx1() =

        val itemsToPrices = Map("sugar" -> 1.10, "flour" -> 0.80, "eggs" -> 1.50)

        val initialCart = Cart(itemsToPrices.map(_._1).toList, itemsToPrices.map(_._2).sum)

        println(initialCart)

        val cartST = addToCart("cheese", 4.50)
        println(cartST.run(initialCart).value)

    // Exercise 2
    def inspect[A,B](f: A => B): State[A,B] =
        State {
            a => (a, f(a))
        }

    def get[A]: State[A,A] =
        State {
            a => (a,a)
        }

    def set[A](value: A): State[A,Unit] =
        State {
            _ => (value, ())
        }

    def modify[A](f: A => A): State[A, Unit] =
        State {
            s => (f(s), ())
        }

    import cats.data.State.*
    val program =
        for
            a <- get[Int]
            _ <- set[Int](a + 10)
            b <- get[Int]
            _ <- modify[Int](_ + 43)
            c <- inspect[Int, Int](_ * 2)
        yield (a,b,c)

    @main def TryStateEx2() =

        println(program.run(1).value)