package com.rockthejvm.cats.abstractMath

object Monoids:

    import cats.Semigroup
    import cats.syntax.semigroup.* 

    import cats.Monoid 
    import cats.syntax.monoid.*


    def combineFold[T:Monoid](xs: List[T]) =
        val m = summon[Monoid[T]]
        xs.foldLeft(m.empty)(m.combine)


    @main def TryMonoids() =

        val xs = (1 to 100).toList 
        println(combineFold(xs))

        val ys = (1 to 100).toList.map(Option(_)) 
        println(combineFold(ys))

        val mOption = summon[Monoid[Option[Int]]]
        println(mOption.combine(Option.empty[Int], Option(5)))


    // Exercise 2: combine a list of phonebook :: Map[String,Int]
    type Phonebook = Map[String,Int]
    val pbs = List(
        Map(
            "Dean" -> 100
        ),
        Map(
            "Suze" -> 200
        ),
        Map(
            "Dean" -> 101,
            "Neve" -> 300
        )
    )
    def combinePhonebooks(xs: List[Phonebook]): Phonebook =
        combineFold(xs)

    @main def TryMonoidsEx2() =

        println(combinePhonebooks(pbs))


    // Exercise 3: combine a list of shopping carts
    case class ShoppingCart(items: List[String], total: Double)
    def checkout(carts: List[ShoppingCart]): ShoppingCart = 
        given Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](ShoppingCart(List.empty[String], 0.0), (a,b) =>
            ShoppingCart(a.items ++ b.items, a.total + b.total)
        )
        combineFold(carts)

    @main def TryMonoidShoppingCart() = 

        val carts = List(
            ShoppingCart(List("food"), 1.0),
            ShoppingCart(List("cats"), 10.0)
        )

        println(checkout(carts))

