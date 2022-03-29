package com.rockthejvm.cats.abstractMath
import cats.instances.int

object Semigroups:

    // Semigroups combine elements of the same type.
    // Basically, a semigroup is a set of elements with an associated binary operation, which 
    // is closed, i.e. returns a member of the set and associative
    import cats.Semigroup

    //Semigroup for int is given by addition (what about multiplication?)
    val naturalIntSemigroup = Semigroup[Int]
    val intCombination = naturalIntSemigroup.combine(2,46)
 
    //Semigroup for strings is given by concat
    val naturalStringSemigroup = summon[Semigroup[String]]
    val stringCombination = naturalStringSemigroup.combine("foo", "bar")


    def reduceList[A:Semigroup](list: List[A]): A = 
        val sg = summon[Semigroup[A]]
        list.reduce(sg.combine)

    @main def TrySemiGroups() = 
        println(intCombination)
        println(stringCombination)

        val lInts = (1 to 100).toList 
        println(reduceList(lInts))

        val lStrings = "The quick brown fox jumped over the lazy dog".split(' ').toList
        println(reduceList(lStrings))


    // Exercise 1: Support a new type
    case class Expense(id: Long, amount: Double)

    object Expense:

        given Semigroup[Expense] = Semigroup.instance {
            case (Expense(id1, amount1), Expense(id2, amount2)) => Expense(id1 max id2, amount1+amount2)
        } 

    @main def SemigroupsEx1() = 

        val l = List(Expense(1, 1.50), Expense(3, 4.50), Expense(10, 10.00))
        println(reduceList(l))


    //Extension methods for semigroup
    import cats.syntax.semigroup.* 
    val expensesConcat = Expense(1,5.0) |+| Expense(4,7.0)

    @main def TrySGExtensionMethods() = 

        println(expensesConcat)
