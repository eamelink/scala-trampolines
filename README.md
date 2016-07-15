Scala Trampolines
===

This is an overview of recursion and trampolines in Scala, all revolving around a recursive definition for `even` and `odd`:

    def even(i: Int): Boolean = i match {
      case 0 => true
      case _ => odd(i - 1)
    }

    def odd(i: Int): Boolean = i match {
      case 0 => false
      case _ => odd(i - 1)
    }

This definition is not stack safe, this repo contains several variants that are stack safe, and we compare the performance.

Running
---

`sbt jmh:run` will run JMH and show you the results.
