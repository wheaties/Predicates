<h1>The Predicates Library</h1>
<h2>Intro</h2>
The idea for this library sprang from the fact that I haven't seen any explicit support for functions that return boolean values in Scala itself. There is a generalization that we work with functions that return Booleans and any function which returns Booleans is just that, a function. However, something is lost when we work this way.  This becomes apparently so when working with the ::filter and ::filterNot methods from <a href="http://www.scala-lang.org/api/current/index.html#scala.collection.GenTraversableLike">GenTraversableLike</a>. Had there been explicit library support for Predicates then the language authors would never had thought to create two similarly named functions which conceptually do the same thing, filter a list of some type by a predicate condition. Instead they could have relied on an aptly named "Not" predicate function.

If it were only the lack of a "Not" function this library would not have happened. I would have been content to petition the language gods for its inclusion in 3.0+.  We're also lacking the tools and language support to work with predicate expressions themselves. I hope that this small library helps authors create syntactically clear and precise code when working with predicates.

<h2>What it Provides</h2>
Scala uses the familiar three operands to denote predicate logic: &&, "and;" ||, "or;" and !, "not."  These form the basis for more complex predicate expressions such as "nor" and "xor."  Unfortunately, these functions work on the Boolean value produced from the evalution of the expression and not the expression itself.  Therefore, it is difficult to create reusable, extendable and composible predicates with them unless we want to use judicious applications of boilerplate (something Scala is, in general, very light on.)

To counter act this limiation within the standard library I've written what is, essentially, a predicate template a la Scala's Function* variants.  Each Predicate (1-22, mirroring Function*) adds the following factory functions that create a new Predicates using predicate logic:
<lu>
<li>and - The resulting predicate returns true iff both predicates are true, false otherwise.</li>
<li>andNot - Similar to above except the second predicate is negated</li>
<li>nand - The negated "and." The resulting predicate returns true iff both predicates are not true</li>
<li>nor - The negated "or."  The resulting predicate returns true iff both predicates are false</li>
<li>or - The resulting predicate returns true if either predicate expression is satisfied.</li>
<li>orNot - Similar to above except the second predicate is negated.</li>
<li>xor - Exclusive "or."  The resulting predicate returns true iff one of the predicates is true but not both.</li>
</lu>

I've also included a few helper objects:
<lu>
<li>Always - 22 function variants which always return true.</li>
<li>Never - 22 function variants which always return false.</li>
<li>Not - 22 function variants which negative the enclosing Predicate expression.</li>
<li>Is - 22 helper methods a la Haskel's <a href="http://hackage.haskell.org/packages/archive/predicates/0.1/doc/html/Data-Function-Predicate.html">Data.Function.Predicate</a> to make predicate formation easier and more expressive.
</lu>

Together, I hope that these methods reduce <a href="http://en.wikipedia.org/wiki/Don't_repeat_yourself">DRY</a> and add a little syntactic sugar to make everyone's lives just a little easier.

<h2>What it Does not Provide</h2>
This library is not a Prolog logic library or logic solver. It does not contain anything close to Clojure's fantastic <a href="https://github.com/clojure/core.logic">core.logic</a> which I highly recommend checking out even if it is written in a dirty dynamic language. It is not a truth engine or compile time evaluation engine. 

Originally it contained my first attempt at adding in predicated application and compoisition (see: <a href="http://stackoverflow.com/q/2295013/178060">here</a>). However, my implementation was non-optimal, too inflexible, and too narrow in focus to be much use to anyone. I've since removed it.  It may come back, although as it's own library.

<h2>Future Plans</h2>
There aren't many more plans for this library other than to add any more specific or helpful methods related to working with predicates and predicate expressions. I am open to suggestions and the needs of anyone who might want to use it.
