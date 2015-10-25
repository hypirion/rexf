# rex

**Re**cursive **xf**orms – generalised transducers and reducers for the Clojure
programming language.

## ...what?

Clojure 1.7 introduced transducers, which are functions that take a reducer
function and returns a reducer function. This in itself sounds straightforward
enough, but a lot of additional complexity was added to support stateful
reducers. 


The ones coming from take, drop, partition-by and similar. Whereas
stateful reducers were available pre 1.7, they were frowned upon; you could
(usually) assume that if you could use a function for reducing, then you could
use it repeatedly, without surprings results. However, as with 1.7, a reducing
function cannot be assumed to be stateless anymore:

```clj
;; A transducer is reducer -> reducer, so this is a reducer, right?
user=> (def vec10 ((take 10) conj))
#'user/vec10
user=> (reduce vec10 [] (range 20))
[0 1 2 3 4 5 6 7 8 9]
user=> (reduce vec10 [] (range 20))
[]
```

In addition, there are strange mismatches between transducers and reducers,
which initially does not seem to make much sense:

```clj
;; (reduce rf coll) != (transduce identity rf coll)
user=> (reduce conj (range 10))

ClassCastException java.lang.Long cannot be cast to clojure.lang.IPersistentCollection  clojure.core/conj--4338 (core.clj:82)
user=> (transduce identity conj (range 10))
[0 1 2 3 4 5 6 7 8 9]

;; (reduce rf (rf) coll) != (transduce identity rf coll)
(def rf-conj! (completing conj! persistent!))

user=> (reduce rf-conj! (rf-conj!) (range 10))
#object[clojure.lang.PersistentVector$TransientVector 0x2251dd42 "clojure.lang.PersistentVector$TransientVector@2251dd42"]
user=> (transduce identity rf-conj! (range 10))
[0 1 2 3 4 5 6 7 8 9]
```

## How

Trick:


## Usage

```clj
(require '[com.hypirion.com.rexf :as rexf])

;; reduce is now a function you have to use from this library, and you need to
wrap reducers.
user=> (rexf/reduce (rexf/stateless-rf conj) coll)
;; == (transduce identity conj coll)
(rexf/reduce (rexf/stateless-rf conj) [] coll)
;; == (transduce identity conj coll)

```

FIXME

## License

Copyright © 2015 Jean Niklas L'orange

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
