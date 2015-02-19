(ns rheinjug.core
  (:use clojure-csv.core)
  (:require [clojure.test :as t]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(defn my-sort [coll] (seq (into (sorted-set) coll)))

(set! *print-length* 30)

(comment

  ;; Clojure is a dialect of the Lisp programming language created by
  ;; Rich Hickey. Clojure is a general-purpose programming language. It
  ;; runs on the JVM, CLR, and JavaScript engines.
  ;; Clojure's focus on programming with immutable values and explicit
  ;; progression-of-time constructs are intended to facilitate the
  ;; development of more robust programs, particularly multithreaded
  ;; ones.
  ;; (Quelle: Wikipedia)


  ;; * LISP
  ;; * JVM
  ;; * Funktional, immutable values
  ;; * Robustheit
  ;; * Nebenläufigkeit


  ;; Basisdatentypen

  ;; Zahlen
  3

  (type 3)

  ;; Strings
  "foo"

  (type "foo")

  ;; Keywords (in Ruby: symbols)
  :foo
  :π!

  (type :foo)

  ;; Symbol
  inc
  even?

  (type 'inc)
  (type inc)

  ;; Es gibt noch mehr, aber die werden wir nicht brauchen
  ;; Character, Ratio, Double, BigInt, BigDecimal, Regex

  ;; Zusammengesetzte Datentypen

  ;; Vektoren

  [1, :zwei, "drei"]

  ;; Listen
  '(+ 1 2 3)

  ;; Listen sind speziell!
  ;; Listen werden als Funktionsaufrufe verwendet
  (min 5 2 3)

  ;; (Funktionsobjekt Argumente...)

  ;; Regel für Lisper-Hasser: Schieb die Klammer eins nach rechts
  ;; min(5,2,3)

  ;; Sets
  #{:eins, 2, 3}

  ;; Map
  {:1 2, "foo" :bar}

  ;; Alles kann geschachtelt werden
  {1 "foo", [] {}, [1] #{{:a :b} #{1 4}}}

  ;; Wir haben jetzt fast die komplette relevante Clojure Syntax
  ;; und Funktionsaufrufe gesehen.

  ;; Clojure ist ein bischen wie Schach.
  ;; Es ist ziemlich leicht, die Regeln zu lernen.
  ;; Java ist eher wie Cricket ;-)






  ;; Clojure Datenstrukturen sind immutable + schnell!

  (def v1 (into [] (range 1e7)))

  (nth v1 1973)

  (def v2 (assoc v1 1973 :foo))

  (nth v2 1973)
  (nth v1 1973)

  (def vs (for [i [71 99991 3485 345867 2353 9494 201 0 9993 9684]]
            (assoc v1 i :foo)))


  (nth (nth vs 3) 345867)
  (nth (nth vs 6) 345867)

  ;; Clojure Datenstrukturen implementieren die Java Interfaces, die man
  ;; erwarten würde! Tatsächlich sind die Datenstrukturen in Java
  ;; implementiert!

  (.isEmpty [])
  (.isEmpty v1)

  ;; Java Interop ist einfach!

  ;; "foo".equals("bar")
  (.equals "foo" "bar")

  (import java.util.ArrayList)

  ;; Mutable objects mögen funktionale Programmierer gar nicht!
  (def a (new ArrayList))
  (.add a 10)
  a

  ;; Das mögen selbst Java Enticker nicht ;-)
  (import (javax.swing JFrame JLabel))

  (doto (JFrame.)
    (.add (JLabel. "Hallo rheinjug!"))
    (.setVisible true)
    (.setSize 200 100)
    .show)


  ;; JFrame frame = new JFrame();
  ;; frame.add(new JLabel("Hallo rheinjug!");
  ;; frame.setVisible(true);
  ;; frame.setSize(200,100);
  ;; frame.show()


  ;; Neulich in Java
  ;; customers.getByName("Bendisposto").getAddress().getPostalCode()

  ;; In Clojure
  ;; (.. customers (getByName "Bendisposto") getAddress getPostalCode)

  ;; Für Lisp-Hasser: Zählt mal die Klammern!


  (def plus1 (fn [x] (+ x 1)))
  ;; Das lambda Symbol erzeugt mein Editor. Eigentlich steht da fn


  (plus1 19)

  (defn plus1 [x] (+ x 1))

  (plus1 2)

  ;; Funktionen als Parameter (Java 8 lässt grüssen)
  ;; In älteren Java-Versionen ging das mit anonymen Klassen
  ;; Beispiel: Listener

  (map plus1 [1 2 3])

  (filter even? (range 20))

  (reduce + (range 101))

  (reduce
   (fn [akkumulator element]
     (println :call akkumulator element)
     (+ akkumulator element))
   20
   (range 5))

  ;; Funktionen als Rückgabewert

  (defn mk-incer [n]
    (fn [x] (+ n x)))

  (def plus20 (mk-incer 20))

  (plus20 5)

  ;; *****************  EXAMPLE BASED TESTING   *****************


  ;; @edial:
  ;; QA Engineer walks into a bar.
  ;; Orders a beer.
  ;; Orders 0 beers.
  ;; Orders 999999999 beers.
  ;; Orders a lizard.
  ;; Orders -1 beers.
  ;; Orders a sfdeljkn


  (t/is (= 1 2))
  (t/is (= 1 1))





  (t/are [x y] (= x y)
         2 2
         (my-sort [1 2]) [1 2]
         (my-sort [3 1 2]) [1 2 3]
         (my-sort [1 3 2]) (my-sort [3 2 1]))


  ;; Reicht das an Tests?


  (t/is (= (my-sort []) []))

  ;; Sonst noch was?


  ;; Generatives Testen = Generieren + Testen


  gen/nat

  (gen/sample gen/nat)
  (gen/sample gen/boolean)
  (gen/sample gen/keyword)
  (gen/sample gen/int)
  (gen/sample gen/any)

  (last (gen/sample gen/any 50))

  (gen/sample gen/any-printable) ;; no bell!
  (gen/sample gen/string 20)
  (gen/sample gen/string-ascii 20)
  (gen/sample gen/string-alphanumeric 20)

  (gen/sample (gen/choose 100 250))
  (gen/sample (gen/elements [-1 2 -3 4]))

  ;; Composed
  (gen/sample (gen/vector gen/boolean))
  (gen/sample (gen/vector gen/boolean 3))
  (gen/sample (gen/tuple gen/int gen/boolean))
  (gen/sample (gen/map gen/keyword gen/int))

  ;; Der langweiligste Generator aller Zeiten
  (gen/sample (gen/return :boring))

  ;; Für unseren Sortieralgorithmus
  (def sort-input (gen/vector gen/int))
  (gen/sample sort-input)

  ;; Filter
  (gen/sample (gen/not-empty (gen/vector gen/boolean)))
  (gen/sample (gen/such-that even? gen/nat) 30)

  (let [g (gen/such-that
           (fn [e] (not= e 5))
           (gen/choose 0 10))
        sample (gen/sample g 1000)]
    (frequencies sample))

  ;; Higher order Generatoren

  (let [x (gen/sample
           (gen/frequency [[70 (gen/return :kopf)]
                           [30 (gen/return :zahl)]]) 1000)]
    (frequencies x))

  (gen/sample (gen/elements [:fizz 4 :buzz]))
  (gen/sample (gen/one-of [gen/int gen/boolean]))

  (gen/sample
   (gen/one-of [(gen/return :fizz) (gen/return 4) (gen/return :buzz)]))


  ;; Transformation
  (gen/sample (gen/fmap str gen/int))

  ;; das ist etwas Anderes als:
  (map str (gen/sample gen/int))


  ;; Komplexere Generatoren
  ;; Ein Tupel bestehend aus einem Vektor und einem Element aus diesem Vektor

  (def vector-gen (gen/not-empty sort-input))
  (gen/sample vector-gen)

  ;; Eine Möglichkeit fmap, hat aber ein Problem!
  (gen/sample
   (gen/fmap (fn [v] [v (rand-nth v)]) vector-gen))


  ;; Wenn wir einen Vektor hätten
  (defn tuple-from-vector-gen [v]
    (gen/tuple (gen/return v)
               (gen/elements v)))

  (gen/sample (tuple-from-vector-gen [1 2 3]))

  ;; Jetzt müsste man nur den Input für tuple-from-vector-gen erzeugen

  (def complex-gen
    (gen/bind vector-gen tuple-from-vector-gen))

  ;; Bind nimmt einen Generator g und eine Funktion, die aus einem von g
  ;; erzeugten Wert einen Generator erzeugt

  (gen/sample complex-gen 20)




  ;; Für die Experten: gen/bind und gen/return heissen nicht zufällig so!




  ;; Generatives Testen = Generieren + Testen

  ;; Zum Testen brauchen wir eine Möglichkeit festzustellen, dass eine
  ;; Funktion korrekte Ergebnisse liefert


  ;; Sortieren
  ;; Eingabe: eine Sequenz v(1), v(2) ... v(n) von Werten, die miteinander vergleichbar sind
  ;; Ausgabe: eine Sequenz der gleichen Werte  so dass für
  ;; alle Indexpositionen i,j gilt i <= j -> v(i) <= v(j)


  ;; Äquivalent:
  ;; * Alle Elemente sind in aufsteigender Reihenfolge sortiert
  ;; * Alle Elemente sind nach dem Sortieren immer noch da


  (defn sortiert? [v]
    (apply <= v))


  (sortiert? [1 2 3])
  (apply <= 1 [2 3])
  (<= 1 2 3)


  (def sortiert-prop
    (prop/for-all [v sort-input]
                  (t/is (sortiert? (my-sort v)))))

  (tc/quick-check 100 sortiert-prop)

  ;; Wir könnten das in sortiert? beheben
  (defn sortiert? [v]
    (or (empty? v)
        (apply <= v)))

  (tc/quick-check 100 sortiert-prop)

  (defn sort-equiv? [v]
    (= v (sort v)))

  (def sort-equiv-prop1
    (prop/for-all [v sort-input]
                  (sort-equiv? (my-sort v))))

  (tc/quick-check 100 sort-equiv-prop1)

  (my-sort [])
  (sort [])


  (defn my-sort2 [coll] (into '() (my-sort coll)))

  (my-sort2 [])

  (def sort-equiv-prop2
    (prop/for-all [v sort-input]
                  (sort-equiv? (my-sort2 v))))

  (tc/quick-check 100 sort-equiv-prop2)

  (my-sort2 [0 1])

  (defn my-sort3 [coll] (into [] (my-sort coll)))

  (def sort-equiv-prop3
    (prop/for-all [v sort-input]
                  (sort-equiv? (my-sort3 v))))

  (tc/quick-check 100 sort-equiv-prop3)
  (tc/quick-check 10000 sort-equiv-prop3)


  ;; da war doch noch was ...


  (defn permutation? [v1 v2]
    (= (frequencies v1) (frequencies v2)))

  (def permutation-prop
    (prop/for-all [v sort-input]
                  (permutation? v (my-sort3 v))))

  (tc/quick-check 100 permutation-prop)


  ;; ok, ich geb's auf
  (def my-sort3 sort)

  (tc/quick-check 100 permutation-prop)


  ;; OK, aber das ist ziemlich popelig!

  ;; Real world Beispiel: Performance Optimierung


  (-> #{} (conj 3) (conj 4) (conj 5))

  (-> #{} transient (conj! 3) (conj! 4) (conj! 5))
  (-> #{} transient (conj! 3) (conj! 4) (conj! 5) persistent!)


  ;; Wie testen wir, ob die Optimierung korrekt ist?
  ;; PS Nicht benutzen, das stammt aus grauer Vorzeit!


  ;; Es ist ziemlich schwierig einen Generator zu schreiben, der nur
  ;; korrekte Programme erzeugt.

  ;; Es ist leichter Sequenzen von Aktionen naiv zu generieren und dann mit
  ;; einer Art Interpreter abzuarbeiten und gleichzeitig zu reparieren

  ;; Aktionen haben die Form [:conj Zahl], [:disj Zahl], [:trans] oder
  ;; [:pers]


  ;; Der Interpreter muss schauen, ob die Collection gerade transient
  ;; ist (dann dürfen wir transient nicht nochmal aufrufen. Und wenn
  ;; die Collection am Ende noch transient ist, muss sie persistiert
  ;; werden.

  (defn transient? [x]
    (instance? clojure.lang.ITransientCollection x))

  (instance? java.util.List [2 4])
  (instance? String [2 4])


  (def gen-mods
    (gen/not-empty
     (gen/vector
      (gen/one-of
       [(gen/elements [[:trans] [:pers]])
        (gen/tuple (gen/elements [:conj :disj]) gen/int)]))))

  (gen/sample gen-mods)


  (defn run-action [c [f & [arg]]]
    (condp = [(transient? c) f]
      [true   :conj]          (conj! c arg)
      [true   :disj]          (disj! c arg)
      [true   :trans]         c
      [true   :pers]          (persistent! c)

      [false  :disj]          (disj c arg)
      [false  :conj]          (conj c arg)
      [false  :trans]         (transient c)
      [false  :pers]          c))

  (run-action #{} [:conj 2])

  (defn apply-actions [coll actions]
    (reduce run-action coll (concat actions [[:pers]])))


  (apply-actions #{} [[:conj 3]])
  (apply-actions #{} [[:conj 2] [:trans] [:conj 4] [:trans] [:pers]])
  (apply-actions #{} [[:conj 2] [:trans] [:conj 4] [:pers] [:trans]])

  (defn filter-actions [actions]
    (filter (fn [[a & args]]
              (#{:conj :disj} a))
            actions))

  (filter-actions [[:conj 2] [:trans] [:conj 4] [:pers] [:trans]])


  ;; Was machen wir?
  ;; 1) Generiere Actions mit :trans und :pers
  ;; 2) Lasse die Actions auf dem leeren Set laufen.
  ;;    Falsche Sequenzen werden vom Interpreter automatisch repariert
  ;; 3) Filtere die Actions und lasse sie auf dem leeren Set laufen
  ;; 4) Die Resultate müssen übereinstimmen

  (def transient-property
    (prop/for-all
     [a gen-mods]
     (= (apply-actions #{} a)
        (apply-actions #{} (filter-actions a)))))


  (tc/quick-check 100 transient-property)


  (set! *print-length* nil)
  (:fail (tc/quick-check 400 transient-property :seed 1422983037254))

  (-> (tc/quick-check 400 transient-property :seed 1422983037254) :shrunk :smallest)

  (def a (-> #{} (conj -49) (conj 48) transient (disj! -49) persistent! (conj -49)))
  a
  (def b (-> #{} (conj -49) (conj 48) (disj -49) (conj -49)))
  b


  (= a b)

  (-> (tc/quick-check 5000 transient-property) :shrunk :smallest)

  ;; Wat?

  ;; Bug in Clojure!
  ;; http://dev.clojure.org/jira/browse/CLJ-1285
  ;; Wurde tatsächlich mit dieser Methode gefunden!
  ;; https://groups.google.com/forum/#!msg/clojure-dev/HvppNjEH5Qc/1wZ-6qE7nWgJ

  (.hashCode 48)
  (.hashCode -49)



  ;; Hätte von euch irgendjemand diesen Testcase geschrieben?



  ;; Fazit: Quickcheck ist billig und findet Fälle, an die man nicht
  ;; denkt! Sollte zusammen mit normalen Unit-Tests benutzt werden.
  


  


  ;; Und Java?

  

  ;; OO Sprachen sind etwas schwieriger. In Clojure ist der Vorrat an
  ;; Datenstrukturen eingeschränkt. In Java ist das erste was gemacht
  ;; wird neue Datenstrukturen einzuführen.






  ;; Trotzdem:
  ;; https://github.com/pholser/junit-quickcheck

  ;; Ich hab es nie ausprobiert, es gibt aber Aktivitäten im Repo




  ;; Code:  https://github.com/bendisposto/quickcheck_rheinjug
  



  )
