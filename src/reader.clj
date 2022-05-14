(ns reader)

(def code-set #{\> \< \+ \- \. \, \[ \]})

(defn codemap [input-codes]
  (let [codes (vec (seq input-codes))]
    (loop [line 1
           col 1
           cursor 0
           result []]
      (let [a-char (get codes cursor)]
        (cond
          (nil? a-char)
          result

          (= \newline a-char)
          (recur (inc line) 1 (inc cursor) result)

          (code-set a-char)
          (recur line (inc col) (inc cursor) (conj result
                                                   {:line line
                                                    :col col
                                                    :cursor cursor
                                                    :code a-char}))

          :else
          (recur line (inc col) (inc cursor) result))))))

(def stack<0> 0)
(def stack<1> 1)
(def find<ON> true)
(def find<OFF> false)

(defn codemap->linked-codemap [codemap]
  (loop [index 0
         stack-count stack<0>
         find-mode? find<OFF>
         saved-index 0
         codemap codemap]
    (let [code (get codemap index)
          a-char (:code code)
          open-bracket? (= \[ a-char)
          close-bracket? (= \] a-char)]
      (cond
        (nil? code)
        codemap

        (:pair-cursor code)
        (recur (inc index) stack-count find-mode? saved-index codemap)

        find-mode?
        (cond
          open-bracket?
          (recur (inc index) (inc stack-count) find-mode? saved-index codemap)

          (and close-bracket? (> stack-count stack<1>))
          (recur (inc index) (dec stack-count) find-mode? saved-index codemap)

          (and close-bracket? (= stack-count stack<1>))
          (let [code-close (assoc code :pair-cursor saved-index)
                code-open (assoc (get codemap saved-index) :pair-cursor index)
                new-codemap (-> codemap
                                (assoc index code-close)
                                (assoc saved-index code-open))]
            (recur (inc saved-index) stack<0> find<OFF> saved-index new-codemap))

          :else
          (recur (inc index) stack-count find-mode? saved-index codemap))

        (not find-mode?)
        (cond
          close-bracket?
          (throw (Exception. "Invalid ] " code))

          open-bracket?
          (do
            (recur (inc index) stack<1> find<ON> index codemap))

          :else
          (recur (inc index) stack-count find-mode? index codemap))))))

(defn excute! [code-text]
  (let [linked-codemap (-> code-text
                           codemap
                           codemap->linked-codemap)]
    (loop [code-index 0
           memory-pointer 0
           memory (vec (replicate 32768 0))]
      (let [code (get linked-codemap code-index)
            a-char (:code code)
            value (get memory memory-pointer)]
        (condp = a-char
          \>
          (recur (inc code-index) (inc memory-pointer) memory)
          \<
          (recur (inc code-index) (dec memory-pointer) memory)
          \+
          (recur (inc code-index) memory-pointer (assoc memory memory-pointer (inc value)))
          \-
          (recur (inc code-index) memory-pointer (assoc memory memory-pointer (dec value)))
          \.
          (do
            (print (char (get memory memory-pointer)))
            (recur (inc code-index) memory-pointer memory))
          \,
          (do
            (print "TODO: get user input")
            (recur (inc code-index) memory-pointer memory))
          \[
          (if (= 0 value)
            (recur (:pair-cursor code) memory-pointer memory)
            (recur (inc code-index) memory-pointer memory))
          \]
          (if (not= 0 value)
            (recur (:pair-cursor code) memory-pointer memory)
            (recur (inc code-index) memory-pointer memory))
          ; default
          (println ""))))))

(comment
  (codemap "[-] H
           ++++++++++ ++++++++++ ++++++++++ ++++++++++ ++++++++++
           ++++++++++ ++++++++++ ++ .")

  (codemap->linked-codemap (codemap "[-] H
                                ++++++++++ ++++++++++ ++++++++++ ++++++++++ ++++++++++
                                ++++++++++ ++++++++++ ++ ."))
  ;                              123 3  2  441
  (codemap->linked-codemap (codemap "[[[-]++]++[]]"))
  (excute! "++++++++++
[>+++++++>++++++++++>+++>+<<<<-]
>++.>+.+++++++..+++.>++++++++++++++.------------.<<+++++++++++++++.>.+++.------.--------.>+.")
  )
