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

(defn code-index++ [{:keys [code-index] :as context}]
  (assoc context :code-index (inc code-index)))

(defn memory-pointer++ [{:keys [memory-pointer] :as context}]
  (assoc context :memory-pointer (inc memory-pointer)))

(defn memory-pointer-- [{:keys [memory-pointer] :as context}]
  (assoc context :memory-pointer (dec memory-pointer)))

(defn value++ [{:keys [memory memory-pointer] :as context}]
  (let [value (get memory memory-pointer)
        next-memory (assoc memory memory-pointer (inc value))]
    (assoc context :memory next-memory)))

(defn value-- [{:keys [memory memory-pointer] :as context}]
  (let [value (get memory memory-pointer)
        next-memory (assoc memory memory-pointer (dec value))]
    (assoc context :memory next-memory)))

(defn jump-to-pair [{:keys [codemap code-index] :as context}]
  (let [code (get codemap code-index)
        pair-cursor (:pair-cursor code)]
    (assoc context :code-index pair-cursor)))

(defn excute! [code-text]
  (let [linked-codemap (->> code-text
                            codemap
                            codemap->linked-codemap)]
    (loop [context {:codemap linked-codemap
                    :code-index 0
                    :memory-pointer 0
                    :memory (vec (repeat 32768 0))}]
      (let [code-index (:code-index context)
            memory (:memory context)
            memory-pointer (:memory-pointer context)
            code (get linked-codemap code-index)
            a-char (:code code)
            value (get memory memory-pointer)]
        (condp = a-char
          \>
          (recur (->> context
                      code-index++
                      memory-pointer++))
          \<
          (recur (->> context
                      code-index++
                      memory-pointer--))
          \+
          (recur (->> context
                      code-index++
                      value++))
          \-
          (recur (->> context
                      code-index++
                      value--))
          \.
          (do
            (print (char (get memory memory-pointer)))
            (recur (->> context
                        code-index++)))
          \,
          (do
            (print "TODO: get user input")
            (recur (->> context
                        code-index++)))
          \[
          (if (= 0 value)
            (recur (->> context
                        jump-to-pair))
            (recur (->> context
                        code-index++)))
          \]
          (if (not= 0 value)
            (recur (->> context
                        jump-to-pair))
            (recur (->> context
                        code-index++)))
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
