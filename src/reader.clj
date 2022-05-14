(ns reader)

(def code-set #{\> \< \+ \- \. \, \[ \]})

(def operator
  {\> (fn [{:keys [pointer memory]}]
        {:pointer (inc pointer)
         :memory  memory})
   \< (fn [{:keys [pointer memory]}]
        {:pointer (dec pointer)
         :memory  memory})
   \+ (fn [{:keys [pointer memory]}]
        {:pointer pointer
         :memory (->> (get memory pointer)
                      inc
                      (assoc memory pointer))})
   \- (fn [{:keys [pointer memory]}]
        {:pointer pointer
         :memory (->> (get memory pointer)
                      dec
                      (assoc memory pointer))})
   \. (fn [{:keys [pointer memory]}]
        (println (get memory pointer))  ; TODO print ascii
        )
   \, (fn [{:keys [pointer memory]}]
        ; TODO get a character (user input)
        )
   \[ (fn [{:keys [pointer memory] :as context}]
        (if (not= 0 (get memory pointer))
          context
          context ; TODO Jump to ]
          ))
   \] (fn [{:keys [pointer memory] :as context}]
        (if (= 0 (get memory pointer))
          context
          context ; TODO Jump to [
          ))})

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

(defn codemap->loop-check [codemap]
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

(comment
  (codemap "[-] H
           ++++++++++ ++++++++++ ++++++++++ ++++++++++ ++++++++++
           ++++++++++ ++++++++++ ++ .")

  (codemap->loop-check (codemap "[-] H
                                ++++++++++ ++++++++++ ++++++++++ ++++++++++ ++++++++++
                                ++++++++++ ++++++++++ ++ ."))
  ;                              123 3  2  441
  (codemap->loop-check (codemap "[[[-]++]++[]]"))
  )
