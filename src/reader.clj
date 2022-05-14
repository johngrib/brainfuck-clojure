(ns reader)

(def operator
  {\> (fn [{:keys [pointer memory]}]
        {:pointer (inc pointer)
         :memory  memory})
   \< (fn [{:keys [pointer memory]}]
        {:pointer (dec pointer)
         :memory  memory})
   \+ (fn [{:keys [pointer memory]}]
        (let [value (get memory pointer)]
          {:pointer pointer
           :memory (assoc memory
                          pointer
                          (inc value))}))
   \- (fn [{:keys [pointer memory]}]
        (let [value (get memory pointer)]
          {:pointer pointer
           :memory (assoc memory
                          pointer
                          (dec value))}))
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

