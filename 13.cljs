(require '[clojure.string :as str])

(def test-data ["939"
                "7,13,x,x,59,x,31,19"])

(def data ["1002576"
           "13,x,x,x,x,x,x,37,x,x,x,x,x,449,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,773,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17"])

(defn str->int [s] (js/parseInt s 10))

(defn parse [input] {:time (str->int (first input))
                     :buses (map str->int (filter (partial not= "x") (str/split (second input) #",")))})

(defn next-starts
  [{time :time buses :buses}]
  (map #(identity {:bus % :at (+ time (- % (mod time %)))}) buses))

(defn next-bus [starts] (reduce (partial min-key #(% :at)) starts))

(println (let [parsed-data (parse data)
               nextbus (next-bus (next-starts parsed-data))
               time (parsed-data :time)]
           (* (nextbus :bus) (- (nextbus :at) time)))) ;; 3865

(def solved-with-chinese-remainder-theroem 415579909629976)
(println solved-with-chinese-remainder-theroem);; 415579909629976
