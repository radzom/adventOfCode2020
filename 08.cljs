(require '[clojure.string :as str])

(def sourcecode ["acc +0"
                 "jmp +516"
                 "acc -16"
                 "nop +326"
                 "jmp +366"
                 "jmp +400"
                 "jmp +494"
                 "acc -5"
                 "jmp +475"
                 "acc +29"
                 "nop +24"
                 "jmp +186"
                 "jmp -5"
                 "acc +50"
                 "acc +49"
                 "acc +49"
                 "nop +507"
                 "jmp +236"
                 "acc -4"
                 "acc +50"
                 "acc +48"
                 "jmp +293"
                 "jmp +461"
                 "acc +48"
                 "acc +2"
                 "jmp +241"
                 "acc +38"
                 "jmp +544"
                 "jmp +1"
                 "jmp +382"
                 "acc -4"
                 "acc -10"
                 "acc +44"
                 "jmp +444"
                 "jmp +1"
                 "acc +20"
                 "acc -1"
                 "acc +25"
                 "jmp +332"
                 "acc +18"
                 "jmp +184"
                 "acc +1"
                 "acc +20"
                 "acc -7"
                 "acc -12"
                 "jmp +84"
                 "acc +30"
                 "jmp -19"
                 "nop +491"
                 "acc +37"
                 "acc +44"
                 "jmp +220"
                 "acc +3"
                 "acc +19"
                 "jmp +485"
                 "jmp +222"
                 "nop +264"
                 "acc +4"
                 "jmp +151"
                 "nop -36"
                 "acc +48"
                 "jmp +356"
                 "acc +28"
                 "acc +29"
                 "nop +397"
                 "jmp +168"
                 "nop +115"
                 "acc -13"
                 "acc -11"
                 "jmp +470"
                 "jmp +201"
                 "acc +48"
                 "acc -6"
                 "acc -18"
                 "acc -13"
                 "jmp +442"
                 "jmp +266"
                 "acc +10"
                 "acc +30"
                 "acc -7"
                 "acc -8"
                 "jmp +380"
                 "acc +1"
                 "nop +106"
                 "acc +9"
                 "acc -11"
                 "jmp -79"
                 "jmp +166"
                 "acc -9"
                 "jmp +300"
                 "acc +39"
                 "acc +1"
                 "acc +27"
                 "acc +26"
                 "jmp +367"
                 "jmp +291"
                 "acc +18"
                 "jmp +100"
                 "acc +25"
                 "acc +43"
                 "acc -8"
                 "acc -12"
                 "jmp +475"
                 "acc +15"
                 "jmp +492"
                 "acc -5"
                 "acc -12"
                 "acc +35"
                 "acc +25"
                 "jmp +360"
                 "nop +301"
                 "acc +32"
                 "acc -1"
                 "jmp +436"
                 "acc +21"
                 "jmp +109"
                 "acc +1"
                 "nop +230"
                 "jmp -113"
                 "acc +24"
                 "acc +12"
                 "acc +13"
                 "acc +27"
                 "jmp -52"
                 "jmp +476"
                 "acc +15"
                 "acc -18"
                 "acc -13"
                 "jmp +323"
                 "acc -10"
                 "acc +35"
                 "acc -6"
                 "nop +377"
                 "jmp +8"
                 "jmp +430"
                 "nop +50"
                 "acc +33"
                 "acc -1"
                 "jmp -72"
                 "acc -19"
                 "jmp -92"
                 "jmp +1"
                 "jmp -87"
                 "nop -120"
                 "nop -34"
                 "jmp +159"
                 "jmp +183"
                 "jmp +1"
                 "acc -4"
                 "acc +49"
                 "acc -4"
                 "jmp +73"
                 "acc -18"
                 "acc +34"
                 "acc -8"
                 "jmp +403"
                 "acc +32"
                 "acc +0"
                 "acc +0"
                 "jmp +390"
                 "acc +19"
                 "jmp +305"
                 "acc -15"
                 "jmp +176"
                 "acc +3"
                 "jmp +36"
                 "acc +29"
                 "acc +21"
                 "jmp +1"
                 "acc -17"
                 "jmp +144"
                 "acc +16"
                 "acc +4"
                 "acc +49"
                 "acc -7"
                 "jmp -65"
                 "acc +28"
                 "acc +2"
                 "acc +23"
                 "acc +46"
                 "jmp +278"
                 "jmp +424"
                 "acc +45"
                 "acc +7"
                 "jmp -49"
                 "jmp -50"
                 "acc -11"
                 "nop -121"
                 "jmp -140"
                 "jmp +1"
                 "acc -8"
                 "jmp +206"
                 "acc -7"
                 "jmp +1"
                 "acc +20"
                 "acc +30"
                 "jmp +375"
                 "nop +312"
                 "jmp +111"
                 "acc +23"
                 "jmp -15"
                 "acc +27"
                 "acc +46"
                 "acc +36"
                 "jmp +360"
                 "acc +1"
                 "acc +25"
                 "acc +29"
                 "jmp -137"
                 "jmp +177"
                 "acc +15"
                 "acc -13"
                 "acc +48"
                 "acc +29"
                 "jmp +76"
                 "nop -174"
                 "acc +27"
                 "jmp +214"
                 "acc +4"
                 "jmp +120"
                 "nop -165"
                 "acc -16"
                 "jmp +2"
                 "jmp +238"
                 "jmp +224"
                 "jmp -154"
                 "acc +12"
                 "jmp +162"
                 "acc +31"
                 "acc +26"
                 "acc -13"
                 "acc +18"
                 "jmp +173"
                 "acc +38"
                 "jmp +1"
                 "jmp +199"
                 "jmp -202"
                 "jmp +127"
                 "acc -12"
                 "jmp -42"
                 "acc -12"
                 "acc +36"
                 "acc -4"
                 "acc +24"
                 "jmp +343"
                 "acc -17"
                 "nop +277"
                 "acc +13"
                 "jmp +56"
                 "jmp +41"
                 "nop +92"
                 "acc +32"
                 "jmp +182"
                 "nop +111"
                 "acc +0"
                 "acc +29"
                 "jmp +219"
                 "nop -234"
                 "acc +0"
                 "acc +31"
                 "acc +49"
                 "jmp +216"
                 "jmp +247"
                 "acc +24"
                 "acc -10"
                 "jmp +218"
                 "acc +14"
                 "acc -19"
                 "acc +14"
                 "jmp +11"
                 "jmp +82"
                 "acc +5"
                 "acc +28"
                 "acc +35"
                 "jmp -93"
                 "acc +24"
                 "jmp -210"
                 "acc +39"
                 "jmp -107"
                 "jmp -150"
                 "acc -7"
                 "jmp -185"
                 "acc +25"
                 "acc -15"
                 "jmp +63"
                 "acc +14"
                 "acc +50"
                 "jmp -7"
                 "acc +13"
                 "jmp +298"
                 "nop +69"
                 "jmp -268"
                 "acc -5"
                 "jmp +66"
                 "jmp +48"
                 "nop -1"
                 "acc +38"
                 "acc +3"
                 "acc +46"
                 "jmp +259"
                 "nop +235"
                 "acc +4"
                 "jmp -186"
                 "jmp +94"
                 "acc -16"
                 "acc +6"
                 "jmp +287"
                 "acc -19"
                 "jmp -221"
                 "jmp -69"
                 "acc +8"
                 "nop -102"
                 "acc +31"
                 "jmp +204"
                 "jmp -81"
                 "acc +12"
                 "acc +21"
                 "acc -8"
                 "nop +151"
                 "jmp -155"
                 "acc +50"
                 "nop +145"
                 "acc -6"
                 "acc +0"
                 "jmp -296"
                 "acc +16"
                 "acc +30"
                 "acc +11"
                 "jmp +254"
                 "acc +0"
                 "acc -8"
                 "nop +162"
                 "acc -17"
                 "jmp -144"
                 "acc +6"
                 "acc +2"
                 "acc +32"
                 "acc -5"
                 "jmp +21"
                 "jmp -77"
                 "acc +38"
                 "jmp +117"
                 "acc +27"
                 "acc -11"
                 "nop -203"
                 "jmp +201"
                 "jmp -305"
                 "acc +40"
                 "nop -330"
                 "acc -7"
                 "jmp -279"
                 "jmp +60"
                 "nop -163"
                 "jmp -53"
                 "nop +115"
                 "acc +24"
                 "jmp +1"
                 "acc +45"
                 "jmp -92"
                 "acc +0"
                 "acc +13"
                 "jmp -302"
                 "acc +25"
                 "jmp +208"
                 "nop -293"
                 "jmp +5"
                 "jmp +1"
                 "acc -13"
                 "nop -272"
                 "jmp +62"
                 "acc -4"
                 "acc +32"
                 "acc +49"
                 "jmp -124"
                 "acc -19"
                 "nop -246"
                 "jmp -353"
                 "nop -349"
                 "acc +13"
                 "acc +21"
                 "jmp -165"
                 "acc +14"
                 "acc +37"
                 "nop +28"
                 "acc +7"
                 "jmp -308"
                 "acc +11"
                 "jmp +71"
                 "jmp -139"
                 "nop -19"
                 "acc -7"
                 "acc -3"
                 "jmp -302"
                 "acc -2"
                 "acc +1"
                 "nop -43"
                 "jmp -280"
                 "acc +10"
                 "acc +9"
                 "jmp -358"
                 "acc +39"
                 "acc +16"
                 "acc -11"
                 "jmp +1"
                 "jmp +142"
                 "acc -15"
                 "acc -17"
                 "acc +9"
                 "acc +11"
                 "jmp -172"
                 "jmp -90"
                 "nop +135"
                 "acc -1"
                 "jmp -379"
                 "acc +39"
                 "acc -13"
                 "jmp -57"
                 "acc -3"
                 "acc -7"
                 "jmp +1"
                 "jmp +80"
                 "acc +7"
                 "acc +11"
                 "acc +30"
                 "nop +69"
                 "jmp -140"
                 "acc +11"
                 "acc +7"
                 "acc +3"
                 "jmp -417"
                 "jmp -314"
                 "acc -6"
                 "jmp -6"
                 "jmp -224"
                 "acc +0"
                 "jmp -131"
                 "acc +7"
                 "acc +33"
                 "jmp +20"
                 "acc +32"
                 "acc -14"
                 "nop -375"
                 "jmp -394"
                 "acc +8"
                 "acc +47"
                 "acc +29"
                 "acc -6"
                 "jmp -381"
                 "acc -8"
                 "jmp -353"
                 "jmp -108"
                 "acc -16"
                 "acc +21"
                 "jmp -329"
                 "acc +13"
                 "acc +0"
                 "acc +0"
                 "jmp -402"
                 "acc -8"
                 "jmp -307"
                 "jmp -308"
                 "jmp -358"
                 "acc +38"
                 "acc -19"
                 "acc +33"
                 "jmp -276"
                 "jmp -114"
                 "acc -6"
                 "jmp +55"
                 "acc +16"
                 "jmp -244"
                 "nop +22"
                 "acc -7"
                 "acc +19"
                 "jmp -154"
                 "jmp +18"
                 "jmp -50"
                 "acc +31"
                 "acc +50"
                 "acc +2"
                 "jmp -260"
                 "acc -16"
                 "jmp -135"
                 "acc +50"
                 "acc -9"
                 "acc +50"
                 "acc +27"
                 "jmp -323"
                 "acc +26"
                 "nop -249"
                 "acc +46"
                 "acc +3"
                 "jmp -474"
                 "acc +50"
                 "acc -7"
                 "jmp -78"
                 "jmp +1"
                 "acc -6"
                 "acc +6"
                 "jmp +94"
                 "acc +47"
                 "acc -3"
                 "acc +13"
                 "jmp -485"
                 "nop -499"
                 "acc +35"
                 "acc +35"
                 "acc -1"
                 "jmp -259"
                 "nop -486"
                 "acc +48"
                 "acc -4"
                 "jmp +23"
                 "acc +33"
                 "acc +37"
                 "acc +10"
                 "jmp +61"
                 "acc -1"
                 "jmp +1"
                 "jmp +1"
                 "acc -8"
                 "jmp +61"
                 "jmp -237"
                 "jmp -164"
                 "acc +39"
                 "acc +38"
                 "nop -302"
                 "jmp -468"
                 "acc -15"
                 "jmp -6"
                 "acc -9"
                 "acc +1"
                 "acc +41"
                 "acc +48"
                 "jmp -285"
                 "acc +1"
                 "acc +24"
                 "jmp -111"
                 "jmp -90"
                 "acc +46"
                 "acc +21"
                 "acc +43"
                 "jmp -103"
                 "acc -9"
                 "nop -5"
                 "jmp -274"
                 "acc -13"
                 "jmp -262"
                 "jmp -39"
                 "acc +43"
                 "nop -463"
                 "acc +15"
                 "acc +23"
                 "jmp -176"
                 "nop -305"
                 "acc +2"
                 "acc +28"
                 "jmp -198"
                 "acc +15"
                 "acc -18"
                 "jmp -473"
                 "acc -13"
                 "acc +9"
                 "jmp -46"
                 "acc +1"
                 "acc +43"
                 "acc +6"
                 "jmp +10"
                 "acc +24"
                 "acc -3"
                 "jmp -424"
                 "acc -1"
                 "acc +47"
                 "jmp +1"
                 "acc +20"
                 "jmp -17"
                 "jmp -59"
                 "nop -19"
                 "acc +42"
                 "acc +47"
                 "jmp -9"
                 "jmp -576"
                 "nop -148"
                 "acc +0"
                 "jmp +12"
                 "acc -15"
                 "jmp -209"
                 "acc +13"
                 "jmp -442"
                 "nop -548"
                 "nop -62"
                 "acc +22"
                 "jmp -92"
                 "jmp -516"
                 "acc +8"
                 "jmp -301"
                 "acc +1"
                 "jmp -146"
                 "acc -15"
                 "jmp -366"
                 "jmp -440"
                 "acc +17"
                 "acc +25"
                 "acc +28"
                 "jmp -240"
                 "acc -2"
                 "acc +25"
                 "nop -68"
                 "acc +14"
                 "jmp +1"])

(defn parse
  [line]
  (let [[op value] (str/split line #" ")]
    [(keyword op) (js/parseInt value 10)]))

(defn compile
  [lines]
  (map parse lines))

(def program (compile sourcecode))

(defn fix-operation
  [[op _]]
  (cond
    (= :nop op) [:jmp _]
    (= :jmp op) [:nop _]
    :else [op _]))

(defn patch
  [program fixes]
  (map-indexed (fn [idx instruction] (if (contains? fixes idx) (fix-operation instruction) instruction)) program))

(defn run
  [program]
  (loop [accumulator 0
         pointer 0
         processed #{}]
    (cond
      ; at end finished
      (= (count program) pointer) accumulator
      ; bad pointer outside of program
      (not (< -1 pointer (count program))) (throw (js/Error. (str "bad pointer at " pointer)))
      ; good pointer get instruction
      :else (let
             [[op value] (nth program pointer)]
              (cond
                (contains? processed pointer) (throw (js/Error. (str "Recursion stopped!" {:acc accumulator :pointers processed})))
                (= op :jmp) (recur accumulator (+ pointer value) (conj processed pointer))
                (= op :nop) (recur accumulator (inc pointer) (conj processed pointer))
                (= op :acc) (recur (+ accumulator value) (inc pointer) (conj processed pointer))
                :else (throw (js/Error. (str "Unknown operation " op))))))))

(println (try
           (run program)
           (catch js/Object e
             (. e -message)))) ;; 1217

(def pointers #{0 96 160 256 352 1 97 129 161 353 417 577 130 290 418 578 131 291 419 451 483 579 132 164 420 452 484 580 5 133 165 197 453 485 517 549 198 486 518 550 582 7 103 487 519 551 583 8 104 520 552 584 41 201 233 329 521 553 42 202 234 266 330 426 43 171 203 235 267 331 427 587 12 44 172 204 268 300 332 364 428 588 45 77 141 173 237 269 301 333 365 397 429 461 493 78 110 142 174 302 398 494 558 79 111 175 399 431 495 559 80 112 240 304 432 560 81 113 241 305 593 18 146 242 306 370 434 466 19 243 371 435 20 116 244 372 500 564 596 21 117 277 309 373 405 501 565 597 118 278 406 502 566 23 55 87 215 407 503 567 24 216 280 408 600 25 217 249 281 377 409 314 378 379 475 571 124 380 572 189 253 573 190 254 574 191 255 575})

(println (filter identity (for [pos pointers
                                :let [program (patch program #{pos})]]
                            (try
                              {:fix pos :result (run program)}
                              (catch js/Object e
                                nil))))) ;; 501