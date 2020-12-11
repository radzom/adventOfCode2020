(def adapters [104
               83
               142
               123
               87
               48
               102
               159
               122
               69
               127
               151
               147
               64
               152
               90
               117
               132
               63
               109
               27
               47
               7
               52
               59
               11
               161
               12
               148
               155
               129
               10
               135
               17
               153
               96
               3
               93
               82
               55
               34
               65
               89
               126
               19
               72
               20
               38
               103
               146
               14
               105
               53
               77
               120
               39
               46
               24
               139
               95
               140
               33
               21
               84
               56
               1
               32
               31
               28
               4
               73
               128
               49
               18
               62
               81
               66
               121
               54
               160
               158
               138
               94
               43
               2
               114
               111
               110
               78
               13
               99
               108
               141
               40
               25
               154
               26
               35
               88
               76
               145])

(def max-adapter (reduce max adapters))

(def differences (let [inputs (->> 0 
                                   (conj adapters) 
                                   sort)
                       ouputs (->> adapters
                                   (reduce max)
                                   (+ 3) 
                                   (conj adapters) 
                                   sort)]
                   (map - ouputs inputs)))

(def result-1 (->> differences
                   (group-by identity)
                   (map (comp count second))
                   (reduce *)))

(defn trib-seq []
  (let [trib-step (fn
                    [[a b c]]
                    [b c (+ a b c)])]
    (map first (iterate trib-step [1 1 2]))))

(def result-2 (->> differences
                   (partition-by (partial = 3))
                   (take-nth 2)
                   (map count)
                     ;; the correct numbers of combinations for a chunk of 1s are the tribonacci numbers
                     ;; 0 1 1 2 4 7 13 24 44 81 149 ... 
                   (map (partial nth (trib-seq)))
                   (reduce *)))

(println result-1) ;; 2201
(println result-2) ;; 169255295254528
