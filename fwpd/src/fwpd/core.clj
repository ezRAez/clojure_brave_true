(ns fwpd.core)
(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (map :name (filter #(>= (:glitter-index %) minimum-glitter) records)))

(defn ispresent?
  [record somekey]
  (not (nil? (record somekey))))


(def validations {:name ispresent?
                   :glitter-index ispresent?})

(defn validate
  [validatingfns record]
  (apply = true (map (fn [akey]
                       ((get validations akey) record akey))
                     (keys validatingfns))))

(defn append
  "Append a new suspect to the list if validated"
  [suspect susps]
  (if (validate validations suspect)
   (concat susps [suspect])
   susps))

(defn csvify
  [record]
  (str (get record :name) "," (get record :glitter-index)))

(defn records-to-csv
  [records]
  (clojure.string/join "\n" (map csvify records)))

(defn sum-bad
  ([vals] (sum-bad vals 0))
  ([vals accumulating-total]
   (if (empty? vals)
     accumulating-total
     (sum-bad (rest vals) (+ (first vals) accumulating-total)))))

(defn sum-good
  ([vals]
   (sum-good vals 0))
  ([vals accumulating-total]
   (if (empty? vals)
     accumulating-total
     (recur (rest vals) (+ (first vals) accumulating-total)))))

