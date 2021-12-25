 (ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(defn error [& messages]
  (apply println messages)
  :error)

(def pantry-ingredients #{:flour :sugar})
(def fridge-ingredients #{:milk :egg :butter})
(def all-ingredients (clojure.set/union pantry-ingredients fridge-ingredients))

(def simple-ingredients #{:butter})
(def squeezed-ingredients #{:egg})
(def scooped-ingredients #{:milk :flour :sugar})

(def locations {:pantry pantry-ingredients
                :fridge fridge-ingredients})

(def cake {:ingredients {:egg 2
                         :flour 2
                         :milk 1
                         :sugar 1}
           :baking-time 25})

(def cookies {:ingredients {:egg 1
                            :flour 1
                            :sugar 1
                            :butter 1}
              :baking-time 30})

(def recipes {:cake cake
               :cookies cookies})

(defn bake [item]
  (let [recipe (get recipes item)
        ingredients (get recipe :ingredients)]
    (doseq [ingredient (keys ingredients)]
      (add ingredient (get ingredients ingredient)))
  (mix)
  (pour-into-pan)
  (bake-pan (get recipe :baking-time))
  (cool-pan)))

(defn bake-amount [item amount]
  (loop [result '()
         n amount]
    (if (= n 0)
      result
    (do
      (recur (merge result (bake item)) (dec n))))))

(defn add-egg []
  (grab :egg)
  (squeeze)
  (add-to-bowl))

(defn add-flour []
  (grab :cup)
  (scoop :flour)
  (add-to-bowl)
  (release))

(defn add-milk []
  (grab :cup)
  (scoop :milk)
  (add-to-bowl)
  (release))

(defn add-sugar []
  (grab :cup)
  (scoop :sugar)
  (add-to-bowl)
  (release))

(defn add-butter []
  (grab :butter)
  (add-to-bowl))

(defn scooped? [ingredient]
  (contains? scooped-ingredients ingredient))

(defn squeezed? [ingredient]
  (contains? squeezed-ingredients ingredient))

(defn simple? [ingredient]
  (contains? simple-ingredients ingredient))

(defn add-eggs [n]
  (dotimes [e n]
    (add-egg))
  :ok)

(defn add-flour-cups [n]
  (dotimes [e n]
    (add-flour))
  :ok)

(defn add-milk-cups [n]
  (dotimes [e n]
    (add-milk))
  :ok)

(defn add-sugar-cups [n]
  (dotimes [e n]
    (add-sugar))
  :ok)

(defn add-butters [n]
  (dotimes [e n]
    (add-butter))
  :ok)

(defn add-squeezed
  ([ingredient amount]
   (if (squeezed? ingredient)
     (do
       (dotimes [i amount]
         (grab ingredient)
         (squeeze)
         (add-to-bowl))
       :ok)
     (error "This function only works on squeezed ingredients. You asked me to squeeze" ingredient)))
  ([ingredient]
   (add-squeezed ingredient 1)))

(defn add-scooped
  ([ingredient amount]
   (if (scooped? ingredient)
     (do
       (dotimes [i amount]
         (grab :cup)
         (scoop ingredient)
         (add-to-bowl)
         (release))
       :ok)
     (error "This function only works on scooped ingredients. You asked me to scoop" ingredient)))
  ([ingredient]
   (add-scooped ingredient 1)))

(defn add-simple
  ([ingredient amount]
   (if (simple? ingredient)
     (do
       (dotimes [i amount]
         (grab ingredient)
         (add-to-bowl))
       :ok)
     (error "This function only works on simple ingredients. You asked me to add" ingredient)))
  ([ingredient]
   (add-simple ingredient 1)))

(defn add
  ([ingredient]
   (add ingredient 1))
  ([ingredient amount]
   (cond
     (squeezed? ingredient)
     (add-squeezed ingredient amount)
     (scooped? ingredient)
     (add-scooped ingredient amount)
     (simple? ingredient)
     (add-simple ingredient amount)
     :else
     (error "I do not know the ingredient" ingredient))))

(defn bake-cake []
  (add :egg 2)
  (add :flour 2)
  (add :milk)
  (add :sugar)
  (mix)
  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn bake-cookies []
  (add :egg)
  (add :flour)
  (add :sugar)
  (add :butter)
  (mix)
  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

(defn -main []
  (bake-cake)
  (bake-cookies))

(defn from-pantry? [ingredient]
  (contains? pantry-ingredients ingredient))

(defn from-fridge? [ingredient]
  (contains? fridge-ingredients ingredient))

(defn fetch-from-pantry
  ([ingredient]
   (fetch-from-pantry ingredient 1))
  ([ingredient amount]
   (if (from-pantry? ingredient)
     (do
       (go-to :pantry)
       (dotimes [i amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [i amount]
         (unload ingredient)))
     (error "Only works with pantry ingredients. Ingredient: " ingredient))))

(defn fetch-from-fridge
  ([ingredient]
   (fetch-from-fridge ingredient 1))
  ([ingredient amount]
   (if (from-fridge? ingredient)
     (do
       (go-to :fridge)
       (dotimes [i amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [i amount]
         (unload ingredient)))
     (error "Only works with fridge ingredients. Ingredient: " ingredient))))

(defn fetch-ingredient
  ([ingredient]
   (fetch-ingredient ingredient 1))
  ([ingredient amount]
   (cond
     (from-fridge? ingredient)
     (fetch-from-fridge ingredient amount)
     (from-pantry? ingredient)
     (fetch-from-pantry ingredient amount)
     :else
     (error "Cannot fetch ingredient: " ingredient))))

(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))

(defn fetch-list [shopping]
  (doseq [location (keys locations)]
    (go-to location)
    (doseq [ingredient (get locations location)]
      (load-up-amount ingredient (get shopping ingredient 0))))
  (go-to :prep-area)
  (doseq [ingredient all-ingredients]
    (unload-amount ingredient (get shopping ingredient 0))))

(defn fetch-list-amount [shopping amount]
  (dotimes [i amount]
    (fetch-list shopping)))

; ({:orderid 8782,
;   :address "530 Cyber Dr",
;   :items {:cake 13, :cookies 21}}
;  {:orderid 8784,
;   :address "368 Robot Ln",
;   :items {:cake 14, :cookies 14}}
;  {:orderid 8786, :address "392 Cyber Dr", :items {:cake 6, :cookies 5}}
;  {:orderid 8788,
;   :address "599 Electro Pkwy",
;   :items {:cake 19, :cookies 8}}
;  {:orderid 8790,
;   :address "408 Robot Ln",
;   :items {:cake 16, :cookies 15}}
;  {:orderid 8792,
;   :address "186 Robot Ln",
;   :items {:cake 18, :cookies 7}}
;  {:orderid 8794,
;   :address "513 Servo St",
;   :items {:cake 24, :cookies 6}}
;  {:orderid 8796,
;   :address "669 Cyber Dr",
;   :items {:cake 1, :cookies 17}}
;  {:orderid 8798, :address "900 Servo St", :items {:cookies 18}}
;  {:orderid 8800,
;   :address "508 Robot Ln",
;   :items {:cake 24, :cookies 12}})

(defn prepare-recipes [items]
  (doseq [recipe (keys items)]
    (fetch-list-amount (get (get recipes recipe) :ingredients) (get items recipe))
    ))

(defn day-at-the-bakery []
  (let [orders (get-morning-orders)]
    (doseq [order orders]
      (let [items (get order :items)]
        (prepare-recipes items)
        (let [rack-ids (flatten (map (fn [[key value]] (bake-amount key value)) items))]
          (delivery {
                     :orderid (get order :orderid)
                     :address (get order :address)
                     :rackids rack-ids
                     }))))))
