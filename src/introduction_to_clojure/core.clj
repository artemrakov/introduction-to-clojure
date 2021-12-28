(ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(defn error [& messages]
  (apply println messages)
  :error)

(def pantry-ingredients #{:flour :sugar :cocoa})
(def fridge-ingredients #{:milk :egg :butter})
(def all-ingredients (clojure.set/union pantry-ingredients fridge-ingredients))

(def simple-ingredients #{:butter})
(def squeezed-ingredients #{:egg})
(def scooped-ingredients #{:milk :flour :sugar :cocoa})

(def locations {:pantry pantry-ingredients
                :fridge fridge-ingredients})

(def cake {:ingredients {:egg 2
                         :flour 2
                         :milk 1
                         :sugar 1}
           :baking-time 25
           :steps [[:egg :flour :milk :sugar]]})

(def cookies {:ingredients {:egg 1
                            :flour 1
                            :sugar 1
                            :butter 1}
              :baking-time 30
              :steps [[:egg :flour :sugar :butter]]})

(def brownies {:ingredients {:egg 2
                             :flour 2
                             :milk 1
                             :sugar 1
                             :butter 2
                             :cocoa 2}
               :baking-time 35
               :steps [[:butter :sugar :cocoa] [:flour :egg :milk]]})

(def recipes {:cake cake
              :cookies cookies
              :brownies brownies})

(defn bake [item]
  (let [recipe (recipes item)
        ingredients (recipe :ingredients)
        steps (recipe :steps)]
    (doseq [step steps]
      (doseq [ingredient step]
        (add ingredient (ingredients ingredient)))
      (mix))
    (pour-into-pan)
    (bake-pan (recipe :baking-time))
    (cool-pan)))

(defn bake-items [items]
  (for [kv items
        i (range (second kv))]
    (bake (first kv))))

(defn scooped? [ingredient]
  (contains? scooped-ingredients ingredient))

(defn squeezed? [ingredient]
  (contains? squeezed-ingredients ingredient))

(defn simple? [ingredient]
  (contains? simple-ingredients ingredient))

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

(defn add-ingredients [first-item second-item]
  (merge-with + first-item second-item))

(defn multiply-ingredients [ingredients amount]
  (into {}
        (for [[item item-amount] ingredients]
          [item (* item-amount amount)])))

(defn item->ingredients [item]
  (get (get recipes item) :ingredients))

(defn order->ingredients [order]
  (let [items (get order :items)]
    (reduce add-ingredients (for [[item amount] items]
                              (multiply-ingredients (item->ingredients item) amount)))))

(defn orders->ingredients [orders]
  (reduce add-ingredients {}
          (for [order orders]
            (order->ingredients order))))

(defn day-at-the-bakery []
  (let [orders (get-morning-orders-day3)]
    (fetch-list (orders->ingredients orders))
    (doseq [order orders]
      (let [rack-ids (bake-items (get order :items))]
        (delivery {
                   :orderid (get order :orderid)
                   :address (get order :address)
                   :rackids rack-ids
                   })))))

(day-at-the-bakery)
(defn -main []
  (day-at-the-bakery))
