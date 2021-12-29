(ns introduction-to-clojure.core
  (:require [bakery.core :refer :all]))

(defn error [& messages]
  (apply println messages)
  :error)

(def cake {:ingredients {:egg 2
                         :flour 2
                         :milk 1
                         :sugar 1}
           :steps [[:add :all]
                   [:mix]
                   [:pour]
                   [:bake 25]
                   [:cool]]})

(def cookies {:ingredients {:egg 1
                            :flour 1
                            :sugar 1
                            :butter 1}
              :steps [[:add :all]
                      [:mix]
                      [:pour]
                      [:bake 30]
                      [:cool]]})

(def brownies {:ingredients {:egg 2
                             :flour 2
                             :milk 1
                             :sugar 1
                             :butter 2
                             :cocoa 2}
               :steps [[:add :butter]
                       [:add :sugar]
                       [:add :cocoa]
                       [:mix]
                       [:add :flour]
                       [:add :egg]
                       [:add :milk]
                       [:mix]
                       [:pour]
                       [:bake 35]
                       [:cool]]})

(def recipes {:cake cake
              :cookies cookies
              :brownies brownies})

(def baking {:recipes recipes
             :ingredients {:egg {:storage :fridge
                                 :usage :squeezed}
                           :milk {:storage :fridge
                                  :usage :scooped}
                           :flour {:storage :pantry
                                   :usage :scooped}
                           :butter {:storage :fridge
                                    :usage :simple}
                           :sugar {:storage :pantry
                                   :usage :scooped}
                           :cocoa {:storage :pantry
                                   :usage :scooped}}})

(def usage {:squeezed (fn [ingredient amount]
                        (dotimes [i amount]
                          (grab ingredient)
                          (squeeze)
                          (add-to-bowl)))
            :simple (fn [ingredient amount]
                      (dotimes [i amount]
                        (grab ingredient)
                        (add-to-bowl)))
            :scooped (fn [ingredient amount]
                         (grab :cup)
                       (dotimes [i amount]
                         (scoop ingredient)
                         (add-to-bowl))
                       (release))})

(defn perform [ingredients step]
  (let [action (actions (first step) (fn [ingredients step]
                                       error "I dont know how to" (first step)))]
    (action ingredients step)))

(defn bake [item]
  (let [recipe (recipes item)
        ingredients (recipe :ingredients)
        steps (recipe :steps)]
    (last (for [step steps]
      (perform ingredients step)))))

(status)
(bake :brownies)

(defn bake-items [items]
  (for [kv items
        i (range (second kv))]
    (bake (first kv))))

(defn ingredient-storage [ingredient]
  (let [ingredients (get baking :ingredients)
        info (get ingredients ingredient)]
    (get info :storage)))

(defn ingredient-usage [ingredient]
  (let [ingredients (get baking :ingredients)
        info (get ingredients ingredient)]
    (get info :usage)))

(defn add
  ([ingredient]
   (add ingredient 1))
  ([ingredient amount]
   (let [ingredient-type (ingredient-usage ingredient)]
     (if (contains? usage ingredient-type)
       ((usage ingredient-type) ingredient amount)
       (error "Unknown ingredient" ingredient)))))

(def actions {:bake (fn [ingedients step] (bake-pan (second step)))
              :pour (fn [ingedients step] (pour-into-pan))
              :mix (fn [ingedients step] (mix))
              :cool (fn [ingedients step] (cool-pan))
              :add (fn [ingredients step]
                      (if (and (= 2 (count step))
                               (= :all (second step)))
                        (doseq [[ingedient amount] ingredients]
                          (add ingedient amount))
                        (add (second step) (ingredients (second step)))))
              })

(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))

(defn fetch-ingredient
  ([ingredient]
   (fetch-ingredient ingredient 1))
  ([ingredient amount]
   (go-to (ingredient-storage ingredient))
   (load-up-amount ingredient amount)
   (go-to :prep-area)
   unload-amount ingredient amount))

(defn fetch-list [shopping]
  (let [location-ingredients (group-by #(ingredient-storage (first %)) shopping)]
    (doseq [[location ingredients] location-ingredients]
      (go-to location)
      (doseq [[ingredient amount] ingredients]
        (load-up-amount ingredient amount)))
    (go-to :prep-area)
    (doseq [[location ingredients] location-ingredients
            [ingredient amount] ingredients]
      (unload-amount ingredient amount))))

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

(defn -main []
  (day-at-the-bakery))
