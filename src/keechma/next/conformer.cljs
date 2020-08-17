(ns keechma.next.conformer
  (:require [com.fulcrologic.guardrails.core :refer [>defn | ? =>]]
            [keechma.next.spec]))

(declare conform-apps)

;; PROVJERA KOJEGA JE TIPA KONTROLER
;; Postoje 3 tipa kontrolera: factory (vector sa 1 elementom---) [:counter], :identity (vector sa 2 elementa, nastaje pomocu factory????) [:counter :1] , singleton-- :role
;; Funkcija prima ime kontrolera i vraca tip kontrolera
;; PRIMJER (get-controller-variant :role) ---> :singleton
(defn get-controller-variant [controller-name]
  (cond
    (and (vector? controller-name) (= 2 (count controller-name))) :identity
    (and (vector? controller-name) (= 1 (count controller-name))) :factory
    :else :singleton))



;; PROVERA DA LI SU PARAMSI CONTROLLERA DYNAMIC (DANA JE FUNKCIJA KOJOM SE ODREDUJE KADA SE KONTROLER PALI), STATIC
;; Funkcija prima definiciju kontrolera i vraca kojeg je tipa params
;; PRIMJER:
;; (get-params-variant {:keechma.controller/params true
;;                      :keechma.controller/type :keechma/entitydb}) --------> provjerava params i vraca :static
(defn get-params-variant [controller-def]
  (if (fn? (:keechma.controller/params controller-def)) :dynamic :static))


;;INICIJALIZACIJA KONTROLERA; VRACA [:ime-kontrolera {::type :singleton ::variant :static}]
;;POSTAVLJA TIP I VARIJANTU PARAMSA
(defn conform-controller [[controller-name controller-def]]
  (let [controller-variant (get-controller-variant controller-name) ;;vraca singleton,  factory ili identity
        params-variant    (get-params-variant controller-def)] ;; vraca dynamic ili static
    [controller-name
     (cond-> controller-def                           ;;cond-> stavlja controller-def na drugo mjesto kod izvrsavanja
       true (update :keechma.controller/type #(or % (if (vector? controller-name) (first controller-name) controller-name))) ;;isto kao i (update controller-def :keechma.controller/type .....)
       ;;gornji red ce updateat key ::type sa jednim imenom kontrolera, ukoliko je kontroler singleton, vratit ce isto :role, ukoliko je vektor [:counter] vratit ce :counter
       true (assoc :keechma.controller/variant controller-variant) ;;postaviti ce ::variant na :dynamic ili :static
       (not= :factory controller-variant) (assoc :keechma.controller.params/variant params-variant))])) ;;??????

;;PRIMA VECTOR KONTROLERA I RADI GORNJU FUNKCIJU ZA SVAKI OD NJIH
;; VRACA: {
;;            [:ime-kontrolera {::type :singleton ::variant :static}]
;;             [:ime-drugog-kontrolera {::type :factory ::variant :dynamic}]
;;         }
(defn conform-controllers [controllers]
  (->> controllers
    (map conform-controller)
    (into {})))


;;ISTO KAO GORE SAMO ZA APP ???
(defn conform-app [[app-name app-def]]
  (let [app-variant (if (contains? app-def :keechma.app/load) :dynamic :static)]
    [app-name
     (cond-> app-def
       true (assoc :keechma.app/variant app-variant)
       (contains? app-def :keechma/controllers) (update :keechma/controllers conform-controllers)
       (contains? app-def :keechma/apps) (update :keechma/apps conform-apps))]))

;;REGISTRIRA SVE APPOVE
(defn conform-apps [apps]
  (->> apps
    (map conform-app)
    (into {})))

(>defn conform [app]
  [any? => :keechma/app]
  (-> app
    (update :keechma/controllers conform-controllers)
    (update :keechma/apps conform-apps)))

(>defn conform-factory-produced [controller-def]
  [any? => :keechma.controller.factory/produced]
  (assoc controller-def :keechma.controller.params/variant (get-params-variant controller-def)))