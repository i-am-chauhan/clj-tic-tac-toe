(ns tic-tac-toe.core)
(require '[clojure.set :as set])
(require '[clojure.string :as string])

(defn column-winning-moves [n]
  (map
    #(range % (inc (* n n)) n)
    (range 1 (inc n))))

(defn row-winning-moves [n]
  (map
    #(vec (range % (+ n %)))
    (range 1 (* n n) n)))

(def dia-winning-moves
  (juxt
    #(vec (range 1 (inc (* % %)) (inc %)))
    #(vec (range % (* % %) (dec %)))))

(defn generate-winning-moves [n]
  (mapcat
    #(% n)
    [row-winning-moves column-winning-moves dia-winning-moves]))

(defn won? [moves winning-moves] (some #(set/subset? % moves) winning-moves))

(defn create-board [n] (vec (repeat (* n n) " ")))

(defn print-board [board order]
  (->> board
       (partition order)
       (map #(string/join " | " %))
       (string/join "\n---------\n")
       println))

(defn prompt-user [user, symbol]
  {:player (do
             (println "Enter" user "player's name: ")
             (read-line))
   :symbol symbol
   :moves  #{}})

(defn game-details [board current-player opponent]
  {:board board :current-player current-player :opponent opponent})

(defn swap-players [game]
  (game-details
    (:board game)
    (:opponent game)
    (:current-player game)))

(defn place-move [game]
  (let [
        player (:current-player game)
        input (do
                (println "Play" (:player player))
                (read))]
    (-> game
        (update-in [:board (dec input)] (constantly (:symbol player)))
        (update-in [:current-player :moves] #(set (cons input %)))
        swap-players)))

(defn play []
  (let [order 1 winning-moves (generate-winning-moves order)]
    (loop [game (game-details (create-board order)
                              (prompt-user "first" "X")
                              (prompt-user "second" "0"))]
      (let
        [board (:board game) opponent (:opponent game)]
        (do
          (print-board board order)
          (if (won? (:moves opponent) winning-moves)
            (println (:player opponent) "has won!")
            (recur (place-move game))))))))