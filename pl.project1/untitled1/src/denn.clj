(ns .denn)




(defn listeleme
  [l1 l2]

  (def tlist (list))
  (def nlist (list))

  (def b)
  (def k)
  (def blist (list))
  (def klist (list))

  (if (>= (count l1) (count l2))
    (do
      (def b (count l1))
      (def k (count l2))
      (def blist l1)
      (def klist l2)
      )
    (do
      (def b (count l2))
      (def k (count l1))
      (def blist l2)
      (def klist l1)
      ))
  (loop [c 0]
    (when (< c k)
    (def tlist (conj tlist (nth blist c)))
    (def tlist (conj tlist (nth klist c)))
    (def nlist (conj nlist tlist))
    (def tlist ())
      (recur (+ c 1))))
  (loop [c k]
    (when (< c b)
      (def tlist (conj tlist (nth blist c)))
      (def tlist (conj tlist -1))
      (def nlist (conj nlist tlist))
      (def tlist ())
      (recur (+ c 1))))

  (def orglist  (reverse  nlist))
  (println orglist)
  )

(defn twoSwap
  [l]
  (def c (count l))
  (def orlist (list))
  (def tmp)
  (loop [control 0]
    (when (< control c)

      (def tmp (nth l control))
      (def orlist (conj orlist (nth l (+ control 1))))
      (def orlist (conj orlist tmp))

      (recur (+ control 2))))

  (println (reverse  orlist))
  )


;;(listeleme '(1,2,3,4) '(11,12,13,14,15))
(twoSwap '(1,5,2,3,6,4))