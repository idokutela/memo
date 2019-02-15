(ns memo.core
  #?(:clj (:import [java.util.concurrent ConcurrentHashMap])))

(defprotocol ICache
  "A cache stores key value pairs.

  The fundamental operation is `retrieve`. This retrieves the value
  associated with a key, and if the value is not present, executes a
  callback to compute the value. `invalidate` clears the value
  associated with a key.

  Caches are mutable! All operations (potentially) mutate the cache."

  (retrieve! [this k cb]
    "Retrieves the value associated with `k`. If it is not stored,
executes `cb`to compute it.")

  (invalidate! [this k]
    "Invalidates the value associated to `k`.")

  (empty! [this]
    "Empties the cache."))


#?(:clj (definline de-nil [x] `(let [x# ~x] (if (nil? x#) ::nil x#)))
   :cljs (defn de-nil [x] (if (nil? x) ::nil x)))


#?(:clj (definline re-nil [x] `(let [x# ~x] (when-not (= ::nil x#) x#)))
   :cljs (defn de-nil [x] (when-not (= ::nil x) x)))

(defmacro memoise-form
  [m f & args]
  `(let [k# (vector ~@args)] (retrieve! ~m k# (fn [] (~f ~@args)))))

(defn memoise-with-cache
  [f cache]
  (fn
    ([] (memoise-form cache f))
    ([x] (memoise-form cache f x))
    ([x y] (memoise-form cache f x y))
    ([x y z] (memoise-form cache f x y z))
    ([x y z u] (memoise-form cache f x y z u))
    ([x y z u v] (memoise-form cache f x y z u v))
    ([x y z u v & rest]
     (retrieve! cache (list* x y z u v rest) #(apply f x y z u v rest)))))

(deftype ArityOneCache [val]

  ICache

  (retrieve! [this k cb]
    (let [[old-k v] @val]
      (if (= old-k k) v (-> (reset! val [k (cb)]) second))))
  (invalidate! [this k]
    (let [[curr-k] @val]
      (when (= curr-k k)
        (empty! this))))
  (empty! [this!] (reset! val [::empty]) nil))

(defn arity-one-cache
  "Creates a simple arity-one cache."
  []
  (ArityOneCache. (atom [::empty])))

#?(:clj
   (deftype UnboundedCache [m]
     ICache
     (retrieve! [this k cb]
       (let [v (.get ^ConcurrentHashMap m k)]
         (if-not (nil? v)
           (re-nil v)
           (let [v (de-nil (cb))]
             (or (.putIfAbsent ^ConcurrentHashMap m k v) (re-nil v))))))

     (invalidate! [this k] (.remove ^ConcurrentHashMap m k) nil)

     (empty! [this] (.clear ^ConcurrentHashMap m)))

   :cljs
   (deftype UnboundedCache [m]
     ICache
     (retrieve! [this k cb]
       (let [val (.get m)]
         (if-not (nil? val)
           (de-nil val)
           (let [val (cb)]
             (.set m k val)
             val))))
     (invalidate! [this k] (.delete m k)  nil)
     (empty! [this] (.clear m))))

(defn unbounded-cache []
  (UnboundedCache.
   #?(:clj (ConcurrentHashMap.)
      :cljs (js/Map.))))
