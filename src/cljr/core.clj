(ns cljr.core
  "Clojure fns for calling JRuby"
  (:refer-clojure :exclude [eval send methods hash-set])
  (:import org.jruby.RubySymbol
           java.io.PrintWriter
           java.io.StringWriter
           java.lang.ref.WeakReference)
  (:use [circle.util.core :only (defn-once)]
        [circle.util.except :only (throw-if-not)])
  (:require [clojure.string :as string])
  (:require fs))

(declare eval ruby get-class get-module send ->ruby)

(def ^{:dynamic true} *runtime* nil)

(defmacro with-runtime [r & body]
  `(binding [*runtime* ~r]
     ~@body))

(defn hash-get
  "Get a value out of a ruby hash"
  [h key]
  (.fastARef h (->ruby key)))

(defn hash-set
  "Set a value in a ruby hash"
  [h key value]
  (.fastASet h (->ruby key) (->ruby value)))

(defn new-runtime
  "Creates and returns a new ruby runtime.

  - argv: a seq of strings
  - env: a map of string->string environment variables that will be set in the new runtime.
  - jruby home: the home directory of the jruby such that $home/bin contains ruby. You probably won't be able to require gems if this isn't set"
  [& {:keys [env argv home]}]
  (let [whole-env (merge (into {} (System/getenv)) env)
        config (doto (org.jruby.RubyInstanceConfig.)
                 (.setArgv (into-array String argv))
                 (.setEnvironment whole-env)
                 (.setCompatVersion org.jruby.CompatVersion/RUBY1_9))
        _ (when home
            (.setJRubyHome config home))
        runtime (org.jruby.Ruby/newInstance config)]
    runtime))

(defn eval
  "Eval a string in the runtime"
  ([s]
     (eval (ruby) s))
  ([runtime s]
     (try
       (-> runtime (.evalScriptlet s))
       (catch Exception e
         (.printStackTrace e)
         (throw e)))))

(defn ruby-require
  ([package]
     (ruby-require (ruby) package))
  ([runtime package]
     (eval runtime (format "require '%s'" (clojure.core/name package)))))

(defn ruby-load
  [s]
  (eval (format "load '%s'" s)))

(defn add-loadpath [runtime path]
  (eval runtime (format "$LOAD_PATH << '%s/%s'" (System/getProperty "user.dir") path)))

(defn require-rails
  "Require rails, but don't start the server"
  []
  (ruby-require (ruby) (format "%s/config/environment" (System/getProperty "user.dir"))))

;; This is the runtime all ruby requests will go through. It's
;; unlikely that Rails' runtime will be returned by getGlobalRuntime,
;; meaning that all rails instance variables will not be
;; visible. To make them visible, call init from rails first.

;; Use a weakref to prevent this atom from keeping the ruby instance alive
(defonce runtime (atom (WeakReference. nil)))

(defn init
  "Call this from the rails runtime, passing in JRuby.runtime. This
  will be used for all calls."
  [r]
  (swap! runtime (constantly (WeakReference. r))))

(defn ensure-runtime []
  (when (not (-> runtime deref (.get)))
    (println "setting default runtime")
    (swap! runtime (constantly (WeakReference. (new-runtime))))))

(defn ruby []
  (or *runtime* (do (ensure-runtime)
                    (-> runtime deref (.get)))))

;; Each defmethod must return an IRubyObject.
(defmulti ->ruby
  "Convert Ruby data to Clojure data"
  class)

(defmethod ->ruby
  clojure.lang.IPersistentMap [m]
  (let [new-h (org.jruby.RubyHash. (ruby))]
    (->> m
         (map (fn [[k v]] [(->ruby k) (->ruby v)]))
         (into {})
         (.putAll new-h))
    new-h))

(defmethod ->ruby
  clojure.lang.Keyword [k]
  (RubySymbol/newSymbol (ruby) (name k)))

(defmethod ->ruby
  org.jruby.runtime.builtin.IRubyObject [e]
  e)

;; RaiseException wraps an exception for Java, but isn't an IRubyObject.
(defmethod ->ruby
  org.jruby.exceptions.RaiseException [e]
  (.getException e))

(defn capture-exception-data [e]
  "Returns a string via printStackTrace"
    (let [w (StringWriter.)
        pw (PrintWriter. w)]
    (.printStackTrace e pw)
    (.toString w)))

;; java Exceptions aren't IRubyObjects
(defmethod ->ruby
  java.lang.Exception [e]
  (capture-exception-data e))

(defmethod ->ruby
  clojure.lang.Sequential [v]
  (let [new-a (org.jruby.RubyArray/newArray (ruby) [])
        values (map ->ruby v)]
    (.addAll new-a values)
    new-a))

(defmethod ->ruby
  java.lang.String [s]
  (org.jruby.RubyString/newString (ruby) s))

(defmethod ->ruby
  org.bson.types.ObjectId [id]
  (send (get-class (get-module "BSON") "ObjectId") :from_string (.toString id)))

(defmethod ->ruby
  java.lang.Float [n]
  (org.jruby.RubyFloat. (ruby) n))

(defmethod ->ruby
  java.lang.Double [n]
  (org.jruby.RubyFloat. (ruby) n))

(defmethod ->ruby
  java.lang.Integer [n]
  (org.jruby.RubyFixnum. (ruby) n))

(defmethod ->ruby
  java.lang.Long [n]
  (org.jruby.RubyFixnum. (ruby) n))

(defmethod ->ruby
  java.lang.Boolean [b]
  (if b
    (-> (ruby) (.getTrue))
    (-> (ruby) (.getFalse))))

(defmethod ->ruby
  nil [n]
  (-> (ruby) (.getNil)))

(defmethod ->ruby
  :default [val]
  (org.jruby.RubyString/newString (ruby) (format "Uncastable values: %s" val)))

(defn rspec
  "runs rspec. Useful from clojure repl."
  [runtime & args]
  (let [options (filter #(= (get % 0) \-) args)
        subdirs (remove #(= (get % 0) \-) args)
        subdirs (if (empty? subdirs) [""] subdirs)
        subdirs (map #(fs/join "spec" %) subdirs)]
    (with-runtime runtime
      (ruby-require "rubygems")
      (ruby-require "rspec/core/rake_task")
      (-> (get-module "RSpec") (get-module "Core") (get-class "Runner") (send :run (concat options subdirs))))))

(defn get-kernel
  "Returns the Kernel module. Used for 'core' functions like puts"
  []
  (.getKernel (ruby)))

(defn get-class
  "Returns the class/module with the given name. With one arg, looks for a class in the root namespace. With two args, looks for a class/module defined under another class, like Foo::Bar"
  ([class-name]
     (.getClass (ruby) (name class-name)))
  ([parent class-name]
     (.getClass parent (name class-name))))

(defn send
  "Call a method on a ruby object"
  [obj method & args]
  (throw-if-not obj "Can't call methods on nil")
  (try
    (.callMethod obj (name method) (into-array org.jruby.runtime.builtin.IRubyObject (map ->ruby args)))
    (catch org.jruby.exceptions.RaiseException e
      (throw (Exception. (capture-exception-data e))))))

(defn get-module
  ([module-name]
     (.getModule (ruby) (name module-name)))
  ([parent module-name]
     (send parent :const_get (name module-name))))

(defn ->instance
  "Takes an object with an _id and fetches the ruby model's instance for that variable"
  [class obj]
  {:pre [obj]}
  (let [id (-> obj :_id)
        rid (->ruby id)
        class (get-class class)
        instance (send class :find rid)]
    (throw-if-not instance)
    instance))

(defn methods
  "Returns the list of ruby methods on the obj"
  [obj]
  (map #(symbol (str (.to_s %))) (seq (send obj :methods))))

(defn mapify [x]
  (into {} x))

(defn setify [x]
  (into #{} x))

(defn vecify [x]
  (into [] x))

(defn bean-loader [x]
  (-> x
      (bean)
      (update-in [:URLs] vecify)
      (assoc-in [:id] (System/identityHashCode x))
      (dissoc :JDBCDriverUnloader)
      (update-in [:parent] (fn [parent]
                             (when parent
                               (bean-loader parent))))))

(defn bean-config
  "returns a fully bean-ified instance config. useful for diffing to determine differences between ruby configs."
  [instance-config]
  (-> (bean instance-config)
      (dissoc :scriptSource :error :input :traceType)
      (update-in [:loader] bean-loader)
      (update-in [:classCache] bean)
      (update-in [:profile] bean)
      (update-in [:optionGlobals] mapify)
      (update-in [:environment] mapify)
      (update-in [:argv] seq)
      (update-in [:excludedMethods] setify)
      (update-in [:profilingMode] str)
      (update-in [:compileMode] str)
      (update-in [:compatVersion] str)))

(defn rails
  "Calls the rails script, as if called from the command line. args is
  argv, a seq of strings that the rails script understands, i.e. [\"console\"] or
  [\"server\"]. If future is true, execute the command in a future."
  [& {:keys [argv env future?]}]
  (let [runtime (new-runtime :env env
                             :argv argv)
        body (fn []
               (with-runtime runtime
                 (ruby-load "./script/rails")))]
    (if future?
      (do
        (future (body))
        runtime)
      (body))))

(defn rails-server
  "Starts a rails server in another thread. Returns the runtime. Convenience method"
  [& {:keys [env]}]
  (rails :argv ["server"] :env env :future? true))

;;
;; This doesn't work. Not sure of the proper way to stop the server
;;

;; (defn stop-server
;;   "Takes the runtime from rails-server"
;;   [runtime]
;;   (.tearDown runtime))
