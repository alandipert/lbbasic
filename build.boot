(set-env!
 :dependencies '[[adzerk/boot-cljs          "2.1.4"]
                 [adzerk/boot-reload        "0.5.2"]
                 [hoplon                    "7.0.3"]
                 [org.clojure/clojure       "1.8.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [tailrecursion/boot-jetty  "0.1.3"]
                 [instaparse "1.4.8"]
                 [org.clojure/data.avl "0.0.16"]
                 [adzerk/cljs-console "0.1.1"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/core.incubator "0.1.4"]]
 :source-paths #{"src"}
 :asset-paths  #{"assets"})

(require
 '[adzerk.boot-cljs         :refer [cljs]]
 '[adzerk.boot-reload       :refer [reload]]
 '[hoplon.boot-hoplon       :refer [hoplon prerender]]
 '[tailrecursion.boot-jetty :refer [serve]])

(deftask dev
  "Build lbbasic for local development."
  []
  (comp
   (watch)
   (speak :theme "woodblock")
   (hoplon)
   (reload)
   ;; Suppress warning about Hoplon variadic -invoke
   (cljs :compiler-options
         {:warnings
          {:protocol-impl-with-variadic-method false}})
   (serve :port 8001)))

(deftask prod
  "Build lbbasic for production deployment."
  []
  (System/setProperty "CLJS_LOG_LEVEL" "INFO")
  (comp
   (hoplon)
   (cljs :optimizations :advanced
         :compiler-options
         {:warnings
          {:protocol-impl-with-variadic-method false}})
   (target :dir #{"target"})))
