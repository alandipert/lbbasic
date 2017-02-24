(set-env!
 :dependencies '[[adzerk/boot-cljs          "1.7.228-2"]
                 [adzerk/boot-reload        "0.4.12"]
                 [hoplon/boot-hoplon        "0.2.4"]
                 [hoplon/hoplon             "6.0.0-alpha16"]
                 [org.clojure/clojure       "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [tailrecursion/boot-jetty  "0.1.3"]
                 [tailrecursion/boot-heredoc "0.1.1"]
                 [com.lucasbradstreet/instaparse-cljs "1.4.1.2"]
                 [org.clojure/data.avl "0.0.16"]
                 [adzerk/cljs-console "0.1.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [com.cognitect/transit-cljs "0.8.237"]
                 [org.clojure/core.incubator "0.1.4"]]
 :source-paths #{"src"}
 :asset-paths  #{"assets"})

(require
 '[adzerk.boot-cljs         :refer [cljs]]
 '[adzerk.boot-reload       :refer [reload]]
 '[hoplon.boot-hoplon       :refer [hoplon prerender]]
 '[tailrecursion.boot-jetty :refer [serve]]
 '[boot.heredoc             :refer [heredoc]])

(deftask dev
  "Build lbbasic for local development."
  []
  (comp
   (watch)
   (speak :theme "woodblock")
   (heredoc :file-ext ".hl")
   (heredoc :file-ext ".cljs")
   (hoplon)
   (reload)
   ;; Suppress because of core.match
   (cljs)
   (serve :port 8001)
   (target)))

(deftask prod
  "Build lbbasic for production deployment."
  []
  (System/setProperty "CLJS_LOG_LEVEL" "INFO")
  (comp
   (heredoc :file-ext ".hl")
   (heredoc :file-ext ".cljs")
   (hoplon)
   (cljs :optimizations :advanced)
   (target :dir #{"target"})))
