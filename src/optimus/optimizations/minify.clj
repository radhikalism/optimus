(ns optimus.optimizations.minify
  (:require [clojure.string :as s]
            [environ.core :refer [env]]))

(defn read-engine-preferences-list [input]
  (cond (sequential? input)
        input
        (string? input)
        (s/split input #",")
        :else
        ["nashorn" "rhino"]))

(defn make-js-engine
  ([preference-list]
     (let [manager (javax.script.ScriptEngineManager.)]
       (or (first (keep #(.getEngineByName manager %) preference-list))
           (throw (Exception. (str "No preferred JS engines found of "
                                   preference-list
                                   " among available engines "
                                   (.getEngineFactories manager)))))))
  ([]
     (-> (env :optimus-js-engines)
         (read-engine-preferences-list)
         (make-js-engine))))

(defn- escape [str]
  (-> str
      (s/replace "\\" "\\\\")
      (s/replace "'" "\\'")
      (s/replace "\n" "\\n")))

(defn- throw-js-exception [#^String text path]
  (if (= (.indexOf text "ERROR: ") 0)
    (let [prefix (when path (str "Exception in " path ": "))
          error (clojure.core/subs text 7)]
      (throw (Exception. (str prefix error))))
    text))

(defn normalize-line-endings [str]
  (-> str
      (s/replace "\r\n" "\n")
      (s/replace "\r" "\n")))

(defn- js-minification-code [js options]
  (str "(function () {
    try {
        var ast = UglifyJS.parse('" (escape (normalize-line-endings js)) "');
        ast.figure_out_scope();
        var compressor = UglifyJS.Compressor();
        var compressed = ast.transform(compressor);
        compressed.figure_out_scope();
        compressed.compute_char_frequency();"
        (if (get options :mangle-js-names true) "compressed.mangle_names();" "")
        "var stream = UglifyJS.OutputStream();
        compressed.print(stream);
        return stream.toString();
    } catch (e) { return 'ERROR: ' + e.message + ' (line ' + e.line + ', col ' + e.col + ')'; }
}());"))

(def ^String uglify
  "The UglifyJS source code, free of dependencies and runnable in a
stripped context"
  (slurp (clojure.java.io/resource "uglify.js")))

(defn prepare-uglify-engine []
  (let [engine (make-js-engine)]
    (.eval engine uglify)
    engine))

(defn- run-script-with-error-handling [engine script file-path]
  (throw-js-exception
   (try
     (.eval engine script)
     (catch Exception e
       (str "ERROR: " (.getMessage e))))
   file-path))

(defn minify-js
  ([js] (minify-js js {}))
  ([js options] (minify-js (prepare-uglify-engine) js options))
  ([engine js options]
     (run-script-with-error-handling engine (js-minification-code js options) (:path options))))

(defn minify-js-asset
  [engine asset options]
  (let [#^String path (:path asset)]
    (if (.endsWith path ".js")
      (update-in asset [:contents] #(minify-js engine % (assoc options :path path)))
      asset)))

(defn minify-js-assets
  ([assets] (minify-js-assets assets {}))
  ([assets options]
     (let [engine (prepare-uglify-engine)]
       (map #(minify-js-asset engine % options) assets))))

;; minify CSS

(defn- css-minify-code [css options]
  (str "
var console = {
    error: function (message) {
        throw new Error(message);
    }
};

(function () {
    try {
        var process;
        var compressor = new CSSOCompressor();
        var translator = new CSSOTranslator();
        var compressed = compressor.compress(srcToCSSP('" (escape (normalize-line-endings css)) "', 'stylesheet', true), " (not (get options :optimize-css-structure true)) ");
        return translator.translate(cleanInfo(compressed));
    } catch (e) { return 'ERROR: ' + e.message; }
}());"))

(def ^String csso
  "The CSSO source code, free of dependencies and runnable in a
stripped context"
  (slurp (clojure.java.io/resource "csso.js")))

(defn prepare-csso-engine []
  (let [engine (make-js-engine)]
    (.eval engine csso)
    engine))

(defn minify-css
  ([css] (minify-css css {}))
  ([css options] (minify-css (prepare-csso-engine) css options))
  ([engine css options]
     (run-script-with-error-handling engine (css-minify-code css options) (:path options))))

(defn minify-css-asset
  [engine asset options]
  (let [#^String path (:path asset)]
    (if (.endsWith path ".css")
      (update-in asset [:contents] #(minify-css engine % (assoc options :path path)))
      asset)))

(defn minify-css-assets
  ([assets] (minify-css-assets assets {}))
  ([assets options]
     (let [engine (prepare-csso-engine)]
       (map #(minify-css-asset engine % options) assets))))
