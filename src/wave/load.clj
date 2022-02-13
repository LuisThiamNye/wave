(ns wave.load
  (:require
   [babashka.fs :as fs]
   [wave.emit :as emit]
   [clojure.java.shell :as shell]))

(defn compile-zig [target-dir filename]
  (shell/sh "zig" "build-lib"
            (str (fs/path target-dir emit/zig-src-folder (str filename ".zig")))
            "-dynamic"
            :dir (str (fs/path target-dir emit/zig-out-folder))))
