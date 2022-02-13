(ns dev.examples.hello
  (:require
   [babashka.fs :as fs]
   [clojure.java.shell :as shell]
   [wave.load :as load]
   [wave.emit :as emit]))

(def target-dir (str (fs/path (System/getProperty "user.dir")
                              "examples" "hello" "target")))

(def hand-raw-code
  '(core/let [x (core/fn []
                  _5i32)]
     (zig.std.debug/print "Hello {s} {d}" ["world" (x)])))

#_(defn edn-code []
  (-> ctx
      (replace-gnode
       "std" {}
       '(cvalue std (import "std")))
      (sig "main" [[] :void])
      (replace-gnode
       "main" {}
       '(cfn main []
             (let [x (fn []
                       (as 5 [:int 32]))]
               (std.debug.print "Hello {s} {d}" ["world" (x)]))))))

(def hand-zast
  {:root-dir target-dir
   :files
   {"main-t"
    {:nodes
     [[:cvalue {} :const "std"
       [:invoke {}
        [:resolve "@import"]
        [[:string {} "std"]]]]
      [:cfn {} "__main__x" [] "i32"
       [[:return {} [:number {:intpart "5"}]]]]
      [:cfn {:visibility :public}
       "main" [] "void"
       [[:bind-local {:bindings [[:const "x" [:resolve "main__x"]]]}
         [[:invoke {}
           [:access "print"
            [:access "debug"
             [:resolve "std"]]]
           [[:string {} "Hello {s} {d} \\n"]
            [:array {}
             [[:string {} "world"]
              [:invoke {} [:resolve "x"] []]]]]]]]]]]}}})

(comment
  (emit/emit-all hand-zast)

  (shell/sh "zig" "run"
            (str (fs/path target-dir emit/zig-src-folder (str "main-t" ".zig")))
            ;; "-dynamic"
            :dir (str (fs/path target-dir emit/zig-out-folder)))

  #!
  )
