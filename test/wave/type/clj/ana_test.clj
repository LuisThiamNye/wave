(ns wave.type.clj.ana-test
  (:require
   [wave.type.clj.ana :as sut]
   [wave.type.clj.type :as cljtype]
   [wave.type.cmn.type :as type]
   [wave.type.clj.aast :as cljaast]
   [clojure.test :as t :refer [deftest testing is]]))

(deftest simple-let-test
  (let [[ctx nid]
        (sut/analyse sut/dctx
                     '(let* [x 5]
                            x))]
    (is (= 0 (count (sut/get-errors ctx))))
    (is (= (cljtype/java-class->type "java.lang.Long")
           (cljtype/node-type ctx (cljaast/get-in-node ctx nid [1 1]))))
    (is (= (cljtype/java-class->type"java.lang.Long")
           (cljtype/node-type ctx (cljaast/get-in-node ctx nid [2]))))
    (is (= (cljtype/java-class->type "java.lang.Long")
           (cljtype/node-type ctx nid)))))

(deftest clj-subs-test
  (testing "normal cases;"
    (doseq [code '[(clojure.core/subs "" 0)
                   (clojure.core/subs "" 1 2)]]
      (testing (str code ";")
        (let [[ctx nid :as ana] (sut/analyse sut/dctx code)]
         (testing "no errors;"
           (is (= 0 (count (sut/get-errors ctx))))
           (let [errs (:errors (cljaast/get-node ctx (cljaast/get-in-node ctx nid [1])))]
             (is (= 0 (count errs)))))
         (testing "types;"
           (is (= (cljtype/java-class->type "java.lang.Object")
                  (cljtype/node-type ctx nid)))
           (is (= (cljtype/java-class->type "java.lang.String")
                  (cljtype/node-type ctx (cljaast/get-in-node ctx nid [1])))))))))
  (testing "invalid arity;"
    (doseq [code '[(clojure.core/subs "")
                   (clojure.core/subs "" 5 5 5)]]
      (testing (str code ";")
        (let [[ctx nid] (sut/analyse sut/dctx code)]
         (is (= 1 (count (sut/get-errors ctx))))
         (let [errs (:errors (cljaast/get-node ctx nid))]
           (is (= :invalid-arity (:e-type (first errs))))
           (is (= 1 (count errs))))))))
  (testing "type mismatch;"
    (let [[ctx nid] (sut/analyse sut/dctx '(clojure.core/subs 5 0))]
      (is (= (cljtype/java-class->type "java.lang.Object")
             (cljtype/node-type ctx nid)))
      (is (= 1 (count (sut/get-errors ctx))))
      (let [errs (:errors (cljaast/get-node ctx (cljaast/get-in-node ctx nid [1])))]
        (is (= :type-mismatch (:e-type (first errs))))
        (is (= 1 (count errs)))))))

(deftest simple-lambda-test
  (testing "no params, nominal case;"
    (let [[ctx nid]
          (sut/analyse sut/dctx
                       '(let* [f (fn* [] "")] (f)))]
      (is (= (cljtype/java-class->type "java.lang.String")
             (cljtype/node-type ctx nid)))
      (is (= 0 (count (sut/get-errors ctx))))))
  (testing "invalid arity;"
    (doseq [code '[(let* [f (fn* [] "")] (f 1))
                   (let* [f (fn* [a b] "")] (f 1))
                   (let* [f (fn* [a] "")] (f))]]
      (testing (str code ";")
        (let [[ctx nid] (sut/analyse sut/dctx code)]
         (is (= 1 (count (sut/get-errors ctx))))
         (let [errs (:errors (cljaast/get-node ctx (cljaast/get-in-node ctx nid [2])))]
           (is (= :invalid-arity (:e-type (first errs))))
           (is (= 1 (count errs))))
         (is (= (cljtype/java-class->type "java.lang.String")
                (cljtype/node-type ctx nid))))))))

(deftest identity-lambda-test
  (let [[ctx nid] (sut/analyse sut/dctx '(let* [f (fn* [x] x)] (f "")))]
    (is (= 0 (count (sut/get-errors ctx))))
    (is (= (cljtype/java-class->type "java.lang.String")
           (type/concrete-type
            ctx (cljtype/node-type ctx (cljaast/get-in-node ctx nid [2])))))))

(comment
  (do
    (println "\n\nDo Test")
    (simple-let-test)
    (clj-subs-test)
    (simple-lambda-test)
    (identity-lambda-test))

  #!
   )
