(ns wave.type.clj.type-test
  (:require
   [wave.type.clj.type :as sut]
   [clojure.test :as t :refer [deftest testing is]]))

(def ctx0 {})

(deftest type-errors-test
  (testing "equal types"
    (is (= [] (sut/type-errors ctx0 sut/any-type sut/any-type)))
    (is (= [] (sut/type-errors ctx0 (sut/java-class->type String)
                                (sut/java-class->type String)))))
  (testing "java class hierarchy"
    (is (= [] (sut/type-errors ctx0 (sut/java-class->type Object)
                               (sut/java-class->type String))))
    (is (= 1 (count (sut/type-errors ctx0 (sut/java-class->type String)
                                     (sut/java-class->type Object)))))))

(comment
  (println "\n\ndotest")
  (type-errors-test)

  #!
  )
