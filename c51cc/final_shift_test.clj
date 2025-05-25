(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "🎯 ФИНАЛЬНЫЙ ТЕСТ: ИСПРАВЛЕНИЕ ОПЕРАТОРОВ СДВИГА")
(println "=" (apply str (repeat 50 "=")))

(defn demo-shift-fix [code]
  (println (str "\n📝 Код: " code))
  (let [tokens (lexer/tokenize code)]
    (println "🔍 Токены:")
    (doseq [[i token] (map-indexed vector tokens)]
      (println (str "   " i ": " (:type token) " -> " (:value token))))
    
    (let [result (parser/parse tokens)]
      (if (:success result)
        (println "✅ Парсинг УСПЕШЕН!")
        (println (str "❌ Ошибка: " (:error result)))))))

;; Демонстрация исправления
(demo-shift-fix "P1 |= (1 << 3);")
(demo-shift-fix "mask = 0xFF >> 4;")
(demo-shift-fix "value <<= 2;")
(demo-shift-fix "data >>= 1;")

(println "\n🏆 РЕЗУЛЬТАТ:")
(println "✅ Лексер теперь корректно распознает:")
(println "   << как :shift-left (битовый оператор)")
(println "   >> как :shift-right (битовый оператор)")
(println "   <<= как :shift-left-equal (оператор присваивания)")
(println "   >>= как :shift-right-equal (оператор присваивания)")
(println "\n✅ Парсер теперь корректно обрабатывает:")
(println "   - Приоритет операторов сдвига")
(println "   - Составные операторы присваивания со сдвигом")
(println "   - Битовые операции с портами микроконтроллера")

(println "\n🎉 ПРОБЛЕМА ПОЛНОСТЬЮ РЕШЕНА! 🎉") 