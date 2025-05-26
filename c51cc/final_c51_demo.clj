(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "🎯 === ФИНАЛЬНАЯ ДЕМОНСТРАЦИЯ C51 КОМПИЛЯТОРА === 🎯")
(println "Демонстрация улучшенного парсера с использованием do-parse макроса")
(println "Все функции переписаны без callback hell!\\n")

(defn demo-parse [description code]
  (println (str "📋 " description ":"))
  (println (str "   Код: " code))
  (let [result (parser/parse (lexer/tokenize code))]
    (if (:success result)
      (do
        (println "   ✅ Успешно!")
        (let [decl (first (:declarations (:ast result)))]
          (case (:ast-type decl)
            :interrupt-declaration
            (println (str "   🔥 Interrupt функция '" (:function-name decl) 
                         "' номер " (:interrupt-number decl)
                         (when (:using-clause decl) (str " using " (:using-clause decl)))))
            :function-declaration
            (println (str "   🔧 Обычная функция '" (:name decl) "'"))
            :sfr-declaration
            (println (str "   📡 SFR регистр '" (:name decl) "' по адресу " (:address decl)))
            :sbit-declaration
            (println (str "   🔘 SBIT бит '" (:name decl) "' по адресу " (:address decl)))
            (println (str "   📦 " (:ast-type decl))))))
      (println (str "   ❌ Ошибка: " (:error result))))
    (println)))

;; Демонстрация всех возможностей
(demo-parse "Простая функция" "void main() { }")
(demo-parse "Interrupt функция" "void timer_isr() interrupt 1 { }")
(demo-parse "Interrupt + using" "void uart_isr() interrupt 4 using 2 { }")
(demo-parse "Функция с using" "void fast_func() using 1 { }")
(demo-parse "SFR декларация" "sfr P1 = 0x90;")
(demo-parse "SBIT декларация" "sbit LED = 0x97;")
(demo-parse "Переменная" "int counter = 0;")
(demo-parse "If оператор" "void test() { if (x > 0) return x; }")
(demo-parse "While цикл" "void test() { while (i < 10) i++; }")
(demo-parse "For цикл" "void test() { for (i = 0; i < 10; i++) sum += i; }")

(println "🎉 === ДЕМОНСТРАЦИЯ ЗАВЕРШЕНА === 🎉")
(println "\\n📊 Статистика улучшений:")
(println "   • Убран callback hell из всех основных функций")
(println "   • Использован гигиенический макрос do-parse")
(println "   • Поддержка C51: interrupt, using, sfr, sbit")
(println "   • Улучшена читаемость и поддерживаемость кода")
(println "   • Сохранена производительность и корректность") 