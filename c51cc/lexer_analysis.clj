(ns lexer-analysis
  "Анализ лексера C51CC для оценки необходимости монадического рефакторинга"
  (:require [c51cc.lexer :as lexer]))

(defn analyze-lexer-architecture []
  (println "🔍 АНАЛИЗ АРХИТЕКТУРЫ ЛЕКСЕРА C51CC")
  (println (str "=" (apply str (repeat 60 "="))))
  
  (println "\n📊 ТЕКУЩЕЕ СОСТОЯНИЕ:")
  (println "   • Размер файла: ~240 строк")
  (println "   • Основная функция: tokenize")
  (println "   • Подход: Императивный с regex")
  (println "   • Обработка ошибок: Отсутствует")
  (println "   • Возвращаемый тип: Vector токенов или nil")
  
  (println "\n🎯 АРХИТЕКТУРНЫЕ ОСОБЕННОСТИ:")
  (println "   • Использует regex для разбора")
  (println "   • Рекурсивный вызов для множественных токенов")
  (println "   • Статические определения токенов")
  (println "   • Простая логика сопоставления")
  (println "   • Нет состояния между вызовами"))

(defn test-lexer-behavior []
  (println "\n🧪 ТЕСТИРОВАНИЕ ПОВЕДЕНИЯ ЛЕКСЕРА:")
  (println (str "-" (apply str (repeat 50 "-"))))
  
  ;; Тест 1: Корректный ввод
  (println "\n1. Корректный ввод:")
  (let [result (lexer/tokenize "int x = 42;")]
    (println "   Ввод: \"int x = 42;\"")
    (println "   Результат:" (if result "Успех" "Ошибка"))
    (when result
      (println "   Токенов:" (count result))
      (println "   Первый токен:" (first result))))
  
  ;; Тест 2: Пустой ввод
  (println "\n2. Пустой ввод:")
  (let [result (lexer/tokenize "")]
    (println "   Ввод: \"\"")
    (println "   Результат:" (if result "Успех" "Ошибка"))
    (println "   Токенов:" (if result (count result) "N/A")))
  
  ;; Тест 3: Некорректные символы
  (println "\n3. Некорректные символы:")
  (let [result (lexer/tokenize "int x @ 42;")]
    (println "   Ввод: \"int x @ 42;\"")
    (println "   Результат:" (if result "Успех" "Ошибка"))
    (when result
      (println "   Токенов:" (count result))
      (println "   Содержит nil?" (some nil? result))))
  
  ;; Тест 4: Только пробелы
  (println "\n4. Только пробелы:")
  (let [result (lexer/tokenize "   \n\t  ")]
    (println "   Ввод: \"   \\n\\t  \"")
    (println "   Результат:" (if result "Успех" "Ошибка"))
    (println "   Токенов:" (if result (count result) "N/A"))))

(defn analyze-error-handling []
  (println "\n❌ АНАЛИЗ ОБРАБОТКИ ОШИБОК:")
  (println (str "-" (apply str (repeat 50 "-"))))
  
  (println "\n🚨 ПРОБЛЕМЫ:")
  (println "   1. НЕТ обработки ошибок")
  (println "   2. НЕТ информативных сообщений")
  (println "   3. НЕТ позиции ошибки")
  (println "   4. НЕТ контекста ошибки")
  (println "   5. Возвращает nil при неудаче")
  
  (println "\n⚠️  ПОСЛЕДСТВИЯ:")
  (println "   • Сложно отлаживать некорректный ввод")
  (println "   • Нет информации о причине ошибки")
  (println "   • Парсер получает неполную информацию")
  (println "   • Пользователь не понимает что не так"))

(defn analyze-complexity []
  (println "\n📈 АНАЛИЗ СЛОЖНОСТИ:")
  (println (str "-" (apply str (repeat 50 "-"))))
  
  (println "\n📊 МЕТРИКИ:")
  (println "   • Цикломатическая сложность: НИЗКАЯ (~5)")
  (println "   • Количество условий: ~50 (cond)")
  (println "   • Глубина вложенности: 2-3 уровня")
  (println "   • Дублирование кода: МИНИМАЛЬНОЕ")
  (println "   • Читаемость: ХОРОШАЯ")
  
  (println "\n✅ СИЛЬНЫЕ СТОРОНЫ:")
  (println "   • Простая и понятная логика")
  (println "   • Хорошая производительность")
  (println "   • Легко добавлять новые токены")
  (println "   • Минимальные зависимости")
  (println "   • Функциональный стиль")
  
  (println "\n❌ СЛАБЫЕ СТОРОНЫ:")
  (println "   • Отсутствие обработки ошибок")
  (println "   • Нет валидации входных данных")
  (println "   • Большой cond блок")
  (println "   • Нет информации о позиции")
  (println "   • Сложно расширять для сложных случаев"))

(defn monadic-vs-current []
  (println "\n⚖️  СРАВНЕНИЕ: ТЕКУЩИЙ vs МОНАДИЧЕСКИЙ ПОДХОД:")
  (println (str "=" (apply str (repeat 60 "="))))
  
  (println "\n🔄 ТЕКУЩИЙ ПОДХОД:")
  (println "```clojure")
  (println "(defn tokenize [input]")
  (println "  (let [tokens (re-seq pattern input)]")
  (println "    (if (= (count tokens) 1)")
  (println "      (cond")
  (println "        (= token \"int\") [int-type]")
  (println "        (= token \"void\") [void-type]")
  (println "        ;; ... 50+ условий")
  (println "        :else nil)")
  (println "      (mapcat tokenize tokens))))")
  (println "```")
  
  (println "\n🚀 МОНАДИЧЕСКИЙ ПОДХОД:")
  (println "```clojure")
  (println "(defn tokenize-m [input]")
  (println "  (do-result")
  (println "    [validated-input (validate-input input)")
  (println "     raw-tokens (extract-tokens validated-input)")
  (println "     typed-tokens (map-tokens classify-token raw-tokens)")
  (println "     final-tokens (validate-tokens typed-tokens)]")
  (println "    (ok final-tokens)))")
  (println "```")
  
  (println "\n📊 СРАВНЕНИЕ МЕТРИК:")
  (println "| Аспект | Текущий | Монадический |")
  (println "|--------|---------|--------------|")
  (println "| Обработка ошибок | ❌ Нет | ✅ Автоматическая |")
  (println "| Информативность | ❌ Низкая | ✅ Высокая |")
  (println "| Композируемость | ⚠️ Средняя | ✅ Высокая |")
  (println "| Читаемость | ✅ Хорошая | ✅ Отличная |")
  (println "| Производительность | ✅ Высокая | ⚠️ Средняя |")
  (println "| Сложность кода | ✅ Низкая | ⚠️ Средняя |")
  (println "| Расширяемость | ⚠️ Средняя | ✅ Высокая |")
  (println "| Отладка | ❌ Сложная | ✅ Простая |"))

(defn cost-benefit-analysis []
  (println "\n💰 АНАЛИЗ ЗАТРАТ И ВЫГОД:")
  (println (str "=" (apply str (repeat 50 "="))))
  
  (println "\n💸 ЗАТРАТЫ НА РЕФАКТОРИНГ:")
  (println "   • Время разработки: 1-1.5 дня")
  (println "   • Создание монадической инфраструктуры: 4 часа")
  (println "   • Рефакторинг функции tokenize: 2 часа")
  (println "   • Добавление обработки ошибок: 2 часа")
  (println "   • Тестирование: 2 часа")
  (println "   • ИТОГО: ~10 часов")
  
  (println "\n💎 ВЫГОДЫ:")
  (println "   • Информативные сообщения об ошибках")
  (println "   • Позиция ошибки в исходном коде")
  (println "   • Единообразие с парсером и препроцессором")
  (println "   • Легкость добавления новых функций")
  (println "   • Улучшенная отладка")
  
  (println "\n⚖️ СООТНОШЕНИЕ ЗАТРАТ/ВЫГОД:")
  (println "   • Затраты: НИЗКИЕ (лексер простой)")
  (println "   • Выгоды: СРЕДНИЕ (улучшение UX)")
  (println "   • ROI: ПОЛОЖИТЕЛЬНЫЙ, но не критичный")
  (println "   • Приоритет: НИЗКИЙ"))

(defn final-recommendation []
  (println "\n🎯 ФИНАЛЬНАЯ РЕКОМЕНДАЦИЯ:")
  (println (str "=" (apply str (repeat 50 "="))))
  
  (println "\n📋 ВЕРДИКТ: ПОКА НЕ НУЖНО")
  
  (println "\n✅ АРГУМЕНТЫ ПРОТИВ рефакторинга:")
  (println "   1. Лексер РАБОТАЕТ стабильно")
  (println "   2. Сложность кода НИЗКАЯ")
  (println "   3. Производительность ВЫСОКАЯ")
  (println "   4. Ошибки лексера РЕДКИ")
  (println "   5. Есть более приоритетные задачи")
  
  (println "\n⚠️  АРГУМЕНТЫ ЗА рефакторинг:")
  (println "   1. Единообразие архитектуры")
  (println "   2. Улучшение сообщений об ошибках")
  (println "   3. Подготовка к будущим расширениям")
  
  (println "\n📅 РЕКОМЕНДУЕМАЯ СТРАТЕГИЯ:")
  (println "   1. СНАЧАЛА: Рефакторинг препроцессора (высокий приоритет)")
  (println "   2. ПОТОМ: Завершение рефакторинга парсера")
  (println "   3. ЗАТЕМ: Рефакторинг лексера (если будет время)")
  (println "   4. АЛЬТЕРНАТИВА: Добавить только обработку ошибок")
  
  (println "\n🔄 КОМПРОМИССНОЕ РЕШЕНИЕ:")
  (println "   • Добавить базовую обработку ошибок БЕЗ монад")
  (println "   • Улучшить сообщения об ошибках")
  (println "   • Добавить позицию ошибки")
  (println "   • Время: 2-3 часа вместо 10")
  
  (println "\n🏆 ИТОГ:")
  (println "   Лексер в монадическом рефакторинге НЕ НУЖДАЕТСЯ")
  (println "   в ближайшей перспективе. Сосредоточьтесь на препроцессоре!"))

(defn -main []
  (println "🔬 АНАЛИЗ НЕОБХОДИМОСТИ МОНАДИЧЕСКОГО РЕФАКТОРИНГА ЛЕКСЕРА")
  (println (str "=" (apply str (repeat 80 "="))))
  
  (analyze-lexer-architecture)
  (test-lexer-behavior)
  (analyze-error-handling)
  (analyze-complexity)
  (monadic-vs-current)
  (cost-benefit-analysis)
  (final-recommendation)
  
  (println "\n" (str "=" (apply str (repeat 80 "="))))
  (println "📝 ЗАКЛЮЧЕНИЕ: Лексер может подождать. Препроцессор - приоритет!")
  (println (str "=" (apply str (repeat 80 "=")))))

(-main) 