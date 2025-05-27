# Руководство по миграции препроцессора C51

## 🔄 Обратная совместимость

**ВАЖНО**: Новый препроцессор v2 обеспечивает **ПОЛНУЮ обратную совместимость** со старым API. Ваш существующий код будет работать без изменений!

## 📋 Сравнение API

### Старый API (v1)
```clojure
;; Возвращает строку результата
(require '[c51cc.preprocessor :as pp])

(def result (pp/preprocess code options))
;; result → String
```

### Новый API (v2)
```clojure
;; Возвращает детальную информацию
(require '[c51cc.preprocessor-v2 :as pp2])

(def result (pp2/preprocess-v2 code options))
;; result → {:result String, :errors [], :success Boolean, :final-state State}
```

## 🔧 Как работает обратная совместимость

Функция `preprocess` в `c51cc.preprocessor` является **wrapper'ом** вокруг нового препроцессора:

```clojure
(defn preprocess [code options]
  ;; Вызывает новый препроцессор
  (let [result (pp2/preprocess-v2 code options)]
    (if (:success result)
      ;; Возвращает только строку результата (как раньше)
      (:result result)
      ;; В случае ошибок добавляет комментарии для отладки
      (str (:result result) 
           "\n// ОШИБКИ ПРЕПРОЦЕССОРА:\n"
           (str/join "\n" (map #(str "// " (:message %)) (:errors result)))))))
```

## 🚀 Стратегии миграции

### 1. Немедленная миграция (рекомендуется для новых проектов)

```clojure
;; Было:
(def processed-code (preprocess source-code {:defines {"DEBUG" "1"}}))

;; Стало:
(let [result (preprocess-v2 source-code {:defines {"DEBUG" "1"}})]
  (if (:success result)
    (def processed-code (:result result))
    (handle-errors (:errors result))))
```

### 2. Постепенная миграция

```clojure
;; Этап 1: Используйте старый API (работает через wrapper)
(def result (preprocess code options))

;; Этап 2: Добавьте обработку ошибок
(let [v2-result (preprocess-v2 code options)]
  (when-not (:success v2-result)
    (log-errors (:errors v2-result)))
  (def result (:result v2-result)))

;; Этап 3: Полный переход на новый API
(def result (preprocess-v2 code options))
```

### 3. Гибридный подход

```clojure
(defn smart-preprocess [code options]
  (let [result (preprocess-v2 code options)]
    (if (:success result)
      ;; Успех - возвращаем только результат
      (:result result)
      ;; Ошибки - логируем и возвращаем частичный результат
      (do
        (log/error "Ошибки препроцессора:" (:errors result))
        (:result result)))))
```

## ✅ Преимущества нового API

### 1. Детальная информация об ошибках
```clojure
{:errors [{:message "Файл не найден: missing.h"
           :line 5
           :file "main.c"
           :type :include-error}]}
```

### 2. Контроль успешности обработки
```clojure
(let [result (preprocess-v2 code options)]
  (if (:success result)
    (compile (:result result))
    (show-errors (:errors result))))
```

### 3. Доступ к финальному состоянию
```clojure
(let [result (preprocess-v2 code options)
      final-defines (:defines (:final-state result))]
  (println "Определенные макросы:" final-defines))
```

## 🧪 Тестирование совместимости

Запустите тесты для проверки совместимости:

```bash
# Простой тест
clojure -M test_compatibility.clj

# Комплексный тест
clojure -M test_comprehensive_compatibility.clj
```

## 🔍 Отладка проблем

### Если старый код не работает:

1. **Проверьте синтаксис**: Убедитесь, что код корректен
2. **Проверьте опции**: Новый препроцессор более строг к валидации
3. **Используйте новый API**: Получите детальную информацию об ошибках

```clojure
;; Отладка через новый API
(let [result (preprocess-v2 problematic-code options)]
  (println "Успех:" (:success result))
  (println "Ошибки:" (:errors result))
  (println "Результат:" (:result result)))
```

## 📈 Производительность

Новый препроцессор использует **transducers** и **protocols**, что обеспечивает:

- ⚡ Лучшую производительность
- 🧩 Модульную архитектуру  
- 🔧 Легкую расширяемость
- ✅ Лучшую тестируемость

## 🎯 Рекомендации

### Для существующих проектов:
- ✅ Продолжайте использовать `preprocess` - он работает через wrapper
- ✅ Постепенно переходите на `preprocess-v2` для лучшей обработки ошибок

### Для новых проектов:
- ✅ Используйте `preprocess-v2` с самого начала
- ✅ Обрабатывайте ошибки через `:errors` и `:success`

## 🔗 Дополнительные ресурсы

- [Архитектура препроцессора v2](docs/preprocessor-v2-architecture.md)
- [Примеры использования](examples/)
- [Тесты совместимости](test_comprehensive_compatibility.clj)

---

**Заключение**: Обратная совместимость полностью обеспечена. Ваш код будет работать без изменений, но новый API предоставляет дополнительные возможности для лучшей обработки ошибок и отладки. 