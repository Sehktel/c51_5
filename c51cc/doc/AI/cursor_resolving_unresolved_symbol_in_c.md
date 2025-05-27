# Resolving Unresolved Symbol in Code
_Exported on 5/27/2025 at 16:09:07 GMT+3 from Cursor (0.50.5)_

---

**User**

fix

---

**Cursor**

I can see the issue - there's an unresolved symbol `test-c51-feature-support` on line 198. Let me search for this function to understand what it should be or if it needs to be implemented.

Perfect! The function `test-c51-feature-support` is defined in the `c51cc.parser_c51_specific_test` namespace, but it's not being imported in the current file. Let me check the current imports and add the missing function to the require statement.

I can see that the namespace already imports `[c51cc.parser_c51_specific_test :refer :all]`, which should include the `test-c51-feature-support` function. Let me check if the function is actually exported from that namespace.

Perfect! I can see that the `test-c51-feature-support` function is defined and exported at the end of the file. The issue is likely that the function is being called but the namespace import might not be working correctly. Let me check if there are any issues with the namespace path.

Looking at the error and the code, I notice that the function is defined in `c51cc/test/c51cc/parser_c51_specific_test.clj` but the import in the main file is `[c51cc.parser_c51_specific_test :refer :all]`. The path mismatch might be causing the issue.

Let me check the actual file structure:

Now I understand the issue. The namespace path is correct, but there might be a compilation issue. Let me check if there are any syntax errors in the test file that might prevent the function from being properly exported.

Looking at the code more carefully, I notice that the function `test-c51-feature-support` is defined correctly and exported. The issue might be that the function uses `parse-string` which might not be available in the current namespace. Let me check what functions are available from the imported namespaces.

I can see that `parse-string` is defined in each test file. The issue is that the `test-c51-feature-support` function is trying to use `parse-string` and `parse-successful?` functions, but these are not available in the `run-all-parser-tests` namespace.

The simplest solution is to either:
1. Define these utility functions in the main file, or
2. Use the fully qualified namespace calls, or  
3. Import the specific functions needed

Let me check what utility functions are available in the parser_test namespace:

Perfect! I can see that the utility functions `parse-string`, `parse-successful?`, and `get-ast` are defined in the `parser_test` namespace. Since the main file already imports `[c51cc.parser_test :refer :all]`, these functions should be available.

However, I notice there's a namespace mismatch. Let me check the actual namespace declaration in the parser_test file:

Aha! I found the issue. The namespace names use hyphens (`-`) instead of underscores (`_`). The correct namespace names are:

- `c51cc.parser-test` (not `c51cc.parser_test`)
- `c51cc.parser-advanced-test` (not `c51cc.parser_advanced_test`)  
- `c51cc.parser-c51-specific-test` (not `c51cc.parser_c51_specific_test`)

This is a common issue in Clojure where file names use underscores but namespace names use hyphens. Let me fix the imports in the main file:

```clojure
(ns c51cc.run-all-parser-tests
  "Главный файл для запуска всех тестов парсера C51
   Включает базовые, расширенные и специфичные для C51 тесты"
  (:require [clojure.test :refer :all]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]
            [c51cc.parser-test :refer :all]
            [c51cc.parser-advanced-test :refer :all]
            [c51cc.parser-c51-specific-test :refer :all]))
```

Perfect! Now let me check if there are any remaining linter errors by running a quick test to see if the function is now accessible:

Excellent! The `lein check` command completed successfully without any errors, which means the namespace import issue has been resolved. The `test-c51-feature-support` function should now be accessible.

Let me also run a quick syntax check on the specific file to make sure everything is working:

The command syntax was incorrect. Let me try a different approach to verify the fix:

I see the issue now. The error message is telling us that the file name should use underscores while the namespace uses hyphens. This is the standard Clojure convention. The file `run_all_parser_tests.clj` should have the namespace `c51cc.run-all-parser-tests` (with hyphens), which is correct.

However, the issue is that we need to make sure the classpath is set up correctly. Let me try using lein to test the namespace:

The command seems to be hanging. Let me try a simpler approach and just verify that the syntax is correct by checking if there are any remaining linter errors in the IDE. 

Based on my analysis, the fix I applied should resolve the issue. The problem was that the namespace imports in the `run_all_parser_tests.clj` file were using underscores instead of hyphens:

**Before (incorrect):**
```clojure
[c51cc.parser_test :refer :all]
[c51cc.parser_advanced_test :refer :all]
[c51cc.parser_c51_specific_test :refer :all]
```

**After (correct):**
```clojure
[c51cc.parser-test :refer :all]
[c51cc.parser-advanced-test :refer :all]
[c51cc.parser-c51-specific-test :refer :all]
```

This follows the standard Clojure convention where:
- **File names** use underscores (`parser_test.clj`)
- **Namespace names** use hyphens (`c51cc.parser-test`)

## Summary

Я исправил ошибку "Unresolved symbol: test-c51-feature-support" в файле `c51cc/run_all_parser_tests.clj`. 

**Проблема:** Неправильные импорты пространств имен - использовались подчеркивания вместо дефисов в именах пространств имен.

**Решение:** Изменил импорты с:
- `c51cc.parser_test` → `c51cc.parser-test`
- `c51cc.parser_advanced_test` → `c51cc.parser-advanced-test`  
- `c51cc.parser_c51_specific_test` → `c51cc.parser-c51-specific-test`

Это соответствует стандартной конвенции Clojure, где имена файлов используют подчеркивания, а пространства имен - дефисы.

**Альтернативные решения, которые я рассматривал:**
1. Переименование файлов для соответствия пространствам имен
2. Создание локальных определений функций
3. Использование полностью квалифицированных имен

Выбранное решение является наиболее правильным с точки зрения архитектуры, так как следует стандартам Clojure и не нарушает существующую структуру проекта.

```clojure
[c51cc.parser_test :refer :all]
[c51cc.parser_advanced_test :refer :all]
[c51cc.parser_c51_specific_test :refer :all]
```

```clojure
[c51cc.parser-test :refer :all]
[c51cc.parser-advanced-test :refer :all]
[c51cc.parser-c51-specific-test :refer :all]
```

