# Makefile для HCL (Haskell C51 Compiler)
# 
# Этот Makefile предоставляет удобные команды для сборки, тестирования
# и разработки HCL компилятора.

.PHONY: all build test clean install docs examples help

# Переменные
STACK = stack
CABAL = cabal
PROJECT_NAME = hcl
EXECUTABLE = hcl-exe

# Цвета для вывода
RED = \033[0;31m
GREEN = \033[0;32m
YELLOW = \033[0;33m
BLUE = \033[0;34m
NC = \033[0m # No Color

# По умолчанию - сборка проекта
all: build

# Справка
help:
	@echo "$(BLUE)HCL (Haskell C51 Compiler) - Makefile$(NC)"
	@echo ""
	@echo "$(GREEN)Основные команды:$(NC)"
	@echo "  build      - Сборка проекта"
	@echo "  test       - Запуск тестов"
	@echo "  clean      - Очистка сборочных файлов"
	@echo "  install    - Установка исполняемого файла"
	@echo "  docs       - Генерация документации"
	@echo ""
	@echo "$(GREEN)Разработка:$(NC)"
	@echo "  dev        - Сборка в режиме разработки"
	@echo "  watch      - Автоматическая пересборка при изменениях"
	@echo "  lint       - Проверка стиля кода"
	@echo "  format     - Форматирование кода"
	@echo ""
	@echo "$(GREEN)Тестирование:$(NC)"
	@echo "  test-unit  - Только модульные тесты"
	@echo "  test-watch - Автоматический запуск тестов"
	@echo "  bench      - Бенчмарки производительности"
	@echo ""
	@echo "$(GREEN)Примеры:$(NC)"
	@echo "  examples   - Компиляция примеров"
	@echo "  demo       - Демонстрация возможностей"
	@echo ""
	@echo "$(GREEN)Утилиты:$(NC)"
	@echo "  deps       - Установка зависимостей"
	@echo "  ghci       - Запуск REPL"
	@echo "  profile    - Профилирование"

# Сборка проекта
build:
	@echo "$(BLUE)Сборка HCL компилятора...$(NC)"
	$(STACK) build

# Быстрая сборка для разработки
dev:
	@echo "$(BLUE)Сборка в режиме разработки...$(NC)"
	$(STACK) build --fast --file-watch

# Автоматическая пересборка
watch:
	@echo "$(BLUE)Запуск автоматической пересборки...$(NC)"
	$(STACK) build --file-watch

# Тестирование
test:
	@echo "$(BLUE)Запуск всех тестов...$(NC)"
	$(STACK) test

# Только модульные тесты
test-unit:
	@echo "$(BLUE)Запуск модульных тестов...$(NC)"
	$(STACK) test --test-arguments="--match Unit"

# Автоматический запуск тестов
test-watch:
	@echo "$(BLUE)Автоматический запуск тестов...$(NC)"
	$(STACK) test --file-watch

# Бенчмарки
bench:
	@echo "$(BLUE)Запуск бенчмарков...$(NC)"
	$(STACK) bench

# Очистка
clean:
	@echo "$(YELLOW)Очистка сборочных файлов...$(NC)"
	$(STACK) clean
	rm -rf .stack-work/
	rm -f *.hi *.o
	find . -name "*.hi" -delete
	find . -name "*.o" -delete

# Установка
install:
	@echo "$(BLUE)Установка HCL...$(NC)"
	$(STACK) install

# Генерация документации
docs:
	@echo "$(BLUE)Генерация документации...$(NC)"
	$(STACK) haddock --open

# Проверка стиля кода
lint:
	@echo "$(BLUE)Проверка стиля кода...$(NC)"
	@if command -v hlint >/dev/null 2>&1; then \
		hlint src/ app/ test/; \
	else \
		echo "$(RED)hlint не установлен. Установите: stack install hlint$(NC)"; \
	fi

# Форматирование кода
format:
	@echo "$(BLUE)Форматирование кода...$(NC)"
	@if command -v stylish-haskell >/dev/null 2>&1; then \
		find src/ app/ test/ -name "*.hs" -exec stylish-haskell -i {} \;; \
		echo "$(GREEN)Код отформатирован$(NC)"; \
	else \
		echo "$(RED)stylish-haskell не установлен. Установите: stack install stylish-haskell$(NC)"; \
	fi

# Установка зависимостей
deps:
	@echo "$(BLUE)Установка зависимостей...$(NC)"
	$(STACK) setup
	$(STACK) build --dependencies-only

# Запуск REPL
ghci:
	@echo "$(BLUE)Запуск GHCi...$(NC)"
	$(STACK) ghci

# Профилирование
profile:
	@echo "$(BLUE)Сборка с профилированием...$(NC)"
	$(STACK) build --profile
	@echo "$(YELLOW)Для запуска с профилированием используйте:$(NC)"
	@echo "$(STACK) exec -- $(EXECUTABLE) +RTS -p -RTS <args>"

# Компиляция примеров
examples: build
	@echo "$(BLUE)Компиляция примеров...$(NC)"
	@if [ -f examples/blink.c ]; then \
		echo "$(GREEN)Компиляция blink.c...$(NC)"; \
		$(STACK) exec -- $(EXECUTABLE) examples/blink.c -o examples/blink.asm -v; \
	else \
		echo "$(RED)Файл examples/blink.c не найден$(NC)"; \
	fi

# Демонстрация возможностей
demo: build
	@echo "$(BLUE)Демонстрация HCL компилятора$(NC)"
	@echo ""
	@echo "$(GREEN)1. Показ токенов:$(NC)"
	@if [ -f examples/blink.c ]; then \
		$(STACK) exec -- $(EXECUTABLE) examples/blink.c --show-tokens --stop-after lex | head -20; \
	fi
	@echo ""
	@echo "$(GREEN)2. Показ AST:$(NC)"
	@if [ -f examples/blink.c ]; then \
		$(STACK) exec -- $(EXECUTABLE) examples/blink.c --show-ast --stop-after parse | head -20; \
	fi

# Проверка качества кода
quality: lint
	@echo "$(BLUE)Проверка качества кода...$(NC)"
	@echo "$(GREEN)Статистика проекта:$(NC)"
	@find src/ -name "*.hs" | wc -l | xargs echo "Модулей:"
	@find src/ -name "*.hs" -exec wc -l {} + | tail -1 | awk '{print "Строк кода: " $$1}'
	@echo ""
	@echo "$(GREEN)Покрытие тестами:$(NC)"
	$(STACK) test --coverage

# Создание релиза
release: clean test
	@echo "$(BLUE)Подготовка релиза...$(NC)"
	$(STACK) build --pedantic
	$(STACK) test
	$(STACK) bench
	@echo "$(GREEN)Релиз готов!$(NC)"

# Проверка зависимостей
deps-check:
	@echo "$(BLUE)Проверка зависимостей...$(NC)"
	$(STACK) list-dependencies

# Обновление зависимостей
deps-update:
	@echo "$(BLUE)Обновление зависимостей...$(NC)"
	$(STACK) update

# Создание Docker образа
docker:
	@echo "$(BLUE)Создание Docker образа...$(NC)"
	docker build -t hcl-compiler .

# Запуск в Docker
docker-run: docker
	@echo "$(BLUE)Запуск в Docker...$(NC)"
	docker run --rm -v $(PWD)/examples:/examples hcl-compiler /examples/blink.c

# Создание архива с исходниками
archive:
	@echo "$(BLUE)Создание архива...$(NC)"
	tar -czf hcl-$(shell date +%Y%m%d).tar.gz \
		--exclude='.stack-work' \
		--exclude='*.tar.gz' \
		--exclude='.git' \
		.

# Информация о проекте
info:
	@echo "$(BLUE)Информация о проекте HCL$(NC)"
	@echo ""
	@echo "$(GREEN)Версия GHC:$(NC) $(shell $(STACK) ghc -- --version)"
	@echo "$(GREEN)Версия Stack:$(NC) $(shell $(STACK) --version)"
	@echo "$(GREEN)Размер проекта:$(NC)"
	@find . -name "*.hs" -exec wc -l {} + | tail -1
	@echo "$(GREEN)Статус сборки:$(NC)"
	@if $(STACK) build --dry-run >/dev/null 2>&1; then \
		echo "✅ Готов к сборке"; \
	else \
		echo "❌ Есть проблемы"; \
	fi

# Быстрая проверка
check: lint test
	@echo "$(GREEN)Быстрая проверка завершена$(NC)"

# Полная проверка
full-check: clean deps build test bench quality
	@echo "$(GREEN)Полная проверка завершена$(NC)" 