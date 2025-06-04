# Примеры конфигураций для тестирования компилятора

## Базовая конфигурация (compiler_options.json)
Стандартная конфигурация для полного тестирования компилятора.

## Быстрая проверка синтаксиса (syntax_check_only.json)
```json
{
  "compiler": {
    "executable": "..\\..\\app\\c51_compiler.exe",
    "project_root": "..\\..",
    "timeout_seconds": 10
  },
  "compilation": {
    "target": "at89s4051",
    "optimize": "none",
    "memory_model": "small",
    "debug": false,
    "additional_flags": []
  },
  "testing": {
    "syntax_check_only": true,
    "verbose_output": false,
    "stop_on_first_error": true,
    "generate_report": false
  },
  "files": {
    "test_files": [
      {
        "source": "test_memory_types.c",
        "output": "test_memory",
        "description": "Типы памяти C51"
      }
    ]
  },
  "colors": {
    "info": "White",
    "success": "Green",
    "error": "Red",
    "warning": "Yellow",
    "debug": "DarkGray"
  }
}
```

## Отладочная конфигурация (debug_verbose.json)
```json
{
  "compiler": {
    "executable": "..\\..\\app\\c51_compiler.exe",
    "project_root": "..\\..",
    "timeout_seconds": 60
  },
  "compilation": {
    "target": "at89s4051",
    "optimize": "none",
    "memory_model": "small",
    "debug": true,
    "additional_flags": ["--verbose", "--dump-ast"]
  },
  "output": {
    "hex_extension": ".hex",
    "obj_extension": ".obj",
    "lst_extension": ".lst",
    "create_listing": true
  },
  "testing": {
    "syntax_check_only": false,
    "verbose_output": true,
    "stop_on_first_error": false,
    "generate_report": true,
    "report_filename": "debug_test_report.txt"
  },
  "colors": {
    "info": "Cyan",
    "success": "Green",
    "error": "Red",
    "warning": "Yellow",
    "debug": "Gray"
  }
}
```

## Производственная конфигурация (production.json)
```json
{
  "compiler": {
    "executable": "..\\..\\app\\c51_compiler.exe",
    "project_root": "..\\..",
    "timeout_seconds": 30
  },
  "compilation": {
    "target": "at89s4051",
    "optimize": "size",
    "memory_model": "small",
    "debug": false,
    "additional_flags": ["--warnings-as-errors"]
  },
  "testing": {
    "syntax_check_only": false,
    "verbose_output": false,
    "stop_on_first_error": true,
    "generate_report": true,
    "report_filename": "production_test_report.txt"
  },
  "build": {
    "auto_build_if_missing": false,
    "build_command": "stack",
    "build_args": ["build", "--fast"],
    "build_timeout_seconds": 180
  }
}
```

## Конфигурация для CI/CD (ci.json)
```json
{
  "compiler": {
    "executable": "..\\..\\app\\c51_compiler.exe",
    "project_root": "..\\..",
    "timeout_seconds": 15
  },
  "compilation": {
    "target": "at89s4051",
    "optimize": "speed",
    "memory_model": "small",
    "debug": false,
    "additional_flags": ["--no-color"]
  },
  "testing": {
    "syntax_check_only": false,
    "verbose_output": false,
    "stop_on_first_error": true,
    "generate_report": true,
    "report_filename": "ci_test_report.xml"
  },
  "colors": {
    "info": "White",
    "success": "White",
    "error": "White",
    "warning": "White",
    "debug": "White"
  },
  "build": {
    "auto_build_if_missing": true,
    "build_command": "stack",
    "build_args": ["build", "--fast", "--no-terminal"],
    "build_timeout_seconds": 600
  }
}
```

## Использование различных конфигураций

### Запуск с альтернативной конфигурацией:
```powershell
# Копирование нужной конфигурации
Copy-Item "syntax_check_only.json" "compiler_options.json"
.\compile_tests.ps1

# Или создание скрипта-обертки
.\compile_tests.ps1 -ConfigFile "debug_verbose.json"
```

### Параметры конфигурации:

#### Секция `compiler`:
- `executable` - путь к исполняемому файлу компилятора
- `project_root` - корневая директория проекта
- `timeout_seconds` - таймаут компиляции в секундах

#### Секция `compilation`:
- `target` - целевая платформа (at89s4051, at89s52, etc.)
- `optimize` - уровень оптимизации (none, speed, size)
- `memory_model` - модель памяти (small, medium, large)
- `debug` - включение отладочной информации
- `additional_flags` - дополнительные флаги компилятора

#### Секция `testing`:
- `syntax_check_only` - только проверка синтаксиса без генерации кода
- `verbose_output` - подробный вывод
- `stop_on_first_error` - остановка при первой ошибке
- `generate_report` - создание отчета
- `report_filename` - имя файла отчета

#### Секция `build`:
- `auto_build_if_missing` - автоматическая сборка если компилятор не найден
- `build_command` - команда сборки
- `build_args` - аргументы команды сборки
- `build_timeout_seconds` - таймаут сборки

## Создание собственной конфигурации

1. Скопируйте базовую конфигурацию
2. Измените нужные параметры
3. Сохраните под новым именем
4. Используйте с параметром `-ConfigFile` 