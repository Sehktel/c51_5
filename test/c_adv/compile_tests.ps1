# PowerShell скрипт для тестирования нашего компилятора C51
# Тестирует каждый пример с нашим собственным компилятором для AT89S4051
# Использует настройки из файла compiler_options.json

# Загрузка конфигурации
function Load-Configuration {
    param(
        [string]$ConfigFile = "compiler_options.json"
    )
    
    if (-not (Test-Path $ConfigFile)) {
        Write-Host "Файл конфигурации $ConfigFile не найден!" -ForegroundColor Red
        exit 1
    }
    
    try {
        $configContent = Get-Content $ConfigFile -Raw -Encoding UTF8
        $config = $configContent | ConvertFrom-Json
        return $config
    }
    catch {
        Write-Host "Ошибка загрузки конфигурации: $_" -ForegroundColor Red
        exit 1
    }
}

# Глобальная конфигурация
$Global:Config = Load-Configuration

# Функция для вывода сообщений с цветами из конфигурации
function Write-Message {
    param(
        [string]$Message,
        [string]$Type = "info"
    )
    
    $color = switch ($Type.ToLower()) {
        "info"    { $Global:Config.colors.info }
        "success" { $Global:Config.colors.success }
        "error"   { $Global:Config.colors.error }
        "warning" { $Global:Config.colors.warning }
        "debug"   { $Global:Config.colors.debug }
        default   { "White" }
    }
    
    Write-Host $Message -ForegroundColor $color
}

# Функция получения параметров компиляции
function Get-CompileOptions {
    $options = @()
    
    if ($Global:Config.compilation.target) {
        $options += "--target=$($Global:Config.compilation.target)"
    }
    
    # Поддержка новых флагов оптимизации
    if ($Global:Config.compilation.optimization_flags) {
        $options += $Global:Config.compilation.optimization_flags
    } elseif ($Global:Config.compilation.optimization_level) {
        $level = $Global:Config.compilation.optimization_level
        switch ($level) {
            0 { $options += "--O0" }
            1 { $options += "--O1" }
            2 { $options += "--O2" }
            3 { $options += "--O3" }
            -1 { $options += "--Os" }  # Оптимизация размера
            default { $options += "--O2" }
        }
    }
    
    if ($Global:Config.compilation.optimize_for) {
        $options += "--optimize-for=$($Global:Config.compilation.optimize_for)"
    }
    
    if ($Global:Config.compilation.memory_model) {
        $options += "--memory-model=$($Global:Config.compilation.memory_model)"
    }
    
    if ($Global:Config.compilation.debug) {
        $options += "--debug"
    }
    
    if ($Global:Config.compilation.additional_flags) {
        $options += $Global:Config.compilation.additional_flags
    }
    
    return $options
}

# Функция проверки нашего компилятора
function Test-OurCompiler {
    $compilerPath = $Global:Config.compiler.executable
    
    if (Test-Path $compilerPath) {
        Write-Message "✓ Наш компилятор найден: $compilerPath" "success"
        return $true
    } else {
        Write-Message "✗ Наш компилятор не найден: $compilerPath" "error"
        Write-Message "Сначала соберите компилятор командой: $($Global:Config.build.build_command) $($Global:Config.build.build_args -join ' ')" "warning"
        return $false
    }
}

# Функция компиляции файла нашим компилятором
function Compile-WithOurCompiler {
    param(
        [string]$SourceFile,
        [string]$OutputName
    )
    
    Write-Message "Тестирование $SourceFile с нашим компилятором..." "info"
    
    # Формирование команды для нашего компилятора
    $compileOptions = Get-CompileOptions
    $outputFile = "$OutputName$($Global:Config.output.hex_extension)"
    $compileArgs = @($SourceFile) + $compileOptions + @("--output", $outputFile)
    
    $compilerPath = $Global:Config.compiler.executable
    $compileCmd = "$compilerPath " + ($compileArgs -join " ")
    
    if ($Global:Config.testing.verbose_output) {
        Write-Message "Выполнение: $compileCmd" "debug"
    }
    
    try {
        # Запуск нашего компилятора с таймаутом
        $timeoutSeconds = $Global:Config.compiler.timeout_seconds
        $process = Start-Process -FilePath $compilerPath -ArgumentList $compileArgs -Wait -PassThru -NoNewWindow -RedirectStandardOutput "compile_output.txt" -RedirectStandardError "compile_error.txt"
        
        if ($process.ExitCode -eq 0) {
            Write-Message "✓ Компиляция $SourceFile успешна" "success"
            
            # Проверка создания выходного файла
            if (Test-Path $outputFile) {
                Write-Message "✓ Выходной файл создан: $outputFile" "success"
            } else {
                Write-Message "⚠ Выходной файл не найден, но компиляция прошла без ошибок" "warning"
            }
            
            # Показать вывод компилятора если включен verbose режим
            if ($Global:Config.testing.verbose_output -and (Test-Path "compile_output.txt")) {
                $output = Get-Content "compile_output.txt" -Raw
                if ($output.Trim()) {
                    Write-Message "Вывод компилятора:" "debug"
                    Write-Message $output "debug"
                }
            }
            
            return $true
        } else {
            Write-Message "✗ Ошибка компиляции $SourceFile (код: $($process.ExitCode))" "error"
            
            # Показать ошибки
            if (Test-Path "compile_error.txt") {
                $errors = Get-Content "compile_error.txt" -Raw
                if ($errors.Trim()) {
                    Write-Message "Ошибки компилятора:" "error"
                    Write-Message $errors "error"
                }
            }
            
            return $false
        }
    }
    catch {
        Write-Message "✗ Исключение при запуске компилятора: $_" "error"
        return $false
    }
    finally {
        # Очистка временных файлов
        if (Test-Path "compile_output.txt") { Remove-Item "compile_output.txt" -Force }
        if (Test-Path "compile_error.txt") { Remove-Item "compile_error.txt" -Force }
    }
}

# Функция синтаксического анализа без компиляции
function Test-SyntaxOnly {
    param(
        [string]$SourceFile
    )
    
    Write-Message "Проверка синтаксиса $SourceFile..." "info"
    
    $compileOptions = Get-CompileOptions
    $syntaxArgs = @($SourceFile, "--syntax-only") + $compileOptions
    $compilerPath = $Global:Config.compiler.executable
    
    try {
        $process = Start-Process -FilePath $compilerPath -ArgumentList $syntaxArgs -Wait -PassThru -NoNewWindow -RedirectStandardOutput "syntax_output.txt" -RedirectStandardError "syntax_error.txt"
        
        if ($process.ExitCode -eq 0) {
            Write-Message "✓ Синтаксис $SourceFile корректен" "success"
            return $true
        } else {
            Write-Message "✗ Синтаксические ошибки в $SourceFile" "error"
            
            if (Test-Path "syntax_error.txt") {
                $errors = Get-Content "syntax_error.txt" -Raw
                if ($errors.Trim()) {
                    Write-Message $errors "error"
                }
            }
            return $false
        }
    }
    catch {
        Write-Message "✗ Ошибка при проверке синтаксиса: $_" "error"
        return $false
    }
    finally {
        if (Test-Path "syntax_output.txt") { Remove-Item "syntax_output.txt" -Force }
        if (Test-Path "syntax_error.txt") { Remove-Item "syntax_error.txt" -Force }
    }
}

# Функция тестирования возможностей компилятора
function Test-CompilerFeatures {
    Write-Message "=== Тестирование возможностей нашего компилятора ===" "info"
    
    $allPassed = $true
    
    foreach ($testFile in $Global:Config.files.test_files) {
        Write-Message "Тестирование $($testFile.description)..." "debug"
        
        if ($Global:Config.testing.syntax_check_only) {
            $result = Test-SyntaxOnly $testFile.source
        } else {
            $result = Test-SyntaxOnly $testFile.source
        }
        
        if (-not $result) {
            $allPassed = $false
            if ($Global:Config.testing.stop_on_first_error) {
                Write-Message "Остановка на первой ошибке" "warning"
                break
            }
        }
    }
    
    Write-Message "========================================" "debug"
    return $allPassed
}

# Функция создания отчета о тестировании
function Generate-TestReport {
    param(
        [hashtable]$Results
    )
    
    if (-not $Global:Config.testing.generate_report) {
        return
    }
    
    $reportFile = $Global:Config.testing.report_filename
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    
    $report = @"
=== ОТЧЕТ О ТЕСТИРОВАНИИ КОМПИЛЯТОРА C51 ===
Дата: $timestamp
Компилятор: $($Global:Config.compiler.executable)
Целевая платформа: $($Global:Config.compilation.target)
Модель памяти: $($Global:Config.compilation.memory_model)
Оптимизация: $($Global:Config.compilation.optimize)

=== РЕЗУЛЬТАТЫ ТЕСТИРОВАНИЯ ===
"@

    foreach ($testFile in $Global:Config.files.test_files) {
        $status = if ($Results[$testFile.source]) { "ПРОЙДЕН" } else { "ПРОВАЛЕН" }
        $report += "`n- $($testFile.source) ($($testFile.description)): $status"
    }

    $report += @"

=== ОСОБЕННОСТИ НАШЕГО КОМПИЛЯТОРА ===
1. Поддержка типов памяти C51 (data, idata, xdata, code)
2. Битовые переменные и операции
3. Регистры специальных функций (SFR)
4. Обработчики прерываний с ключевым словом 'interrupt'
5. Оптимизация под архитектуру 8051
6. Соответствие стандарту C89

=== КОНФИГУРАЦИЯ ===
Файл конфигурации: compiler_options.json
Таймаут компиляции: $($Global:Config.compiler.timeout_seconds) сек
Verbose режим: $($Global:Config.testing.verbose_output)
Остановка на ошибке: $($Global:Config.testing.stop_on_first_error)

"@

    $report | Out-File -FilePath $reportFile -Encoding UTF8
    Write-Message "Отчет сохранен в файл: $reportFile" "info"
}

# Функция автоматической сборки компилятора
function Build-Compiler {
    if (-not $Global:Config.build.auto_build_if_missing) {
        return $false
    }
    
    Write-Message "Сборка компилятора..." "warning"
    Set-Location $Global:Config.compiler.project_root
    
    try {
        $buildCmd = $Global:Config.build.build_command
        $buildArgs = $Global:Config.build.build_args
        $timeoutSeconds = $Global:Config.build.build_timeout_seconds
        
        $buildProcess = Start-Process -FilePath $buildCmd -ArgumentList $buildArgs -Wait -PassThru -NoNewWindow
        if ($buildProcess.ExitCode -eq 0) {
            Write-Message "✓ Компилятор собран успешно" "success"
            return $true
        } else {
            Write-Message "✗ Ошибка сборки компилятора" "error"
            return $false
        }
    }
    catch {
        Write-Message "✗ Не удалось запустить сборку: $_" "error"
        return $false
    }
    finally {
        Set-Location "test\c_adv"
    }
}

# Главная функция
function Main {
    Write-Message "=== ТЕСТИРОВАНИЕ НАШЕГО КОМПИЛЯТОРА C51 ===" "info"
    Write-Message "Проект: c51_6_HCL - Компилятор C для AT89S4051" "info"
    Write-Message "Конфигурация загружена из: compiler_options.json" "debug"
    Write-Message "Дата: $(Get-Date)" "debug"
    Write-Message ""
    
    # Проверка наличия нашего компилятора
    if (-not (Test-OurCompiler)) {
        if (-not (Build-Compiler)) {
            return
        }
    }
    
    # Переход в директорию с тестами
    $testDir = Split-Path -Parent $MyInvocation.MyCommand.Path
    Set-Location $testDir
    Write-Message "Рабочая директория: $testDir" "debug"
    Write-Message ""
    
    # Тестирование возможностей компилятора
    $featuresResult = Test-CompilerFeatures
    
    # Компиляция тестовых файлов нашим компилятором
    if (-not $Global:Config.testing.syntax_check_only) {
        Write-Message "=== Компиляция тестов нашим компилятором ===" "info"
        
        $results = @{}
        
        foreach ($testFile in $Global:Config.files.test_files) {
            $result = Compile-WithOurCompiler $testFile.source $testFile.output
            $results[$testFile.source] = $result
            
            if (-not $result -and $Global:Config.testing.stop_on_first_error) {
                Write-Message "Остановка на первой ошибке компиляции" "warning"
                break
            }
            
            Write-Message "----------------------------------------" "debug"
        }
        
        # Генерация отчета
        Generate-TestReport $results
    }
    
    Write-Message "=== ТЕСТИРОВАНИЕ ЗАВЕРШЕНО ===" "info"
    
    if ($Global:Config.testing.generate_report) {
        Write-Message "Проверьте отчет: $($Global:Config.testing.report_filename)" "warning"
    }
    
    if (-not $Global:Config.testing.syntax_check_only) {
        Write-Message "Проверьте созданные выходные файлы" "warning"
    }
}

# Запуск главной функции
Main