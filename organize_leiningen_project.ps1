# Скрипт для организации Leiningen проекта
# Автор: AI Помощник
# Версия: 1.2
# Назначение: Систематизация проекта с сохранением структуры Leiningen

# Параметры скрипта
param (
    [switch]$CreateBackup = $true,
    [switch]$Force = $false,
    [string]$ProjectNamespace = "c51cc",
    [switch]$WhatIf = $false
)

# Стандартная структура Leiningen:
# project/
# ├── project.clj
# ├── README.md
# ├── doc/
# ├── src/
# │   └── [namespace]/
# ├── test/
# │   └── [namespace]/
# ├── resources/
# └── target/

# Функция для логирования операций
function Write-LogMessage {
    param (
        [string]$Message,
        [string]$Type = "Info"
    )
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $color = switch ($Type) {
        "Info" { "Cyan" }
        "Warning" { "Yellow" }
        "Error" { "Red" }
        "Success" { "Green" }
        default { "White" }
    }
    Write-Host "[$timestamp][$Type] $Message" -ForegroundColor $color
}

# Функция для создания бэкапа
function Backup-Project {
    $backupDir = "backup_$(Get-Date -Format 'yyyyMMdd_HHmmss')"
    Write-LogMessage "Создание резервной копии проекта в $backupDir" -Type "Info"
    
    try {
        New-Item -ItemType Directory -Path $backupDir | Out-Null
        Copy-Item -Path "*.clj" -Destination $backupDir -Recurse -ErrorAction Stop
        Write-LogMessage "Резервная копия создана успешно" -Type "Success"
        return $true
    }
    catch {
        Write-LogMessage ("Ошибка при создании резервной копии: " + $_.Exception.Message) -Type "Error"
        return $false
    }
}

# Функция проверки валидности Clojure файла
function Test-ClojureFile {
    param (
        [string]$FilePath
    )
    
    try {
        $content = Get-Content $FilePath -Raw
        # Базовая проверка на парные скобки
        $openCount = ($content.ToCharArray() | Where-Object { $_ -eq '(' } | Measure-Object).Count
        $closeCount = ($content.ToCharArray() | Where-Object { $_ -eq ')' } | Measure-Object).Count
        
        if ($openCount -ne $closeCount) {
            Write-LogMessage "Предупреждение: Файл $FilePath может содержать несбалансированные скобки" -Type "Warning"
            return $false
        }
        return $true
    }
    catch {
        Write-LogMessage ("Ошибка при проверке файла " + $FilePath + ": " + $_.Exception.Message) -Type "Error"
        return $false
    }
}

# Создаем стандартные директории Leiningen, если их нет
$leinDirectories = @(
    "src",
    "test", 
    "resources", 
    "doc", 
    "target"
)

# Проверка существования project.clj
if (Test-Path "project.clj") {
    Write-LogMessage "Обнаружен существующий project.clj" -Type "Info"
    if (-not $Force) {
        Write-LogMessage "Используйте параметр -Force для перезаписи существующих файлов" -Type "Warning"
        exit
    }
}

# Создание бэкапа если требуется
if ($CreateBackup) {
    if (-not (Backup-Project)) {
        Write-LogMessage "Прерывание операции из-за ошибки создания бэкапа" -Type "Error"
        exit
    }
}

# Создаем стандартные директории Leiningen, если их нет
foreach ($dir in $leinDirectories) {
    if (-not (Test-Path -Path $dir)) {
        try {
            New-Item -ItemType Directory -Path $dir | Out-Null
            Write-LogMessage ("Создана директория: " + $dir) -Type "Success"
        }
        catch {
            Write-LogMessage ("Ошибка при создании директории " + $dir + ": " + $_.Exception.Message) -Type "Error"
        }
    }
}

# Функция анализа неймспейса из содержимого файла
function Get-ClojureNamespace {
    param (
        [string]$FilePath
    )
    
    try {
        $content = Get-Content $FilePath -Raw
        if ($content -match '(?m)^\s*\((?:clojure.core/)?ns\s+([^\s\)]+)') {
            return $matches[1]
        }
        # Если ns не найден, извлекаем из имени файла
        $fileName = [System.IO.Path]::GetFileNameWithoutExtension($FilePath)
        return $fileName -replace "^(test_|debug_)", ""
    }
    catch {
        Write-LogMessage ("Ошибка при анализе неймспейса файла " + $FilePath + ": " + $_.Exception.Message) -Type "Error"
        return $null
    }
}

# Функция определения типа Clojure файла
function Get-ClojureFileType {
    param (
        [string]$FilePath
    )
    
    $extension = [System.IO.Path]::GetExtension($FilePath)
    switch ($extension) {
        ".clj"  { return "clj" }    # Clojure
        ".cljc" { return "cljc" }   # Cross-platform
        ".cljs" { return "cljs" }   # ClojureScript
        default { return "unknown" }
    }
}

# Функция для анализа и планирования перемещений
function Get-FileOperationPlan {
    $plan = @()
    
    # Исключаем специальные директории
    $excludeDirs = @('.clj-kondo', '.lsp', '.vscode', '.git')
    
    Get-ChildItem -Path . -Filter "*.clj*" | Where-Object {
        $file = $_
        # Проверяем что это файл и не находится в исключенных директориях
        $file.PSIsContainer -eq $false -and
        ($excludeDirs | ForEach-Object { $file.FullName -notmatch [regex]::Escape($_) }) -notcontains $false
    } | ForEach-Object {
        $file = $_
        $fileType = Get-ClojureFileType $file.FullName
        $namespace = Get-ClojureNamespace $file.FullName
        
        $destinationDir = if ($file.Name -match "^test_|^debug_" -or $file.Directory.Name -eq "test") {
            "test"
        } else {
            "src"
        }

        # Определяем поддиректорию на основе неймспейса
        $subDir = if ($namespace) {
            $namespace -replace "\.", "/"
        } else {
            $file.BaseName -replace "^(test_|debug_)", ""
        }

        $targetPath = Join-Path $destinationDir $subDir
        
        $plan += @{
            SourceFile = $file.FullName
            FileType = $fileType
            Namespace = $namespace
            DestinationDir = $targetPath
            FileName = $file.Name
            IsTest = $destinationDir -eq "test"
        }
    }
    
    return $plan
}

# Функция для отображения плана операций
function Show-OperationPlan {
    param (
        [array]$Plan
    )
    
    Write-LogMessage "План организации файлов:" -Type "Info"
    Write-LogMessage "========================" -Type "Info"
    
    $Plan | ForEach-Object {
        $color = if ($_.IsTest) { "Yellow" } else { "Cyan" }
        $direction = "[$($_.FileType)] $($_.SourceFile) -> $($_.DestinationDir)"
        Write-Host $direction -ForegroundColor $color
        
        if ($_.Namespace) {
            Write-Host "   Namespace: $($_.Namespace)" -ForegroundColor Gray
        }
    }
    
    Write-LogMessage "========================" -Type "Info"
    Write-LogMessage "Всего файлов к перемещению: $($Plan.Count)" -Type "Info"
}

# Основной блок выполнения
try {
    # Получаем план операций
    $operationPlan = Get-FileOperationPlan
    
    # Показываем план
    Show-OperationPlan $operationPlan
    
    if ($WhatIf) {
        Write-LogMessage "Режим предварительного просмотра. Изменения не были применены." -Type "Warning"
        exit
    }
    
    # Создаем backup если требуется
    if ($CreateBackup) {
        if (-not (Backup-Project)) {
            Write-LogMessage "Прерывание операции из-за ошибки создания бэкапа" -Type "Error"
            exit
        }
    }
    
    # Выполняем операции по плану
    foreach ($operation in $operationPlan) {
        try {
            $targetDir = $operation.DestinationDir
            
            # Создаем директорию назначения
            if (-not (Test-Path -Path $targetDir)) {
                New-Item -ItemType Directory -Path $targetDir -Force | Out-Null
                Write-LogMessage "Создана директория: $targetDir" -Type "Success"
            }
            
            # Определяем путь назначения
            $destinationPath = Join-Path $targetDir $operation.FileName
            
            # Проверяем конфликты
            if (Test-Path $destinationPath) {
                $newName = "$($operation.FileName -replace '\.clj.*$', '')_$(Get-Date -Format 'yyyyMMddHHmmss')$([System.IO.Path]::GetExtension($operation.FileName))"
                $destinationPath = Join-Path $targetDir $newName
                Write-LogMessage "Конфликт имен: $($operation.FileName) будет переименован в $newName" -Type "Warning"
            }
            
            # Перемещаем файл
            Move-Item -Path $operation.SourceFile -Destination $destinationPath -Force
            Write-LogMessage "Перемещен файл: $($operation.FileName) в $destinationPath" -Type "Success"
        }
        catch {
            Write-LogMessage ("Ошибка при обработке файла " + $operation.FileName + ": " + $_.Exception.Message) -Type "Error"
        }
    }
    
    # Создаем project.clj, если его нет
    if (-not (Test-Path "project.clj") -or $Force) {
        $projectClj = @"
(defproject $ProjectNamespace "0.1.0-SNAPSHOT"
  :description "Reorganized Leiningen Project"
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot $ProjectNamespace.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/tools.namespace "1.4.4"]]
                   :plugins [[lein-ancient "1.0.0-RC3"]]}})
"@
        $projectClj | Out-File -Encoding UTF8 "project.clj"
        Write-LogMessage "Создан project.clj с неймспейсом $ProjectNamespace" -Type "Success"
    }

    # Очистка пустых директорий
    Get-ChildItem -Directory | Where-Object { 
        $_.GetFiles().Count -eq 0 -and 
        $_.Name -notin $leinDirectories -and
        $_.Name -notlike "backup_*"
    } | ForEach-Object {
        try {
            Remove-Item $_.FullName
            Write-LogMessage "Удалена пустая директория: $($_.Name)" -Type "Success"
        }
        catch {
            Write-LogMessage "Ошибка при удалении директории $($_.Name): $_" -Type "Error"
        }
    }

    Write-LogMessage "Организация Leiningen проекта завершена!" -Type "Success"
}
catch {
    Write-LogMessage ("Критическая ошибка: " + $_.Exception.Message) -Type "Error"
    exit 1
} 