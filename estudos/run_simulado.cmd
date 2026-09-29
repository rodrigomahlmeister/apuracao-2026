@echo off
REM Roda o cliente contra o simulado ate 16:10 (Brasilia). Chamado pelo Agendador de Tarefas.
cd /d "%~dp0.."
"C:\Program Files\R\R-4.3.2\bin\Rscript.exe" R\run_simulado.R 16:10 >> "%TEMP%\run_simulado_console.txt" 2>&1
