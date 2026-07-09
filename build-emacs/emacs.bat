@echo off

setlocal

rem Adjust these two to your MSYS2 root and the install PREFIX
rem (as a Windows path) used by install-emacs-msys2.sh.
set "MSYS2_ROOT=C:\tools\msys64"
set "EMACS_HOME=C:\Users\probe\emacs"

rem Emacs needs the UCRT64 DLLs (and MSYSTEM for subprocesses).
set "PATH=%MSYS2_ROOT%\ucrt64\bin;%PATH%"
set "MSYSTEM=UCRT64"

start "" "%EMACS_HOME%\bin\runemacs.exe" %*

endlocal
