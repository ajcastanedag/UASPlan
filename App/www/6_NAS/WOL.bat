@echo off

set url=

echo Waking up NAS!

start "" wakeup.exe 

echo Firefox will start after timeout!

timeout /t 30

start firefox.exe %url%

exit
