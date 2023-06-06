@echo off

set url="http://132.187.202.171:5000/"

echo Waking up NAS!

start "" wakeup.exe 90:09:D0:19:67:03

echo Firefox will start after timeout!

timeout /t 30

start firefox.exe %url%

exit
