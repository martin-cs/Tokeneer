cd code\core
gnatmake -Ptis
cd ..\simulators
gnatmake -Psim
cd ..\guidemo\src
gnatmake -Ptis_main
cd ..\..\test
gnatmake -Pmakecard

