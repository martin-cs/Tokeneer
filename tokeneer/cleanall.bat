cd code\core
gnatclean -Ptis
cd ..\simulators
gnatclean -Psim
cd ..\guidemo\src
gnatclean -Ptis_main
cd ..\..\test
gnatclean -Pmakecard

