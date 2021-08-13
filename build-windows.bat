
git clone --single-branch https://github.com/Verdagon/stdlib --branch staging

cd Valestrom

call sbt assembly

cd ..

cd Midas

cmake -B build -D LLVM_DIR="C:\llvm-install-minimum\lib\cmake\llvm"

cd build

cmake --build .

cd ..\..\Driver

call build.bat

cd ..\Tester

call build.bat

build\tester --valestrom_dir_override ..\Valestrom --midas_dir_override ..\Midas\build --builtins_dir_override ..\Midas\src\builtins --valec_dir_override ..\Driver\build --midas_tests_dir ..\Midas\test --concurrent 6 @assist

cd ..\scripts

call package-windows.bat

cd ..\release-windows

PATH=%PATH%;C:\Program Files\7-Zip
PATH=%PATH%;C:\Program Files\7-Zip\7z.exe
7z a ValeCompiler.zip *
