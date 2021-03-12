@echo off
echo Kasowanie œmieci...
echo ------------------------------
del /s/q *.bak;*.dbg;*.dcu;*.map >nul
rd lib /Q/S >> nul
for %%x in ( . ) do (
  echo Folder %%x\
  if exist %%x\__history\nul (
     del %%x\__history\*.* /Q >> nul
     rd  %%x\__history /Q/S >> nul
  ) 
  if exist %%x\backup\nul (
     del %%x\backup\*.* /Q >> nul
     rd  %%x\backup /Q/S >> nul
  )
)
rem pause
echo on
