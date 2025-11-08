{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TinySysInfo;

{$warn 5023 off : no warning about unused units}
interface

uses
  frmMxSysInfo, siNodeIntf, siNodeParam, lxKeyValueArray, lxWinVer4, 
  lxWinVer32, lxWinVerNT, LazarusPackageIntf;

implementation

procedure Register;
begin
end ;

initialization
  RegisterPackage( 'TinySysInfo', @Register) ;
end .
