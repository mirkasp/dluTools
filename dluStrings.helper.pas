unit dluStrings.Helper;

{$I dluOptions.inc}

interface

uses Classes;

type

{ TStringsHelper }

 TStringsHelper = class helper for TStrings
   public
      procedure AddFmt( const AText: string; const AParam: array of const );
      procedure AddLine( const AChar: char; const ACount: integer = 80 );
      procedure AddPair( const AKey: string; const AValue: Variant ); overload;
      procedure AddPair( const AKey: string; const AValue: string  ); overload;
      procedure AddPair( const AKey: string; const AValue: integer ); overload;
      procedure AddPair( const AKey: string; const AFormat: string; const AParam: array of const ); overload;
end;

implementation

uses SysUtils, Variants;

{ TStringsHelper }

procedure TStringsHelper.AddFmt(const AText: string; const AParam: array of const);
begin
  self.Add( Format( AText, AParam ) );
end;

procedure TStringsHelper.AddLine(const AChar: char; const ACount: integer);
begin
  self.Add( StringOfChar( AChar, ACount ) );
end;

procedure TStringsHelper.AddPair(const AKey: string; const AValue: Variant);
  var vt: Word;
begin
   vt := VarType( AValue );
   case vt of
      varEmpty    : self.AddPair( AKey,  'Value: Unassigned' );     // The variant is Unassigned.
      varNull     : self.AddPair( AKey,  'Value: NULL' );           // The variant is Null.
      varSmallint : self.AddPair( AKey,  Integer( AValue ) ); // 16-bit signed integer (type Smallint in Delphi, short in C++ ).
      varInteger  : self.AddPair( AKey,  Integer( AValue ) );// 32-bit signed integer (type Integer in Delphi, int in C++).
//      varSingle   : // Single-precision floating-point value (type Single in Delphi, float in C++).
//      varDouble   : // Double-precision floating-point value (type double).
//      varCurrency :  // Currency floating-point value (type Currency).
//      varDate     : // Date and time value (type TDateTime).
//      varOleStr   : // Reference to a dynamically allocated UNICODE string.
//      varDispatch :  // Reference to an Automation object (an IDispatch interface pointer).
//      varError    : // Operating system error code.
      varBoolean  : self.AddPair( AKey,  BoolToStr( Boolean( AValue ), true ) ); // 16-bit boolean (type WordBool).
//      varVariant  : // A variant.
//      varUnknown  : // Reference to an unknown object (an IInterface or IUnknown interface pointer).
//      varShortInt : // 8-bit signed integer (type ShortInt in Delphi or signed char in C++)
//      varByte     : // A Byte
//      varWord     : // unsigned 16-bit value (Word)
//      varLongWord : // unsigned 32-bit value (type LongWord in Delphi or unsigned long in C++)
//      varInt64    : // 64-bit signed integer (Int64 in Delphi or __int64 in C++)
//      varStrArg   : // COM-compatible string.
      varString   : self.AddPair( AKey,  String(AValue) );            // Reference to a dynamically allocated string (not COM compatible).
      varUString  : self.AddPair( AKey,  UnicodeString(AValue) );     //
      else self.AddPair( AKey,  'Unsupported variant value (%s)', [ VarTypeAsText( vt ) ] );
   end;
end;

procedure TStringsHelper.AddPair(const AKey, AValue: string);
begin
   self.Add( AKey + '=' + AValue );
end;

procedure TStringsHelper.AddPair(const AKey: string; const AValue: integer);
begin
   self.AddPair( AKey, IntToStr( AValue ) );
end;

procedure TStringsHelper.AddPair(const AKey, AFormat: string; const AParam: array of const);
begin
   self.AddPair( AKey, Format( AFormat, AParam ) );
end;

end.

