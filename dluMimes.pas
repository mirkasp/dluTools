unit dluMimes;

{$mode ObjFPC}{$H+}

interface

uses SysUtils;

// Retrieves MIME types for a given file extension.
// The result is an array of strings, guaranteed to contain at least one element.
function GetMimeForExt( const AFileName: string ): TStringArray;

// Retrieves file extensions for a given MIME type.
// The result is an array of strings.
function GetExtForMime( const AMimeType: string ): TStringArray;

// Retrieves the default MIME type for a given file.
// The result is a single string.
function GetDefaultMime( const AFileName: string ): string;

// Returns a string containing statistics about the internal dictionaries.
function GetStatistics(): string;

implementation

uses Classes
   , Generics.Collections
   , SyncObjs
   , Registry
   , dluRegistry.helper
   , dluAssociatedStrings
   ;

type TLocalCriticalSection = SyncObjs.TCriticalSection;

type TMimeDescription = record
     Extension        : string; // Extension with the dot, e.g., '.txt'
     MimeType         : string;
end;

const MAX_MIME_COUNT = 951;
const cStandardMimeList: array[0..MAX_MIME_COUNT-1] of TMimeDescription = (
{$I StandardMime.inc} // Include standard MIME type definitions
);

{ TMimeManager }
type TMimeManager = class
   strict private
      const cDefaultMime = 'application/octet-stream';
      const REG_MIME_KEY = '\MIME\Database\Content Type\';             {do not localize}
      //
      type TSpecializeDict = specialize TObjectDictionary<string, TAssociatedStrings>;
      //
      // Two dictionaries: one maps Extension->MimeList, the other Mime->ExtensionList.
      // We use TObjectDictionary with [doOwnsValues] to automatically free TAssociatedStrings.
      var FExtensionToMime : TSpecializeDict;
          FMimeToExtension : TSpecializeDict;

      // Internal methods
      procedure AddToDictionary( ADict: TSpecializeDict; const AKey, AValue: string);
      procedure AddMapping(const AExtension, AMimeType: string);
      function FetchData( ADict: TSpecializeDict; const AKey: string): TStringArray;
      // private constructor
      {%H-}constructor CreateManager();

      // Methods called during reg.Iterate procedure
      function IsFileExtensionKey( const ABuffer: PChar ): boolean;
      procedure MapExtensionToMime( ARegistry: TRegistry; const ASubKey, AKeyName: string );
      procedure MapMimeToExtension( ARegistry: TRegistry; const ASubKey, AKeyName: string );
   public
      class function Instance: TMimeManager;
      destructor Destroy; override;

      // Main public methods
      function GetMimeTypes(const AFileName: string): TStringArray;
      function GetExtensions(const AMimeType: string): TStringArray;
      function GetDefaultMimeForFile(const AFileName: string): string;
      function GetStatisticsInfo(): string;
end;

(******************************************************************************)
function GetMimeForExt( const AFileName: string) : TStringArray;
begin
   Result := TMimeManager.Instance.GetMimeTypes(AFileName);
end;

function GetExtForMime( const AMimeType: string) : TStringArray;
begin
   Result := TMimeManager.Instance.GetExtensions(AMimeType);
end;

function GetDefaultMime( const AFileName: string ) : string;
begin
   Result := TMimeManager.Instance.GetDefaultMimeForFile(AFileName);
end;

function GetStatistics(): string;
begin
   Result := TMimeManager.Instance.GetStatisticsInfo();
end;

(******************************************************************************)

var
  _MimeManager : TMimeManager = nil;
  _InitLock    : TLocalCriticalSection = nil;

{ TMimeManager }

class function TMimeManager.Instance: TMimeManager;
begin
   // Double-checked locking pattern
   if not Assigned(_MimeManager) then begin
      _InitLock.Enter;
      try
         if not Assigned(_MimeManager) then
            _MimeManager := TMimeManager.CreateManager();
      finally
         _InitLock.Leave;
      end;
   end;
   Result := _MimeManager;
end;

constructor TMimeManager.CreateManager();
  var i: integer;
begin
   inherited Create;

   // doOwnsValues means the dictionary will free TAssociatedStrings objects when
   // keys are removed or the dictionary is destroyed
   FExtensionToMime := TSpecializeDict.Create( [doOwnsValues] );
   FMimeToExtension := TSpecializeDict.Create( [doOwnsValues] );

   //-----------------------------------------------------------------------------
   // populate dictionaries with standard data
   //
   for i:=0 to MAX_MIME_COUNT-1 do AddMapping( cStandardMimeList[i].Extension, cStandardMimeList[i].MimeType );

   //-----------------------------------------------------------------------------
   // augment dictionaries with data from the registry
   //
   with TRegistry.Create(KEY_READ) do begin
      RootKey := HKEY_CLASSES_ROOT;
      try

         // 1. Now we search all extension types,
         //    looking for the appropriate MIME type in the 'Content Type' property.
         IterateKeys( '\', @IsFileExtensionKey, @MapExtensionToMime );

         // 2. Now we search all keys in the MIME branch,
         //    looking for the corresponding 'Extension' property.
         IterateKeys(  REG_MIME_KEY, nil, @MapMimeToExtension );

      finally
         Free;
      end;
   end;

end;

destructor TMimeManager.Destroy;
begin
   FExtensionToMime.Free;
   FMimeToExtension.Free;
   inherited Destroy;
end;

function TMimeManager.FetchData(ADict: TSpecializeDict; const AKey: string) : TStringArray;
  var List: TAssociatedStrings;
      LowerKey : string;
begin
   Result   := nil;
   LowerKey := LowerCase( AKey );
   if LowerKey.IsEmpty then Exit;

   if ADict.TryGetValue( LowerKey, List) then begin
      Result := List.GetValues;
   end else begin
      SetLength( Result, 1 );
      Result[0] := cDefaultMime;
   end ;
end;

procedure TMimeManager.AddToDictionary( ADict: TSpecializeDict; const AKey, AValue: string );
  var List: TAssociatedStrings;
begin
   if not ADict.TryGetValue( AKey, List) then begin
      List := TAssociatedStrings.Create();
      ADict.Add( AKey, List );
   end;
   List.Add( AValue ) ;
end;

procedure TMimeManager.AddMapping( const AExtension, AMimeType: string);
  var LMime     : string;
      LExtension: string;
begin
   // input validation
   if AExtension.IsEmpty or AMimeType.IsEmpty then Exit;

   LExtension := LowerCase( AExtension) ;
   // Ensure the extension starts with a dot
   if not LExtension.StartsWith( '.') then LExtension := '.' + LExtension;

   LMime := LowerCase( AMimeType);

   // 1. Map Extension -> MIME
   AddToDictionary( FExtensionToMime, LExtension, LMime );

   // 2. Map MIME -> Extension
   AddToDictionary( FMimeToExtension, LMime, LExtension );

end;

function TMimeManager.GetDefaultMimeForFile( const AFileName: string) : string;
  var LExtension: string;
      List      : TAssociatedStrings;
begin
   LExtension := LowerCase( ExtractFileExt(AFileName) );
   // If the extension exists, return its default MIME type, otherwise return the global default.
   if not LExtension.IsEmpty and FExtensionToMime.TryGetValue(LExtension, List)
      then Result := List.DefaultValue
      else Result := cDefaultMime;
end;

function TMimeManager.GetStatisticsInfo(): string;

   function GetStatForDictionary( const ADict: TSpecializeDict ): string;
      var k, min, max, sum: integer;
          pair : specialize TPair<string, TAssociatedStrings>;
          maxS : string;
   begin
       min := 999; max := 0; sum := 0; k := 0; maxS := '';
       // Iterate over all pairs (Key, TAssociatedStrings)
       for pair in ADict do begin
          if pair.Value.Count < min then min := pair.Value.Count;
          if pair.Value.Count > max then begin max := pair.Value.Count; maxS := pair.Key; end;
          Inc( sum, pair.Value.Count ); // Sum of the number of items
          Inc( k,   1 ); // Number of keys
       end ;
       Result := Format( 'count=%4d', [k] );
       if k > 0
          then Result := Result + Format( ', min=%d, avg=%4.2f, max=%2d (%s)', [ min, sum / k, max, maxS ] );
   end;

begin
   Result := 'Statistics:' + sLineBreak +
             '--------------------------------------------' + sLinebreak +
             'ExtToMime: ' + GetStatForDictionary( FExtensionToMime ) + sLineBreak +
             'MimeToExt: ' + GetStatForDictionary( FMimeToExtension );
end;

(******************************************************************************)
function TMimeManager.GetMimeTypes(const AFileName: string): TStringArray;
begin
   Result := FetchData( FExtensionToMime, ExtractFileExt(AFileName) );
end;

function TMimeManager.GetExtensions(const AMimeType: string): TStringArray;
begin
   Result := FetchData( FMimeToExtension, AMimeType );
end;

// Checks if the key in the registry is a file extension key (starts with a dot)
function TMimeManager.IsFileExtensionKey( const ABuffer: PChar) : boolean;
begin
   Result := Assigned(ABuffer) and (ABuffer[0] = '.');
end ;

// Call-back procedure for searching extension keys (.ext)
// ASubKey = '\'
// AKeyName ?= '.xyz'
procedure TMimeManager.MapExtensionToMime( ARegistry: TRegistry; const ASubKey, AKeyName: string) ;
begin
   with ARegistry do begin
      if OpenKeyReadOnly( ASubKey + AKeyName ) then begin
         try
            // Read the 'Content Type' value and add the mapping
            AddMapping( AKeyName, LowerCase( ReadString('Content Type') ) );
         except
            // Ignore errors (e.g., if 'Content Type' doesn't exist)
         end;
      end;
      CloseKey;
   end;
end;

// Call-back procedure for searching keys in the MIME branch
// ASubKey = REG_MIME_KEY
// AKeyName ?= 'video/mpeg'
procedure TMimeManager.MapMimeToExtension( ARegistry: TRegistry; const ASubKey, AKeyName: string) ;
begin
   with ARegistry do begin
      if OpenKeyReadOnly( ASubKey + AKeyName ) then begin
         try
            // Read the 'Extension' value and add the mapping
            AddMapping( LowerCase( ReadString('Extension') ), AKeyName );
         except
            // Ignore errors (e.g., if 'Extension' doesn't exist)
         end;
      end;
      CloseKey;
   end;
end ;

initialization
   _InitLock := TLocalCriticalSection.Create;

finalization
   if Assigned(_MimeManager) then _MimeManager.Free;
   _InitLock.Free;

end.
