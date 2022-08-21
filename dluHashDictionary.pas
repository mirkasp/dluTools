unit dluHashDictionary;
(*******************************************************************************
  dluHashDictionary - based on hash table general dictionary module.

  2022-08-21 add AValueDup parameter in Indert procedure
  2022-08-20 initial version
********************************************************************************)
{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

interface

type generic TKeyArray<K> = array of K;

type generic IDictionary<K, V> = interface
   procedure Insert( const Key: K; const Value: V; const AValueDup: boolean );
   function GetFirstValueForKey( const Key: K; var Value: V ): boolean;
   function GetNextValueForKey( var Value: V ): boolean;
   function Keys(): specialize TKeyArray<K>;
   //
end;

type generic TEqualFunc<T>    = function( const Left, Right: T ): boolean;

type THashIndex = Byte;
type generic THashFunction<T> = function( const AKey: T ): THashIndex;

{ THashDictionary }
type generic THashDictionary<K, V> = class( TInterfacedObject, specialize IDictionary<K, V> )
  strict private
    type
       TKeyEqualFunc   = specialize TEqualFunc<K>;
       TValueEqualFunc = specialize TEqualFunc<V>;
       THashFunc       = specialize THashFunction<K>;

       PValueItem      = ^TValueItem;
       TValueItem      = record
               Key     : K;
               Values  : array of V;
               Next    : PValueItem;
            end;
    var
       fKeyEqualFunc   : TKeyEqualFunc;
       fValueEqualFunc : TValueEqualFunc;
       fHashFunc       : THashFunc;
       //
       fHashTable      : array[ THashIndex ] of PValueItem;
       fSearchItemPtr  : PValueItem;
       fSearchItemIdx  : integer;
       //
    function LocateItemPtr( const AHashIndex: THashIndex; const AKey: V ): PValueItem;
  public
    constructor Create( AKeyEqualFunc: TKeyEqualFunc; AValueEqualFunc: TValueEqualFunc; AHashFunc: THashFunc ); virtual;
    destructor Destroy; override;
    //
    procedure Insert( const AKey: K; const AValue: V; const AValueDup: boolean = false );
    //
    function GetFirstValueForKey( const AKey: K; var Value: V ): boolean;
    function GetNextValueForKey( var Value: V ): boolean;
    //
    function Keys(): specialize TKeyArray<K>;
end;



implementation

{ HashDictionary }

constructor THashDictionary.Create( AKeyEqualFunc  : TKeyEqualFunc;
                                    AValueEqualFunc: TValueEqualFunc;
                                    AHashFunc      : THashFunc );
  var idx : THashIndex;
begin
   inherited Create;
   fKeyEqualFunc   := AKeyEqualFunc;
   fValueEqualFunc := AValueEqualFunc;
   fHashFunc       := AHashFunc;
   for idx := Low( fHashTable ) to High( fHashTable ) do fHashTable[ idx ] := nil;
end;

destructor THashDictionary.Destroy;
  var idx : THashIndex;
      px  : PValueItem;
begin
   for idx := Low( fHashTable ) to High( fHashTable ) do begin
      while Assigned( fHashTable[ idx ] ) do begin
         px := fHashTable[ idx ]^.Next;
         Dispose( fHashTable[ idx ] );
         fHashTable[ idx ] := px;
      end;
      Assert( fHashTable[ idx ] = nil )
   end;
   inherited Destroy;
end;

function THashDictionary.LocateItemPtr( const AHashIndex: THashIndex; const AKey: V ): PValueItem;
begin
   Result := fHashTable[ AHashIndex ];
   while Result <> nil do
      if fKeyEqualFunc( Result^.Key, AKey )
         then break
         else Result := Result^.Next;
end;

procedure THashDictionary.Insert( const AKey: K; const AValue: V; const AValueDup: boolean);
  var px      : THashIndex;
      itemPtr : PValueItem;
      kI, i   : integer;
begin
   px      := fHashFunc( AKey );
   itemPtr := LocateItemPtr( px, AKey );

   if itemPtr = nil then begin
      New( itemPtr );
      with itemPtr^ do begin
         Key       := AKey;
         SetLength( Values, 0 );
         Next      := fHashTable[ px ];
      end;
      fHashTable[ px ] := itemPtr;
   end;

   kI := Length( itemPtr^.Values );

   // locate value
   if fValueEqualFunc <> nil
      then i := kI -1
      else i := -1;

   while i >= 0 do
      if fValueEqualFunc( itemPtr^.Values[i], AValue )
         then break
         else Dec(i);

   if (i < 0) or AValueDup then begin
      SetLength( itemPtr^.Values, kI + 1 );
      itemPtr^.Values[ kI ] := AValue;
   end;

end;

function THashDictionary.GetFirstValueForKey(const AKey: K; var Value: V ): boolean;
  var ItemPtr : PValueItem;
begin
   itemPtr := LocateItemPtr( fHashFunc( AKey ), AKey );
   Result  := Assigned( ItemPtr );
   if Result then begin
      fSearchItemPtr := ItemPtr;
      fSearchItemIdx := 0;
      Result         := GetNextValueForKey( Value );
   end;

end;

function THashDictionary.GetNextValueForKey(var Value: V): boolean;
begin
   Result := (fSearchItemIdx >= 0) and (fSearchItemIdx <= High( fSearchItemPtr^.Values) );
   if Result then begin
      Value := fSearchItemPtr^.Values[ fSearchItemIdx ];
      Inc( fSearchItemIdx );
   end;
end;

function THashDictionary.Keys: specialize TKeyArray<K>;
  var idx : THashIndex;
      px  : PValueItem;
      cnt : integer;
begin
   Result := nil;
   cnt    := 0;
   for idx := Low( fHashTable ) to High( fHashTable ) do begin
      px := fHashTable[ idx ];
      while Assigned( px ) do begin
         Inc( cnt );
         px := px^.Next;
      end;
   end;
   SetLength( Result, cnt );
   if cnt > 0 then begin
      cnt := 0;
      for idx := Low( fHashTable ) to High( fHashTable ) do begin
         px := fHashTable[ idx ];
         while Assigned( px ) do begin
            Result[ cnt ] := px^.Key;
            Inc( cnt );
            px := px^.Next;
         end;
      end;
   end;
end;

end.

