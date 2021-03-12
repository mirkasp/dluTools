unit dluPriorityQueue;

{$IFDEF FPC}
   {$mode Delphi}{$H+}
{$ELSE}
   {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

uses SysUtils;

type TCompFunc<T> = function(const Left, Right: T): Integer;

type IuPriorityQueue<T> = interface
    procedure Insert( Item: T );
    function IsEmpty: boolean;
    function Size: integer;
    function Top: T;
    function DelTop: T;
end;

// kolejka priorytetowa o zmiennej (nieustalonej) d³ugoœci
//
type TuVariableLengthPQ<Key> = class( TInterfacedObject, IuPriorityQueue<Key> )
  strict private
    var
       fCompare : TCompFunc<Key>;
       fPQ      : array of Key;
       fSwapTmp : Key;
       fN       : integer;
    procedure Resize( const newCapacity: integer );
    procedure Swim( k: integer ); inline; register;
    procedure Sink( k: integer ); inline; register;
    procedure Exchg(const i, j: integer); inline; register;
    function isMaxHeap( const k: integer = 1): boolean;     // is subtree of pq[1..n] rooted at k a max heap?
    function Less( const i, j: integer ): boolean; inline; register;
  public
    constructor Create( ACompare: TCompFunc<Key>; const initCapacity: integer = 1 ); virtual;
    procedure Insert( Item: Key ); virtual;
    function IsEmpty: boolean;
    function Size: integer;
    function Top: Key;
    function DelTop: Key;
end;

//------------------------------------------------------------------------------------------------
// Kolejka priorytetowa o sta³ej d³ugoœci
//
//   Pozwala na optymalne rozwi¹zywanie problemów typu:
//       znajdŸ n ekstremalnych (minimalnych/maksymalnych) elementów spoœród danych wejœciowych.
//
//   Podstawowe znaczenie ma wartoœæ parametru ACompare - czyli definicja funkcji porównuj¹cej
//   elementy wejœciowe. Jest to funkcja ustalaj¹ca porz¹dek w zbiorze danych wejœciowych.
//      ACompare( a, b ) == | -1 gdy a jest "mniejsze" ni¿ b,
//                          |  0 gdy a jest "równe" b
//                          | +1 gdy a jest "wiêksze" ni¿ b
//
//    Przyk³adowo:
//       ACompare( a,b: integer ) := a - b      [ znajdowanie wartoœci minimalnych ]
//       ACompare( a,b: integer ) := b - a      [ znajdowanie wartoœci maksymalnych ]
//
//------------------------------------------------------------------------------------------------
type TuFixedLengthPQ<Key> = class( TuVariableLengthPQ<Key> )
  strict private
    fInitialValue : integer;
  public
    constructor Create( ACompare: TCompFunc<Key>; const initCapacity: integer = 1 ); override;
    procedure Insert( Item: Key ); override;
end;


implementation

(* web resources
https://algs4.cs.princeton.edu/24pq/
https://algs4.cs.princeton.edu/24pq/MaxPQ.java.html
*)

{ TuVariableLengthPQ<T> }
constructor TuVariableLengthPQ<Key>.Create( ACompare: TCompFunc<Key>; const initCapacity: integer);
begin
   SetLength( fPQ, initCapacity+1 );
   fN       := 0;
   fCompare := ACompare;
end;

function TuVariableLengthPQ<Key>.Less(const i, j: integer): boolean;
begin
   Result := fCompare( fPQ[i], fPQ[j] ) < 0;
end;

function TuVariableLengthPQ<Key>.isMaxHeap(const k: integer): boolean;
  var left, right : integer;
begin
   if k>fN then Result := true
   else begin
       left  := k shl 1;
       right := left + 1;
       if (left <= fN) and Less( k, left ) then Result := false
       else
          if (right <= fN) and Less( k, right ) then Result := false
          else
             Result := isMaxHeap( left ) and isMaxHeap( right );
   end;
end;

procedure TuVariableLengthPQ<Key>.Exchg(const i, j: integer);
begin
   if i <> j then begin
      fSwapTmp  := fPQ[ i ];
      fPQ[ i ]  := fPQ[ j ];
      fPQ[ j ]  := fSwapTmp;
   end;
end;

procedure TuVariableLengthPQ<Key>.Swim(k: integer);
  var n : integer;
begin
   n := k shr 1;
   while (k>1) and Less( n, k ) do begin
       Exchg( k, n );
       k := n;
       n := k shr 1;
   end;
end;

procedure TuVariableLengthPQ<Key>.Sink(k: integer);
  var j, tmp : integer;
begin
   tmp := k shl 1;
   while (tmp <= fN) do begin
      j   := tmp;
      if (j<fN) and Less( j, j+1 ) then Inc(j);
      if not Less( k, j ) then break;
      Exchg( k, j );
      k   := j;
      tmp := k shl 1;
   end;
end;

procedure TuVariableLengthPQ<Key>.Resize(const newCapacity: integer);
begin
   Assert( newCapacity > fN );
   SetLength( fPQ, newCapacity );
end;

procedure TuVariableLengthPQ<Key>.Insert(Item: Key);
begin
   // double size of array if necessary
   if (fN = High( fPQ))
      then resize( 2 * Length(fPQ) );
   // add x, and percolate it up to maintain heap invariant
   Inc(fN);
   fPQ[ fN ] := Item;
   swim( fN );
   Assert( isMaxHeap( 1 ) );
end;

function TuVariableLengthPQ<Key>.IsEmpty: boolean;
begin
   Result := (fN = 0);
end;

function TuVariableLengthPQ<Key>.Top: Key;
//
// Zwraca topowy (najwiêkszy lub najmniejszy) klucz z kolejki priorytetowej.
// Zg³asza wyj¹tek, jeœli kolejka jest pusta.
//
begin
   if isEmpty() then raise Exception.Create('Brak elementow w kolejce priorytetowej');
   Result := fPQ[1];
end;


function TuVariableLengthPQ<Key>.DelTop: Key;
//
// Zwraca i usuwa topowy (najwiêkszy lub najmniejszy) klucz z kolejki priorytetowej.
// Zg³asza wyj¹tek, jeœli kolejka jest pusta.
//
  var m : Key;
begin
   Result := self.Top();
   ExChg( 1, fN );
   Dec( fN );
   sink( 1 );
   fPQ[ fN+1 ] := Default( Key ); //nil;  // to avoid loiteing and help with garbage collection
   if (fN > 0) and (fN = (Length(fPQ) -1) div 4 ) then Resize( Length(fPQ) div 2 );
   Assert( IsMaxHeap() );
end;

function TuVariableLengthPQ<Key>.Size: integer;
begin
   Result := fN;
end;

{ TuFixedLengthPriorityQueue<Key> }

constructor TuFixedLengthPQ<Key>.Create(ACompare: TCompFunc<Key>; const initCapacity: integer);
begin
   inherited Create( ACompare, initCapacity+1 );
   fInitialValue := initCapacity;
end;

procedure TuFixedLengthPQ<Key>.Insert(Item: Key);
begin
   inherited Insert( Item );
   if size() > fInitialValue
      then DelTop();
end;

end.
