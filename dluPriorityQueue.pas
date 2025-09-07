unit dluPriorityQueue;

{$IFDEF FPC}
   {$mode Delphi}{$H+}
   {$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$ELSE}
   {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

uses SysUtils;

//------------------------------------------------------------------------------------------------
// Definicja generycznej funkcji porównującej
//------------------------------------------------------------------------------------------------
type TCompareFunc<T> = function(const Left, Right: T): Integer;

//------------------------------------------------------------------------------------------------
// Interfejs kolejki priorytetowej
//------------------------------------------------------------------------------------------------
type IPriorityQueue<T> = interface
    ['{2A5D191A-58B6-4921-9971-ADE9DF656552}']
    procedure Insert(Item: T);
    function IsEmpty: boolean;
    function Size: integer;
    function Top: T;
    function Dequeue: T;
end;

//------------------------------------------------------------------------------------------------
// Kolejka priorytetowa o zmiennej (nieustalonej) długości
// Zaimplementowana jako generyczny kopiec binarny z dynamiczną alokacją.
//------------------------------------------------------------------------------------------------
type TVariableLengthPriorityQueue<Key> = class(TInterfacedObject, IPriorityQueue<Key>)
  strict private
    var 
       //FCompare : TCompareFunc<Key>;
       //FHeap    : TArray<Key>;
       FCount   : integer;
       FCapacity: integer;

    procedure Resize(const newCapacity: integer);
    procedure Swim(k: integer);
    procedure Exchange(const i, j: integer);
    function IsLess(const i, j: integer): boolean;

  protected
    var
       FCompare : TCompareFunc<Key>;
       FHeap    : TArray<Key>;
    procedure Sink(k: integer);

  public
    constructor Create(ACompare: TCompareFunc<Key>; const initCapacity: integer = 10); virtual;
    destructor Destroy; override;
    
    // IPriorityQueue<T>
    procedure Insert(Item: Key); virtual;
    function IsEmpty: boolean;
    function Size: integer;
    function Top: Key;
    function Dequeue: Key;
end;

//------------------------------------------------------------------------------------------------
// Kolejka priorytetowa o stałej długości
//
//   Optymalne do znajdowania n ekstremalnych (minimalnych/maksymalnych) elementów.
//
//   Podstawowe znaczenie ma wartość parametru ACompare - czyli definicja funkcji porównującej
//   elementy wejściowe. Jest to funkcja ustalająca porządek w zbiorze danych wejściowych.
//      ACompare( a, b ) == | -1 gdy a jest "mniejsze" niż b,
//                          |  0 gdy a jest "równe" b
//                          | +1 gdy a jest "większe" niż b
//
//    Przykładowo:
//       ACompare( a,b: integer ) := a - b      [ znajdowanie wartości minimalnych ]
//       ACompare( a,b: integer ) := b - a      [ znajdowanie wartości maksymalnych ]
//
//------------------------------------------------------------------------------------------------
type TFixedLengthPriorityQueue<Key> = class(TVariableLengthPriorityQueue<Key>)
  strict private
    FCapacityLimit: integer;
  public
    constructor Create(ACompare: TCompareFunc<Key>; const initCapacity: integer); override;
    procedure Insert(Item: Key); override;
end;


implementation

(* web resources
https://algs4.cs.princeton.edu/24pq/
https://algs4.cs.princeton.edu/24pq/MaxPQ.java.html
*)


{ TVariableLengthPriorityQueue<T> }

constructor TVariableLengthPriorityQueue<Key>.Create(ACompare: TCompareFunc<Key>; const initCapacity: integer);
begin
   inherited Create;
   Assert(initCapacity > 0, 'Initial capacity must be positive');
   FCompare  := ACompare;
   FCount    := 0;
   FCapacity := initCapacity;
   SetLength(FHeap, FCapacity + 1);
end;

destructor TVariableLengthPriorityQueue<Key>.Destroy;
begin
   FHeap := nil;
   inherited Destroy;
end;

function TVariableLengthPriorityQueue<Key>.IsLess(const i, j: integer): boolean;
begin
   Result := FCompare(FHeap[i], FHeap[j]) < 0;
end;

procedure TVariableLengthPriorityQueue<Key>.Exchange(const i, j: integer);
  var tmp: Key;
begin
   if i <> j then begin
      tmp      := FHeap[i];
      FHeap[i] := FHeap[j];
      FHeap[j] := tmp;
   end;
end;

procedure TVariableLengthPriorityQueue<Key>.Swim(k: integer);
begin
   while (k > 1) and IsLess(k div 2, k) do begin
      Exchange(k, k div 2);
      k := k div 2;
   end;
end;

procedure TVariableLengthPriorityQueue<Key>.Sink(k: integer);
  var j: integer;
begin
   while (2 * k <= FCount) do begin
      j := 2 * k;
      if (j < FCount) and IsLess(j, j + 1) then
         Inc(j);
      if not IsLess(k, j) then
         break;
      Exchange(k, j);
      k := j;
   end;
end;

procedure TVariableLengthPriorityQueue<Key>.Resize(const newCapacity: integer);
begin
   Assert(newCapacity >= FCount + 1, 'New capacity cannot be less than current size');
   FCapacity := newCapacity;
   SetLength(FHeap, FCapacity + 1);
end;

procedure TVariableLengthPriorityQueue<Key>.Insert(Item: Key);
begin
   if FCount >= FCapacity then
      Resize(2 * FCapacity);

   Inc(FCount);
   FHeap[FCount] := Item;
   Swim(FCount);
end;

function TVariableLengthPriorityQueue<Key>.IsEmpty: boolean;
begin
   Result := FCount = 0;
end;

function TVariableLengthPriorityQueue<Key>.Top: Key;
begin
   if IsEmpty then
      raise Exception.Create('Brak elementow w kolejce priorytetowej');
   Result := FHeap[1];
end;

function TVariableLengthPriorityQueue<Key>.Dequeue: Key;
begin
   Result := Self.Top;
   Exchange(1, FCount);
   Dec(FCount);
   Sink(1);

   // Ustawienie wartości domyślnej, aby uniknąć błędów
   if FCount < High(FHeap) then
      FHeap[FCount + 1] := Default(Key);

   // Zmniejszenie rozmiaru tablicy, jeśli jest zbyt duża
   if (FCount > 0) and (FCount <= FCapacity div 4) then
      Resize(FCapacity div 2);
end;

function TVariableLengthPriorityQueue<Key>.Size: integer;
begin
   Result := FCount;
end;

{ TFixedLengthPriorityQueue<Key> }

constructor TFixedLengthPriorityQueue<Key>.Create(ACompare: TCompareFunc<Key>; const initCapacity: integer);
begin
   inherited Create(ACompare, initCapacity);
   FCapacityLimit := initCapacity;
end;

procedure TFixedLengthPriorityQueue<Key>.Insert(Item: Key);
begin
   if Size() < FCapacityLimit then begin
      inherited Insert(Item);
   end else if FCompare(Item, FHeap[1]) < 0 then begin
      FHeap[1] := Item;
      Sink(1);
   end;
end;

end.
