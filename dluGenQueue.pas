unit dluGenQueue;

{$mode ObjFpc}{$H+}

interface

{ TuQueue }
type generic TuQueue<_T> = class
   strict private
      type PQueueItem = ^TQueueItem;
           TQueueItem = record
                 Data : _T;
                 Next : PQueueItem;
              end;
      var  FHead      : PQueueItem;
           FTail      : PQueueItem;
           FCursor    : PQueueItem;     // kursor iteracji GetFirst/GetNext
           {$IFDEF DEBUG}
           FIterating : boolean;        // guard: wykrywa porzuconą iterację
           {$ENDIF}
   public
      constructor Create;
      destructor  Destroy; override;
      procedure   Clear;
      procedure   Push(const AElem: _T);
      function    Pop: _T;
      function    IsEmpty: boolean;
      function    TryPop(out AElem: _T): boolean;
      function    TryTest(out AElem: _T): boolean;
      //
      function    GetFirst(out AElem: _T): boolean;
      function    GetNext(out AElem: _T): boolean;
      //
end;

implementation

uses SysUtils;

{ TuQueue }

constructor TuQueue.Create;
begin
   inherited Create;
   FHead := nil;
   FTail := nil;
end;

destructor TuQueue.Destroy;
begin
   Clear;
   inherited Destroy;
end;

procedure TuQueue.Clear;
begin
   while not IsEmpty do Pop;
   {$IFDEF DEBUG}
   // Przerwana iteracja przez Clear jest sytuacją legalną — resetujemy guard
   FIterating := False;
   {$ENDIF}
end;

procedure TuQueue.Push(const AElem: _T);
   var p : PQueueItem;
begin
   New(p);
   p^.Data := AElem;
   p^.Next := nil;
   if FTail <> nil then
      FTail^.Next := p
   else
      FHead := p;  // kolejka była pusta — ustaw głowę
   FTail := p;
end;

function TuQueue.Pop: _T;
   var p : PQueueItem;
begin
   if IsEmpty then
      raise Exception.CreateFmt('%s.Pop: kolejka jest pusta', [ClassName]);
   p     := FHead;
   FHead := FHead^.Next;
   if FHead = nil then FTail := nil;
   Result := p^.Data;
   Dispose(p);
end;

function TuQueue.IsEmpty: boolean;
begin
   Result := (FTail = nil);
   {$IFDEF DEBUG}
   if Result and (FHead <> nil) then
      raise Exception.CreateFmt('%s.IsEmpty: niespójny stan (FHead<>nil, FTail=nil)', [ClassName]);
   {$ENDIF}
end;

function TuQueue.TryPop(out AElem: _T): boolean;
begin
   Result := not IsEmpty;
   if Result then AElem := Pop;
end;

function TuQueue.TryTest(out AElem: _T): boolean;
begin
   Result := not IsEmpty;
   if Result then AElem := FHead^.Data;
end;

function TuQueue.GetFirst(out AElem: _T): boolean;
begin
   {$IFDEF DEBUG}
   // Wywołanie GetFirst przy aktywnej iteracji oznacza porzucenie poprzedniej
   // (nie doszło do Result=False w GetNext) — wykrywamy jako błąd użycia
   if FIterating then
      raise Exception.CreateFmt('%s.GetFirst: poprzednia iteracja nie została zakończona', [ClassName]);
   FIterating := True;
   {$ENDIF}
   FCursor  := FHead;
   Result := GetNext(AElem);
end;

function TuQueue.GetNext(out AElem: _T): boolean;
begin
   Result := (FCursor <> nil);
   if Result then begin
      AElem := FCursor^.Data;
      FCursor := FCursor^.Next;
   end else begin
      {$IFDEF DEBUG}
      FIterating := False;  // iteracja zakończona poprawnie
      {$ENDIF}
   end;
end;

end.
