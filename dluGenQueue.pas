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
           {$IFOPT D+}
           FIterating : boolean;        // guard: wykrywa porzuconą iterację
           {$ENDIF}
      procedure DebugLegalCheck(const AMethod: string);
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

procedure TuQueue.DebugLegalCheck(const AMethod: string);
begin
   {$IFOPT D+}
   if FIterating then
      raise Exception.CreateFmt( '%s.%s: nielegalna operacja wewnątrz bloku GetFirst/GetNext', [ClassName, AMethod]);
   {$ENDIF}
end;

constructor TuQueue.Create;
begin
   inherited Create;
   FHead   := nil;
   FTail   := nil;
   FCursor := nil;
   {$IFOPT D+}
   FIterating := False;
   {$ENDIF}
end;

destructor TuQueue.Destroy;
begin
   Clear;
   inherited Destroy;
end;

procedure TuQueue.Clear;
  var p: PQueueItem;
begin
   DebugLegalCheck('Clear');
   while FHead <> nil do begin
      p := FHead;
      FHead := p^.Next;
      Dispose(p);
   end;
   FTail   := nil;
   FCursor := nil;
   {$IFOPT D+}
   FIterating := False;
   {$ENDIF}
end;

procedure TuQueue.Push(const AElem: _T);
   var p : PQueueItem;
begin
   DebugLegalCheck('Push');

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
   DebugLegalCheck('Pop');
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
   Result := (FHead = nil);
   {$IFOPT D+}
   if Result <> (FTail = nil) then
      raise Exception.CreateFmt('%s.IsEmpty: niespójny stan kolejki', [ClassName]);
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
   // Wywołanie GetFirst przy aktywnej iteracji oznacza porzucenie poprzedniej
   {$IFOPT D+}
   if FIterating then
      raise Exception.CreateFmt( '%s.GetFirst: poprzednia iteracja nie została zakończona', [ClassName]);
   FIterating := true;
   {$ENDIF}
   FCursor    := FHead;
   Result     := GetNext(AElem);
end;

function TuQueue.GetNext(out AElem: _T): boolean;
begin
   Result := (FCursor <> nil);
   if Result then begin
      AElem := FCursor^.Data;
      FCursor := FCursor^.Next;
   end else begin
      {$IFOPT D+}
      FIterating := False;  // iteracja zakończona poprawnie
      {$ENDIF}
   end;
end;

end.
