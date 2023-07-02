unit dluGenQueue;

{$mode Delphi}{$H+}

interface

type

{ TuQueue }

 TuQueue<_T> = class
   strict private
     type PQueueItem = ^TQueueItem;
          TQueueItem = record
              Data : _T;
              Next : PQueueItem;
          end;
     var  fHead : PQueueItem;
          fTail : PQueueItem;
          fTemp : PQUeueItem;
   public
     constructor Create;
     destructor Destroy; override;
     procedure Clear;  //inline;
     procedure Push( const AElem: _T ); inline;
     function Pop: _T; //inline;
     function IsEmpty: boolean; //inline;
     function TryPop( var AElem: _T ): boolean;
     function TryTest( var AElem: _T ): boolean;
     //
     function GetFirst( var AElem: _T ): boolean;
     function GetNext( var AElem: _T ): boolean;
     //
end;

implementation

uses SysUtils;

{ TuQueue }

constructor TuQueue<_T>.Create;
begin
   inherited Create;
   fHead := nil;
   fTail := nil;
end;

destructor TuQueue<_T>.Destroy;
begin
   self.Clear;
   inherited Destroy;
end;

procedure TuQueue<_T>.Clear;
begin
   while not IsEmpty() do begin
      Pop();
   end;
   fHead := nil;
end;

procedure TuQueue<_T>.Push(const AElem: _T);
  var p : PQueueItem;
begin
   if self.IsEmpty() then begin
      New( fHead );
      fHead^.Data := AElem;
      fHead^.Next := nil;
      fTail       := fHead;
   end else begin
      New( p );
      p^.Data     := AElem;
      fTail^.Next := p;
      p^.Next     := nil;
      fTail       := p;
   end;
end;

function TuQueue<_T>.Pop: _T;
  var p : PQueueItem;
begin
   if not self.IsEmpty() then begin
      p      := fHead;
      fHead  := fHead^.Next;
      if fHead = nil then fTail := nil;
      Result := p^.Data;
      Dispose( p );
   end else
      raise Exception.CreateFmt( 'Error in "%s" class procedure "Pop"', [ self.ClassName ] );
end;

function TuQueue<_T>.IsEmpty: boolean;
begin
   Result := fTail = nil;
   if Result and (fHead <> nil) then
      raise Exception.CreateFmt( 'Error in "%s" class procedure "IsEmpty"', [ self.ClassName ] );
end;

function TuQueue<_T>.TryPop(var AElem: _T): boolean;
begin
   Result := not self.IsEmpty;
   if Result then AElem := self.Pop();
end;

function TuQueue<_T>.TryTest(var AElem: _T): boolean;
begin
   Result := not self.IsEmpty;
   if Result then AElem := fHead^.Data;
end;

function TuQueue<_T>.GetFirst(var AElem: _T): boolean;
begin
   fTemp := fHead;
   Result := GetNext( AElem );
end;

function TuQueue<_T>.GetNext(var AElem: _T): boolean;
begin
   Result := (fTemp <> nil);
   if Result then begin
      AElem := fTemp^.Data;
      fTemp := fTemp^.Next;
   end;
end;

end.

