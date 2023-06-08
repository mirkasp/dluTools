unit dluGenStack;

//{$I dluOptions.inc}
{$mode Delphi}{$H+}

interface

uses Classes
   ;

type TuStack<_T> = class
  strict private
     fStartCapacity  : integer;
     fListLen        : integer;
     fNextElem       : integer;
     fList           : array of _T;
  public
     const DEFAULT_START_CAPACITY = 1024;
     constructor Create( const AStartCapacity: integer = DEFAULT_START_CAPACITY );
     //
     procedure Clear;  inline;
     procedure Push( const xElem: _T ); inline;
     function Pop: _T; inline;
     function IsEmpty: boolean; inline;
     function TryPop( var AElem: _T ): boolean;
end;

implementation

uses SysUtils;

{ TuStack<ITEM> }

constructor TuStack<_T>.Create(const AStartCapacity: integer);
begin
   inherited Create;
   fStartCapacity := AStartCapacity;
   fListLen       := 0;
   {%H-}Clear;
   SetLength( fList, fListLen );
end;

procedure TuStack<_T>.Clear;
begin
   fNextElem := 0;
end;

function TuStack<_T>.IsEmpty: boolean;
begin
   Result := (fNextElem = 0);
end;

function TuStack<_T>.TryPop(var AElem: _T): boolean;
begin
   try
      Result := not IsEmpty;
      if Result then AElem := Pop;
    finally
    end;
end;

function TuStack<_T>.Pop: _T;
begin
   if {%H-}IsEmpty() then raise Exception.Create( 'Generic LIFO queue is empty' );
   Dec( fNextElem );
   Result := fList[ fNextElem ];
end;

procedure TuStack<_T>.Push(const xElem: _T);
begin
  if fNextElem = fListLen then begin
     Inc( fListlen, fStartCapacity );
     SetLength( fList, fListLen );
  end;
  fList[ fNextElem ] := xElem;
  Inc( fNextElem );
end;

end.
