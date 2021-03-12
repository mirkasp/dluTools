unit dluGenStack;

{$I dluOptions.inc}

interface


type
  {$IFDEF FPC}generic{$ENDIF}
  TuStack<ITEM> = class
  strict private
     fStartCapacity : integer;
     fListLen       : integer;
     fNextElem      : integer;
     fList          : array of ITEM;
  public
     const DEFAULT_START_CAPACITY = 1024;
     constructor Create( const AStartCapacity: integer = DEFAULT_START_CAPACITY );
     //
     procedure Clear;  {$IFNDEF FPC}inline;{$ENDIF}
     procedure Push( const xElem: ITEM ); inline;
     function Pop: ITEM; inline;
     function IsEmpty: boolean; {$IFNDEF FPC}inline;{$ENDIF}
end;

implementation

uses SysUtils;

{ TuStack<ITEM> }

constructor TuStack{$IFNDEF FPC}<ITEM>{$ENDIF}.Create(const AStartCapacity: integer);
begin
   inherited Create;
   fStartCapacity := AStartCapacity;
   fListLen       := 0;
   Clear;
   SetLength( fList, fListLen );
end;

procedure TuStack{$IFNDEF FPC}<ITEM>{$ENDIF}.Clear;
begin
   fNextElem := 0;
end;

function TuStack{$IFNDEF FPC}<ITEM>{$ENDIF}.IsEmpty: boolean;
begin
   Result := (fNextElem = 0);
end;

function TuStack{$IFNDEF FPC}<ITEM>{$ENDIF}.Pop: ITEM;
begin
   if IsEmpty() then raise Exception.Create( 'Generic LIFO queue is empty' );
   Dec( fNextElem );
   Result := fList[ fNextElem ];
end;

procedure TuStack{$IFNDEF FPC}<ITEM>{$ENDIF}.Push(const xElem: ITEM);
begin
  if fNextElem = fListLen then begin
     Inc( fListlen, fStartCapacity );
     SetLength( fList, fListLen );
  end;
  fList[ fNextElem ] := xElem;
  Inc( fNextElem );
end;

end.
