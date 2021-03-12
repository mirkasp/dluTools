unit dluControls;

interface

uses Controls;

type TdluAlignMode = (amLeft, amCenter, amRight);

procedure AlignControl( const mode: TdluAlignMode; X, BaseControl : TControl );

// blokuje/odblokowuje wyœwietlanie zmian kontrolki
procedure LockControl(c: TWinControl; const bLock: Boolean);

implementation

uses Windows, Messages, SysUtils;

procedure AlignControl( const mode: TdluAlignMode; X, BaseControl : TControl );
begin
  case mode of
    amLeft   : X.Left := BaseControl.Left;
    amCenter : X.Left := BaseControl.Left + (BaseControl.Width - X.Width) div 2;
    amRight  : X.Left := BaseControl.Left + BaseControl.Width - X.Width;
  end;
end;

procedure LockControl(c: TWinControl; const bLock: Boolean);
begin
  if not Assigned( c ) then exit;
  if c.Handle = 0 then exit;

  if SendMessage(c.Handle, WM_SETREDRAW, Ord( not bLock ), 0) <> 0
  then raise Exception.Create( 'WM_SETREDRAW error!' );

  if not bLock then begin
      RedrawWindow( c.Handle, nil, 0,
                    RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;

end;
end.
