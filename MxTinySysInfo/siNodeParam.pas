unit siNodeParam;

{$IFDEF FPC}
   {$mode OBJFPC}{$H+}
   {$modeswitch UNICODESTRINGS+}
{$ELSE}
   {$MESSAGE HINT 'Tested only for LAZARUS!'}
{$ENDIF}

interface

uses Controls, siNodeIntf;

type TsiParamObj = class(TsiBaseObj)
   public
      constructor Create( const ACaption, AValue, AHint: string );
end;

type TsiSubParamObj = class(TsiBaseObj)
    public
       constructor Create( const ACaption, AValue, AHint: string );
end;

type TsiUrlObj = class(TsiBaseObj)
    strict private
      fUrl: string;
    public
      constructor Create( const ACaption, AHyperLink, AValue: string );
      function GetExtraParam( const n: integer ): string; override;
end;


implementation

uses Graphics;

const  aHintCursor: array[ boolean ] of TCursor = ( crDefault, crHelp );

(******************************************************************************)
{ TsiParamObj }
constructor TsiParamObj.Create( const ACaption, AValue, AHint: string );
begin
   inherited Create;
   fNodeType := sinParam;
   with fNodeParams[0] do begin
      NormalText    := TryAddColon( ACaption );
      FontStyles    := [];
   end;
   with fNodeParams[1] do begin
      NormalText    := AValue;
      FontStyles    := [fsBold];
      Hint          := AHint;
      Cursor        := aHintCursor[ Hint <> '' ];
   end;
end;


(******************************************************************************)
{ TsiSubParamObj }
constructor TsiSubParamObj.Create(const ACaption, AValue, AHint: string);
begin
   inherited Create;
   fNodeType := sinSubParam;
   with fNodeParams[0] do begin
      NormalText    := TryAddColon( ACaption );
      FontStyles    := [fsItalic];
   end;
   with fNodeParams[1] do begin
      NormalText    := AValue;
      FontStyles    := [fsItalic];
      Hint          := AHint;
      Cursor        := aHintCursor[ Hint <> '' ];
   end;
end;

(******************************************************************************)
{ TvtHyperlinkObject }
constructor TsiUrlObj.Create( const ACaption, AHyperLink, AValue: string) ;
begin
   inherited Create;
   fNodeType      := sinUrl;
   with fNodeParams[0] do begin
      NormalText    := TryAddColon( ACaption );
      FontStyles    := [];
   end;
   with fNodeParams[1] do begin
      NormalText    := AValue;
      FontColor     := clBlue;
      FontStyles    := [fsUnderline];
      Cursor        := crHandPoint;
   end;
   fUrl := AHyperLink;
end;

function TsiUrlObj.GetExtraParam( const n: integer) : string;
begin
   case n of
      NODE_URL: Result := fUrl;
      else      Result := inherited GetExtraParam( n ) ;
   end;
end;



end.

