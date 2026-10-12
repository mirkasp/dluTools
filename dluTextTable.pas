unit dluTextTable;

{$mode ObjFPC}{$H+}
{$modeswitch ADVANCEDRECORDS}

interface

uses Classes;

type
   TTableFrameMode = (tfNone, tfSingle, tfHalfDouble, tfDouble);

   { TTableFormatOptions }

   TTableFormatOptions = record
      SrcSep     : Char;
      UseMarkers : Boolean;
      Mode       : TTableFrameMode;

      // Defaults: SrcSep = '|', UseMarkers = True, Mode = tfDouble.
      class function Create: TTableFormatOptions; static;

      function WithSrcSep(const ASrcSep: Char): TTableFormatOptions;
      function WithUseMarkers(const AUseMarkers: Boolean): TTableFormatOptions;
      function WithMode(const AMode: TTableFrameMode): TTableFormatOptions;
   end;

   { TTextTableHelper }

   // Note: only one class helper for TStrings can be active in a given scope.
   // If another unit in the uses clause declares a helper for TStrings
   // (or a descendant), use FormatTable instead.
   TTextTableHelper = class helper for TStrings
      procedure FormatAsTable; overload;
      procedure FormatAsTable(const AOptions: TTableFormatOptions); overload;
   end;

// Formats delimited text rows as an aligned table, in place.
//
// Each row is split on AOptions.SrcSep, which must be a single-byte (ASCII)
// character. A trailing separator does not create an additional empty cell.
// Right-side whitespace is removed from each cell. Rows with fewer cells
// than the widest row are padded with empty cells, so that every row and
// the frame have the same width.
//
// When AOptions.UseMarkers is True, the first character of a cell may
// specify its alignment:
//   '<' - left-aligned (default)
//   '>' - right-aligned
//   '^' - centered
// The marker is removed from the resulting cell text. Disable UseMarkers
// if cells may legitimately start with one of these characters.
//
// AOptions.Mode selects the frame drawn around the table.
//
// Column widths are calculated using UTF-8 code point counts. Combining
// marks and double-width characters are not taken into account.
procedure FormatTable(AStrings: TStrings); overload;
procedure FormatTable(AStrings: TStrings; const AOptions: TTableFormatOptions); overload;

implementation

uses SysUtils;

type
   TCellAlign = (caLeft, caRight, caCenter);

   TCell = record
      Text  : string;
      Width : SizeInt;       // UTF-8 code point count
      Align : TCellAlign;
   end;

   TCellRow = array of TCell;

   TFrameModel = record
      TopLeftChar     : string;
      HorizontalChar  : string;
      TDownChar       : string;
      TopRightChar    : string;
      VerticalChar    : string;
      BottomLeftChar  : string;
      TUpChar         : string;
      BottomRightChar : string;
   end;

const
   // U+250C, U+2500, U+252C, U+2510, U+2502, U+2514, U+2534, U+2518
   SingleFrameModel: TFrameModel = (
      TopLeftChar    : '┌';
      HorizontalChar : '─';
      TDownChar      : '┬';
      TopRightChar   : '┐';
      VerticalChar   : '│';
      BottomLeftChar : '└';
      TUpChar        : '┴';
      BottomRightChar: '┘'
   );

   // U+2554, U+2550, U+2566, U+2557, U+2551, U+255A, U+2569, U+255D
   DoubleFrameModel: TFrameModel = (
      TopLeftChar    : '╔';
      HorizontalChar : '═';
      TDownChar      : '╦';
      TopRightChar   : '╗';
      VerticalChar   : '║';
      BottomLeftChar : '╚';
      TUpChar        : '╩';
      BottomRightChar: '╝'
   );

   // Double horizontal lines, single vertical lines.
   // U+2552, U+2550, U+2564, U+2555, U+2502, U+2558, U+2567, U+255B
   HalfFrameModel: TFrameModel = (
      TopLeftChar    : '╒';
      HorizontalChar : '═';
      TDownChar      : '╤';
      TopRightChar   : '╕';
      VerticalChar   : '│';
      BottomLeftChar : '╘';
      TUpChar        : '╧';
      BottomRightChar: '╛'
   );

   NoneFrameModel: TFrameModel = (
      TopLeftChar    : ' ';
      HorizontalChar : ' ';
      TDownChar      : ' ';
      TopRightChar   : ' ';
      VerticalChar   : ' ';
      BottomLeftChar : ' ';
      TUpChar        : ' ';
      BottomRightChar: ' '
   );

function UTF8CodePointCount(const AText: string): SizeInt;
   var ch: Char;
begin
   Result := 0;
   for ch in AText do
      if (Ord(ch) and $C0) <> $80 then Inc(Result);
end;

function Spaces(const N: SizeInt): string;
begin
   Result := StringOfChar(' ', N);
end;

function RepeatStr(const AText: string; const ACount: SizeInt): string;
   var I: SizeInt;
begin
   Result := '';
   for I := 1 to ACount do
      Result := Result + AText;
end;

// Extracts an optional alignment marker from the beginning of a cell.
//
// The marker is removed from AText after it has been recognized.
// If no marker is present, caLeft is used.
procedure ParseCellMarker(var AText: string; out AAlign: TCellAlign);
begin
   AAlign := caLeft;

   if AText = '' then
      Exit;

   case AText[1] of
      '<': AAlign := caLeft;
      '>': AAlign := caRight;
      '^': AAlign := caCenter;
   else
      Exit;
   end;

   Delete(AText, 1, 1);
end;

// Splits a source row into cells and determines their alignment.
//
// A trailing source separator does not create an additional empty cell.
// Right-side whitespace is removed from each cell before its length
// is calculated.
function ParseCellRow(const ASrc: string; const ASrcSep: Char;
                      const AUseMarkers: Boolean): TCellRow;
   var Parts : TStringArray;
       I     : SizeInt;
begin
   Parts := ASrc.Split([ASrcSep]);

   // A trailing separator is treated as a row terminator,
   // not as an additional empty cell.
   if (ASrc <> '') and (ASrc[Length(ASrc)] = ASrcSep) then
      SetLength(Parts, Length(Parts) - 1);

   SetLength(Result{%H-}, Length(Parts));

   for I := 0 to High(Parts) do begin
      Result[I].Text := Parts[I].TrimRight;

      if AUseMarkers
         then ParseCellMarker(Result[I].Text, Result[I].Align)
         else Result[I].Align := caLeft;

      Result[I].Width := UTF8CodePointCount(Result[I].Text);
   end;
end;

// Pads a cell with spaces to the specified column width.
//
// For centered text, an odd number of padding spaces is placed
// on the right side of the cell.
function PadCell(const ACell: TCell; const AWidth: SizeInt): string;
   var Pad     : SizeInt;
       LeftPad : SizeInt;
begin
   Pad := AWidth - ACell.Width;

   if Pad <= 0 then
      Exit(ACell.Text);

   case ACell.Align of
      caRight : Result := Spaces(Pad) + ACell.Text;
      caCenter: begin
                   LeftPad := Pad div 2;
                   Result  := Spaces(LeftPad) + ACell.Text + Spaces(Pad - LeftPad);
                end;
      else      Result  := ACell.Text + Spaces(Pad);
   end;
end;

// Builds a horizontal frame line (top or bottom) for the given column widths.
// Each column is surrounded by one padding character on both sides,
// which matches the layout of the data rows.
function BuildFrameLine(const AWidths: array of SizeInt;
                        const ALeft, AJunction, ARight, AHoriz: string): string;
   var I: SizeInt;
begin
   Result := ALeft + AHoriz;

   if Length(AWidths)>0 then begin
      Result += RepeatStr(AHoriz, AWidths[0]);
      for i:=1 to High(AWidths) do begin
          Result += AHoriz + AJunction + AHoriz + RepeatStr(AHoriz, AWidths[I]);
      end;
   end;
   Result += AHoriz + ARight;
end;

procedure FormatTable(AStrings: TStrings; const AOptions: TTableFormatOptions);
   var Rows       : array of TCellRow = nil;
       Widths     : array of SizeInt = nil;
       Padded     : TStringArray = nil;
       Joiner     : string;
       FrameModel : TFrameModel;
       I, C       : SizeInt;
begin
   if (AStrings = nil) or (AStrings.Count = 0) then
      Exit;

   case AOptions.Mode of
      tfSingle    : FrameModel := SingleFrameModel;
      tfDouble    : FrameModel := DoubleFrameModel;
      tfHalfDouble: FrameModel := HalfFrameModel;
      else          FrameModel := NoneFrameModel;
   end;

   Joiner := ' ' + FrameModel.VerticalChar + ' ';
   SetLength(Rows, AStrings.Count);

   // Parse all rows and determine the maximum width of each column.
   for I := 0 to AStrings.Count - 1 do begin
      Rows[I] := ParseCellRow(AStrings[I], AOptions.SrcSep, AOptions.UseMarkers);

      if Length(Rows[I]) > Length(Widths) then
         SetLength(Widths, Length(Rows[I]));

      for C := 0 to High(Rows[I]) do
         if Rows[I][C].Width > Widths[C] then
            Widths[C] := Rows[I][C].Width;
   end;

   SetLength(Padded, Length(Widths));

   // Rebuild the source rows using the calculated column widths.
   AStrings.BeginUpdate;
   try
      for I := 0 to AStrings.Count - 1 do begin
         for C := 0 to High(Widths) do
            if C < Length(Rows[I])
               then Padded[C] := PadCell(Rows[I][C], Widths[C])
               else Padded[C] := Spaces(Widths[C]);   // missing cell

         AStrings[I] := FrameModel.VerticalChar + ' ' + string.Join(Joiner, Padded) +
                        ' ' + FrameModel.VerticalChar;
      end;

      if AOptions.Mode <> tfNone then begin
         AStrings.Insert(0, BuildFrameLine(Widths, FrameModel.TopLeftChar,
                                           FrameModel.TDownChar, FrameModel.TopRightChar,
                                           FrameModel.HorizontalChar));

         AStrings.Add(BuildFrameLine(Widths, FrameModel.BottomLeftChar,
                                     FrameModel.TUpChar, FrameModel.BottomRightChar,
                                     FrameModel.HorizontalChar));
      end;
   finally
      AStrings.EndUpdate;
   end;
end;

procedure FormatTable(AStrings: TStrings);
begin
   FormatTable(AStrings, TTableFormatOptions.Create);
end;

{ TTableFormatOptions }

class function TTableFormatOptions.Create: TTableFormatOptions;
begin
   Result.SrcSep     := '|';
   Result.UseMarkers := True;
   Result.Mode       := tfDouble;
end;

function TTableFormatOptions.WithSrcSep(const ASrcSep: Char): TTableFormatOptions;
begin
   Result := Self;
   Result.SrcSep := ASrcSep;
end;

function TTableFormatOptions.WithUseMarkers(const AUseMarkers: Boolean): TTableFormatOptions;
begin
   Result := Self;
   Result.UseMarkers := AUseMarkers;
end;

function TTableFormatOptions.WithMode(const AMode: TTableFrameMode): TTableFormatOptions;
begin
   Result := Self;
   Result.Mode := AMode;
end;

{ TTextTableHelper }

procedure TTextTableHelper.FormatAsTable;
begin
   FormatTable(Self, TTableFormatOptions.Create);
end;

procedure TTextTableHelper.FormatAsTable(const AOptions: TTableFormatOptions);
begin
   FormatTable(Self, AOptions);
end;

end.
