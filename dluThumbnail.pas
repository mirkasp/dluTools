unit dluThumbnail;

{$mode ObjFPC}{$H+}
{$modeswitch ADVANCEDRECORDS}

interface

uses Classes
   , Graphics
   , fpeGlobal
   , BGRABitmap
   , DB
   , PdfiumCore
   ;

type TThumbMode = ( tm_none, tm_int, tm_gen );

type TFrameMode = ( fm_none, fm_auto, fm_custom );

{ TuThumbnail }
type TuThumbnail = class
    strict private
      function GetPngHeight: integer;
      function GetPngWidth: integer;
    protected
      type TImgSize = record
             Width  : integer;
             Height : integer;
             constructor Create( AWidth, AHeight: integer );
           end;
      var
       fThumbMode       : TThumbMode;
       fFrameMode       : TFrameMode;
       fFrameColorInt   : TColor;                       // frame color for internal thumbnail
       fFrameColorGen   : TColor;                       // frame color for generated thumbnail
       fMaxWidth        : integer;
       fMaxHeight       : integer;
       fPng             : TPortableNetworkGraphic;
       fStatusStr       : string;
       //
       fOnLog           : TGetStrProc;
       //fFramed          : boolean;
       //fFrameColor      : TColor;
       procedure DoOnLog( const AText: string ); dynamic; overload;
       procedure DoOnLog( const Fmt: string; const AValues: array of const ); dynamic; overload;
       procedure Initialize(); virtual;
       function SizeNormalization( const SrcWidth, SrcHeight: integer ): TImgSize; overload;
       function SizeNormalization( const SrcSize : TImgSize ): TImgSize; overload;
       function InternalLoadFromStream( AStream: TStream; const AOrientation: TExifOrientation; const AThumbMode: TThumbMode ): boolean;
       procedure ConvertToPng( ABitmap: TBGRABitmap );
    public
       class function IsPfdExtension( const xFileName: AnsiString ): boolean; overload;
       class function IsPfdExtension( const xFileName: UnicodeString ): boolean; overload;
       //
       constructor Create(); overload;
       constructor Create( const AWidth, AHeight: integer ); overload;
       //
       destructor Destroy; override;
       //
       function LoadFromClipboard(): boolean;
       function LoadFromBitmap( ABitmap: TBitmap ): boolean;
       function LoadFromStream( AStream: TStream; const AAutoRotate: boolean = true ): boolean;
       function LoadFromFile( const AFileName: UnicodeString; const AAutoRotate: boolean = true ): boolean;
       function LoadFromBlob( AField: TBlobField ): boolean;
       //
       function SaveToFile( const AFileName: UnicodeString ): boolean;
       function SaveToBlob( AField: TBlobField ): boolean;
       //
       property PNG       : TPortableNetworkGraphic read fPng;
       property Status    : string                  read fStatusStr;
       property Width     : integer                 read GetPngWidth;
       property Height    : integer                 read GetPngHeight;
       //
       property OnLog         : TGetStrProc         read fOnLog         write fOnLog;
       //property Framed    : boolean                 read fFramed        write fFramed;
       property FrameMode     : TFrameMode          read fFrameMode     write fFrameMode;
       property FrameColorInt : TColor              read fFrameColorInt write fFrameColorInt;
       property FrameColorGen : TColor              read fFrameColorGen write fFrameColorGen;
       //property FrameColor    : TColor              read fFrameColor    write fFrameColor;
       property ThumbMode     : TThumbMode          read fThumbMode;
 end;


type TOpenPdfProc = procedure(const S: string; const APages: integer) of object;

{ TuPdfThumbnail }
type TuPdfThumbnail = class( TuThumbnail )
   strict private
      fPDFiumDllFile : UnicodeString;
      fDocFileName   : UnicodeString;
      fDocument      : TPdfDocument;
      fPageCount     : integer;
      fOnPdfOpen     : TOpenPdfProc;
      procedure DoOnPdfOpen( const AFileName: string; const APages: integer );
   protected
      procedure Initialize(); override;
   public
      destructor Destroy; override;
      //
      function OpenPdf( const AFileName: UnicodeString ): boolean;
      procedure ThumbnailForPage( const APageNr: integer = 1 );
      //
      property PDFiumDllFile : UnicodeString  read fPDFiumDLLFile  write fPDFiumDllFile;
      property PageCount     : integer        read fPageCount;
      property OnPdfOpen     : TOpenPdfProc   read fOnPdfOpen      write fOnPdfOpen;
end;


implementation

uses SysUtils
   , LCLIntf, LCLType
   , Clipbrd
   , BGRABitmapTypes
   , fpeMetadata
   , dluFileInfo
   ;

const DEFAULT_MAX_WIDTH  = 200;
const DEFAULT_MAX_HEIGHT = 200;

const FRAME_AUTO_COLORS : array[ TThumbMode ] of TColor = ( clNone, clBlue, clRed );

{ TuThumbnail }

constructor TuThumbnail.Create();
begin
   inherited Create;
   fMaxWidth  := DEFAULT_MAX_WIDTH;
   fMaxHeight := DEFAULT_MAX_HEIGHT;
   Initialize();
end;

constructor TuThumbnail.Create(const AWidth, AHeight: integer);
begin
   inherited Create;
   fMaxWidth  := AWidth;
   fMaxHeight := AHeight;
   Initialize();
end;

destructor TuThumbnail.Destroy;
begin
   fPng.Free;
   inherited Destroy;
end;

procedure TuThumbnail.Initialize();
begin
   fThumbMode := tm_none;
   fFrameMode := fm_auto;

   fFrameColorInt := FRAME_AUTO_COLORS[ tm_int ];
   fFrameColorGen := FRAME_AUTO_COLORS[ tm_gen ];

   fPng       := TPortableNetworkGraphic.Create;
   fStatusStr := '';
end;

function TuThumbnail.GetPngHeight: integer;
begin
  if Assigned( fPng )
     then Result := fPng.Height
     else Result := -1;
end;

function TuThumbnail.GetPngWidth: integer;
begin
  if Assigned( fPng )
     then Result := fPng.Width
     else Result := -1;
end;

procedure TuThumbnail.DoOnLog(const AText: string);
begin
   if Assigned( fOnLog ) then fOnLog( AText );
end;

procedure TuThumbnail.DoOnLog(const Fmt: string; const AValues: array of const);
begin
   DoOnLog( Format( Fmt, AValues ) );
end;

function TuThumbnail.SizeNormalization(const SrcWidth, SrcHeight: integer): TImgSize;
 const cPow = 1000;
   var alfa, beta: Double;
begin
   Assert( SrcWidth > 0 );
   Assert( SrcHeight > 0 );
   alfa := fMaxHeight / SrcHeight;
   beta := fMaxWidth / SrcWidth;

   if (alfa >= 1.0) and (beta >= 1.0) then begin
      Result.Height := SrcHeight;
      Result.Width  := SrcWidth;
   end else
      if alfa <= beta then begin
         Result.Height := fMaxHeight;
         Result.Width  := Round( cPow * alfa * SrcWidth ) div cPow;
      end else begin
         Result.Height := Round( cPow * beta * SrcHeight ) div cPow;
         Result.Width  := fMaxWidth;
      end;
end;

function TuThumbnail.SizeNormalization(const SrcSize: TImgSize): TImgSize;
begin
  Result := SizeNormalization( SrcSize.Width, SrcSize.Height );
end;

function TuThumbnail.InternalLoadFromStream( AStream: TStream; const AOrientation: TExifOrientation; const AThumbMode: TThumbMode ): boolean;
  var DstSize : TImgSize;
      BmpX    : TBGRABitmap;
      tmp     : TBGRABitmap;
      tmp1    : TBGRABitmap;
begin
   AStream.Position := 0;

   Result  := false;
   BmpX    := TBGRABitmap.Create( AStream );
   DstSize := SizeNormalization( TImgSize.Create( BmpX.Width, BmpX.Height ) );
   try

      tmp := BmpX.Resample( DstSize.Width, DstSize.Height, rmFineResample ) as TBGRABitmap;

      DoOnLog( 'Image orientation: %d', [ Ord( AOrientation ) ] );
      if AOrientation in [ eoRotate90, eoRotate180, eoRotate270 ] then begin
         // de-rotation
         case AOrientation of
            eoRotate90  : tmp1 := tmp.RotateCW;
            eoRotate180 : tmp1 := tmp.RotateCCW;
            eoRotate270 : tmp1 := tmp.RotateUD;
            else          tmp1 := tmp.Duplicate();
         end;
         tmp.Assign( tmp1.Bitmap );
         tmp1.Free;
      end;

      fThumbMode := AThumbMode;

      ConvertToPng( tmp );

      tmp.Free;
      Result := true;
   except
     on E: Exception do DoOnLog( 'Error: %s', [ E.Message ] );
   end;
   BmpX.Free;
end;

procedure TuThumbnail.ConvertToPng( ABitmap: TBGRABitmap );

   function ColorForCustomFrameMode( const AThumbMode: TThumbMode ): TColor;
   begin
      case AThumbMode of
         tm_int : Result := fFrameColorInt;
         tm_gen : Result := fFrameColorGen;
         else     Result := clNone;
      end;
   end;

  var lColor : TColor;
begin
   case fFrameMode of
      fm_auto    : lColor := FRAME_AUTO_COLORS[ fThumbMode ];
      fm_custom  : lColor := ColorForCustomFrameMode( fThumbMode );
      else         lColor := clNone;
   end;

   if lColor <> clNone
      then ABitmap.Rectangle( ABitmap.ClipRect, ColorToBGRA( lColor, $FF ) );

   fPng.Assign( ABitmap.Bitmap );
   fStatusStr := Format( '%dx%d', [ fPng.Width, fPng.Height ] );
end;

function TuThumbnail.SaveToFile(const AFileName: UnicodeString): boolean;
begin
   Result := false;
   if not Assigned( fPng ) then begin
      DoOnLog( 'Thumbnail not assigned' );
      exit;
   end;
   try
      fPng.SaveToFile( UTF8Encode( AFileName ) );
      Result := true;
   except
      DoOnLog( 'Thumbnail saving error' );
   end;
end;

function TuThumbnail.SaveToBlob(AField: TBlobField): boolean;
  var tmp : TMemoryStream;
begin
  Result := Assigned( fPng );
  if Result then begin
     tmp := TMemoryStream.Create;
     fPng.SaveToStream( tmp );
     AField.LoadFromStream( tmp );
     tmp.Free;
  end;
end;

function TuThumbnail.LoadFromClipboard(): boolean;
   var pdfc : TPredefinedClipboardFormat;
       xc   : TClipboardFormat;
       ms   : TMemoryStream;
begin
   Result := false;
   for pdfc in TPredefinedClipboardFormat do begin
      xc := PredefinedClipboardFormat( pdfc );
      if Clipboard.HasFormat( xc ) then begin

         ms := TMemoryStream.Create;
         if Clipboard.GetFormat( xc, ms ) then begin
            InternalLoadFromStream( ms, eoUnknown, tm_gen );
            Result := true;
         end;
         ms.Free;

         break;
      end;
   end;
   if Result
      then fStatusStr := 'Ok, ' + PredefinedClipboardMimeTypes[ pdfc ]
      else fStatusStr := 'Unknown data in clipboard';

end;

function TuThumbnail.LoadFromBitmap(ABitmap: TBitmap): boolean;
  var ms : TMemoryStream;
begin
   ms := TMemoryStream.Create;
   ABitmap.SaveToStream( ms );
   Result := InternalLoadFromStream( ms, eoUnknown, tm_gen );
   ms.Free;
end;


function TuThumbnail.LoadFromStream(AStream: TStream; const AAutoRotate: boolean): boolean;
 //const acThumbMode: array[ boolean ] of TThumbMode = ( tm_gen, tm_int );
  var mini   : TMemoryStream;
      fpe_img: fpeMetadata.TImgInfo;
      imgRot : TExifOrientation;
      imgFmt : boolean;
begin
   Result  := false;
   AStream.Position := 0;
   imgRot  := eoUnknown;
   imgFmt  := true;
   fpe_img := TImgInfo.Create();

   try
      fpe_img.LoadFromStream( AStream );
      if fpe_img.HasExif and AAutoRotate then imgRot := fpe_img.ExifData.ImgOrientation;
      imgFmt := fpe_img.HasThumbnail;
      if imgFmt then begin
         mini := TMemoryStream.Create;
         try
            fpe_img.SaveThumbnailToStream( mini );
            Result := InternalLoadFromStream( mini, imgRot, tm_int );
         except
           imgFmt := false;
         end;
         mini.Free;
      end;
   except
     imgFmt := false;
   end;

   fpe_img.Free;

   if not imgFmt then begin
      InternalLoadFromStream( AStream, imgRot, tm_gen );
      Result := true;
   end;

end;

function TuThumbnail.LoadFromFile(const AFileName: UnicodeString; const AAutoRotate: boolean): boolean;
  var stream : TFileStream;
begin
   stream := TFileStream.Create( UTF8Encode( AFileName ), fmOpenRead or fmShareDenyNone );
   Result := LoadFromStream( stream, AAutoRotate );
   stream.Free;
end;


function TuThumbnail.LoadFromBlob(AField: TBlobField): boolean;
  var tmp : TMemoryStream;
begin
   tmp := TMemoryStream.Create;
   AField.SaveToStream( tmp );
   Result := InternalLoadFromStream( tmp, eoUnknown, tm_gen );
   tmp.Free;
end;

{ TuThumbnail.TImgSize }

constructor TuThumbnail.TImgSize.Create(AWidth, AHeight: integer);
begin
   self.Width  := AWidth;
   self.Height := AHeight;
end;


class function TuThumbnail.IsPfdExtension(const xFileName: AnsiString ): boolean;
begin
   Result := SameText( ExtractFileExt( xFileName ), '.PDF' );
end;

class function TuThumbnail.IsPfdExtension(const xFileName: UnicodeString ): boolean;
begin
   Result := IsPfdExtension( UTF8Encode( xFileName ) );
end;

{ TuPdfThumbnail }

procedure TuPdfThumbnail.DoOnPdfOpen(const AFileName: string; const APages: integer);
begin
   if Assigned( fOnPdfOpen ) then fOnPdfOpen( AFileName, APages );
end;

procedure TuPdfThumbnail.Initialize;
begin
   inherited Initialize;
   fPDFiumDllFile := '';
   fDocFileName   := '';
   fDocument      := nil;
   fPageCount     := -1;
end;

destructor TuPdfThumbnail.Destroy;
begin
   fDocument.Free;
   inherited Destroy;
end;

function TuPdfThumbnail.OpenPdf(const AFileName: UnicodeString): boolean;
  const PDFiumDLL = 'pdfium.dll';
  const ErrorStr  = '"'+PDFiumDLL+'" not found';
begin
   Result := false;
   if fPDFiumDllFile = '' then begin
      fPDFiumDllFile := dluFileInfo.LookForFile( PDFiumDLL, true, [ '.' ] ) ;
      if fPDFiumDllFile = '' then begin
         DoOnLog( ErrorStr );
         exit;
      end;
      PdfiumCore.PDFiumDllFileName := fPDFiumDllFile;
   end;

   if Assigned( fDocument ) then begin
      fDocument.Free;
      fDocFileName := '';
   end;

   fDocument     := TPdfDocument.Create();
   fDocument.LoadFromFile( AFileName );

   fPageCount   := fDocument.PageCount;
   fDocFileName := AFileName;

   DoOnPdfOpen( UTF8Encode( AFileName ), fPageCount );

   Result       := true;
end;

procedure TuPdfThumbnail.ThumbnailForPage(const APageNr: integer);
  var pdfPage  : TPdfPage;
      pageSize : TImgSize;
      DstSize  : TImgSize;
      tmp      : TBGRABitmap;
begin
   if (APageNr < 1) or (APageNr > fPageCount ) then begin
      DoOnLog( 'Page number (%d) out of range', [ APageNr ] );
      exit;
   end;

   pdfPage  := fDocument.Pages[ APageNr-1 ];
   pageSize := TImgSize.Create( Round( pdfPage.Width ), Round( pdfPage.Height ) );

   with TBGRABitmap.Create( pageSize.Width, pageSize.Height, ColorToBGRA( clBlue, $FF ) ) do begin

      pdfPage.Draw( Canvas.Handle, 0, 0, pageSize.Width, pageSize.Height, prNormal, [proAnnotations]);

      DstSize := SizeNormalization( pageSize );
      tmp     := Resample( DstSize.Width, DstSize.Height, rmFineResample ) as TBGRABitmap;
      fThumbMode := tm_gen;
      ConvertToPng( tmp );
      tmp.Free;

      Free;
   end;

end;

end.

