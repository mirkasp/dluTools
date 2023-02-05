unit dluExif.helper;
(*******************************************************************************

  2022.08.16: "late creating" ExifTagDefsEx object
  2022.07.30: initial version

*******************************************************************************)

{$mode objfpc}{$H+}

interface

uses fpeMetaData, fpeGlobal
   , fpeTags
   , fpeExifData
   ;

type
 TuDimensionMethod = ( tdmUnknown, tdmStandalone, tdmPrimaryTag, tdmExifTag, tdmExtra );

{ TuImgInfo }

 TuImgInfo = class helper for TImgInfo
    strict private
      function ImgDimension( var XHeight, XWidth: Word ): boolean;
    public
      function GetImageDimensions( out AWidth, AHeight: Word ): TuDimensionMethod; overload;
      function GetImageDimensions( out AWidth, AHeight: Word; out AMethod: TuDimensionMethod ): boolean;
      //
      function GetCameraModel(): string;
end;

{ TuExifDataEx }

TuExifDataEx = class helper for TExifData
   strict private
   public
      //
      function GetTagExValueAsInt( const AName: string; const ADefault: integer = 0 ): integer; overload;
      function GetTagExValueAsInt( const AId  : TTagId; const ADefault: integer = 0 ): integer; overload;
      //
      function GetTagExValueAsString( const AName: string; const ADefault: string = '' ): string; overload;
      function GetTagExValueAsString( const AId  : TTagId; const ADefault: string = '' ): string; overload;
      //
end;


function GetExTagName( const AId  : TTagId; const ADefault: string = '' ): string;
function GetExTags(): TTagDefList;

function GetExTagInfo( const ATag: TTag ): string;
function ParentAsString( const APrim: Word ): string;

implementation

uses Classes, SysUtils
   , Dialogs
   , fpeStrConsts
   ;

var ExifTagDefsEx: TTagDefList = nil;


{ TuImgInfo }

// https://www.disktuna.com/list-of-jpeg-markers/
// http://svn.code.sf.net/p/flyingsheep/code/trunk/MijnLib/picslib.pp

function TuImgInfo.ImgDimension( var XHeight, XWidth: Word): boolean;
 type TSOF0 = packed record
         prec   : Byte;
         Height : Word;
         Width  : Word;
      end;

  const MARKER = $FF;
        M_SOI  = $D8;
        M_EOI  = $D9;
  const Parameterless = [$01, $D0, $D1, $D2, $D3, $D4, $D5, $D6, $D7];

  var fs      : TFileStream;
      pBuf    : ^TSOF0;
      SgmMark : byte;
      SgmSize : Word;
begin
   fs := TFileStream.Create( self.FileName, fmOpenRead or fmShareDenyNone );

   Result := (fs.ReadByte = MARKER) and (fs.ReadByte = M_SOI);
   if Result then begin
      Result := false;
      while fs.Position < (fs.Size-3) do begin
         repeat SgmMark := fs.ReadByte; until SgmMark = $FF;
         repeat SgmMark := fs.ReadByte; until SgmMark <> $FF;
         if SgmMark = M_EOI then
            break;

         if SgmMark in Parameterless then
            continue;

         SgmSize := BEtoN( fs.ReadWord ) - 2;
         if SgmMark in [ $C0, $C1, $C2, $C3 ] then begin
            New( pBuf );
            fs.Read( pBuf^, SizeOf( TSOF0 ) );
            XHeight := BEtoN( pBuf^.Height );
            XWidth  := BEtoN( pBuf^.Width );
            Dispose( pBuf );
            Result := true;
            break;
         end;
         fs.Seek( SgmSize, soFromCurrent );
      end;

   end;
   fs.Free;
end;

function TuImgInfo.GetImageDimensions(out AWidth, AHeight: Word ): TuDimensionMethod;
begin
   Result  := tdmStandalone;
   AWidth  := self.ImgWidth;
   AHeight := self.ImgHeight;
   if (AWidth <> 0) and (AHeight <> 0) then exit;

   if self.HasExif then begin;
      Result  := tdmPrimaryTag;
      AWidth  := self.ExifData.GetTagExValueAsInt( 'ImageWidth' );
      AHeight := self.ExifData.GetTagExValueAsInt( 'ImageHeight' );
      if (AWidth <> 0) and (AHeight <> 0) then exit;

      Result  := tdmExifTag;
      AWidth  := self.ExifData.GetTagExValueAsInt( 'ExifImageWidth' );
      AHeight := self.ExifData.GetTagExValueAsInt( 'ExifImageHeight' );
      if (AWidth <> 0) and (AHeight <> 0) then exit;
   end;

   if self.ImgDimension( AHeight, AWidth )
      then Result := tdmExtra
      else Result := tdmUnknown;

end;

function TuImgInfo.GetImageDimensions(out AWidth, AHeight: Word; out AMethod: TuDimensionMethod ): boolean;
begin
   AMethod := GetImageDimensions( AWidth, AHeight );
   Result := AMethod > Low( TuDimensionMethod );
end;

function TuImgInfo.GetCameraModel(): string;
  var s  : string;
      i  : integer;
begin
   if HasExif then begin
      s := self.ExifData.GetTagExValueAsString( 'Make' );
      Result := self.ExifData.GetTagExValueAsString( 'Model' );
      if Pos( s, Result ) <> 1 then Result := s + ' ' + Result;
      for i:=1 to Length(Result) do if Result[i] in [#0..#31] then Result[i] := '_';
      Result := Trim( Result );
   end else
      Result := '';
end;


{ TuExifDataEx }

function TuExifDataEx.GetTagExValueAsInt(const AName: string; const ADefault: integer): integer;
  var tx : TTag;
begin
   tx := self.TagByName[ AName ];
   if tx = nil
      then Result := ADefault
      else Result := tx.AsInteger;
end;

function TuExifDataEx.GetTagExValueAsInt(const AId: TTagId; const ADefault: integer): integer;
  var tx : TTag;
begin
   tx := self.TagByID[ AId ];
   if tx = nil
      then Result := ADefault
      else Result := tx.AsInteger;
end;

function TuExifDataEx.GetTagExValueAsString(const AName: string; const ADefault: string): string;
  var tx : TTag;
begin
   tx := self.TagByName[ AName ];
   if tx = nil
      then Result := ADefault
      else Result := tx.AsString;
end;

function TuExifDataEx.GetTagExValueAsString(const AId: TTagId; const ADefault: string): string;
  var tx : TTag;
begin
   tx := self.TagByID[ AId ];
   if tx = nil
      then Result := ADefault
      else Result := tx.AsString;
end;

function GetExTagInfo( const ATag: TTag) : string;
  var _tagType : string;
      _tagGroup : string;

begin
   WriteStr( _tagType,  ATag.TagType );
   WriteStr( _tagGroup, ATag.Group   );
   Result := Format( '[$%s-$%s, %d] {%s} (%s) %s, %s, %s, %s, %s',
                     [ IntToHex( ATag.TagIDRec.Parent, 4 ), IntToHex( ATag.TagIDRec.Tag, 4 ),
                       ATag.TagIDRec.Tag,
                       _tagType,
                       _tagGroup,
                       ATag.Name,
                       ATag.Description,
                       QuotedStr( BoolToStr( ATag.ReadOnly, 'RO', 'rw' ) ),
                       ATag.AsString,
                       BoolToStr( ATag.HasData, 'true', 'false' )
                     ] );

end;

function ParentAsString( const APrim: Word ): string;
begin
   case APrim of
      TAG_PRIMARY         : Result := 'TAG_PRIMARY';         // = $0001;
      TAG_THUMBNAIL       : Result := 'TAG_THUMBNAIL';       // = $0002;
      TAG_EXIF_OFFSET     : Result := 'TAG_EXIF_OFFSET';     // = $8769;
      TAG_GPS_OFFSET      : Result := 'TAG_GPS_OFFSET';      // = $8825;
      TAG_INTEROP_OFFSET  : Result := 'TAG_INTEROP_OFFSET';  // = $A005;
      TAG_SUBIFD_OFFSET   : Result := 'TAG_SUBIFD_OFFSET';   // = $014A;
      TAG_IPTC            : Result := 'TAG_IPTC';            // = $83BB;
      TAG_MAKERNOTE       : Result := 'TAG_MAKERNOTE';       // = $927C;
      else                  Result := 'UNKNOWN PARENT ID';
   end;
end;

procedure BuildExifTagDefsEx;
  const P = TAGPARENT_PRIMARY;         // $00010000;
        T = TAGPARENT_THUMBNAIL;       // $00020000;
        E = TAGPARENT_EXIF;            // $87690000;
        //G = TAGPARENT_GPS;             // $88250000;
        I = TAGPARENT_INTEROP;         // $A0050000;
begin
   with ExifTagDefsEx do begin
     AddStringTag   (P+$000B, 'ProcessingSoftware',        1, 'Processing Software' );             // MK 2022.01.12
     AddUShortTag   (T+$0102, 'BitsPerSample',             1, rsBitsPerSample);    // MK 2021.0925
     AddUShortTag   (T+$0106, 'PhotometricInterpretation', 1, rsPhotometricInt, rsPhotometricIntLkup); // MK 2021.09.25
     AddStringTag   (T+$010F, 'Make',                      1, rsMake);             // MK 2021.09.24
     AddStringTag   (T+$0110, 'Model',                     1, rsModel);            // MK 2021.09.24
     AddUShortTag   (T+$0115, 'SamplesPerPixel',           1, rsSamplesPerPixel);  // MK 2021.09.25
     AddUShortTag   (P+$0140, 'ColorMap',                  2, '' );                // MK 2021.09.24
     AddUShortTag   (P+$0152, 'ExtraSamples',              1, '' );                // MK 2021.09.24
     AddULongTag    (E+$0201, 'ThumbnailOffset',           1, rsThumbnailOffset, '', '', TOffsetTag);
     AddULongTag    (E+$0202, 'ThumbnailSize',             1, rsThumbnailSize);
     AddStringTag   (P+$02BC, 'ExtensibleMetadataPlatform',1, rsExtensibleMetadataPlatform);
     AddURationalTag(P+$0301, 'Gamma',                     1, '' );   // MK 2021.09.25
     AddUShortTag   (P+$0303, 'SRGBRenderingIntent',       1, '' );  // MK 2021.09.25
     AddUShortTag   (P+$1001, 'RelatedImageWidth',         1, rsRelatedImageWidth  );   // MK 2021.09.25
     AddUShortTag   (P+$1002, 'RelatedImageHeight',        1, rsRelatedImageHeight );   // MK 2021.09.25
//.     AddUShortTag   (P+$4746, 'Rating',                    1, 'Rating'             );   // MK 2021.09.24, deleted 2023.01.13
//.     AddUShortTag   (P+$4749, 'RatingPercent',             1, 'Rating percent'     );   // MK 2021.09.24, deleted 2023.01.13
     AddUShortTag   (P+$5100, 'FrameDelay',                1, 'Frame delay'        );   // MK 2021.09.26
     AddUShortTag   (P+$5110, 'PixelUnit'     );   // MK 2021.09.24
     AddULongTag    (P+$5111, 'PixelPerUnitX' );   // MK 2021.09.24
     AddULongTag    (P+$5112, 'PixelPerUnitY' );   // MK 2021.09.24
     AddURationalTag(P+$829A, 'ExposureTime',              1, rsExposureTime, '', '', TExposureTimeTag); //, nil, '%0:.0f/%1:.0f s');
     AddULongTag(    P+$84E0, 'Site'                     );
     AddULongTag(    P+$84E1, 'ColorSequence'            );
     AddULongTag(    P+$84E2, 'IT8Header'                );
     AddULongTag(    P+$84E3, 'RasterPadding', 1, 'Raster Padding',
                              '0=Byte;1=Word;2=Long Word;9=Sector;10=Long Sector' );
     AddULongTag(    P+$84E4, 'BitsPerRunLength'         );
     AddULongTag(    P+$84E5, 'BitsPerExtendedRunLength' );
     AddUShortTag(   P+$84E6, 'ColorTable'               );
     AddUShortTag(   P+$84E7, 'ImageColorIndicator', 1, 'Image Color Indicator',
                              '0=Unspecified Image Color;1=Specified Image Color' );
     AddUShortTag(   P+$84E8, 'BackgroundColorIndicator', 1, 'Background Color Indicator',
                              '0=Unspecified Image Color;1=Specified Image Color' );
     AddUShortTag(   P+$84E9, 'ImageColorValue'          );
     AddUShortTag(   P+$84EA, 'BackgroundColorValue'     );
     AddUShortTag(   P+$84EB, 'PixelIntensityRange'      );
     AddUShortTag(   P+$84EC, 'TransparencyIndicator'    );
     AddULongTag(    P+$84ED, 'ColorCharacterization'    );
     AddULongTag(    P+$84EE, 'HCUsage',       1, 'HCUsage',
                              '0=CT;1=Line Art;2=Trap'   );
     AddULongTag(    P+$84EF, 'TrapIndicator'            );
     AddBinaryTag   (P+$8773, 'InterColorProfile',         1, '');                 // MK 2021.09.24
     AddUShortTag   (P+$8830, 'SensitivityType',           1, rsSensitivityType, rsSensitivityTypeLkup);  // MK 2021.09.24
     AddULongTag    (E+$8832, 'RecommendedExposureIndex',  1, rsRecExpIndex );
     AddUShortTag   (I+$9209, 'Flash',                     1, rsFlash, rsFlashLkup);
     AddUShortTag   (E+$920B, 'FlashEnergy',               1 );
     AddUShortTag   (E+$920C, 'SpatialFrequencyResponse',  1 );                                              // MK 2021.09.25
     AddUShortTag   (E+$920D, 'Noise',                     1 );                                              // MK 2021.09.25
     AddUShortTag   (E+$920E, 'FocalPlaneXResolution',     1, 'Focal Plane X Resolution', '', '%f' );        // MK 2021.09.25
     AddUShortTag   (E+$920F, 'FocalPlaneYResolution',     1, 'Focal Plane Y Resolution', '', '%f' );        // MK 2021.09.25
     AddUShortTag   (E+$9210, 'FocalPlaneResolutionUnit',  1, 'Focal Plane Resolution Unit',                 // MK 2021.09.25
                       '1=None specified;'+
                       '2=inches;'+
                       '3=cm'
                    );
     AddUShortTag   (P+$9214, 'SubjectArea',               4, rsSubjectArea);
     AddUShortTag   (P+$A002, 'ExifImageWidth',            1, rsExifImageWidth);
     AddUShortTag   (P+$A401, 'CustomRendered',            1, rsCustomRendered, rsCustomRenderedLkup);       // MK 2021.09.25
     AddUShortTag   (P+$A402, 'ExposureMode',              1, rsExposureMode, rsExposureModeLkup);           // MK 2021.09.25
     AddUShortTag   (P+$A403, 'WhiteBalance',              1, rsWhiteBalance, rsAutoManual);                 // MK 2021.09.25
     AddURationalTag(P+$A404, 'DigitalZoomRatio',          1, rsDigitalZoomRatio);                           // MK 2021.09.25
     AddUShortTag   (P+$A405, 'FocalLengthIn35mmFilm',     1, rsFocalLengthIn35mm, '', '%d mm');             // MK 2021.09.25
     AddUShortTag   (P+$A406, 'SceneCaptureType',          1, rsSceneCaptureType, rsSceneCaptureTypeLkup);   // MK 2021.09.25
     AddUShortTag   (P+$A407, 'GainControl',               1, rsGainControl, rsGainControlLkup);             // MK 2021.09.25
     AddUShortTag   (P+$A408, 'Contrast',                  1, rsContrast, rsNormalLowHigh);                  // MK 2021.09.25
     AddUShortTag   (P+$A409, 'Saturation',                1, rsSaturation, rsNormalLowHigh);                // MK 2021.09.25
     AddUShortTag   (P+$A40A, 'Sharpness',                 1, rsSharpness, rsNormalSoftHard);                // MK 2021.09.25
     AddBinaryTag   (P+$A40B, 'DeviceSettingDescription',  1, rsDeviceSettingDescription);
     AddUShortTag   (P+$A40C, 'SubjectDistanceRange',      1, rsSubjectDistancerange, rsSubjectDistanceRangeLkup);
     AddBinaryTag   (P+$EA1C, 'Padding',               $FFFF, '', '', '', nil, true);   // MK 2021.09.24

   end;

end;

function GetExTagName(const AId: TTagId; const ADefault: string = ''): string;
  var tmp : TTagDef;
begin
   if ExifTagDefsEx = nil then begin
      ExifTagDefsEx := TTagDefList.Create();
      BuildExifTagDefsEx();
   end;

   tmp := ExifTagDefsEx.FindByID( AId );
   if tmp = nil
      then Result := ADefault
      else Result := tmp.Name;
end;

function GetExTags: TTagDefList;
begin
   if ExifTagDefsEx = nil then begin
      ExifTagDefsEx := TTagDefList.Create();
      BuildExifTagDefsEx();
   end;

   Result := ExifTagDefsEx;

end;


finalization
   if ExifTagDefsEx <> nil then ExifTagDefsEx.Free;

end.

