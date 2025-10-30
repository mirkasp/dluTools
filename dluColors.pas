unit dluColors;
     
interface

// special color !
const clGold          = $0000D7FF;
const clBisque        = $00C4E4FF;
const clSeashell      = $00EEF5FF;
const clMistyRose     = $00E1E4FF;
const clLemonChiffon  = $00CDFAFF;
const clPapayaWhip    = $00D5EFFF;
const clPowderBlue    = $00E6E0B0;
const clPaleTurquoise = $00EEEEAF;
const clLightGreen    = $0090EE90;

function GetRandomNxColor(): Integer;

implementation

const RndColor : array[ 0..6 ] of integer = ( clPapayaWhip,  clMistyRose, clSeashell,
                                              clBisque,      clGold,      clPowderBlue,
                                              clLemonChiffon
                                            );
var xRandom: boolean = false;


function GetRandomNxColor(): Integer;
begin
   if not xRandom then begin
      Randomize;
      xRandom := true;
   end;
   Result := RndColor[ Random( Length( RndColor ) ) ];
end;

end.
