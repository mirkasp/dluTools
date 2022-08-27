unit dluMimeDict;

{$mode ObjFPC}{$H+}

interface

uses dluDictString;


type

{ TMimeDict }

 TMimeDict = class (TDictString)
   strict private
     procedure BuildStandardDef();
     procedure BuildSystemDef();
     procedure AddItem( const AKey, AValue: string );
   public
     constructor Create();
     function MimeForExtension( const AExtension: string ): string;
     function ExtensionsForMime( const AMime: string ): string;
end;



implementation

uses Classes, SysUtils, Registry;

{ TMimeDict }

constructor TMimeDict.Create;
begin
   inherited Create;
   BuildStandardDef;
   BuildSystemDef;
end;

procedure TMimeDict.BuildStandardDef;
begin
   { Animation }
   AddItem( '.nml',  'animation/narrative');    {Do not Localize}

   { Audio }
   AddItem( '.aac',    'audio/mp4'        );
   AddItem( '.aif',    'audio/x-aiff'     );           {Do not Localize}
   AddItem( '.aifc',   'audio/x-aiff'     );          {Do not Localize}
   AddItem( '.aiff',   'audio/x-aiff'     );          {Do not Localize}

   AddItem( '.au',     'audio/basic'      );             {Do not Localize}
   AddItem( '.gsm',    'audio/x-gsm'      );            {Do not Localize}
   AddItem( '.kar',    'audio/midi'       );             {Do not Localize}
   AddItem( '.m3u',    'audio/mpegurl');          {Do not Localize}
   AddItem( '.m4a',    'audio/x-mpg');    {Do not Localize}
   AddItem( '.mid',    'audio/midi');    {Do not Localize}
   AddItem( '.midi',   'audio/midi');    {Do not Localize}
   AddItem( '.mpega',  'audio/x-mpg');    {Do not Localize}
   AddItem( '.mp2',    'audio/x-mpg');    {Do not Localize}
   AddItem( '.mp3',    'audio/x-mpg');    {Do not Localize}
   AddItem( '.mpga',   'audio/x-mpg');    {Do not Localize}
   AddItem( '.m3u',    'audio/x-mpegurl');    {Do not Localize}
   AddItem( '.pls',    'audio/x-scpls');   {Do not Localize}
   AddItem( '.qcp',    'audio/vnd.qcelp');    {Do not Localize}
   AddItem( '.ra',     'audio/x-realaudio');    {Do not Localize}
   AddItem( '.ram',    'audio/x-pn-realaudio');    {Do not Localize}
   AddItem( '.rm',     'audio/x-pn-realaudio');    {Do not Localize}
   AddItem( '.sd2',    'audio/x-sd2'         );    {Do not Localize}
   AddItem( '.sid',    'audio/prs.sid'       );   {Do not Localize}
   AddItem( '.snd',    'audio/basic'         );   {Do not Localize}
   AddItem( '.wav',    'audio/x-wav'         );    {Do not Localize}
   AddItem( '.wax',    'audio/x-ms-wax'      );    {Do not Localize}
   AddItem( '.wma',    'audio/x-ms-wma'      );    {Do not Localize}

   AddItem( '.mjf',    'audio/x-vnd.AudioExplosion.MjuiceMediaFile');    {Do not Localize}

   { Image }
   AddItem( '.art',    'image/x-jg'          );    {Do not Localize}
   AddItem( '.bmp',    'image/bmp'          );    {Do not Localize}
   AddItem( '.cdr',    'image/x-coreldraw');    {Do not Localize}
   AddItem( '.cdt',    'image/x-coreldrawtemplate');    {Do not Localize}
   AddItem( '.cpt',    'image/x-corelphotopaint');    {Do not Localize}
   AddItem( '.djv',    'image/vnd.djvu');    {Do not Localize}
   AddItem( '.djvu',   'image/vnd.djvu');    {Do not Localize}
   AddItem( '.gif',    'image/gif');    {Do not Localize}
   AddItem( '.ief',    'image/ief');    {Do not Localize}
   AddItem( '.ico',    'image/x-icon');    {Do not Localize}
   AddItem( '.jng',    'image/x-jng');    {Do not Localize}
   AddItem( '.jpg',    'image/jpeg');    {Do not Localize}
   AddItem( '.jpeg',   'image/jpeg');    {Do not Localize}
   AddItem( '.jpe',    'image/jpeg');    {Do not Localize}
   AddItem( '.pat',    'image/x-coreldrawpattern');   {Do not Localize}
   AddItem( '.pcx',    'image/pcx');    {Do not Localize}
   AddItem( '.pbm',    'image/x-portable-bitmap');    {Do not Localize}
   AddItem( '.pgm',    'image/x-portable-graymap');    {Do not Localize}
   AddItem( '.pict',   'image/x-pict');    {Do not Localize}
   AddItem( '.png',    'image/x-png');    {Do not Localize}
   AddItem( '.pnm',    'image/x-portable-anymap');    {Do not Localize}
   AddItem( '.pntg',   'image/x-macpaint');    {Do not Localize}
   AddItem( '.ppm',    'image/x-portable-pixmap');    {Do not Localize}
   AddItem( '.psd',    'image/x-psd');    {Do not Localize}
   AddItem( '.qtif',   'image/x-quicktime');    {Do not Localize}
   AddItem( '.ras',    'image/x-cmu-raster');    {Do not Localize}
   AddItem( '.rf',     'image/vnd.rn-realflash');    {Do not Localize}
   AddItem( '.rgb',    'image/x-rgb');    {Do not Localize}
   AddItem( '.rp',     'image/vnd.rn-realpix');    {Do not Localize}
   AddItem( '.sgi',    'image/x-sgi');    {Do not Localize}
   AddItem( '.svg',    'image/svg+xml');    {Do not Localize}
   AddItem( '.svgz',   'image/svg+xml');    {Do not Localize}
   AddItem( '.targa',  'image/x-targa');    {Do not Localize}
   AddItem( '.tif',    'image/x-tiff');    {Do not Localize}
   AddItem( '.wbmp',   'image/vnd.wap.wbmp');    {Do not Localize}
   AddItem( '.webp',   'image/webp'); {Do not localize}
   AddItem( '.xbm',    'image/xbm');    {Do not Localize}
   AddItem( '.xbm',    'image/x-xbitmap');    {Do not Localize}
   AddItem( '.xpm',    'image/x-xpixmap');    {Do not Localize}
   AddItem( '.xwd',    'image/x-xwindowdump');    {Do not Localize}

   { Text }
   AddItem( '.323',    'text/h323');    {Do not Localize}

   AddItem( '.xml',    'text/xml');    {Do not Localize}
   AddItem( '.uls',    'text/iuls');    {Do not Localize}
   AddItem( '.txt',    'text/plain');    {Do not Localize}
   AddItem( '.rtx',    'text/richtext');    {Do not Localize}
   AddItem( '.wsc',    'text/scriptlet');    {Do not Localize}
   AddItem( '.rt',     'text/vnd.rn-realtext');    {Do not Localize}
   AddItem( '.htt',    'text/webviewhtml');    {Do not Localize}
   AddItem( '.htc',    'text/x-component');    {Do not Localize}
   AddItem( '.vcf',    'text/x-vcard');    {Do not Localize}

   { Video }
   AddItem( '.asf',    'video/x-ms-asf'      );    {Do not Localize}
   AddItem( '.asx',    'video/x-ms-asf' );    {Do not Localize}
   AddItem( '.avi',    'video/x-msvideo');    {Do not Localize}
   AddItem( '.dl',     'video/dl');    {Do not Localize}
   AddItem( '.dv',     'video/dv');  {Do not Localize}
   AddItem( '.flc',    'video/flc');    {Do not Localize}
   AddItem( '.fli',    'video/fli');    {Do not Localize}
   AddItem( '.gl',     'video/gl');    {Do not Localize}
   AddItem( '.lsf',    'video/x-la-asf');    {Do not Localize}
   AddItem( '.lsx',    'video/x-la-asf');    {Do not Localize}
   AddItem( '.mng',    'video/x-mng');    {Do not Localize}

   AddItem( '.mp2',    'video/mpeg');    {Do not Localize}
   AddItem( '.mp3',    'video/mpeg');    {Do not Localize}
   AddItem( '.mp4',    'video/mpeg');    {Do not Localize}
   AddItem( '.mpeg',   'video/x-mpeg2a');    {Do not Localize}
   AddItem( '.mpa',    'video/mpeg');    {Do not Localize}
   AddItem( '.mpe',    'video/mpeg');    {Do not Localize}
   AddItem( '.mpg',    'video/mpeg');    {Do not Localize}
   AddItem( '.ogv',    'video/ogg');    {Do not Localize}
   AddItem( '.moov',   'video/quicktime');     {Do not Localize}
   AddItem( '.mov',    'video/quicktime');    {Do not Localize}
   AddItem( '.mxu',    'video/vnd.mpegurl');   {Do not Localize}
   AddItem( '.qt',     'video/quicktime');    {Do not Localize}
   AddItem( '.qtc',    'video/x-qtc'); {Do not loccalize}
   AddItem( '.rv',     'video/vnd.rn-realvideo');    {Do not Localize}
   AddItem( '.ivf',    'video/x-ivf');    {Do not Localize}
   AddItem( '.webm',   'video/webm');    {Do not Localize}
   AddItem( '.wm',     'video/x-ms-wm');    {Do not Localize}
   AddItem( '.wmp',    'video/x-ms-wmp');    {Do not Localize}
   AddItem( '.wmv',    'video/x-ms-wmv');    {Do not Localize}
   AddItem( '.wmx',    'video/x-ms-wmx');    {Do not Localize}
   AddItem( '.wvx',    'video/x-ms-wvx');    {Do not Localize}
   AddItem( '.rms',    'video/vnd.rn-realvideo-secure');    {Do not Localize}
   AddItem( '.asx',    'video/x-ms-asf-plugin');    {Do not Localize}
   AddItem( '.movie',  'video/x-sgi-movie');    {Do not Localize}

   { Application }
   AddItem( '.7z',  'application/x-7z-compressed');   {Do not Localize}
   AddItem( '.a',  'application/x-archive');   {Do not Localize}
   AddItem( '.aab',  'application/x-authorware-bin');    {Do not Localize}
   AddItem( '.aam',  'application/x-authorware-map');    {Do not Localize}
   AddItem( '.aas',  'application/x-authorware-seg');    {Do not Localize}
   AddItem( '.abw',  'application/x-abiword');    {Do not Localize}
   AddItem( '.ace',  'application/x-ace-compressed');  {Do not Localize}
   AddItem( '.ai',  'application/postscript');    {Do not Localize}
   AddItem( '.alz',  'application/x-alz-compressed');    {Do not Localize}
   AddItem( '.ani',  'application/x-navi-animation');   {Do not Localize}
   AddItem( '.arj',  'application/x-arj');    {Do not Localize}
   AddItem( '.asf',  'application/vnd.ms-asf');    {Do not Localize}
   AddItem( '.bat',  'application/x-msdos-program');    {Do not Localize}
   AddItem( '.bcpio',  'application/x-bcpio');    {Do not Localize}
   AddItem( '.boz',  'application/x-bzip2');     {Do not Localize}
   AddItem( '.bz',  'application/x-bzip');
   AddItem( '.bz2',  'application/x-bzip2');    {Do not Localize}
   AddItem( '.cab',  'application/vnd.ms-cab-compressed');    {Do not Localize}
   AddItem( '.cat',  'application/vnd.ms-pki.seccat');    {Do not Localize}
   AddItem( '.ccn',  'application/x-cnc');    {Do not Localize}
   AddItem( '.cco',  'application/x-cocoa');    {Do not Localize}
   AddItem( '.cdf',  'application/x-cdf');    {Do not Localize}
   AddItem( '.cer',  'application/x-x509-ca-cert');    {Do not Localize}
   AddItem( '.chm',  'application/vnd.ms-htmlhelp');    {Do not Localize}
   AddItem( '.chrt',  'application/vnd.kde.kchart');    {Do not Localize}
   AddItem( '.cil',  'application/vnd.ms-artgalry');    {Do not Localize}
   AddItem( '.class',  'application/java-vm');    {Do not Localize}
   AddItem( '.com',  'application/x-msdos-program');    {Do not Localize}
   AddItem( '.clp',  'application/x-msclip');    {Do not Localize}
   AddItem( '.cpio',  'application/x-cpio');    {Do not Localize}
   AddItem( '.cpt',  'application/mac-compactpro');    {Do not Localize}
   AddItem( '.cqk',  'application/x-calquick');    {Do not Localize}
   AddItem( '.crd',  'application/x-mscardfile');    {Do not Localize}
   AddItem( '.crl',  'application/pkix-crl');    {Do not Localize}
   AddItem( '.csh',  'application/x-csh');    {Do not Localize}
   AddItem( '.dar',  'application/x-dar');    {Do not Localize}
   AddItem( '.dbf',  'application/x-dbase');    {Do not Localize}
   AddItem( '.dcr',  'application/x-director');    {Do not Localize}
   AddItem( '.deb',  'application/x-debian-package');    {Do not Localize}
   AddItem( '.dir',  'application/x-director');    {Do not Localize}
   AddItem( '.dist',  'vnd.apple.installer+xml');    {Do not Localize}
   AddItem( '.distz',  'vnd.apple.installer+xml');    {Do not Localize}
   AddItem( '.dll',  'application/x-msdos-program');    {Do not Localize}
   AddItem( '.dmg',  'application/x-apple-diskimage');    {Do not Localize}
   AddItem( '.doc',  'application/msword');    {Do not Localize}
   AddItem( '.dot',  'application/msword');    {Do not Localize}
   AddItem( '.dvi',  'application/x-dvi');    {Do not Localize}
   AddItem( '.dxr',  'application/x-director');    {Do not Localize}
   AddItem( '.ebk',  'application/x-expandedbook');    {Do not Localize}
   AddItem( '.eps',  'application/postscript');    {Do not Localize}
   AddItem( '.evy',  'application/envoy');    {Do not Localize}
   AddItem( '.exe',  'application/x-msdos-program');    {Do not Localize}
   AddItem( '.fdf',  'application/vnd.fdf');    {Do not Localize}
   AddItem( '.fif',  'application/fractals');    {Do not Localize}
   AddItem( '.flm',  'application/vnd.kde.kivio');    {Do not Localize}
   AddItem( '.fml',  'application/x-file-mirror-list');    {Do not Localize}
   AddItem( '.gzip',  'application/x-gzip');  {Do not Localize}
   AddItem( '.gnumeric',  'application/x-gnumeric');    {Do not Localize}
   AddItem( '.gtar',  'application/x-gtar');    {Do not Localize}
   AddItem( '.gz',  'application/x-gzip');    {Do not Localize}
   AddItem( '.hdf',  'application/x-hdf');    {Do not Localize}
   AddItem( '.hlp',  'application/winhlp');    {Do not Localize}
   AddItem( '.hpf',  'application/x-icq-hpf');    {Do not Localize}
   AddItem( '.hqx',  'application/mac-binhex40');    {Do not Localize}
   AddItem( '.hta',  'application/hta');    {Do not Localize}
   AddItem( '.ims',  'application/vnd.ms-ims');    {Do not Localize}
   AddItem( '.ins',  'application/x-internet-signup');    {Do not Localize}
   AddItem( '.iii',  'application/x-iphone');    {Do not Localize}
   AddItem( '.iso',  'application/x-iso9660-image');    {Do not Localize}
   AddItem( '.jar',     'application/java-archive');    {Do not Localize}
   AddItem( '.karbon',  'application/vnd.kde.karbon');    {Do not Localize}
   AddItem( '.kfo',     'application/vnd.kde.kformula');    {Do not Localize}
   AddItem( '.kon',     'application/vnd.kde.kontour');    {Do not Localize}
   AddItem( '.kpr',     'application/vnd.kde.kpresenter');    {Do not Localize}
   AddItem( '.kpt',     'application/vnd.kde.kpresenter');    {Do not Localize}
   AddItem( '.kwd',     'application/vnd.kde.kword');    {Do not Localize}
   AddItem( '.kwt',     'application/vnd.kde.kword');    {Do not Localize}
   AddItem( '.latex',   'application/x-latex');    {Do not Localize}
   AddItem( '.lha',     'application/x-lzh');    {Do not Localize}
   AddItem( '.lcc',     'application/fastman');    {Do not Localize}
   AddItem( '.lrm',     'application/vnd.ms-lrm');    {Do not Localize}
   AddItem( '.lz',      'application/x-lzip');    {Do not Localize}
   AddItem( '.lzh',     'application/x-lzh');    {Do not Localize}
   AddItem( '.lzma',    'application/x-lzma');  {Do not Localize}
   AddItem( '.lzo',     'application/x-lzop'); {Do not Localize}
   AddItem( '.lzx',     'application/x-lzx');
   AddItem( '.m13',     'application/x-msmediaview');    {Do not Localize}
   AddItem( '.m14',     'application/x-msmediaview');    {Do not Localize}
   AddItem( '.mpp',     'application/vnd.ms-project');    {Do not Localize}
   AddItem( '.mvb',     'application/x-msmediaview');    {Do not Localize}
   AddItem( '.man',     'application/x-troff-man');    {Do not Localize}
   AddItem( '.mdb',     'application/x-msaccess');    {Do not Localize}
   AddItem( '.me',      'application/x-troff-me');    {Do not Localize}
   AddItem( '.ms',      'application/x-troff-ms');    {Do not Localize}
   AddItem( '.msi',     'application/x-msi');    {Do not Localize}
   AddItem( '.mpkg',    'vnd.apple.installer+xml');    {Do not Localize}
   AddItem( '.mny',     'application/x-msmoney');    {Do not Localize}
   AddItem( '.nix',     'application/x-mix-transfer');    {Do not Localize}
   AddItem( '.o',       'application/x-object');    {Do not Localize}
   AddItem( '.oda',     'application/oda');    {Do not Localize}
   AddItem( '.odb',     'application/vnd.oasis.opendocument.database');    {Do not Localize}
   AddItem( '.odc',     'application/vnd.oasis.opendocument.chart');    {Do not Localize}
   AddItem( '.odf',     'application/vnd.oasis.opendocument.formula');    {Do not Localize}
   AddItem( '.odg',     'application/vnd.oasis.opendocument.graphics');    {Do not Localize}
   AddItem( '.odi',     'application/vnd.oasis.opendocument.image');    {Do not Localize}
   AddItem( '.odm',     'application/vnd.oasis.opendocument.text-master');    {Do not Localize}
   AddItem( '.odp',     'application/vnd.oasis.opendocument.presentation');    {Do not Localize}
   AddItem( '.ods',     'application/vnd.oasis.opendocument.spreadsheet');    {Do not Localize}
   AddItem( '.ogg',     'application/ogg'                                         );    {Do not Localize}
   AddItem( '.odt',     'application/vnd.oasis.opendocument.text'                 );    {Do not Localize}
   AddItem( '.otg',     'application/vnd.oasis.opendocument.graphics-template'    );    {Do not Localize}
   AddItem( '.oth',     'application/vnd.oasis.opendocument.text-web'             );    {Do not Localize}
   AddItem( '.otp',     'application/vnd.oasis.opendocument.presentation-template');    {Do not Localize}
   AddItem( '.ots',     'application/vnd.oasis.opendocument.spreadsheet-template' );    {Do not Localize}
   AddItem( '.ott',     'application/vnd.oasis.opendocument.text-template'        );    {Do not Localize}
   AddItem( '.p10',     'application/pkcs10'                                      );    {Do not Localize}
   AddItem( '.p12',     'application/x-pkcs12'                                    );    {Do not Localize}
   AddItem( '.p7b',     'application/x-pkcs7-certificates'                        );    {Do not Localize}
   AddItem( '.p7m',     'application/pkcs7-mime'                                  );    {Do not Localize}
   AddItem( '.p7r',     'application/x-pkcs7-certreqresp'                         );    {Do not Localize}
   AddItem( '.p7s',     'application/pkcs7-signature'                             );    {Do not Localize}
   AddItem( '.package', 'application/vnd.autopackage'                         );    {Do not Localize}
   AddItem( '.pfr',     'application/font-tdpfr'                                  );    {Do not Localize}
   AddItem( '.pkg',     'vnd.apple.installer+xml'                                 );    {Do not Localize}
   AddItem( '.pdf',     'application/pdf'                                         );    {Do not Localize}
   AddItem( '.pko',     'application/vnd.ms-pki.pko'                              );    {Do not Localize}
   AddItem( '.pl',      'application/x-perl'                                       );    {Do not Localize}
   AddItem( '.pnq',     'application/x-icq-pnq'                                   );    {Do not Localize}
   AddItem( '.pot',     'application/mspowerpoint'                                );    {Do not Localize}
   AddItem( '.pps',     'application/mspowerpoint'                                );    {Do not Localize}
   AddItem( '.ppt',     'application/mspowerpoint'                                );    {Do not Localize}
   AddItem( '.ppz',     'application/mspowerpoint'                                );    {Do not Localize}
   AddItem( '.ps',      'application/postscript'                                   );    {Do not Localize}
   AddItem( '.pub',     'application/x-mspublisher'                               );    {Do not Localize}
   AddItem( '.qpw',     'application/x-quattropro'                                );    {Do not Localize}
   AddItem( '.qtl',     'application/x-quicktimeplayer'                           );    {Do not Localize}
   AddItem( '.rar',     'application/rar'                                         );    {Do not Localize}
   AddItem( '.rdf',     'application/rdf+xml'                                     );    {Do not Localize}
   AddItem( '.rjs',     'application/vnd.rn-realsystem-rjs'                       );    {Do not Localize}
   AddItem( '.rm',      'application/vnd.rn-realmedia'                             );    {Do not Localize}
   AddItem( '.rmf',     'application/vnd.rmf'                                     );    {Do not Localize}
   AddItem( '.rmp',     'application/vnd.rn-rn_music_package'                     );    {Do not Localize}
   AddItem( '.rmx',     'application/vnd.rn-realsystem-rmx'                       );    {Do not Localize}
   AddItem( '.rnx',     'application/vnd.rn-realplayer'                           );    {Do not Localize}
   AddItem( '.rpm',     'application/x-redhat-package-manager');
   AddItem( '.rsml',    'application/vnd.rn-rsml');    {Do not Localize}
   AddItem( '.rtsp',    'application/x-rtsp');    {Do not Localize}
   AddItem( '.rss',     'application/rss+xml');    {Do not Localize}
   AddItem( '.scm',     'application/x-icq-scm');    {Do not Localize}
   AddItem( '.ser',     'application/java-serialized-object');    {Do not Localize}
   AddItem( '.scd',     'application/x-msschedule');    {Do not Localize}
   AddItem( '.sda',     'application/vnd.stardivision.draw');    {Do not Localize}
   AddItem( '.sdc',     'application/vnd.stardivision.calc');    {Do not Localize}
   AddItem( '.sdd',     'application/vnd.stardivision.impress');    {Do not Localize}
   AddItem( '.sdp',     'application/x-sdp');    {Do not Localize}
   AddItem( '.setpay',  'application/set-payment-initiation');    {Do not Localize}
   AddItem( '.setreg',  'application/set-registration-initiation');    {Do not Localize}
   AddItem( '.sh',      'application/x-sh');    {Do not Localize}
   AddItem( '.shar',    'application/x-shar');    {Do not Localize}
   AddItem( '.shw',     'application/presentations');    {Do not Localize}
   AddItem( '.sit',     'application/x-stuffit');    {Do not Localize}
   AddItem( '.sitx',    'application/x-stuffitx');  {Do not localize}
   AddItem( '.skd',     'application/x-koan');    {Do not Localize}
   AddItem( '.skm',     'application/x-koan');    {Do not Localize}
   AddItem( '.skp',     'application/x-koan');    {Do not Localize}
   AddItem( '.skt',     'application/x-koan');    {Do not Localize}
   AddItem( '.smf',     'application/vnd.stardivision.math');    {Do not Localize}
   AddItem( '.smi',     'application/smil');    {Do not Localize}
   AddItem( '.smil',    'application/smil');    {Do not Localize}
   AddItem( '.spl',     'application/futuresplash');    {Do not Localize}
   AddItem( '.ssm',     'application/streamingmedia');    {Do not Localize}
   AddItem( '.sst',     'application/vnd.ms-pki.certstore');    {Do not Localize}
   AddItem( '.stc',     'application/vnd.sun.xml.calc.template');    {Do not Localize}
   AddItem( '.std',     'application/vnd.sun.xml.draw.template');    {Do not Localize}
   AddItem( '.sti',     'application/vnd.sun.xml.impress.template');    {Do not Localize}
   AddItem( '.stl',     'application/vnd.ms-pki.stl');    {Do not Localize}
   AddItem( '.stw',     'application/vnd.sun.xml.writer.template');    {Do not Localize}
   AddItem( '.svi',     'application/softvision');    {Do not Localize}
   AddItem( '.sv4cpio', 'application/x-sv4cpio');    {Do not Localize}
   AddItem( '.sv4crc',  'application/x-sv4crc');    {Do not Localize}
   AddItem( '.swf',     'application/x-shockwave-flash');    {Do not Localize}
   AddItem( '.swf1',    'application/x-shockwave-flash');    {Do not Localize}
   AddItem( '.sxc',     'application/vnd.sun.xml.calc');    {Do not Localize}
   AddItem( '.sxi',     'application/vnd.sun.xml.impress');    {Do not Localize}
   AddItem( '.sxm',     'application/vnd.sun.xml.math');    {Do not Localize}
   AddItem( '.sxw',     'application/vnd.sun.xml.writer');    {Do not Localize}
   AddItem( '.sxg',     'application/vnd.sun.xml.writer.global');    {Do not Localize}
   AddItem( '.t',       'application/x-troff');    {Do not Localize}
   AddItem( '.tar',     'application/x-tar');    {Do not Localize}
   AddItem( '.tcl',     'application/x-tcl');    {Do not Localize}
   AddItem( '.tex',     'application/x-tex');    {Do not Localize}
   AddItem( '.texi',    'application/x-texinfo');    {Do not Localize}
   AddItem( '.texinfo', 'application/x-texinfo');    {Do not Localize}
   AddItem( '.tbz',     'application/x-bzip-compressed-tar');   {Do not Localize}
   AddItem( '.tbz2',    'application/x-bzip-compressed-tar' );   {Do not Localize}
   AddItem( '.tgz',     'application/x-compressed-tar'      );    {Do not Localize}
   AddItem( '.tlz',     'application/x-lzma-compressed-tar' );    {Do not Localize}
   AddItem( '.tr',      'application/x-troff'               );    {Do not Localize}
   AddItem( '.trm',     'application/x-msterminal'          );    {Do not Localize}
   AddItem( '.troff',   'application/x-troff'               );    {Do not Localize}
   AddItem( '.tsp',     'application/dsptype'               );    {Do not Localize}
   AddItem( '.torrent', 'application/x-bittorrent'          );    {Do not Localize}
   AddItem( '.ttz',     'application/t-time'                );    {Do not Localize}
   AddItem( '.txz',     'application/x-xz-compressed-tar'   ); {Do not localize}
   AddItem( '.udeb',    'application/x-debian-package'      );    {Do not Localize}

   AddItem( '.uin',     'application/x-icq'                  );    {Do not Localize}
   AddItem( '.urls',    'application/x-url-list'             );    {Do not Localize}
   AddItem( '.ustar',   'application/x-ustar'                );    {Do not Localize}
   AddItem( '.vcd',     'application/x-cdlink'               );    {Do not Localize}
   AddItem( '.vor',     'application/vnd.stardivision.writer');    {Do not Localize}
   AddItem( '.vsl',     'application/x-cnet-vsl'             );    {Do not Localize}
   AddItem( '.wcm',     'application/vnd.ms-works'           );    {Do not Localize}
   AddItem( '.wb1',     'application/x-quattropro'           );    {Do not Localize}
   AddItem( '.wb2',     'application/x-quattropro'           );    {Do not Localize}
   AddItem( '.wb3',     'application/x-quattropro'           );    {Do not Localize}
   AddItem( '.wdb',     'application/vnd.ms-works'           );    {Do not Localize}
   AddItem( '.wks',     'application/vnd.ms-works'           );    {Do not Localize}
   AddItem( '.wmd',     'application/x-ms-wmd'               );    {Do not Localize}
   AddItem( '.wms',     'application/x-ms-wms'               );    {Do not Localize}
   AddItem( '.wmz',     'application/x-ms-wmz'               );    {Do not Localize}
   AddItem( '.wp5',     'application/wordperfect5.1'         );    {Do not Localize}
   AddItem( '.wpd',     'application/wordperfect'            ); {Do not Localize}
   AddItem( '.wpl',     'application/vnd.ms-wpl'             ); {Do not Localize}
   AddItem( '.wps',     'application/vnd.ms-works'           ); {Do not Localize}
   AddItem( '.wri',     'application/x-mswrite'              ); {Do not Localize}
   AddItem( '.xfdf',    'application/vnd.adobe.xfdf'         ); {Do not Localize}
   AddItem( '.xls',     'application/x-msexcel'              ); {Do not Localize}
   AddItem( '.xlb',     'application/x-msexcel'              ); {Do not Localize}
   AddItem( '.xpi',     'application/x-xpinstall'            ); {Do not Localize}
   AddItem( '.xps',     'application/vnd.ms-xpsdocument'     ); {Do not Localize}
   AddItem( '.xsd',     'application/vnd.sun.xml.draw'       ); {Do not Localize}
   AddItem( '.xul',     'application/vnd.mozilla.xul+xml'    ); {Do not Localize}
   AddItem( '.z',       'application/x-compress'             ); {Do not Localize}
   AddItem( '.zoo',     'application/x-zoo'                  ); {Do not Localize}
   AddItem( '.zip',     'application/x-zip-compressed'       ); {Do not Localize}

   { WAP }
   AddItem( '.wbmp',    'image/vnd.wap.wbmp'                 ); {Do not Localize}
   AddItem( '.wml',     'text/vnd.wap.wml'                   ); {Do not Localize}
   AddItem( '.wmlc',    'application/vnd.wap.wmlc'           ); {Do not Localize}
   AddItem( '.wmls',    'text/vnd.wap.wmlscript'             ); {Do not Localize}
   AddItem( '.wmlsc',   'application/vnd.wap.wmlscriptc'     ); {Do not Localize}

   { Non-web text}
   {
   IMPORTANT!!

   You should not use a text MIME type definition unless you are
   extremely certain that the file will NOT be a binary.  Some browsers
   will display the text instead of saving to disk and it looks ugly
   if a web-browser shows all of the 8bit charactors.
   }
   //of course, we have to add this :-).
   AddItem( '.asm',     'text/x-asm'                         ); {Do not Localize}
   AddItem( '.p',       'text/x-pascal'                      ); {Do not Localize}
   AddItem( '.pas',     'text/x-pascal'                      ); {Do not Localize}

   AddItem( '.cs',      'text/x-csharp'                      ); {Do not Localize}

   AddItem( '.c',       'text/x-csrc'                        ); {Do not Localize}
   AddItem( '.c++',     'text/x-c++src'                      ); {Do not Localize}
   AddItem( '.cpp',     'text/x-c++src'                      ); {Do not Localize}
   AddItem( '.cxx',     'text/x-c++src'                      ); {Do not Localize}
   AddItem( '.cc',      'text/x-c++src'                      ); {Do not Localize}
   AddItem( '.h',       'text/x-chdr'                        ); {Do not localize}
   AddItem( '.h++',     'text/x-c++hdr'                      ); {Do not Localize}
   AddItem( '.hpp',     'text/x-c++hdr'                      ); {Do not Localize}
   AddItem( '.hxx',     'text/x-c++hdr'                      ); {Do not Localize}
   AddItem( '.hh',      'text/x-c++hdr'                      ); {Do not Localize}
   AddItem( '.java',    'text/x-java'                        ); {Do not Localize}

   { WEB }
   AddItem( '.css',     'text/css'                           ); {Do not Localize}
   AddItem( '.js',      'text/javascript'                    ); {Do not Localize}
   AddItem( '.htm',     'text/html'                          ); {Do not Localize}
   AddItem( '.html',    'text/html'                          ); {Do not Localize}
   AddItem( '.xhtml',   'application/xhtml+xml'              ); {Do not localize}
   AddItem( '.xht',     'application/xhtml+xml'              ); {Do not localize}
   AddItem( '.rdf',     'application/rdf+xml'                ); {Do not localize}
   AddItem( '.rss',     'application/rss+xml'                ); {Do not localize}

   AddItem( '.ls',      'text/javascript'                    ); {Do not Localize}
   AddItem( '.mocha',   'text/javascript'                    ); {Do not Localize}
   AddItem( '.shtml',   'server-parsed-html'                 ); {Do not Localize}
   AddItem( '.xml',     'text/xml'                           ); {Do not Localize}
   AddItem( '.sgm',     'text/sgml'                          ); {Do not Localize}
   AddItem( '.sgml',    'text/sgml'                          ); {Do not Localize}

   { Message }
   AddItem( '.mht',     'message/rfc822'                     ); {Do not Localize}

end;

procedure TMimeDict.BuildSystemDef;
  function BeginWithDot( const AString: string ): boolean;
  begin
     Result := Length( AString ) > 0;
     if Result then Result := (AString[1] = '.');
  end;

  var Reg    : TRegistry;
     KeyList: TStringList;
     i      : integer;
     LExt   : string;
     s      : string;
begin
   // Build the file type/MIME type map
   Reg := TRegistry.Create;
   try
    KeyList := TStringList.create;
    try
       Reg.RootKey := HKEY_CLASSES_ROOT;
       if Reg.OpenKeyReadOnly('\') then begin  {do not localize}
          Reg.GetKeyNames( KeyList );
          Reg.Closekey;
       end;
       // get a list of registered extentions
       for i := 0 to KeyList.Count - 1 do begin
          LExt := KeyList[ i ];
          if BeginWithDot( LExt ) then begin
             if Reg.OpenKeyReadOnly( LExt ) then begin
                 s := Reg.ReadString( 'Content Type' );  {do not localize}
                 if Length(s) > 0 then begin
                  AddItem( LowerCase( LExt), LowerCase( s ) );
                 end;
                 Reg.CloseKey;
             end;
          end;
       end;

       if Reg.OpenKeyReadOnly( '\MIME\Database\Content Type' ) then begin {do not localize}
          // get a list of registered MIME types
          KeyList.Clear;
          Reg.GetKeyNames( KeyList );
          Reg.CloseKey;

         for i := 0 to KeyList.Count - 1 do begin
           if Reg.OpenKeyReadOnly('\MIME\Database\Content Type\' + KeyList[ i ]) then begin {do not localize}
            LExt := LowerCase( Reg.ReadString( 'Extension' ) );  {do not localize}
            if Length(LExt) > 0 then begin
              if LExt[1] <> '.' then begin
                 LExt := '.' + LExt; {do not localize}
              end;
              AddItem( LExt, LowerCase( KeyList[ i ] ) );
            end;
            Reg.CloseKey;
          end;
         end;
      end;

    finally
      KeyList.Free;
    end;
   finally
    Reg.Free;
   end;
end;

procedure TMimeDict.AddItem(const AKey, AValue: string);
begin
   self.Insert( LowerCase(AKey), LowerCase(AValue) );
end;

function TMimeDict.MimeForExtension(const AExtension: string): string;
  var sdef: string = '';
begin
   Result := '';
   if GetFirstValueForKey( LowerCase( AExtension ), Result ) then begin
      while GetNextValueForKey( sdef ) do Result := Result + ', ' + sdef;
   end;
end;

function TMimeDict.ExtensionsForMime(const AMime: string): string;
  var vKeys: array of string;
      i : integer;
      s : string;
begin
   vKeys  := Keys();
   Result := '';
   if Length( vKeys ) > 0 then begin
      for i := 0 to High( vKeys ) do begin
         s := '';
         if GetFirstValueForKey( vKeys[i], s ) then begin
            repeat
               if s = AMime then Result := Result + ' "' + vKeys[i] + '"';
            until not GetNextValueForKey( s );
         end;
      end;
   end;

end;

end.

