unit dluHyperlinks;

{$mode objfpc}{$H+}

interface

uses Classes
   ;

type
  THyperlinkList = TStringList;

procedure GetHyperlinksFromURL(const AURL: string; ALinkList: THyperlinkList; const ValidateUrl: boolean = true);

//<summary>
//Sprawdza, czy wejściowy ciąg znaków jest poprawnym linkiem do strony internetowej (URL HTTP lub HTTPS).
//Funkcja wykonuje walidację składniową przy użyciu wyrażenia regularnego.
//</summary>
//<param name="AInputString">Ciąg znaków do walidacji.</param>
//<returns>True, jeśli ciąg pasuje do typowego wzorca linku internetowego, False w przeciwnym razie.</returns>
function IsWebLink(const AInputString: AnsiString): Boolean; overload;

//<summary>
//Sprawdza, czy wejściowy ciąg znaków jest poprawnym linkiem do strony internetowej (URL HTTP lub HTTPS).
//Funkcja wykonuje walidację składniową przy użyciu wyrażenia regularnego.
//</summary>
//<param name="AInputString">Ciąg znaków do walidacji.</param>
//<returns>True, jeśli ciąg pasuje do typowego wzorca linku internetowego, False w przeciwnym razie.</returns>
function IsWebLink(const AInputString: UnicodeString): Boolean; overload;



implementation

uses SysUtils
   , Dialogs
   , RegExpr
   , fphttpclient // Ta jednostka jest częścią standardowej instalacji FPC
   , openssl
   , opensslsockets
   , dluStrings.Helper
   ;

function GetHttpContent( const AURL: string ): string;
  var HTTPClient : TFPHttpClient;
      //k : integer;
      //s : string;
      url : string;
begin
   Result := '';

   { SSL initialization has to be done by hand here }
   InitSSLInterface;

   HTTPClient := TFPHttpClient.Create(nil);
   try
      // Ustawienie User-Agenta, aby serwer nie odrzucił żądania
      HTTPClient.AddHeader( 'User-Agent', 'Mozilla/5.0 (compatible; FreePascalBot/1.0)' ); //UserAgent := 'Mozilla/5.0 (compatible; FreePascalBot/1.0)';

      // Pozwól na przekierowania (np. z http na https)
      HTTPClient.AllowRedirect := True;

      // Opcjonalnie: ustaw timeout
      HTTPClient.ConnectTimeout := 5000; // 5 sekund

      url := LowerCase( AURL );
      if Pos( 'http', url ) <> 1 then Insert( 'http://', url, 1 );

      try
         Result := HTTPClient.Get(URL);
      except
         on E: Exception do begin
            ShowMessage('Błąd podczas pobierania strony: '#10+E.Message);
            Exit; // Wyjście z procedury w przypadku błędu pobierania
         end;
      end;
   finally
      HTTPClient.Free;
   end;

end;

procedure GetLinkListEx(const AHtmlContent: string; ALinkList: THyperlinkList; const ValidateUrl: boolean);
  var k : integer;
      s : string;
begin
   // Parsowanie HTML za pomocą wyrażeń regularnych
   with TRegExpr.Create do begin
      try
         // Wyrażenie regularne do wyszukiwania hiperlinków w tagach <a>
         // Szuka: <a (dowolne znaki, niechętne) href=" (link) " (dowolne znaki, niechętne)>
         // Przechwytuje link w grupie 1: (.*?)
         Expression := '<a\s+(?:[^>]*?\s+)?href=["''](.*?)["'']';

         ModifierI  := true;
         ModifierM  := true;

         if Exec(AHtmlContent) then begin
            ALinkList.BeginUpdate;
            repeat
               for k:=1 to SubExprMatchCount do begin
                   s := Trim( Match[k] );
                   if (s <> '') {and (LinkList.IndexOf(s) < 0)}  then
                      ALinkList.AddBoolean( Match[k], ValidateUrl and IsWebLink( Match[k] ) );
               end;
            until not ExecNext;
            ALinkList.EndUpdate;
         end;

      finally
         Free;
      end;
   end;

end;

procedure GetHyperlinksFromURL(const AURL: string; ALinkList: THyperlinkList; const ValidateUrl: boolean);
  var HTMLContent: string;
begin
   HTMLContent := GetHttpContent( AURL );

   ALinkList.Clear;
   if HtmlContent <> '' then begin
      GetLinkListEx( HtmlContent, ALinkList, ValidateUrl );
   end;

end;


function IsWebLink(const AInputString: UnicodeString): Boolean; overload;
begin
   Result := IsWebLink( UTF8Encode( AInputString ) );
end;

function IsWebLink(const AInputString: string): Boolean;
  var Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    // Ten wzorzec został zaprojektowany, aby być solidnym dla typowych URL HTTP/HTTPS
    // i zawiera kotwice ^ i $, aby upewnić się, że *cały* ciąg pasuje.
    //
    // Rozkład wyrażenia regularnego:
    // ^                                  - Początek ciągu
    // (http|https)                       - Dopasowuje 'http' lub 'https'
    // ://                                - Dopasowuje dosłowne '://'
    // ([a-zA-Z0-9-]+.)*                  - Zero lub więcej subdomen (np. 'www.', 'blog.')
    // [a-zA-Z0-9-]+                      - Domena drugiego poziomu (np. 'example')
    // \.                                 - Dosłowna kropka jako separator
    // [a-zA-Z]{2,}                       - Domena najwyższego poziomu (TLD) z co najmniej 2 literami
    // (                                  - Początek opcjonalnej grupy ścieżki/zapytania/fragmentu
    //   /[a-zA-Z0-9\-\._~:/?#[\]@!\$&''\(\)\*\+,;=]*
    //                                    - Ścieżka zaczynająca się od '/', a następnie znaki bezpieczne dla URL
    // )*                                 - Zero lub więcej segmentów ścieżki
    // $                                  - Koniec ciągu
    //
    // Uwaga dotycząca literałów stringowych w Pascalu:
    // Pojedyncze cudzysłowy wewnątrz wyrażenia regularnego muszą być podwojone (np. ''')
    // aby były traktowane jako dosłowny pojedynczy cudzysłów w stringu Pascala.
    // Ukośniki wsteczne w wyrażeniu regularnym (np. \. dla dosłownej kropki)
    // nie wymagają podwojenia w TRegExpr w większości przypadków,
    // ale dla czytelności i bezpieczeństwa często się je podwaja w literałach stringowych.
    // W tym przypadku, dla uproszczenia i zgodności z typowym użyciem TRegExpr,
    // użyto pojedynczych ukośników wstecznych dla uciekania metaznaków.
    //Regex.Expression := '^(http|https)://([a-zA-Z0-9-]+\.)*[a-zA-Z0-9-]+\.[a-zA-Z]{2,}(/[a-zA-Z0-9\-\._~:/?#[\]@!\$&''\(\)\*\+,;=]*)*$';
    //RegEx.Expression := '^(?:https?:\/\/|www\.)[a-zA-Z0-9.\-]+\.[a-zA-Z]{2,}(/[a-zA-Z0-9\-\._~:/?#[\]%@!\$&''\(\)\*\+,;=]*)*$';
    RegEx.Expression := '^(?:https?:\/\/|www\.)[a-zA-Z0-9.\-]+\.[a-zA-Z]{2,}(?:\S*)?$';



    // Ustawienie ModifierI na True dla dopasowania niewrażliwego na wielkość liter.
    // Jest to ważne, ponieważ nazwy hostów w URL-ach są generalnie niewrażliwe na wielkość liter.
    // Chociaż ścieżki i zapytania mogą być wrażliwe na wielkość liter, dla ogólnego
    // "rozpoznawania linku" niewrażliwość na wielkość liter w początkowych częściach
    // sprawia, że funkcja jest bardziej elastyczna i przyjazna dla użytkownika.
    Regex.ModifierI := True;

    // 4. Wykonanie wyrażenia regularnego na ciągu wejściowym.
    // Metoda Exec zwraca True, jeśli wzorzec pasuje do całego ciągu, False w przeciwnym razie.
    Result := Regex.Exec(AInputString);
  finally
    // 5. Zawsze zwalniaj obiekt TRegExpr, aby zapobiec wyciekom pamięci.
    Regex.Free;
  end;
end;




end.

