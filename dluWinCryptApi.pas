unit dluWinCryptApi;

// Raw declarations of Windows CryptoAPI (types, constants, imports from crypt32.dll).
// This unit provides a direct mapping of WinCrypt.h for use when reading certificate
// information from PKCS#7 signatures. No application logic is included.

{$mode objfpc}{$H+}

interface

uses Windows
   ;

const
   // Encoding flags used by CryptoAPI for certificates and PKCS#7 messages.
   PKCS_7_ASN_ENCODING = $00010000;
   X509_ASN_ENCODING   = $00000001;

   // Combined encoding type commonly used when working with PKCS#7 signed data.
   MY_ENCODING_TYPE     = X509_ASN_ENCODING or PKCS_7_ASN_ENCODING;

const
   // Certificate name string types used by CertGetNameStringW.
   CERT_NAME_SIMPLE_DISPLAY_TYPE = 4;   // Human-readable display name
   CERT_NAME_ATTR_TYPE           = 3;   // Specific RDN attribute (OID)
   CERT_NAME_EMAIL_TYPE          = 1;   // Email address field
   CERT_NAME_ISSUER_FLAG         = $1;  // Select issuer instead of subject
   CERT_HASH_PROP_ID             = 3;   // SHA-1 hash property of certificate

const
   // OIDs for selected RDN attributes used with CERT_NAME_ATTR_TYPE.
   szOID_ORGANIZATION_NAME       : PAnsiChar = '2.5.4.10';   // Organization (O)
   szOID_COUNTRY_NAME            : PAnsiChar = '2.5.4.6';    // Country (C)
   szOID_SERIAL_NUMBER           : PAnsiChar = '2.5.4.5';    // New serial number (tu: nośnik PESEL)
   szOID_ORGANIZATION_IDENTIFIER : PAnsiChar = '2.5.4.97';   // NIP/REGON wystawcy lub podmiotu (ETSI EN 319 412-1)
const
   // DLL providing CryptoAPI functions.
   CRYPT32_DLL = 'crypt32.dll';

// Generic binary blob used throughout CryptoAPI.
// cbData = size of the buffer in bytes
// pbData = pointer to raw byte buffer
type _CRYPTOAPI_BLOB = record
   cbData : DWORD;
   pbData : PByte;
end;

type
   // Handle to a cryptographic message (PKCS#7, CMS, etc.)
   HCRYPTMSG = Pointer;

   // Aliases used by CryptoAPI for different blob types.
   CRYPT_INTEGER_BLOB = _CRYPTOAPI_BLOB;
   CERT_NAME_BLOB     = _CRYPTOAPI_BLOB;
   CRYPT_OBJID_BLOB   = _CRYPTOAPI_BLOB;

   // Algorithm identifier structure.
   // pszObjId   = OID string of the algorithm
   // Parameters = optional algorithm parameters (ASN.1 encoded)
   CRYPT_ALGORITHM_IDENTIFIER = record
      pszObjId   : PAnsiChar;
      Parameters : CRYPT_OBJID_BLOB;
   end;

   // Attributes attached to a cryptographic message.
   // rgAttr is a pointer to an array of CRYPT_ATTRIBUTE structures.
   CRYPT_ATTRIBUTES = record
      cAttr  : DWORD;
      rgAttr : Pointer;   // PCRYPT_ATTRIBUTE (not used directly here)
   end;

   // Signer information extracted from a PKCS#7 message.
   PCMSG_SIGNER_INFO = ^CMSG_SIGNER_INFO;
   CMSG_SIGNER_INFO = record
      dwVersion               : DWORD;
      Issuer                  : CERT_NAME_BLOB;
      SerialNumber            : CRYPT_INTEGER_BLOB;
      HashAlgorithm           : CRYPT_ALGORITHM_IDENTIFIER;
      HashEncryptionAlgorithm : CRYPT_ALGORITHM_IDENTIFIER;
      EncryptedHash           : CRYPT_INTEGER_BLOB;
      AuthAttrs               : CRYPT_ATTRIBUTES;
      UnauthAttrs             : CRYPT_ATTRIBUTES;
   end;

type CERT_INFO = record
      dwVersion            : DWORD;
      SerialNumber         : CRYPT_INTEGER_BLOB;
      SignatureAlgorithm   : CRYPT_ALGORITHM_IDENTIFIER;
      Issuer               : CERT_NAME_BLOB;

      NotBefore            : FILETIME;
      NotAfter             : FILETIME;

      Subject              : CERT_NAME_BLOB;
      SubjectPublicKeyInfo : Pointer;   // PCERT_PUBLIC_KEY_INFO
      IssuerUniqueId       : Pointer;
      SubjectUniqueId      : Pointer;
      cExtension           : DWORD;
      rgExtension          : Pointer;   // PCERT_EXTENSION
   end;
   PCERT_INFO = ^CERT_INFO;

type
   // Certificate store handle.
   HCERTSTORE  = Pointer;
   PHCERTSTORE = ^HCERTSTORE;

type CERT_CONTEXT = record
      dwCertEncodingType : DWORD;
      pbCertEncoded      : PByte;
      cbCertEncoded      : DWORD;
      pCertInfo          : PCERT_INFO;
      hCertStore         : HCERTSTORE;
   end;
   PCCERT_CONTEXT = ^CERT_CONTEXT;

// Retrieves a parameter from a cryptographic message (PKCS#7).
function CryptMsgGetParam(
                     hCryptMsg    : HCRYPTMSG;
                     dwParamType  : DWORD;
                     dwIndex      : DWORD;
                     pvData       : Pointer;
                     pcbData      : PDWORD
         ): BOOL; stdcall; external CRYPT32_DLL Name 'CryptMsgGetParam';

// Closes a cryptographic message handle.
function CryptMsgClose(
                     hCryptMsg    : HCRYPTMSG
         ): BOOL; stdcall; external CRYPT32_DLL Name 'CryptMsgClose';

// Parses an object (file, memory buffer, certificate, PKCS#7 message).
function CryptQueryObject(
                     dwObjectType              : DWORD;
                     pvObject                  : Pointer;
                     dwExpectedContentTypeFlags: DWORD;
                     dwExpectedFormatTypeFlags : DWORD;
                     dwFlags                   : DWORD;
                     pdwMsgAndCertEncodingType : PDWORD;
                     pdwContentType            : PDWORD;
                     pdwFormatType             : PDWORD;
                     phCertStore               : PHCERTSTORE;
                     phMsg                     : Pointer;
                     ppvContext                : Pointer
         ): BOOL; stdcall; external CRYPT32_DLL Name 'CryptQueryObject';

// Searches for a certificate in a certificate store.
function CertFindCertificateInStore(
                     hCertStore                : HCERTSTORE;
                     dwCertEncodingType        : DWORD;
                     dwFindFlags               : DWORD;
                     dwFindType                : DWORD;
                     pvFindPara                : Pointer;
                     pPrevCertContext          : PCCERT_CONTEXT
         ): PCCERT_CONTEXT; stdcall; external CRYPT32_DLL Name 'CertFindCertificateInStore';

// Retrieves a property of a certificate (e.g., hash, key provider info).
function CertGetCertificateContextProperty(
                     pCertContext : PCCERT_CONTEXT;
                     dwPropId     : DWORD;
                     pvData       : Pointer;
                     var pcbData  : DWORD
         ): BOOL; stdcall; external CRYPT32_DLL Name 'CertGetCertificateContextProperty';

// Retrieves a certificate name string (subject or issuer).
function CertGetNameStringW(
                     pCertContext: PCCERT_CONTEXT;
                     dwType: DWORD;
                     dwFlags: DWORD;
                     pvTypePara: Pointer;
                     pszNameString: PWideChar;
                     cchNameString: DWORD
         ): DWORD; stdcall; external CRYPT32_DLL Name 'CertGetNameStringW';

// Frees a certificate context.
function CertFreeCertificateContext(
                     pCertContext: PCCERT_CONTEXT
         ): BOOL; stdcall; external CRYPT32_DLL Name 'CertFreeCertificateContext';

// Closes a certificate store.
function CertCloseStore(
                     hCertStore: HCERTSTORE;
                     dwFlags: DWORD
        ): BOOL; stdcall; external CRYPT32_DLL Name 'CertCloseStore';

implementation

end.

