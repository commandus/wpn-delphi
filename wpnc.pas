unit wpnc;

interface

type
  TVAPIDContentEncoding = (AESGCM, AES128GCM);

// Wrappers

function webpushVapidCmd(
	const publicKey: AnsiString;
	const privateKey: AnsiString;
	const filename: AnsiString;
	const endpoint: AnsiString;
	const p256dh: AnsiString;
	const auth: AnsiString;
	const body: AnsiString;
	const contact: AnsiString;
	contentEncoding: TVAPIDContentEncoding;
	expiration: Cardinal
): AnsiString;

procedure generateVAPIDKeys(
  var privateKey: AnsiString;
  var publicKey: AnsiString;
  var authSecret: AnsiString
);

function checkIn(
  var androidId: UInt64;
	var securityToken: UInt64
): Integer;

implementation

uses
  WinTypes, WinProcs;

type
  TwebpushVapidCmdC = function(
    retval: PAnsiChar;
    retvalsize: Cardinal;
    const publicKey: PAnsiChar;
    const privateKey: PAnsiChar;
    const filename: PAnsiChar;
    const endpoint: PAnsiChar;
    const p256dh: PAnsiChar;
    const auth: PAnsiChar;
    const body: PAnsiChar;
    const contact: PAnsiChar;
    contentEncoding: Integer;
    expiration: Cardinal
): Cardinal; cdecl;

  TgenerateVAPIDKeysC = procedure
  (
    privateKey: PAnsiChar;
    privateKeySize: Cardinal;
    publicKey: PAnsiChar;
    publicKeySize: Cardinal;
    authSecret: PAnsiChar;
    authSecretSize: Cardinal
  ); cdecl;

  TcheckInC = function (
    androidId: PUInt64;
  	securityToken: PUInt64;
    verbosity: Integer
  ): Integer; cdecl;

const
  CMD_MAX_SIZE = 4096;
var
  iwebpushVapidCmdC: TwebpushVapidCmdC;
  igenerateVAPIDKeysC: TgenerateVAPIDKeysC;
  icheckInC: TcheckInC;

function webpushVapidCmdC(
  retval: PAnsiChar;
	retvalsize: Cardinal;
	const publicKey: PAnsiChar;
	const privateKey: PAnsiChar;
	const filename: PAnsiChar;
	const endpoint: PAnsiChar;
	const p256dh: PAnsiChar;
	const auth: PAnsiChar;
	const body: PAnsiChar;
	const contact: PAnsiChar;
	contentEncoding: Integer;
	expiration: Cardinal
): Cardinal; cdecl; external 'wpn-c' name 'webpushVapidCmdC';

procedure generateVAPIDKeysC(
  privateKey: PAnsiChar;
  privateKeySize: Cardinal;
  publicKey: PAnsiChar;
  publicKeySize: Cardinal;
  authSecret: PAnsiChar;
  authSecretSize: Cardinal
); cdecl; external 'wpn-c' name 'generateVAPIDKeysC';

function checkInC (
  androidId: PUInt64;
	securityToken: PUInt64;
  verbosity: Integer
): Integer; cdecl; external 'wpn-c' name 'checkInC';

function webpushVapidCmd(
	const publicKey: AnsiString;
	const privateKey: AnsiString;
	const filename: AnsiString;
	const endpoint: AnsiString;
	const p256dh: AnsiString;
	const auth: AnsiString;
	const body: AnsiString;
	const contact: AnsiString;
	contentEncoding: TVAPIDContentEncoding;
	expiration: Cardinal
): AnsiString;
  var retval: array[0..CMD_MAX_SIZE - 1] of AnsiChar;
begin
  FillChar(retval, CMD_MAX_SIZE, 0);
  webpushVapidCmdC(
    @retval[0],
    CMD_MAX_SIZE,
    PAnsiChar(publicKey),
    PAnsiChar(privateKey),
    PAnsiChar(filename),
    PAnsiChar(endpoint),
    PAnsiChar(p256dh),
    PAnsiChar(auth),
    PAnsiChar(body),
    PAnsiChar(contact),
    Ord(contentEncoding),
  	expiration
  );
  Result:= AnsiString(retval);
end;

procedure generateVAPIDKeys(
  var privateKey: AnsiString;
  var publicKey: AnsiString;
  var authSecret: AnsiString
);
var
  privateKeyA: array[0..240] of AnsiChar;
  publicKeyA: array[0..96] of AnsiChar;
  authSecretA: array[0..48] of AnsiChar;
begin
  generateVAPIDKeysC(
    privateKeyA,
    240,
    publicKeyA,
    96,
    authSecretA,
    48
  );
  privateKey:= PAnsiChar(@privateKeyA);
  publicKey:= PAnsiChar(@publicKeyA);
  authSecret:= PAnsiChar(@authSecretA);
end;

function load(): Boolean;
var
  h:  THandle;
  r: TwebpushVapidCmdC;
begin
  Result:= false;
  h:= LoadLibrary('wpn-c.dll');
  if h >= 32 then
  begin
    iwebpushVapidCmdC:= GetProcAddress(h, 'webpushVapidCmdC');
    igenerateVAPIDKeysC:= GetProcAddress(h, 'generateVAPIDKeysC');
    icheckInC:= GetProcAddress(h, 'checkInC');
    Result:= true;
  end;
  FreeLibrary(h);
end;

function checkIn(
  var androidId: UInt64;
	var securityToken: UInt64
): Integer;
begin
  Result:= checkInC(@androidId, @securityToken, 0);
end;

begin
  // load();
end.
