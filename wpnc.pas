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

function registerDevice(
  var retGCMToken: AnsiString;
  androidId: UInt64;
  securityToken: UInt64;
  const appId: AnsiString
): Integer;

function initClient(
  var privateKey: AnsiString;
  var publicKey: AnsiString;
  var authSecret: AnsiString;
  var androidId: UInt64;
  var securityToken: UInt64
): Integer;

function qr2string(
    const value: AnsiString;
    const mode: Integer;
    const foregroundSymbols: AnsiString;
    const backgroundSymbols: AnsiString
): AnsiString;

implementation

uses
  WinTypes, WinProcs;

const DLL_NAME: AnsiString = 'wpn-c.dll';
const LIB = 'wpn-c';

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

  TcheckInC = function(
    androidId: PUInt64;
  	securityToken: PUInt64;
    verbosity: Integer
  ): Integer; cdecl;

  TregisterDeviceC = function(
    retGCMToken: PAnsiChar;
    GCMTokenSize: Cardinal;
    androidId: UInt64;
    securityToken: UInt64;
    appId: PAnsiChar;
    verbosity:  Integer
  ): Integer; cdecl;

  TinitClient = function(
    privateKey: PAnsiChar;
    privateKeySize: Cardinal;
    publicKey: PAnsiChar;
    publicKeySize: Cardinal;
    authSecret: PAnsiChar;
    authSecretSize: Cardinal;
    androidId: PUInt64;
    securityToken: PUInt64;
    verbosity: Integer
  ): Integer; cdecl;
  {
  Return QR lines using two pseudographics symbols full block (\u2588\u2588).
  If retval is NULL, return required size
  @param retval return buffer. Can be NULL
  @param retsize return buffer size
  @param value string to conversion
  @param mode 0- pseudo graphics, 1- pseudo graphics inverted
  }
  Tqr2pchar = function(
    retval: PAnsiChar;
    retsize: Cardinal;
    const value: PAnsiChar;
    const mode: Integer;
    const foregroundSymbols: PAnsiChar;
    const backgroundSymbols: PAnsiChar
  ): Cardinal; cdecl;

  TNotifyMessageC = record
  	authorizedEntity: PAnsiChar;	///< e.g. 246829423295
    title: PAnsiChar;
    body: PAnsiChar;
    icon: PAnsiChar;				      ///< Specifies an icon filename or stock icon to display.
    sound: PAnsiChar;				      ///< sound file name
    link: PAnsiChar;				      ///< click action
    linkType: PAnsiChar;			    ///< click action content type
    urgency: Integer; 					  ///< low- 0, normal, critical
    timeout: Integer; 					  ///< timeout in milliseconds at which to expire the notification.
    category: PAnsiChar;
    extra: PAnsiChar;
    data: PAnsiChar;				      ///< extra data in JSON format
  end;

  TOnNotifyC = procedure(
  	env: PVOID;
    const persistent_id: PAnsiChar;
    const from: PAnsiChar;
    const appName: PAnsiChar;
    const appId: PAnsiChar;
    sent: UInt64;
    const request: TNotifyMessageC
  );

  TOnLogC = procedure(
  	env: PVOID;
  	severity: Integer;
	  const msg: PAnsiChar
  );

  Tclient = procedure(
    const privateKey: PAnsiChar;
    const authSecret: PAnsiChar;
    androidId: UInt64;
    securityToken: UInt64;
    onNotify: TOnNotifyC;
    onNotifyEnv: PVOID;
    onLog: TOnLogC;
    onLogEnv: PVOID;
    verbosity: Integer
);

const
  CMD_MAX_SIZE = 4096;
var
  iwebpushVapidCmdC: TwebpushVapidCmdC;
  igenerateVAPIDKeysC: TgenerateVAPIDKeysC;
  icheckInC: TcheckInC;
  iregisterDeviceC: TregisterDeviceC;
  iqr2pcharC: Tqr2pchar;
  iclient: Tclient;
  iinitClient: Tinitclient;

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
): Cardinal; cdecl; external LIB name 'webpushVapidCmdC';

procedure generateVAPIDKeysC(
  privateKey: PAnsiChar;
  privateKeySize: Cardinal;
  publicKey: PAnsiChar;
  publicKeySize: Cardinal;
  authSecret: PAnsiChar;
  authSecretSize: Cardinal
); cdecl; external LIB name 'generateVAPIDKeysC';

function checkInC (
  androidId: PUInt64;
	securityToken: PUInt64;
  verbosity: Integer
): Integer; cdecl; external LIB name 'checkInC';

 // Register device and obtain GCM token
function registerDeviceC(
	retGCMToken: PAnsiChar;
	GCMTokenSize: Cardinal;
	androidId: UInt64;
	securityToken: UInt64;
	appId: PAnsiChar;
	verbosity:  Integer
): Integer; cdecl; external LIB name 'registerDeviceC';

function initClientC(
  privateKey: PAnsiChar;
  privateKeySize: Cardinal;
  publicKey: PAnsiChar;
  publicKeySize: Cardinal;
  authSecret: PAnsiChar;
  authSecretSize: Cardinal;
  androidId: PUInt64;
  securityToken: PUInt64;
  verbosity: Integer
): Integer; cdecl; external LIB name 'initClientC';

function qr2pchar(
  retval: PAnsiChar;
  retsize: Cardinal;
  const value: PAnsiChar;
  const mode: Integer;
  const foregroundSymbols: PAnsiChar;
  const backgroundSymbols: PAnsiChar
): Cardinal; cdecl; external LIB name 'qr2pchar';

procedure client(
  const privateKey: PAnsiChar;
  const authSecret: PAnsiChar;
  androidId: UInt64;
  securityToken: UInt64;
  onNotify: TOnNotifyC;
  onNotifyEnv: PVOID;
  onLog: TOnLogC;
  onLogEnv: PVOID;
  verbosity: Integer
); cdecl; external LIB name 'client';

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
    privateKeyA, 240,
    publicKeyA, 96,
    authSecretA, 48
  );
  privateKey:= PAnsiChar(@privateKeyA);
  publicKey:= PAnsiChar(@publicKeyA);
  authSecret:= PAnsiChar(@authSecretA);
end;

function load(): Boolean;
var
  h:  THandle;
begin
  Result:= false;
  h:= LoadLibraryA(PAnsiChar(DLL_NAME));
  if h >= 32 then
  begin
    iwebpushVapidCmdC:= GetProcAddress(h, 'webpushVapidCmdC');
    igenerateVAPIDKeysC:= GetProcAddress(h, 'generateVAPIDKeysC');
    icheckInC:= GetProcAddress(h, 'checkInC');
    iinitclient:= GetProcAddress(h, 'initClient');
    iregisterDeviceC:= GetProcAddress(h, 'registerDeviceC');
    iqr2pcharC:= GetProcAddress(h, 'qr2pchar');
    iclient:= GetProcAddress(h, 'client');
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

function initClient(
  var privateKey: AnsiString;
  var publicKey: AnsiString;
  var authSecret: AnsiString;
  var androidId: UInt64;
  var securityToken: UInt64
): Integer;
var
  privateKeyA: array[0..240] of AnsiChar;
  publicKeyA: array[0..96] of AnsiChar;
  authSecretA: array[0..48] of AnsiChar;
begin
  Result:= initClientC(
    privateKeyA, 240,
    publicKeyA, 96,
    authSecretA, 48,
    @androidId, @securityToken, 0
  );
  privateKey:= PAnsiChar(@privateKeyA);
  publicKey:= PAnsiChar(@publicKeyA);
  authSecret:= PAnsiChar(@authSecretA);
end;

function registerDevice(
  var retGCMToken: AnsiString;
  androidId: UInt64;
  securityToken: UInt64;
  const appId: AnsiString
): Integer;
var
  token: array[0..255] of AnsiChar;
begin
  Result:= registerDeviceC(token, 255, androidId, securityToken, PAnsiChar(appId), 0);
  retGCMToken:= PAnsiChar(@token);
end;

function qr2string(
  const value: AnsiString;
  const mode: Integer;
  const foregroundSymbols: AnsiString;
  const backgroundSymbols: AnsiString
): AnsiString;
var
  sz: Integer;
begin
  sz:= qr2pchar(Nil, 0, PAnsiChar(value), mode,
    PAnsiChar(foregroundSymbols), PAnsiChar(backgroundSymbols));
  if (sz > 0) then begin
     SetLength(Result, sz);
     qr2pchar(PAnsiChar(@Result[1]), sz, PAnsiChar(value), mode,
       PAnsiChar(foregroundSymbols), PAnsiChar(backgroundSymbols));
  end
  else Result:= '';;
end;

begin
  // load();
end.
