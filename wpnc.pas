unit wpnc;

interface

type
  TVAPIDContentEncoding = (AESGCM, AES128GCM);

// Wrapper
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

const 
  CMD_MAX_SIZE = 4096;
var
  iwebpushVapidCmdC: TwebpushVapidCmdC;

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
	var retvalsize: Cardinal;
begin
  FillChar(retval, CMD_MAX_SIZE, 0);
  retvalsize:= webpushVapidCmdC(
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

function load(): TwebpushVapidCmdC;
var
  h:  THandle;
  r: TwebpushVapidCmdC;
begin  
  Result:= Nil;
  h:= LoadLibrary('wpn-c.dll');
  if h >= 32 then
  begin
    r:= GetProcAddress(h, 'webpushVapidCmdC');
  end;
  FreeLibrary(h);
  Result:= r;
end;

begin
  // iwebpushVapidCmdC:= load();
end.
