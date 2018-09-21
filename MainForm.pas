unit MainForm;

interface

uses
  Wintypes, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  inifiles,
  wpnc, FMX.ScrollBox, FMX.Memo, FMX.Edit;

type
  TFormMain = class(TForm)
    BCurl: TButton;
    Memo1: TMemo;
    BGenerate: TButton;
    BCheckin: TButton;
    BRegister: TButton;
    BQR: TButton;
    BInit: TButton;
    BStart: TButton;
    BStop: TButton;
    procedure BCurlClick(Sender: TObject);
    procedure BGenerateClick(Sender: TObject);
    procedure BCheckinClick(Sender: TObject);
    procedure BRegisterClick(Sender: TObject);
    procedure BQRClick(Sender: TObject);
    procedure BInitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BStartClick(Sender: TObject);
    procedure BStopClick(Sender: TObject);
  private
    { Private declarations }
    pclient: PVoid;
    appId: AnsiString;
    registrationId: AnsiString;
    privateKey: AnsiString;
    publicKey: AnsiString;
    authSecret: AnsiString;
    androidId: UInt64;
    securityToken: UInt64;
    procedure load(const fn: AnsiString);
    procedure save(const fn: AnsiString);
  public
    { Public declarations }
  end;

const
  INI = 'wpn.ini';
  INI_SECTION_CLIENT = 'client';

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.BCheckinClick(Sender: TObject);
  var androidId: UInt64;
	var securityToken: UInt64;
begin
  androidId:= 0;
  securityToken:= 0;
  checkIn(androidId, securityToken);
  Memo1.Lines.Add(IntToStr(androidId));
  Memo1.Lines.Add(IntToStr(securityToken));
end;

procedure OnNotify(
  env: PVOID;
  const persistent_id: PAnsiChar;
  const from: PAnsiChar;
  const appName: PAnsiChar;
  const appId: PAnsiChar;
  sent: UInt64;
  const request: TNotifyMessageC
); cdecl;
begin
  TFormMain(env).Memo1.Lines.Add('notify');
end;

procedure OnLog(
  env: PVOID;
  severity: Integer;
  const msg: PAnsiChar
); cdecl;
begin
  TFormMain(env).Memo1.Lines.Add('log');
end;

procedure TFormMain.BStartClick(Sender: TObject);
var
  r: Integer;
begin
  //
  if pclient = Nil then
  begin
    pclient:= startClient(
      r,
      privateKey,
      authSecret,
      androidId,
      securityToken,
      @onNotify,
      Self,
      @onLog,
      Self,
      3
    );
    Memo1.Lines.Add('Started');
  end;
end;

procedure TFormMain.BStopClick(Sender: TObject);
begin
  if pclient <> Nil then
  begin
    stopClient(pclient);
    pclient:= Nil;
    Memo1.Lines.Add('Stopped');
  end;
end;

procedure TFormMain.BCurlClick(Sender: TObject);
var
  cmd: AnsiString;
	publicKey: AnsiString;
	privateKey: AnsiString;
	filename: AnsiString;
	endpoint: AnsiString;
	p256dh: AnsiString;
	auth: AnsiString;
	body: AnsiString;
	contact: AnsiString;
	contentEncoding: TVAPIDContentEncoding;
	expiration: Cardinal;
begin
	publicKey:= 'BM9Czc7rYYOinc7x_ALzqFgPSXV497qg76W6csYRtCFzjaFHGyuzP2a08l1vykEV1lgq6P83BOhB9xp-H5wCr1A';
	privateKey:= '_93Jy3cT0SRuUA1B9-D8X_zfszukGUMjIcO5y44rqCk';
	filename:= 'a.1';
	endpoint:= 'https://updates.push.services.mozilla.com/wpush/v2/gAAAAABbZ7cIJuyrIqApNuZd0AVjSSrYk5Cef5cI29-g8iRpHvFZzvqO6bI0ymUcf1tJpvg0lCIF7GxAbU7yg7EMXUh6c4MKaFPsSEsLzC7Mlb1JyIAMz5Wf0orVg15A2OD9dBCCUwbol78DdinNpwz-ExA67dH7InfiUDeYZS6QmVNXaPhzpGo';
	p256dh:= 'BBpYsgvCmjRZTlwQ__nWoeaLwuqxVc9Eg-GSloPxQdvVxapVybJKJMns8IMkYQUDiLBrnXp-qFugkPBq3fOncvY';
	auth:= '4SgZbJVmKUP56tJ39wcWPw';
	body:= 'body';
	contact:= 'andrei.i.ivanov@gmail.com';
	contentEncoding:= AES128GCM;
	expiration:= 0;

  cmd:= webpushVapidCmd(
    publicKey,
    privateKey,
    filename,
    endpoint,
    p256dh,
    auth,
    body,
    contact,
    contentEncoding,
    expiration);
  Memo1.Lines.Add(cmd);
end;

procedure TFormMain.BGenerateClick(Sender: TObject);
var
  privateKey: AnsiString;
  publicKey: AnsiString;
  authSecret: AnsiString;
begin
  generateVAPIDKeys(privateKey, publicKey, authSecret);
  Memo1.Lines.Add(privateKey);
  Memo1.Lines.Add(publicKey);
  Memo1.Lines.Add(authSecret);
end;

procedure TFormMain.BInitClick(Sender: TObject);
var
  guid: TGUID;
begin
  CreateGUID(guid);
  appId:= GUIDToString(guid);
  initClient(registrationId, privateKey, publicKey, authSecret, androidId, securityToken, appId);
  Memo1.Lines.Add(appId);
  Memo1.Lines.Add(registrationId);
  Memo1.Lines.Add(privateKey);
  Memo1.Lines.Add(publicKey);
  Memo1.Lines.Add(authSecret);
  Memo1.Lines.Add(IntToStr(androidId));
  Memo1.Lines.Add(IntToStr(securityToken));
  save(INI);
end;

procedure TFormMain.BQRClick(Sender: TObject);
var
  s: AnsiString;
begin
  //
  s:= qr2string('123', 0, UTF8Encode('██'), '  ');
  Memo1.Lines.Add(UTF8Decode(s));
end;

procedure TFormMain.BRegisterClick(Sender: TObject);
var
  r: Integer;
  token: AnsiString;
  androidId: UInt64;
  securityToken: UInt64;
  appId: AnsiString;
begin
  androidId:= 1;
  securityToken:= 2;
  appId:= '3';
  r:= registerDevice(token, androidId, securityToken, appId);
  Memo1.Lines.Add(intToStr(r));
  Memo1.Lines.Add(token);
end;

procedure TFormMain.load(const fn: AnsiString);
var
  f: TIniFile;
  s: AnsiString;
begin
  f:= TIniFile.Create(GetHomePath + fn);
  appId:= f.ReadString(INI_SECTION_CLIENT, 'appId', '');
  registrationId:= f.ReadString(INI_SECTION_CLIENT, 'registrationId', '');
  privateKey:= f.ReadString(INI_SECTION_CLIENT, 'privateKey', '');
  publicKey:= f.ReadString(INI_SECTION_CLIENT, 'publicKey', '');
  authSecret:= f.ReadString(INI_SECTION_CLIENT, 'authSecret', '');
  s:= f.ReadString(INI_SECTION_CLIENT, 'androidId', '');
  if Length(s) > 0 then begin
    androidId:= Uint64.Parse(s);
  end else begin
    androidId:= 0;
  end;

  s:= f.ReadString(INI_SECTION_CLIENT, 'securityToken', '');
  if Length(s) > 0 then begin
    securityToken:= Uint64.Parse(s);
  end else begin
    securityToken:= 0;
  end;

  FreeAndNil(f);
end;

procedure TFormMain.save(const fn: AnsiString);
var
  f: TIniFile;
begin
  f:= TIniFile.Create(GetHomePath + fn);
  f.WriteString(INI_SECTION_CLIENT, 'appId', appId);
  f.WriteString(INI_SECTION_CLIENT, 'registrationId', registrationId);
  f.WriteString(INI_SECTION_CLIENT, 'privateKey', privateKey);
  f.WriteString(INI_SECTION_CLIENT, 'publicKey', publicKey);
  f.WriteString(INI_SECTION_CLIENT, 'authSecret', authSecret);
  f.WriteString(INI_SECTION_CLIENT, 'androidId', Uint64.ToString(androidId));
  f.WriteString(INI_SECTION_CLIENT, 'securityToken', Uint64.ToString(securityToken));
  FreeAndNil(f);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  load(INI);
  Memo1.Lines.Add(appId);
  Memo1.Lines.Add(registrationId);
  Memo1.Lines.Add(privateKey);
  Memo1.Lines.Add(publicKey);
  Memo1.Lines.Add(authSecret);
  Memo1.Lines.Add(IntToStr(androidId));
  Memo1.Lines.Add(IntToStr(securityToken));
end;

end.
