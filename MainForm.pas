unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  wpnc, FMX.ScrollBox, FMX.Memo, FMX.Edit;

type
  TFormMain = class(TForm)
    BCurl: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Memo1: TMemo;
    BGenerate: TButton;
    BCheckin: TButton;
    BRegister: TButton;
    BQR: TButton;
    procedure BCurlClick(Sender: TObject);
    procedure BGenerateClick(Sender: TObject);
    procedure BCheckinClick(Sender: TObject);
    procedure BRegisterClick(Sender: TObject);
    procedure BQRClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

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

procedure TFormMain.BQRClick(Sender: TObject);
begin
  //
  Memo1.Lines.Add(qr2string('123', 0));
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

end.
