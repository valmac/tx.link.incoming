{************************************************}
{  Тест API Quik                                 }
{  Ginger, Иван                                  }
{  http://www.quik.ru/user/forum/import/24427/   }
{  Версия Quik должна быть не ниже 5.10          }
{  Не забудьте включить поддержку API в Quik:    }
{  Торговля/Внешние транзакции/Начать обработку  }
{************************************************}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, trans2quik_api, StdCtrls, Quik;

type

  TForm1 = class(TForm)
    CBShowMsg: TCheckBox;
    Label1: TLabel;
    QUIKTerminalPathEdit: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Label2: TLabel;
    Memo1: TMemo;
    procedure OnConnect(Sender: TObject);
    procedure OnIsDllConnected(Sender: TObject);
    procedure onDisconnectClick(Sender: TObject);
    procedure onIsQuikConnectedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OnAsyncClick(Sender: TObject);
    procedure OnSyncClick(Sender: TObject);

    procedure OnStatusChange(Sender: TObject; aEvent: longint; aExtendedErrorCode: Longint; const aMessage: string);
    procedure OnReplyEvent(Sender: TObject; aResult: longint; aExtendedErrorCode: Longint; aReplyCode: Longint; aTransactionId: DWORD; aOrderNum: double; const aMessage: string);
  private
    { Private declarations }
    m_nResult: Integer;
  public
    { Public declarations }
    Quik: TQuik;
  end;

function AskSpr(const fmt: string; const Param: array of const): Boolean;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Quik := TQuik.Create(Self);
  Quik.OnReply:= OnReplyEvent;
  Quik.OnStatus:= OnStatusChange;
  Quik.Parent:= Self;
end;

procedure TForm1.OnStatusChange(Sender: TObject; aEvent,
  aExtendedErrorCode: Integer; const aMessage: string);
begin
  case aEvent of
    TRANS2QUIK_DLL_CONNECTED     : askspr ('Callback: DLL is connected',[]);
    TRANS2QUIK_DLL_DISCONNECTED  : askspr ('Callback: DLL is disconnected: %s', [aMessage]);
    TRANS2QUIK_QUIK_CONNECTED    : askspr ('Callback: QUIK is connected', []);
    TRANS2QUIK_QUIK_DISCONNECTED : askspr ('Callback: QUIK is disconnected: %s', [aMessage]);
  end;
end;

procedure TForm1.OnReplyEvent(Sender: TObject; aResult, aExtendedErrorCode,
  aReplyCode: Integer; aTransactionId: DWORD; aOrderNum: double;
  const aMessage: string);
begin
  askspr('TransReplyStatusCallback: nTransactionResult %d', [aResult]);
end;

procedure TForm1.OnConnect(Sender: TObject);
begin
  Quik.QuikPath := QUIKTerminalPathEdit.Text;
  if (not Quik.Connect) then AskSpr('Error code: %d'#$0d#$0a'Result code: %d'#$0d#$0a'Message: %s', [Quik.LastErrorCode, m_nResult, Quik.LastErrorMsg])
	                else if CBShowMsg.Checked then askspr('DLL is connected to QUIK terminal', []);
end;

procedure TForm1.onDisconnectClick(Sender: TObject);
begin
  if (not Quik.Disconnect) then
    AskSpr('Error code: %d'#$0d#$0a'Message: %s'#$0d#$0a'Result %d', [Quik.LastErrorCode, Quik.LastErrorMsg, m_nResult])
  else
    if CBShowMsg.Checked then askspr('DLL is disconnected to QUIK terminal', []);
end;

procedure TForm1.OnIsDllConnected(Sender: TObject);
begin
  if (Quik.Connected) then askspr('DLL is connected to QUIK terminal', [])
	              else AskSpr('Error code: %d'#$0d#$0a'Message: %s', [Quik.LastErrorCode, Quik.LastErrorMsg])
end;

procedure TForm1.onIsQuikConnectedClick(Sender: TObject);
begin
  if (Quik.QuikConnected) then askspr('QUIK is connected', [])
                          else AskSpr('Error code: %d'#$0d#$0a'Message: %s', [Quik.LastErrorCode, Quik.LastErrorMsg])
end;

procedure TForm1.OnAsyncClick(Sender: TObject);
var TransStr: String;
begin
  TransStr := StringReplace(Memo1.Text, #13, ' ', [rfReplaceAll]);
  TransStr := StringReplace(TransStr,   #10, ' ', [rfReplaceAll]);
  if ( Quik.SendASyncTransaction( TransStr ) <> TRANS2QUIK_SUCCESS ) then
    AskSpr('Error code: %d'#$0d#$0a'Message: %s', [Quik.LastErrorCode, Quik.LastErrorMsg])
  else
    if CBShowMsg.Checked then askspr('OK SendASyncTransaction', []);
end;

procedure TForm1.OnSyncClick(Sender: TObject);
var TransStr: String;
    ReplyCode: Integer; TransId: cardinal; ReturnCode: Integer;
    OrderNum: double;
    ResultMessage: string;
begin
  TransStr := StringReplace(Memo1.Text, #13, ' ', [rfReplaceAll]);
  TransStr := StringReplace(TransStr,   #10, ' ', [rfReplaceAll]);
  ReturnCode :=  Quik.SendSyncTransaction( TransStr, ReplyCode, TransId, OrderNum, ResultMessage);
  if ( ReturnCode <> TRANS2QUIK_SUCCESS ) then
    AskSpr('Error code: %d'#$0d#$0a'Message: %s', [Quik.LastErrorCode, Quik.LastErrorMsg])
  else
    AskSpr('Sync transaction result: %d'#$0d#$0a'Message: %s'#$0d#$0a'ReplyCode: %d'#$0d#$0a'TransId: %d'#$0d#$0a'OrderNum: %.0f'#$0d#$0a'ResultMessage: %s',
           [Quik.LastErrorCode, Quik.LastErrorMsg, ReplyCode, TransId, OrderNum, ResultMessage])
end;

function AskSpr(const fmt: string; const Param: array of const): Boolean;
begin
  result := (MessageDlg(format(fmt, param), mtInformation, mbOKCancel,0) = mrOk);
end;


end.
