{************************************************}
{  Простейший класс для работы с API Quik        }
{                                                }
{  Ginger, Иван                                  }
{  http://www.quik.ru/user/forum/import/24427/   }
{************************************************}

unit quik;

interface

uses  Windows, Classes, Sysutils, Messages, Controls,
      trans2quik_api;

const WM_InsertQueue = WM_USER + 1;

type  TOnStatusEvent = procedure (Sender: TObject; aEvent: longint; aExtendedErrorCode: Longint; const aMessage: string) of object;
      TOnReplyEvent  = procedure (Sender: TObject; aResult: longint; aExtendedErrorCode: Longint; aReplyCode: Longint; aTransactionId: DWORD; aOrderNum: double; const aMessage: string) of object;

type  TQuik = class(TWinControl)
      private
        fQuikPath      : String;
        fLastErrorMsg  : String;
        fLastErrorCode : Integer;
        fOnStatusEvent : TOnStatusEvent;
        fOnReplyEvent  : TOnReplyEvent;
        function    IS_DLL_CONNECTED: Boolean;
        function    IS_QUIK_CONNECTED: Boolean;
        procedure   WMInsertQueue(var Message: TWMSysCommand); message WM_InsertQueue;
      protected
        procedure   CreateHandle; override;
      public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        function    Connect: Boolean;
        function    Disconnect: Boolean;
        function    SendASyncTransaction (TransactionString: String): Integer;
        function    SendSyncTransaction (TransactionString: String; var ReplyCode: integer; var TransId: cardinal; var OrderNum: double; var ResultMessage: String) : integer;
        property    LastErrorMsg: String read fLastErrorMsg;
        property    LastErrorCode: Integer read fLastErrorCode;
        property    Connected: Boolean read IS_DLL_CONNECTED;
        property    QuikConnected: Boolean read IS_QUIK_CONNECTED;
      published
        property    QuikPath : String read fQuikPath write fQuikPath;
        property    OnStatus : TOnStatusEvent read fOnStatusEvent write fOnStatusEvent;
        property    OnReply  : TOnReplyEvent read fOnReplyEvent write fOnReplyEvent;
      end;

implementation

type  TReplyItemType  = (itmConnectionEvent, itmTransactionResult);

      PReplyQueueItem = ^TReplyQueueItem;
      TReplyQueueItem = record
        Message       : String;
        case ItemType : TReplyItemType of
          itmConnectionEvent   : (
                                   ConnectionEvent              : Longint;
                                   ConnectionExtendedErrorCode  : Longint;
                                 );
          itmTransactionResult : (
                                   TransactionResult            : Longint;
                                   TransactionExtendedErrorCode : Longint;
                                   TransactionReplyCode         : Longint;
                                   TransactionId                : DWORD;
                                   OrderNum                     : double;
                                 );
      end;

      TReplyQueue = class(tThreadList)
      end;

var   ReplyQueue      : TReplyQueue = nil;
      ComponentHandle : longint     = 0;

{ callback functions }

procedure ConnectionStatusCallback (nConnectionEvent: Longint; nExtendedErrorCode: Longint; lpcstrInfoMessage: LPCSTR); stdcall;
var itm : PReplyQueueItem;
begin
  if assigned(ReplyQueue) and (ComponentHandle <> 0) then begin
    itm:= new(PReplyQueueItem);
    with itm^ do begin
      ItemType:= itmConnectionEvent;
      if assigned(lpcstrInfoMessage) then SetString(Message, lpcstrInfoMessage, strlen(lpcstrInfoMessage))
                                     else SetLength(Message, 0);
      ConnectionEvent             := nConnectionEvent;
      ConnectionExtendedErrorCode := nExtendedErrorCode;
    end;
    with ReplyQueue.LockList do try
      Add(itm);
    finally ReplyQueue.UnlockList; end;
    if (ComponentHandle <> 0) then PostMessage(ComponentHandle, WM_InsertQueue, 0, 0);
  end;
end;

procedure TransReplyStatusCallback (nTransactionResult: Longint; nTransactionExtendedErrorCode: Longint; nTransactionReplyCode: Longint; dwTransId: DWORD; dOrderNum: double; lpcstrTransactionReplyMessage: LPCSTR); stdcall;
var itm : PReplyQueueItem;
begin
  if assigned(ReplyQueue) and (ComponentHandle <> 0) then begin
    itm:= new(PReplyQueueItem);
    with itm^ do begin
      ItemType:= itmTransactionResult;
      if assigned(lpcstrTransactionReplyMessage) then SetString(Message, lpcstrTransactionReplyMessage, strlen(lpcstrTransactionReplyMessage))
                                                 else SetLength(Message, 0);
      TransactionResult            := nTransactionResult;
      TransactionExtendedErrorCode := nTransactionExtendedErrorCode;
      TransactionReplyCode         := nTransactionReplyCode;
      TransactionId                := dwTransId;
      OrderNum                     := dOrderNum;
    end;
    with ReplyQueue.LockList do try
      Add(itm);
    finally ReplyQueue.UnlockList; end;
    if (ComponentHandle <> 0) then PostMessage(ComponentHandle, WM_InsertQueue, 0, 0);
  end;
end;

{ TQuik }

constructor TQuik.Create(AOwner: TComponent);
var errcode : longint;
    buf     : array[0..255] of char;
begin
  inherited;
  TRANS2QUIK_SET_CONNECTION_STATUS_CALLBACK (ConnectionStatusCallback, errcode, buf, sizeof (buf));
  TRANS2QUIK_SET_TRANSACTIONS_REPLY_CALLBACK (TransReplyStatusCallback, errcode, buf, sizeof (buf));
  SetLength(fQuikPath, 0);
end;

destructor TQuik.Destroy;
begin
  ComponentHandle:= 0;
  if Connected then Disconnect;
  inherited;
end;

procedure TQuik.CreateHandle;
begin
  inherited;
  ComponentHandle:= Handle;
end;

procedure TQuik.WMInsertQueue(var Message: TWMSysCommand);
var itm : PReplyQueueItem;
begin
  if assigned(ReplyQueue) then
    repeat
      itm:= nil;
      with ReplyQueue.LockList do try
        if (count > 0) then begin
          itm:= items[0];
          delete(0);
        end;
      finally ReplyQueue.UnlockList; end;
      if assigned(itm) then try
        with itm^ do begin
          case ItemType of
            itmConnectionEvent   : if assigned(fOnStatusEvent) then fOnStatusEvent(Self, ConnectionEvent, ConnectionExtendedErrorCode, Message);
            itmTransactionResult : if assigned(fOnReplyEvent) then fOnReplyEvent(Self, TransactionResult, TransactionExtendedErrorCode,
                                                                                 TransactionReplyCode, TransactionId, OrderNum, Message);
          end;
        end;
      finally dispose(itm); end;
    until not assigned(itm);
end;

function TQuik.Connect: Boolean;
var buf : Array [0..255] of char;
begin
  if not FileExists(fQuikPath+'\info.exe') then begin
    fLastErrorMsg := 'Не задан путь к info.exe';
    fLastErrorCode := TRANS2QUIK_QUIK_TERMINAL_NOT_FOUND;
    Result := false;
  end else begin
    FillChar(buf, SizeOf(buf), 0);
    Result := (TRANS2QUIK_CONNECT( PChar(fQuikPath), fLastErrorCode, buf, SizeOf(buf)) = TRANS2QUIK_SUCCESS);
    fLastErrorMsg := buf;
  end;
end;

function TQuik.Disconnect: Boolean;
var buf : Array [0..255] of char;
begin
  FillChar(buf, SizeOf(buf), 0);
  Result := (TRANS2QUIK_DISCONNECT(fLastErrorCode, buf, SizeOf(buf)) = TRANS2QUIK_SUCCESS);
  fLastErrorMsg := buf;
end;

function TQuik.IS_DLL_CONNECTED: Boolean;
var buf : Array [0..255] of char;
begin
  FillChar(buf, SizeOf(buf), 0);
  Result := (TRANS2QUIK_IS_DLL_CONNECTED (fLastErrorCode, buf, SizeOf(buf)) = TRANS2QUIK_DLL_CONNECTED);
  fLastErrorMsg := buf;
end;

function TQuik.IS_QUIK_CONNECTED: Boolean;
var buf : Array [0..255] of char;
begin
  FillChar(buf, SizeOf(buf), 0);
  Result := (TRANS2QUIK_IS_QUIK_CONNECTED (fLastErrorCode, buf, SizeOf(buf)) = TRANS2QUIK_QUIK_CONNECTED);
  fLastErrorMsg := buf;
end;

function TQuik.SendASyncTransaction (TransactionString: String): Integer;
var buf : Array [0..255] of char;
begin
  FillChar(buf, SizeOf(buf), 0);
  Result := TRANS2QUIK_SEND_ASYNC_TRANSACTION (pChar(TransactionString), fLastErrorCode, buf, sizeof (buf));
  fLastErrorMsg := buf;
end;

function TQuik.SendSyncTransaction (TransactionString: String;
                                    var ReplyCode: integer;
                                    var TransId: cardinal;
                                    var OrderNum: double;
                                    var ResultMessage: String) : integer;
var pResultMessage : array [0..1024] of Char;
    buf            : Array [0..255] of char;
begin
  FillChar(buf, SizeOf(buf), 0);
  FillChar(pResultMessage, SizeOf(pResultMessage), 0);
  Result := TRANS2QUIK_SEND_SYNC_TRANSACTION ( PChar(TransactionString),
                                               ReplyCode,
                                               TransId,
                                               OrderNum,
                                               pResultMessage,
                                               SizeOf(pResultMessage),
                                               fLastErrorCode, buf, sizeof (buf));
  fLastErrorMsg := buf;
  ResultMessage := pResultMessage;
end;

initialization
  ReplyQueue := TReplyQueue.Create;

finalization
  if assigned(ReplyQueue) then FreeAndNil(ReplyQueue);

end.
