unit p2_baseless_VCL_example_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, AppEvnts, StdCtrls, Controls,
  ActiveX, OleServer, ComObj, P2ClientGate_TLB;

// класс для получения времени с точностью до милисекунд
type
  TPreciseTime = class(TComponent)
  private
    fTime: tDateTime;
    fStart: int64;
    fFreq: int64;
  public
    constructor Create(AOwner: TComponent); override;
    function    Now: TDateTime;
    function    Msecs: longint;
  end;

// главная форма приложения
type
  TForm1 = class(TForm)
    LogListBox: TListBox;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
  private
    fPreciseTime: TPreciseTime;
    fApp: TCP2Application;
    fConn: TCP2Connection;
    fStream: TCP2DataStream;

    procedure ProcessPlaza2Messages(Sender: TObject; var Done: Boolean);

    procedure ConnectionStatusChanged(Sender: TObject; var conn: OleVariant; newStatus: TConnectionStatus);

    procedure StreamStateChanged(Sender: TObject; var stream: OleVariant; newState: TDataStreamState);
    procedure StreamLifeNumChanged(Sender: TObject; var stream: OleVariant; LifeNum: Integer);

    procedure StreamDataBegin(Sender: TObject; var stream: OleVariant);
    procedure StreamDataInserted(Sender: TObject; var stream: OleVariant; var tableName: OleVariant; var rec: OleVariant);
    procedure StreamDataEnd(Sender: TObject; var stream: OleVariant);
  public
    function CheckAndReopen(AStream: TCP2DataStream): boolean;
    procedure log(const alogstr: string); overload;
    procedure log(const alogstr: string; const aparams: array of const); overload;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{ TPreciseTime }

constructor TPreciseTime.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  QueryPerformanceFrequency(fFreq);
  FTime:= SysUtils.now;
  QueryPerformanceCounter(fStart);
end;

function TPreciseTime.Now: TDateTime;
var fEnd : int64;
begin
  QueryPerformanceCounter(fEnd);
  result:= fTime + (((fEnd - fStart) * 1000) div fFreq) / 86400000.0;
end;

function TPreciseTime.Msecs: longint;
var fEnd : int64;
begin
  QueryPerformanceCounter(fEnd);
  result:= (fEnd * 1000) div fFreq;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // устанавливаем обработчик события Application.OnIdle, в нем мы будем запускать
  // обработку сообщений plaza2
  Application.OnIdle:= ProcessPlaza2Messages;

  // создаем счетчик времени
  fPreciseTime:= TPreciseTime.Create(Self);

  // проверяем наличие конфигурационного файла
  if not fileexists('P2ClientGate.ini') then begin
    // если файл отсутствует, выводим сообщение
    MessageBox(0, 'Отсутствует файл настроек P2ClientGate.ini', 'Ошибка', 0);
    // закрываем приложение
    PostMessage(Handle, WM_CLOSE, 0, 0);
  end;

  // создаем экземпляр приложения plaza2
  fApp:= TCP2Application.Create(Self);
  // указываем имя ini-файла с настройками библиотеки
  fApp.StartUp('P2ClientGate.ini');

  // создаем соединение plaza2
  fConn:= TCP2Connection.create(Self);
  with fConn do begin
    // устанавливаем адрес машины с роутером (в данном случае - локальный)
    Host:= 'localhost';
    // указываем порт, к которому подключается это приложение
    Port:= 4001;
    // задаем произвольное имя приложения
    AppName:= 'P2VCLTestApp';
    // устанавливаем обработчик изменения статуса потока (connected, error...)
    OnConnectionStatusChanged:= ConnectionStatusChanged;
  end;

  // создаем поток plaza2
  fStream  := TCP2DataStream.create(Self);
  with fStream do begin
    // указываем режим открытия потока (в delphi слово type - ключевое, при импорте
    // библиотеки типов оно было автоматически переименовано
    type_:= RT_COMBINED_DYNAMIC;
    // задаем имя потока, например сделки по фьючерсам
    StreamName:= 'FORTS_FUTTRADE_REPL';

    // устанавливаем обработчик изменения статуса потока (remote_snapshot, online...)
    OnStreamStateChanged:= StreamStateChanged;
    // устанавливаем обработчик смены номера жизни, он необходим для корректного
    // перехода потока в online
    OnStreamLifeNumChanged:= StreamLifeNumChanged;

    // устанавливаем обработчик "начало данных"
    OnStreamDataBegin:= StreamDataBegin;
    // устанавливаем обработчик получения данных (будем выводить все данные построчно в ListBox)
    OnStreamDataInserted:= StreamDataInserted;
    // устанавливаем обработчик "конец данных"
    OnStreamDataEnd:= StreamDataEnd;
  end;
end;

procedure TForm1.ProcessPlaza2Messages(Sender: TObject; var Done: Boolean);
var cookie : longword;
begin
  // проверяем статус потока и переоткрываем его, если это необходимо
  if assigned(fConn) and (fConn.Status and CS_CONNECTION_CONNECTED <> 0) then begin
    CheckAndReopen(fStream);
    // ...
  end;

  // запускаем обработку сообщения plaza2
  cookie:= 0;
  if assigned(fConn) then fConn.ProcessMessage(cookie, 1);

  // указываем, что обработка не завершена, для того чтобы vcl не уходил в ожидание сообщений windows
  Done:= false;
end;

function TForm1.CheckAndReopen(AStream: TCP2DataStream): boolean;
begin
  // проверка и переоткрытие потока
  result:= true;
  if assigned(AStream) then
    with AStream do try
      // если статус потока - ошибка или закрыт
      if (State = DS_STATE_ERROR) or (State = DS_STATE_CLOSE) then begin
        // ксли ошибка, то закрываем
        if (State = DS_STATE_ERROR) then Close;
        // далее пытаемся открыть его вновь
        if assigned(fConn) then Open(fConn.DefaultInterface);
      end;
    except result:= false; end;
end;

procedure TForm1.ConnectionStatusChanged(Sender: TObject; var conn: OleVariant; newStatus: TConnectionStatus);
begin
  // выводим сообщение об изменении статуса соединения
  log('Connection status changed to: %.8x', [longint(newStatus)]);
end;

procedure TForm1.StreamStateChanged(Sender: TObject; var stream: OleVariant; newState: TDataStreamState);
const state_unknown = -1;
const streamstates: array[state_unknown..DS_STATE_ERROR] of pChar = ('UNKNOWN', 'DS_STATE_CLOSE',
        'DS_STATE_LOCAL_SNAPSHOT', 'DS_STATE_REMOTE_SNAPSHOT', 'DS_STATE_ONLINE', 'DS_STATE_CLOSE_COMPLETE',
        'DS_STATE_REOPEN', 'DS_STATE_ERROR');
var   st: longint;
begin
  // выводим сообщение об изменении статуса потока
  st:= newState;
  if (st < low(streamstates)) or (st > high(streamstates)) then st:= state_unknown;
  log('Stream %s state changed to %s (%.8x)', [stream.StreamName, streamstates[st], st]);
end;

procedure TForm1.StreamLifeNumChanged(Sender: TObject; var stream: OleVariant; LifeNum: Integer);
begin
  // при изменении номера жизни потока, указываем потоку новый номер жизни
  stream.TableSet.LifeNum:= olevariant(lifenum);
  // выводим сообщение об этом
  log('Stream %s LifeNum changed to: %d', [string(stream.StreamName), lifenum]);
end;

procedure TForm1.StreamDataBegin(Sender: TObject; var stream: OleVariant);
begin
  // выводим сообщение о том, что начата обработка пачки
  log('Stream: %s data begin', [string(stream.StreamName)]);
end;

procedure TForm1.StreamDataInserted(Sender: TObject; var stream, tableName, rec: OleVariant);
var i    : longint;
    data : string;
begin
  // обработка пришедших данных

  // получаем имя потока и таблицы
  data:= format('%s %s ', [string(stream.StreamName), string(tableName)]);
  // последовательно получаем все поля записи в виде строк и помещаем в строку для последующего вывода

  with IP2Record(IUnknown(rec)) do
    for i:= 0 to Count - 1 do data:= data + GetValAsStringByIndex(i) + ',';

  // выводим подготовленную строку в лог
  log(data);
end;

procedure TForm1.StreamDataEnd(Sender: TObject; var stream: OleVariant);
begin
  // выводим сообщение о том, что обработка данных закончена
  log('Stream: %s data end', [string(stream.StreamName)]);
end;


procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
  // установление соединения по кнопке connect

  // при импорте библиотеки типов метод Connect был автоматически переименован в
  // Connect1 для того, чтобы избежать пересечения со стандартным методом Connect
  // Ole-сервера дельфи
  if assigned(fConn) then try
    fConn.Connect1;
  except
    on e: exception do log('Connect exception: %s', [e.message]);
  end;
end;

procedure TForm1.DisconnectButtonClick(Sender: TObject);
begin
  // разрыв соединения по кнопке disconnect

  // при импорте библиотеки типов метод Disconnect был автоматически переименован в
  // Disconnect1 для того, чтобы избежать пересечения со стандартным методом Connect
  // Ole-сервера дельфи
  if assigned(fConn) then try
    fConn.Disconnect1;
  except
    on e: exception do log('Disconnect exception: %s', [e.message]);
  end;
end;

procedure TForm1.log(const alogstr: string);
begin
  // вывод информации в LogListBox
  if assigned(LogListBox) then with LogListBox.Items do begin
    // храним только 50 строк
    if (Count > 50) then Delete(Count - 1);
    // добавляем строки в начало
    Insert(0, formatdatetime('hh:nn:ss.zzz ', fPreciseTime.Now) + alogstr);
  end;
end;

procedure TForm1.log(const alogstr: string; const aparams: array of const);
begin
  // вывод лога с форматированием строки
  log(format(alogstr, aparams));
end;

initialization
  // инициализируем COM
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

finalization
  CoUnInitialize;

end.
