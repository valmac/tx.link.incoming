unit p2_baseless_VCL_privod_example_main;

interface

uses
  Windows, SysUtils, Classes, ActiveX, ComObj, Forms, Controls, StdCtrls, Grids, ComCtrls, ExtCtrls, 
  P2ClientGate_TLB,
  p2_orderbook_collector;

// имена потоков, таблиц
const
  INI_FILE_NAME      = 'P2ClientGate.ini';

  STREAM_INFO_NAME   = 'FORTS_FUTINFO_REPL';
  STREAM_AGGR_NAME   = 'FORTS_FUTAGGR20_REPL';
  STREAM_POS_NAME    = 'FORTS_POS_REPL';
  STREAM_PART_NAME   = 'FORTS_PART_REPL';

  TABLE_AGGR_NAME    = 'orders_aggr';
  TABLE_FUTSESS_NAME = 'fut_sess_contents';
  TABLE_POS_NAME     = 'position';
  TABLE_PART_NAME    = 'part';

// класс для хранения параметров инструментов
type
  PIsinListItem = ^TIsinListItem;
  TIsinListItem = record
    isin_id     : longint;
    isin        : string[20];
    short_isin  : string[20];
    name        : string[50];
  end;          

  TIsinList    = class(tSortedList)
    procedure freeitem(item: pointer); override;
    function  checkitem(item: pointer): boolean; override;
    function  compare(item1, item2: pointer): longint; override;
    function  additem(aisin_id: longint; const aisin, ashort_isin, aname: string): PIsinListItem;
    function  isinbyid(aisin_id: longint): string;
    function  isintextbyid(aisin_id: longint): string;
  end;

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

type
  IP2Async2Reply         = interface(IDispatch)
    ['{98C080E2-7CB1-43C4-B036-DB144AF3BCDB}']
    procedure    SendAsync2Reply(const reply: IP2BLMessage; errCode: LongWord; eventParam: int64); stdcall;
  end;

  tAsyncMessageObject = class(TInterfacedObject, IDispatch, IP2Async2Reply)
    function    GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function    GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function    GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function    Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    procedure   SendAsync2Reply(const reply: IP2BLMessage; errCode: LongWord; eventParam: int64); stdcall;
  end;

// главная форма приложения
type
  TForm1 = class(TForm)
    LogListBox: TListBox;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    InstrumentComboBox: TComboBox;
    OrderBookGrid: TStringGrid;
    Label3: TLabel;
    clientcodeedit: TEdit;
    Label4: TLabel;
    priceedit: TEdit;
    Label5: TLabel;
    qtyedit: TEdit;
    buysellgroup: TRadioGroup;
    SendOrderButton: TButton;
    positions: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure InstrumentComboBoxChange(Sender: TObject);
    procedure SendOrderButtonClick(Sender: TObject);
    procedure OrderBookGridClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure positionsChange(Sender: TObject; Node: TTreeNode);
  private
    fPreciseTime: TPreciseTime;
    fApp: TCP2Application;
    fConn: TCP2Connection;
    fMsgFactory: TCP2BLMessageFactory;
    fAsyncReplyObject: IP2Async2Reply;

    FAddress: string;

    FIsinList: TIsinList;
    FOrderBookList: tOrderBookList;

    procedure ProcessPlaza2Messages(Sender: TObject; var Done: Boolean);

    procedure ConnectionStatusChanged(Sender: TObject; var conn: OleVariant; newStatus: TConnectionStatus);

    procedure StreamStateChanged(Sender: TObject; var stream: OleVariant; newState: TDataStreamState);
    procedure StreamLifeNumChanged(Sender: TObject; var stream: OleVariant; LifeNum: Integer);

    procedure StreamDataInserted(Sender: TObject; var stream: OleVariant; var tableName: OleVariant; var rec: OleVariant);
    procedure StreamDataDeleted(Sender: TObject; var stream: OleVariant; var tableName: OleVariant; Id: Int64; var rec: OleVariant);
    procedure StreamDataEnd(Sender: TObject; var stream: OleVariant);

    procedure StreamDatumDeleted(Sender: TObject; var stream: OleVariant; var tableName: OleVariant; rev: Int64);

    function  ResolveAddress: string;

    procedure RedrawOrderBook(forceredraw: boolean);


    function  GetIsin(aisin_id: longint): string;
    function  GetIsinText(aisin_id: longint): string;

    function  GetCurrentIsin: string;
    procedure UpdateIsinNamesInPos(aisin_id: longint; const aisin: string);

    procedure AddPosition(aisin_id: longint; const aclient, aisin_text, apos: string);
  public
    function CheckAndReopen(AStream: TCP2DataStream): boolean;
    procedure log(const alogstr: string); overload;
    procedure log(const alogstr: string; const aparams: array of const); overload;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function _StrToFloatDef(const str: string; Default: extended): extended;
begin if not TextToFloat(PChar(str), Result, fvExtended) then Result:= Default; end;

{ TIsinList }

procedure TIsinList.freeitem(item: pointer);
begin if assigned(item) then dispose(PIsinListItem(item)); end;

function TIsinList.checkitem(item: pointer): boolean;
begin result:= assigned(item); end;

// сортируем по isin_id
function TIsinList.compare(item1, item2: pointer): longint;
begin result:= PIsinListItem(item1)^.isin_id - PIsinListItem(item2)^.isin_id; end;

// добавление/обновление элемента
function TIsinList.additem(aisin_id: longint; const aisin, ashort_isin, aname: string): PIsinListItem;
var itm : TIsinListItem;
    idx : longint;
begin
  itm.isin_id:= aisin_id;
  if not search(@itm, idx) then begin
    result:= new(PIsinListItem);
    result^.isin_id:= aisin_id;
    insert(idx, result);
  end else result:= PIsinListItem(items[idx]);
  if assigned(result) then with result^ do begin
    isin       := aisin;
    short_isin := ashort_isin;
    name       := aname;
  end;
end;

// получение isin по isin_id
function TIsinList.isinbyid(aisin_id: longint): string;
var itm : TIsinListItem;
    idx : longint;
begin
  itm.isin_id:= aisin_id;
  if search(@itm, idx) then result:= PIsinListItem(items[idx])^.isin else setlength(result, 0);
end;

// получение инструмента в виде текстовой строки
function TIsinList.isintextbyid(aisin_id: longint): string;
var itm : TIsinListItem;
    idx : longint;
begin
  itm.isin_id:= aisin_id;
  if search(@itm, idx) then with PIsinListItem(items[idx])^ do result:= format('%s [%s]', [short_isin, name])
                       else setlength(result, 0);
end;

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
  function InitDataStream(astrm: TCP2DataStream; const aname, aschemeini, aschemename: string): TCP2DataStream;
  begin
    if assigned(astrm) then
      with astrm do begin
        // указываем режим открытия потока (в delphi слово type - ключевое, при импорте
        // библиотеки типов оно было автоматически переименовано
        type_:= RT_COMBINED_DYNAMIC;
        // задаем имя потока
        StreamName:= aname;
        // инициализируем схему, если нужно
        if (length(aschemeini) > 0) and fileexists(aschemeini) then begin
          TableSet:= CreateComObject(CLASS_CP2TableSet) as IP2TableSet;
          TableSet.InitFromIni2(aschemeini, aschemename);
        end;
        // устанавливаем обработчик изменения статуса потока (remote_snapshot, online...)
        OnStreamStateChanged:= StreamStateChanged;
        // устанавливаем обработчик смены номера жизни, он необходим для корректного
        // перехода потока в online
        OnStreamLifeNumChanged:= StreamLifeNumChanged;
        // устанавливаем обработчик DatumDeleted
        OnStreamDatumDeleted:= StreamDatumDeleted;

        // устанавливаем обработчик получения данных
        OnStreamDataInserted:= StreamDataInserted;
        // устанавливаем обработчик удаления данных
        OnStreamDataDeleted:= StreamDataDeleted;
        // устанавливаем обработчик "конец данных"
        OnStreamDataEnd:= StreamDataEnd;
      end;
    result:= astrm;
  end;
begin
  // устанавливаем обработчик события Application.OnIdle, в нем мы будем запускать
  // обработку сообщений plaza2
  Application.OnIdle:= ProcessPlaza2Messages;

  // создаем счетчик времени
  fPreciseTime:= TPreciseTime.Create(Self);

  // создаем список инструментов
  FIsinList:= TIsinList.create;

  // создаем список стаканов
  FOrderBookList:= tOrderBookList.create;

  // проверяем наличие конфигурационного файла
  if not fileexists(INI_FILE_NAME) then begin
    // если файл отсутствует, выводим сообщение
    MessageBox(0, 'Отсутствует файл настроек P2ClientGate.ini; создаем пустой...', 'Ошибка', 0);
    // создаем файл
    fileclose(filecreate(INI_FILE_NAME));
  end;

  // создаем экземпляр приложения plaza2
  fApp:= TCP2Application.Create(Self);
  // указываем имя ini-файла с настройками библиотеки
  fApp.StartUp(INI_FILE_NAME);

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

  // создаем потоки plaza2
  InitDataStream(TCP2DataStream.create(Self), STREAM_INFO_NAME, 'forts_scheme.ini', 'FutInfo');
  InitDataStream(TCP2DataStream.create(Self), STREAM_AGGR_NAME, 'forts_scheme.ini', 'Aggr');
  InitDataStream(TCP2DataStream.create(Self), STREAM_POS_NAME,  'forts_scheme.ini', 'Pos');
  InitDataStream(TCP2DataStream.create(Self), STREAM_PART_NAME, 'forts_scheme.ini', 'Part');

  // создаем фабрику сообщений plaza2
  fMsgFactory:= TCP2BLMessageFactory.create(Self);
  fMsgFactory.Init('p2fortsgate_messages.ini', '');

  // создаем объект для приема асинхронных событий
  FAsyncReplyObject:= tAsyncMessageObject.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // уничтожаем созданные объекты-списки
  if assigned(FIsinList) then freeandnil(FIsinList);
  if assigned(FOrderBookList) then freeandnil(FOrderBookList);
end;

procedure TForm1.ProcessPlaza2Messages(Sender: TObject; var Done: Boolean);
const cs_connected = CS_CONNECTION_CONNECTED or CS_ROUTER_CONNECTED;
var cookie : longword;
    i      : longint;
    cmp    : tComponent;
begin
  // проверяем статус потока и переоткрываем его, если это необходимо
  if assigned(fConn) and (fConn.Status and cs_connected = cs_connected) then begin
    for i:= 0 to ComponentCount - 1 do begin
      cmp:= components[i];
      if assigned(cmp) and (cmp is TCP2DataStream) then CheckAndReopen(TCP2DataStream(cmp));
    end;
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
  log('Статус соединения изменился на: %.8x', [longint(newStatus)]);
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
  if (st <= low(streamstates)) or (st > high(streamstates)) then st:= state_unknown;
  log('Поток %s статус изменился на %s (%.8x)', [stream.StreamName, streamstates[st], st]);
end;

procedure TForm1.StreamLifeNumChanged(Sender: TObject; var stream: OleVariant; LifeNum: Integer);
var strmname : string;
begin
  // при изменении номера жизни потока, указываем потоку новый номер жизни
  try
    stream.TableSet.LifeNum:= olevariant(lifenum);
  except
    on e: exception do log('Исключение при смене номера жизни на %d: %s', [LifeNum, e.message]);
  end;

  strmname:= string(stream.StreamName);
  // очищаем таблицы, согласно документации
  if (CompareText(strmname, STREAM_INFO_NAME) = 0) then begin
    InstrumentComboBox.Items.Clear;
  end else
  if (CompareText(strmname, STREAM_AGGR_NAME) = 0) then begin
    if assigned(FOrderBookList) then FOrderBookList.Clear;
  end;

  // выводим сообщение об этом
  log('Поток %s LifeNum изменился на: %d', [strmname, lifenum]);
end;

procedure TForm1.StreamDataInserted(Sender: TObject; var stream, tableName, rec: OleVariant);
var isin_id, idx : longint;
    strmname, tblname, tmp : string;
begin
  if (StrToInt64Def(rec.GetValAsString('replAct'), 1) = 0) then begin
    strmname := string(stream.StreamName);
    tblname  := string(tableName);
    // обработка пришедших данных
    if (CompareText(strmname, STREAM_INFO_NAME) = 0) then begin
      // поток INFO
      if (CompareText(tblname, TABLE_FUTSESS_NAME) = 0) then begin
        // таблица fut_sess_contents, формируем список инструментов
        isin_id:= rec.GetValAsLong('isin_id');
        if assigned(FIsinList) then FIsinList.additem(isin_id,
                                                      rec.GetValAsString('isin'),
                                                      rec.GetValAsString('short_isin'),
                                                      rec.GetValAsString('name'));
        // помещаем инструменты в combobox
        tmp:= GetIsinText(isin_id);
        with InstrumentComboBox.Items do begin
          // добавляем инструмент combobox, если его там еще нет
          idx:= IndexOfObject(pointer(isin_id));
          if (idx < 0) then Objects[Add(tmp)]:= pointer(isin_id);
        end;
        // обновляем названия инструментов в дереве позиций
        UpdateIsinNamesInPos(isin_id, tmp);
      end;
    end else
    if (CompareText(strmname, STREAM_AGGR_NAME) = 0) then begin
      // поток AGGR
      if (CompareText(tblname, TABLE_AGGR_NAME) = 0) then begin
        // добавляем строку в один из стаканов
        if assigned(FOrderBookList) then FOrderBookList.addrecord(rec.GetValAsLong('isin_id'),
                                                                  StrToInt64Def(rec.GetValAsString('replID'), 0),
                                                                  StrToInt64Def(rec.GetValAsString('replRev'), 0),
                                                                  _StrToFloatDef(rec.GetValAsString('price'), 0),
                                                                  _StrToFloatDef(rec.GetValAsString('volume'), 0),
                                                                  rec.GetValAsLong('dir'));
      end;
    end else
    if (CompareText(strmname, STREAM_POS_NAME) = 0) then begin
      // поток POS
      if (CompareText(tblname, TABLE_POS_NAME) = 0) then begin
        // добавляем строку с информацией о позициях
        isin_id:= rec.GetValAsLong('isin_id');
        AddPosition(isin_id, rec.GetValAsString('client_code'), GetIsinText(isin_id), rec.GetValAsString('pos'));
      end;
    end else
    if (CompareText(strmname, STREAM_PART_NAME) = 0) then begin
      // поток POS
      if (CompareText(tblname, TABLE_PART_NAME) = 0) then begin
        // добавляем строку с информацией о позициях (для денег принимаем isin_id = -1)
        AddPosition(-1, rec.GetValAsString('client_code'), 'Деньги', rec.GetValAsString('money_free'));
      end;
    end
  end;
end;

procedure TForm1.StreamDataDeleted(Sender: TObject; var stream, tableName: OleVariant; Id: Int64; var rec: OleVariant);
var strmname, tblname : string;
begin
  strmname := string(stream.StreamName);
  tblname  := string(tableName);
  if (CompareText(strmname, STREAM_AGGR_NAME) = 0) then begin
    // поток AGGR
    if (CompareText(tblname, TABLE_AGGR_NAME) = 0) then begin
      // удаляем строку из одного из стаканов
      if assigned(FOrderBookList) then FOrderBookList.delrecord(Id);
    end;
  end;
end;

procedure TForm1.StreamDatumDeleted(Sender: TObject; var stream, tableName: OleVariant; rev: Int64);
var strmname, tblname : string;
begin
  strmname := string(stream.StreamName);
  tblname  := string(tableName);
  if (CompareText(strmname, STREAM_AGGR_NAME) = 0) then begin
    // поток AGGR
    if (CompareText(tblname, TABLE_AGGR_NAME) = 0) then begin
      // удаляем строки из всех стаканов с ревиженом меньше заданного
      if assigned(FOrderBookList) then FOrderBookList.clearbyrev(rev);
      // перерисовываем стакан
      RedrawOrderBook(false);
    end;
  end;
end;

procedure TForm1.StreamDataEnd(Sender: TObject; var stream: OleVariant);
var strmname : string;
begin
  strmname := string(stream.StreamName);
  if (CompareText(strmname, STREAM_AGGR_NAME) = 0) then begin
    // если закончился прием изменений потока AGGR, перерисовываем стакан
    RedrawOrderBook(false);
  end;
end;

function TForm1.ResolveAddress: string;
begin
  if (length(FAddress) = 0) then begin
    if assigned(fConn) then FAddress:= fConn.ResolveService('FORTS_SRV');
    result:= FAddress;
  end else result:= FAddress;
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
    on e: exception do log('Исключение при попытке соединения: %s', [e.message]);
  end;
end;

procedure TForm1.DisconnectButtonClick(Sender: TObject);
begin
  // разрыв соединения по кнопке disconnect

  // при импорте библиотеки типов метод Disconnect был автоматически переименован в
  // Disconnect1 для того, чтобы избежать пересечения со стандартным методом Disconnect
  // Ole-сервера дельфи
  if assigned(fConn) then try
    fConn.Disconnect1;
  except
    on e: exception do log('Исключение при разрыве соединения: %s', [e.message]);
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

procedure TForm1.InstrumentComboBoxChange(Sender: TObject);
begin
  // переключаем инструмент в стакане
  if assigned(Sender) and (Sender is TComboBox) then begin
    with TComboBox(Sender) do begin
      // если в комбобоксе что-то выбрано, то устанавливаем isin_id для отрисовывания гридом,
      // если нет, присваиваем -1
      if ItemIndex >= 0 then OrderBookGrid.Tag:= longint(Items.Objects[ItemIndex])
                        else OrderBookGrid.Tag:= -1;
    end;
    // принудительно перерисовываем стакан
    RedrawOrderBook(true);
  end;
end;

procedure TForm1.RedrawOrderBook(forceredraw: boolean);
var   itm : tOrderBook;
      i   : longint;
const bsn : array[boolean] of longint = (0, 2);
  // очистка грида
  procedure ClearGrid;
  var i : longint;
  begin
    // устанавливаем кол-во строк = 1
    OrderBookGrid.RowCount:= 1;
    // очищаем ячейки строки
    for i:= 0 to OrderBookGrid.ColCount - 1 do OrderBookGrid.Cells[i, 0]:= '';
  end;
const bv : array[boolean] of string = ('false', 'true'); 
begin
  // если установлен isin_id для отрисовки
  if (OrderBookGrid.Tag >= 0) then begin
    if assigned(FOrderBookList) then begin
      // ищем стакан, соответствующий isin_id
      itm:= FOrderBookList.searchadditem(OrderBookGrid.Tag);
      // если он есть и изменился либо принудительная отрисовка
      if assigned(itm) and (forceredraw or itm.changed) then with itm do begin
        // если стакан не пуст
        if Count > 0 then begin
          // устанавливаем кол-во строк в гриде
          OrderBookGrid.RowCount:= Count;
          // заполняем ячейки грида
          for i:= 0 to Count - 1 do
            with pOrderBookItem(items[i])^ do begin
              // заполняем цену
              OrderBookGrid.Cells[1, i]:= FloatToStr(price);
              // помещаем кол-во справа или слева от цены, в зависимости от buysell
              OrderBookGrid.Cells[bsn[(buysell and 1 = 1)], i]     := FloatToStr(volume);
              // противоположную ячейку очищаем
              OrderBookGrid.Cells[bsn[not (buysell and 1 = 1)], i] := '';
            end;
        end else ClearGrid;
        changed:= false;
      end;
    end else ClearGrid;
  end else ClearGrid;
end;

procedure TForm1.AddPosition(aisin_id: longint; const aclient, aisin_text, apos: string);
var root, client, isin, pos : tTreeNode;
begin
  // строим дерево с позициями
  // добавляем корневой элемент, если его нет
  root:= positions.Items.GetFirstNode;
  if not assigned(root) then root:= positions.Items.Add(nil, 'Позиции');

  // добавляем код клиента, если его нет
  client:= root.GetFirstChild;
  while assigned(client) and (comparetext(client.Text, aclient) <> 0) do client:= root.GetNextChild(client);
  if not assigned(client) then client:= positions.Items.AddChild(root, aclient);

  // добавляем инструмент, если его нет
  isin:= client.GetFirstChild;
  while assigned(isin) and (isin.data <> pointer(aisin_id)) do isin:= client.GetNextChild(isin);
  if not assigned(isin) then begin
    isin:= positions.Items.AddChild(client, aisin_text);
    isin.data:= pointer(aisin_id);
  end;

  // добавляем позицию или обновляем имеющееся значение
  pos:= isin.GetFirstChild;
  if not assigned(pos) then positions.Items.AddChild(isin, apos) else pos.text:= apos;
end;

function TForm1.GetIsin(aisin_id: longint): string;
begin
  // получаем код выбранного инструмента
  setlength(result, 0);
  if assigned(FIsinList) then result:= FIsinList.isinbyid(aisin_id);
  if (length(result) = 0) then result:= 'Unknown';
end;

function TForm1.GetIsinText(aisin_id: longint): string;
begin
  // название выбранного инструмента
  setlength(result, 0);
  if assigned(FIsinList) then result:= FIsinList.isintextbyid(aisin_id);
  if (length(result) = 0) then result:= 'Unknown';
end;

function TForm1.GetCurrentIsin: string;
begin
  // получаем код выбранного инструмента
  setlength(result, 0);
  with InstrumentComboBox do
    if (ItemIndex >= 0) then result:= GetIsin(longint(Items.Objects[ItemIndex]));
end;

procedure TForm1.UpdateIsinNamesInPos(aisin_id: longint; const aisin: string);
var i   : longint;
    nod : tTreeNode;
begin
  // обновление имени инструмента в дереве позиций
  for i:= 0 to positions.Items.Count - 1 do begin
    nod:= positions.Items[i];
    if assigned(nod) and (nod.Data = pointer(aisin_id)) then nod.text:= aisin;
  end;
end;

procedure TForm1.SendOrderButtonClick(Sender: TObject);
var msg : IP2BLMessage;
begin
  try
    // создаем сообщение plaza2 при помощи фабрики
    msg:= FMsgFactory.CreateMessageByName('FutAddOrder');

    if assigned(msg) then begin
      // служебные поля
      msg.Field['P2_Category'] := 'FORTS_MSG';
      msg.Field['P2_Type']     := '1';  // служебные поля

      // заполняем поле "код"
      msg.Field['isin']        := variant(widestring(GetCurrentIsin));
      // заполняем поле код клиента
      msg.Field['client_code'] := variant(widestring(clientcodeedit.Text));
      // заполняем тип заявки. ставим всегда встречные, чтобы не было лимитных заявок
      msg.Field['type']        := 2;
      // заполняем направление операции покупка/продажа
      with buysellgroup do
        if (ItemIndex >= 0) then msg.Field['dir'] := ItemIndex + 1;
      // кол-во
      msg.Field['amount']      := variant(widestring(qtyedit.Text));
      // цена
      msg.Field['price']       := variant(widestring(priceedit.Text));

      // необязательные поля
//      msg.Field['comment']     := '';
//      msg.Field['broker_to']   := '';
//      msg.Field['ext_id']      := 0;

      // адрес назначения plaza2
      msg.DestAddr:= ResolveAddress;
      // шлем заявку асинхронно
      msg.SendAsync2(fConn.DefaultInterface, 60000, fAsyncReplyObject, fPreciseTime.msecs);
    end;
  except
    on e: exception do log('Исключение при постановке заявки: %s', [e.message]);
  end;
end;

{ tAsyncMessageObject }

function tAsyncMessageObject.GetTypeInfoCount(out Count: Integer): HResult;
begin result:= E_NOTIMPL; end;
function tAsyncMessageObject.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin result:= E_NOTIMPL; end;
function tAsyncMessageObject.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin result:= E_NOTIMPL; end;
function tAsyncMessageObject.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin result:= E_NOTIMPL; end;

procedure tAsyncMessageObject.SendAsync2Reply(const reply: IP2BLMessage; errCode: LongWord; eventParam: int64);
var msg: string;
begin
  // получаем ответ биржи
  try msg:= string(reply.Field['message']);
  except on e: exception do msg:= format('Ошибка получения ответа: %s', [e.message]); end;
  // выводим раундтрип и ответ биржи
  if assigned(Form1) then Form1.log('Заявка обработана биржей за %d мc. Ответ: %s', [Form1.fPreciseTime.msecs - eventParam, msg])
end;

procedure TForm1.OrderBookGridClick(Sender: TObject);
begin
  // заполняем форму ввода заявки
  if assigned(Sender) and (Sender is TStringGrid) then with TStringGrid(Sender) do begin
    // цена
    priceedit.Text:= cells[1, row];
    // кол-во и направление
    qtyedit.Text:= cells[0, row];
    if length(qtyedit.Text) = 0 then begin
      qtyedit.Text:= cells[2, row];
      buysellgroup.ItemIndex:= 0;
    end else buysellgroup.ItemIndex:= 1;
  end;
end;

procedure TForm1.positionsChange(Sender: TObject; Node: TTreeNode);
var idx : longint;
begin
  // если в дереве позиций кликнули на позу по инструменту, переключаем активный стакан
  if assigned(node) then idx:= InstrumentComboBox.Items.IndexOfObject(node.Data) else idx:= -1;
  if (idx >= 0) then begin
    InstrumentComboBox.ItemIndex:= idx;
    InstrumentComboBoxChange(InstrumentComboBox);
  end;
end;

initialization
  // нужно для корректного перевода из строк, возвращаемых методом GetValAsString, в числа
  decimalseparator:= '.';

  // инициализируем COM
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

finalization
  CoUnInitialize;

end.
