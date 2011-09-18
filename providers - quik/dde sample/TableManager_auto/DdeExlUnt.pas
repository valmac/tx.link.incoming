unit DdeExlUnt;

interface

uses
  Windows, Classes, Types, Forms, DdeMl, SysUtils, StrUtils, Controls,
  Messages;

const
  ServiceName = 'Excel';          // им€ DDE-сервера - подстраиваемс€ под MS Excel
  WM_DDE_ADDQUE = WM_USER + 2;

type
  TVariantTable = record  // “аблица вариантных величин
    Cells: array of array of Variant;       // [строка, столбец]
    RowCount: integer;
    ColCount: integer;
  end;

  procedure ClearTable(Table: TVariantTable);overload;

type
  TPokeAction = (paAccept, paPass, paReject);  // прин€ть данные, пропустить(сделать вид что прин€ли), отклонить
                                               // пропустить - чтобы отправитель не отключилс€, и продолжал передавать

  TDdePokeEvent = procedure(Topic: string; var Action: TPokeAction) of object;
  TDdeDataEvent = procedure(Topic: string; Cells: TRect; Data: TVariantTable) of object;

  PDdeQueItem = ^TDdeQueItem;
  TDdeQueItem = record
    data: pointer;
    size: integer;
    sTopic: shortstring;
    sCells: shortstring;
  end;

  TDdeExcel = class(TWinControl)
  private
    Inst: Integer;
    ServiceHSz: HSz;
    TopicHSz: HSz;
    fOnPoke: TDdePokeEvent;
    fOnData: TDdeDataEvent;
    fDdeQue: TThreadList;
  protected
    function XLTDecodeV(data: pointer; datasize: integer): TVariantTable;
    function DecodeCellAddr(CellAddr: string): TRect;
    procedure AddQue(var Message: TWMSysCommand); message WM_DDE_ADDQUE;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  published
    property OnPoke: TDdePokeEvent read fOnPoke write fOnPoke;
    property OnData: TDdeDataEvent read fOnData write fOnData;
  end;

var
  DdeExcel: TDdeExcel;

// функции дл€ работы с указател€ми
function addp(p: Pointer; increment: integer = 1): Pointer; overload;
function addp(p: Pointer; increment: pointer): Pointer; overload;
function subp(p: Pointer; decrement: integer = 1): Pointer; overload;
function subp(p: Pointer; decrement: pointer): Pointer; overload;


implementation

function CallbackProc(CallType, Fmt: UINT; Conv: HConv; hsz1, hsz2: HSZ;
    Data: HDDEData; Data1, Data2: DWORD): HDDEData stdcall;
var bAllow: boolean;
    action: TPokeAction;
    sTopic: string;
    p: PDdeQueItem;
    buf: array [0..255] of char;
begin
  result := 0;
  Case CallType of
    XTYP_CONNECT:
          result := 1;
    XTYP_POKE:
        begin
          DdeQueryString(DdeExcel.Inst, HSz1, buf, sizeof(buf), CP_WINANSI);
          sTopic := string(buf);
          action := paPass;
          If Assigned(DdeExcel.fOnPoke) then DdeExcel.fOnPoke(sTopic, action);
          Case action of
            paAccept: begin
                        DdeQueryString(DdeExcel.Inst, HSz2, buf, sizeof(buf), CP_WINANSI);
                        New(p);
                        p^.sTopic := sTopic;
                        p^.sCells := string(buf);
                        p^.size := DdeGetData(Data, nil, 4, 0);
                        GetMem(p^.data, p^.size);
                        DdeGetData(Data, p^.data, p^.size, 0);
                        With DdeExcel.fDdeQue.LockList do
                        try
                          add(p)
                        finally
                          DdeExcel.fDdeQue.UnlockList;
                        end;
                        If (DdeExcel.Handle <> 0) then
                          PostMessage(DdeExcel.Handle, WM_DDE_ADDQUE, 0, 0);
                        result := Dde_fAck;
                      end;
            paPass:   result := Dde_fAck;
            paReject: result := Dde_fNotProcessed;
          end; // case
        end;
  end; // case
end;


constructor TDdeExcel.Create;
begin
  inherited;
  fDdeQue := TThreadList.Create;
  Parent := TWinControl(AOwner);
  CreateHandle;
  Inst := 0;
  if DdeInitialize(Inst, CallbackProc, APPCLASS_STANDARD	, 0) = dmlErr_No_Error then
  begin
    ServiceHSz := DdeCreateStringHandle(Inst, ServiceName, cp_WinAnsi);
    TopicHSz := DdeCreateStringHandle(Inst, PAnsiChar('Topic'), cp_WinAnsi);
    If (DdeNameService(Inst, ServiceHSz, 0, dns_Register) = 0) then
      raise Exception.Create('Ќе удалось зарегистрировать им€ DDE-сервиса ''' + ServiceName + '''');
  end
  else
    raise Exception.Create('Ќе удалось выполнить инициализацию DDE');
end;

destructor TDdeExcel.Destroy;
begin
  DdeNameService(Inst, ServiceHsz, 0, dns_Unregister);
  fDdeQue.Free;
  DdeFreeStringHandle(Inst, ServiceHsz);
  DdeUninitialize(Inst);
  inherited;
end;

procedure TDdeExcel.AddQue(var Message: TWMSysCommand);
var
  vt: TVariantTable;
  Cells: TRect;
  p: PDdeQueItem;
  i: integer;
begin
  With fDdeQue.LockList do
  try
    p := PDdeQueItem(Items[Count-1]);
    Cells := DecodeCellAddr(p^.sCells);
    vt := XLTDecodeV(p^.data, p^.size);
    If Assigned (DdeExcel.fOnData) then DdeExcel.fOnData(p^.sTopic, Cells, vt);
    For i := Count-1 downto 0 do
    begin
      p := Items[i];
      FreeMem(p.data, p^.size);
      Delete(i);
      Dispose(p);
    end;
  finally
    fDdeQue.UnlockList
  end;
end;


function TDdeExcel.XLTDecodeV(data: pointer; datasize: integer): TVariantTable;
var
  i: integer;
  curr: pointer;
  BlockType: word;
  BlockSize: word;
  StringSize: byte;
  RealData: real;
  StringData: shortstring;
  DataNum: integer;
  s: string;
begin
  curr := addp(data, 4);
  result.RowCount := Word(curr^); // читаем количество строк
  curr := addp(curr, 2);
  result.ColCount := Word(curr^); // читаем количество столбцов
  curr := addp(curr, 2);
  // задаем размеры массива
  SetLength(result.Cells, result.RowCount);
  For i := 0 to result.RowCount - 1 do
    SetLength(result.Cells[i], result.ColCount);

  DataNum := 0;
  While (Integer(subp(curr, data)) < datasize) do    // обработка блока данных
  begin
    BlockType := Word(curr^);
    curr := addp(curr, 2);
    BlockSize := Word(curr^);
    curr := addp(curr, 2);
    Case BlockType of
      1: begin                              // число
           While BlockSize > 0 do
           begin
             RealData := Real(curr^);
             curr := addp(curr, 8);
             dec(BlockSize, 8);
             result.Cells[(DataNum div result.ColCount), (Datanum mod result.ColCount)] := RealData;
             inc(DataNum);
           end;
         end;
      2: begin                              // текст
           While BlockSize > 0 do
           begin
             StringSize := Byte(curr^);
             curr := addp(curr);
             StringData[0] := chr(StringSize);
             For i := 1 to StringSize do
             begin
               StringData[i] := Char(curr^);
               curr := addp(curr);
             end;
             result.Cells[(DataNum div result.ColCount), (Datanum mod result.ColCount)] := StringData;
             inc(DataNum);
             dec(BlockSize, StringSize+1);
           end;
         end;
    end; // case
  end;
  SetLength(s, datasize);
  For i := 0 to datasize-1 do
    s[i+1] := char(addp(data,i)^);
end;


function TDdeExcel.DecodeCellAddr(CellAddr: string): TRect;
var
  tmp1, tmp2: integer;
begin
  CellAddr := UpperCase(CellAddr);
  tmp1 := PosEx('R', CellAddr);
  tmp2 := PosEx('C', CellAddr, tmp1);
  try
    result.Top := strtoint(copy(CellAddr, tmp1+1, tmp2-tmp1-1)) - 1; // в конце -1, потому что Excel нумерует €чейки с 1, а мы с 0
  except
    exit
  end;
  tmp1 := PosEx('R', CellAddr, tmp2);
  try
    Result.Left := strtoint(copy(CellAddr, tmp2+1, tmp1-tmp2-1-1)) - 1;  // еще -1 потому что там еще ':' (R1C1:R5C12)
  except
    exit
  end;
  tmp2 := PosEx('C', CellAddr, tmp1);
  try
    result.Bottom := strtoint(copy(CellAddr, tmp1+1, tmp2-tmp1-1)) - 1;
    result.Right := strtoint(copy(CellAddr, tmp2+1, Length(CellAddr)-tmp2)) - 1;
  except
    exit
  end;
end;

procedure ClearTable(Table: TVariantTable);
begin
  SetLength(Table.Cells, 0);
  Table.RowCount := 0;
  Table.ColCount := 0;
end;


function addp(p: Pointer; increment: integer = 1): Pointer; overload;
begin
  result := Pointer(Integer(p) + increment);
end;

function addp(p: Pointer; increment: pointer): Pointer; overload;
begin
  result := Pointer(Integer(p) + Integer(increment));
end;

function subp(p: Pointer; decrement: integer = 1): Pointer; overload;
begin
  result := Pointer(Integer(p) - decrement);
end;

function subp(p: Pointer; decrement: pointer): Pointer; overload;
begin
  result := Pointer(Integer(p) - Integer(decrement));
end;

end.
