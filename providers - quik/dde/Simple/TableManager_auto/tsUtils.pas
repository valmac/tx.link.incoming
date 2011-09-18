unit tsUtils;


interface

uses
  Controls, SysUtils, DateUtils, Dialogs;

const
  eps = 0.01;  // если цена меньше этой величины, то она равнозначна нулю 

// функции обработки целочисленного времени и даты
function IntToDate(ADate: integer): TDate;
function DateToInt(ADate: TDate): integer;
function IntToTime(ATime: integer): TTime;
function TimeToInt(ATime: TTime): integer;
function IntToDateTime(ADate, ATime: integer): TDateTime;
procedure DateTimeToInt(ADateTime: TDateTime; var ADate, ATime: integer);
function IntTimeToSec(ATime: integer): integer;
function SecToIntTime(ASeconds: integer): integer;
function IntSecondsBetween(ANowTime, AThenTime: integer): integer;
// конвертеры
function fts(Value: real): string;
// функции для работы с указателями
function addp(p: Pointer; increment: integer = 1): Pointer; overload;
function addp(p: Pointer; increment: pointer): Pointer; overload;
function subp(p: Pointer; decrement: integer = 1): Pointer; overload;
function subp(p: Pointer; decrement: pointer): Pointer; overload;


type

  TComplPrice = record
    Date: integer;
    Time: integer;
    Price1: real;
    Price2: real;
  end;

  TComplPriceCollector = object
    Count: integer;
    Prices: array of TComplPrice;
    procedure AddPrices(ADate, ATime: integer; APrice1, APrice2: real);
    procedure AddPrice1(ADate, ATime: integer; APrice: real);
    procedure AddPrice2(ADate, ATime: integer; APrice: real);
    procedure Clear;
    procedure Sort;
    procedure ReplaceZeros;
  end;

  TPriceQueue = object
    fLength: integer;
    fData: array of real;
    fPointer: integer;
    procedure Init(aLength: integer; aPrice: real);
    function appPrice(aPrice: real): real;
  end;

  TTimePriceQueue = object
  private
    fLength: integer;
    fData: array of real;
    fPointer: integer;
    fLastTime: integer;
  public
    Round: boolean;
    property Pointer: integer read fPointer;
    procedure Init(aLength: integer; aPrice: real; ATime: integer = 0);
    function appPrice(aPrice: real; ATime: integer = 0): real;
  end;



  TPriceType = (ptNone, ptAction, ptFutures, ptBoth, ptJointFile);
  TPriceData = class
  private
    fPriceType: TPriceType;
    fData: TComplPriceCollector;
    fLastDate: integer;
    fFile: TextFile;
    aFile: TextFile;
    aFileName: TFileName;
    fFileName: TFileName;
    fNextIndex: integer;
    function ReadNextDay: integer;
  public
    procedure Start(ActionFile, FuturesFile: TFileName);overload;
    procedure Start(ADate: integer; ActionFile, FuturesFile: TFileName);overload;
    procedure Start(ActionFuturesFile: TFileName);overload;
    procedure Start(Adate: integer; ActionFuturesFile: TFileName);overload;
    function GetNext(var ADate, ATime: integer; var ActionPrice, FuturesPrice: real): integer;overload;
    function GetNext(var ADate, ATime: integer; var APrice: real):integer;overload;
    procedure Stop;
  end;

var
  PriceData: TPriceData;

implementation

uses LogThread;

function IntToDate(ADate: integer): TDate;
begin
  try
    result := EncodeDate(ADate div 10000, ADate div 100 mod 100, ADate mod 100);
  except
    Log.AddEvent('Ошибка конвертации даты');
  end;
end;

function DateToInt(ADate: TDate): integer;
var
  y, m, d: word;
begin
  try
    DecodeDate(ADate, y, m, d);
  except
    y:=0;
    m := 0;
    d := 0;
    Log.AddEvent('Ошибка конвертации даты');
  end;
  result := (y*100 + m)*100 + d;
end;

function IntToTime(ATime: integer): TTime;
begin
  try
    result := EncodeTime(ATime div 10000, ATime div 100 mod 100, ATime mod 100, 0);
  except
//    ShowMessage(inttostr(ATime))
    Log.AddEvent('Ошибка конвертации времени');
  end;
end;

function TimeToInt(ATime: TTime): integer;
var
  h, m, s, t: word;
begin
  try
    DecodeTime(ATime, h, m, s, t);
  except
    h := 0;
    m := 0;
    s := 0;
    Log.AddEvent('Ошибка конвертации времени');
  end;
  result := (h*100 + m)*100 + s
end;

function IntToDateTime(ADate, ATime: integer): TDateTime;
begin
  result := IntToDate(ADate) + IntToTime(ATime)
end;

procedure DateTimeToInt(ADateTime: TDateTime; var ADate, ATime: integer);
begin
  ADate := DateToInt(ADateTime);
  ATime := TimeToInt(ADateTime);
end;

function inttimetosec(ATime: integer): integer;
begin
  result := (ATime mod 100) + ((ATime div 100) mod 100)*60 + (ATime div 10000)*3600;
end;

function sectointtime(ASeconds: integer): integer;
begin
  result := (ASeconds mod 60) + (ASeconds mod 3600 div 60)*100 + (ASeconds div 3600)*10000
end;

function IntSecondsBetween(ANowTime, AThenTime: integer): integer;
begin
  result := IntTimeToSec(ANowTime) - IntTimeToSec(AThenTime);
end;

function fts(Value: real): string;
begin
  result := FloatToStrF(Value, ffFixed, 25, 2)
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


//----------TComplPriceCollector--------
procedure TComplPriceCollector.Clear;
begin
  Count := 0;
  SetLength(Prices, Count);
end;

procedure TComplPriceCollector.AddPrices(ADate, ATime: integer; APrice1, APrice2: real);
var
  i: integer;
  new: boolean;
begin
  new := true;
  For i := 0 to Count - 1 do
    If (Prices[i].Date = ADate) and (Prices[i].Time = ATime) then
    begin
      new := false;
      If APrice1 <> 0 then
        Prices[i].Price1 := APrice1;
      If APrice2 <> 0 then
        Prices[i].Price2 := APrice2;
      Break
    end;

  If new then
  begin
    SetLength(Prices, Count+1);
    Prices[Count].Date := ADate;
    Prices[Count].Time := ATime;
    Prices[Count].Price1 := APrice1;
    Prices[Count].Price2 := APrice2;
    inc(Count);
  end;
end;

procedure TComplPriceCollector.AddPrice1(ADate, ATime: integer; APrice: real);
begin
  AddPrices(ADate, ATime, APrice, 0);
end;

procedure TComplPriceCollector.AddPrice2(ADate, ATime: integer; APrice: real);
begin
  AddPrices(ADate, ATime, 0, APrice);
end;

procedure TComplPriceCollector.Sort;
var
  i: integer;
  tmp: TComplPrice;
  done: boolean;
begin
  Repeat
    done := true;
    For i := 0 to Count - 2 do
      // елси  текущая дата больше следующей или дата равна, а время больше, то меняем местами
      If (Prices[i].Date > Prices[i+1].Date) or ((Prices[i].Date = Prices[i+1].Date) and (Prices[i].Time > Prices[i+1].Time)) then
      begin
        tmp := Prices[i];
        Prices[i] := Prices[i+1];
        Prices[i+1] := tmp;
        done := false
      end;
  Until done;
end;

procedure TComplPriceCollector.ReplaceZeros;
var
  i, n: integer;
begin
  Sort;
  For i := 1 to Count - 1 do
  With Prices[i] do
    begin
      If Price1 = 0 then Price1 := Prices[i-1].Price1;
      If Price2 = 0 then Price2 := Prices[i-1].Price2;
    end;

  n := 0;
  While (Prices[n].Price1 = 0) or (Prices[n].Price2 = 0) do
    inc(n);               // подсчет нулей в начале списка

  If n > 0 then
    For i := n to Count - 1 do
      Prices[i - n] := Prices[i];
  Count := Count - n;
  SetLength(Prices, Count);

end;
//--------------------------------------


//----------TPriceQueue----------
procedure TPriceQueue.Init;
var
  i: integer;
begin
  fLength := aLength;
  SetLength(fData, fLength);
  fPointer := 0;
  For i := 0 to fLength - 1 do
    fData[i] := aPrice;
end;

function TPriceQueue.appPrice;
var
  i: integer;
  sum: real;
begin
  fData[fPointer] := aPrice;
  inc(fPointer);
  If fPointer >= fLength then fPointer := 0;
  sum := 0;
  For i := 0 to fLength - 1 do
    sum := sum + fData[i];
  result := sum / fLength
end;
//--------------------------------------

//----------TTimePriceQueue----------
procedure TTimePriceQueue.Init;
var
  i: integer;
begin
  fLength := aLength;
  SetLength(fData, fLength);
  fPointer := 0;
  For i := 0 to fLength - 1 do
    fData[i] := aPrice;
  If ATime = 0 then
    ATime := timetoint(Now);
  fLastTime := ATime;
  Round := false;
end;

function TTimePriceQueue.appPrice;
var
  i, t: integer;
  sum: real;
begin
  If ATime = 0 then
    ATime := TimeToInt(Now);
  t := intSecondsBetween(ATime, fLastTime);
  If t = 0 then
    fData[fPointer] := (fData[fPointer] + aPrice)/2
  else
    For i := 1 to t do
    begin
      fData[fPointer] := aPrice;
      inc(fPointer);
      If fPointer >= fLength then
      begin
        fPointer := 0;
        round := true;
      end;
    end;

  fLastTime := ATime;

  sum := 0;
  For i := 0 to fLength - 1 do
    sum := sum + fData[i];
  result := sum / fLength
end;
//--------------------------------------


//----------TPriceData------------------

function TPriceData.ReadNextDay: integer;
var
  fDate, fTime, hDate: integer;
  fPrice: real;
begin
  result := -1;
  fData.Clear;
  If fPriceType in [ptAction, ptBoth] then  // читаем акции
  begin
    If eof(aFile) then Exit;
//    reset(aFile);
    Repeat       // проматываем, то что уже было обработано раньше
      Readln(aFile, fDate, fTime, fPrice)
    Until eof(aFile) or (fDate = fLastDate) or (fLastDate = 0);
    If not eof(aFile) then
    begin
      hDate := fDate;
      Repeat
        fData.AddPrice1(fDate, fTime, fPrice);
        Readln(aFile, fDate, fTime, fPrice);
      Until eof(aFile) or (fDate > hDate);
      fData.AddPrice1(fDate, fTime, fPrice);
    end;
//    closefile(aFile);
  end;

  If fPriceType in [ptFutures, ptBoth] then  // читаем фьючи
  begin
    If eof(fFile) then Exit;
//    reset(fFile);
    Repeat       // проматываем, то что уже было обработано раньше
      Readln(fFile, fDate, fTime, fPrice)
    Until eof(fFile) or (fDate = fLastDate) or (fLastDate = 0);
    If not eof(fFile) then
    begin
      hDate := fDate;
      Repeat
        fData.AddPrice2(fDate, fTime, fPrice);
        Readln(fFile, fDate, fTime, fPrice);
      Until eof(fFile) or (fDate > hDate);
      fData.AddPrice2(fDate, fTime, fPrice);
    end;
//    closefile(fFile);
  end;

  fData.Sort;
//  fData.ReplaceZeros;
  fLastDate := fDate;
  fNextIndex := 0;
  result := fData.Count;
end;


procedure TPriceData.Start(ADate: integer; ActionFile, FuturesFile: TFileName);
var
  i: integer;
begin
  i := 0;
  If FileExists(ActionFile) then
  begin
    aFileName := ActionFile;
    assignfile(aFile, ActionFile);
    reset(aFile);
    inc(i);
  end;
  If FileExists(FuturesFile) then
  begin
    fFileName := FuturesFile;
    assignfile(fFile, FuturesFile);
    reset(fFile);
    inc(i, 2);
  end;
  fPriceType := TPriceType(i);
  fLastDate := ADate;
  ReadNextDay;
end;

procedure TPriceData.Start(ActionFile, FuturesFile: TFileName);
begin
  Start(0, ActionFile, FuturesFile);
end;

procedure TPriceData.Start(ADate: integer; ActionFuturesFile: TFileName);
var
  fDate, fTime, hDate: integer;
  fAPrice, fFPrice: real;
begin
  If FileExists(ActionFuturesFile) then
  begin
    assignfile(aFile, ActionFuturesFile);
    reset(aFile);
    fPriceType := ptJointFile;
  end;
  Repeat
    readln(aFile, fDate, fTime, fAPrice, fFPrice);
  Until eof(aFile) or (fDate >= ADate) or (ADate = 0);
end;

procedure TPriceData.Start(ActionFuturesFile: TFileName);
begin
  Start(0, ActionFuturesFile);
end;

procedure TPriceData.Stop;
begin
  try
    closefile(aFile);
  except end;
  try
    closefile(fFile);
  except end;  
  fLastDate := 0;
  fPriceType := ptNone;;
  fData.Clear;
end;

function TPriceData.GetNext(var ADate, ATime: integer; var ActionPrice, FuturesPrice: real): integer;
begin
  If fPriceType = ptNone then
  begin
    result := -1;
    Exit;
  end;


  If fPriceType = ptJointFile then
  begin
    result := 0;
    If eof(aFile) then
      result := -1
    else
      Readln(AFile, ADate, ATime, ActionPrice, FuturesPrice);
    Exit;
  end;    


  If fNextIndex >= fData.Count then // если буфер исчерпан, то обновляем буфер
    ReadNextDay;
  If fData.Count > 0 then
  begin
    ADate := fData.Prices[fNextIndex].Date;
    ATime := fData.Prices[fNextIndex].Time;
    ActionPrice := fData.Prices[fNextIndex].Price1;
    FuturesPrice := fData.Prices[fNextIndex].Price2;
    inc(fNextIndex);
    result := 0;
  end
  else
    result := -1;

end;

function TPriceData.GetNext(var ADate, ATime: integer; var APrice: real):integer;
var
  dum: real;
begin
  Case fPriceType of
    ptAction : result := GetNext(ADate, ATime, APrice, dum);
    ptFutures: result := GetNext(ADate, ATime, dum, APrice);
  else result := -1;
  end;


end;
//--------------------------------------

Initialization
  PriceData := TPriceData.Create;

Finalization
//  PriceData.Stop;
  PriceData.Free;

end.
