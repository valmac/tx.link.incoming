unit winros_main;

interface

uses
  Windows, Messages, SysUtils, Forms, Classes, Controls, StdCtrls;

type
  pTickData = ^tTickData;             // формат передаваемых данных
  tTickData = packed record
    pkttype  : longint;               // тип пакета? всегда 1
    unknown0 : longint;               // всегда 0
    datetime : longint;               // unixtime
    price    : double;                // цена (value1)
    unknown1 : longint;               // всегда 0
    quantity : double;                // кол-во (value2)
    unknown2 : longint;               // всегда 0
    name     : array[0..$3F] of char; // тикер
  end;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBox1DblClick(Sender: TObject);
  private
    fMMFHandle : THandle;
  public
    procedure WMCopyData(var msg: TWMCopyData); message WM_COPYDATA;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function UnixTimeToDateTime(Value: Longword; InUTC: Boolean): TDateTime;
var Days       : LongWord;
    Hour       : Word;
    Min        : Word;
    Sec        : Word;
    tz         : TTimeZoneInformation;
    localtime  : TSystemTime;
    systemtime : TSystemTime;
begin
  Days  := Value div SecsPerDay;
  Value := Value mod SecsPerDay;
  Hour  := Value div 3600;
  Value := Value mod 3600;
  Min   := Value div 60;
  Sec   := Value mod 60;
  result:= 25569 + Days + EncodeTime(Hour, Min, Sec, 0);
  if InUTC then begin
    GetTimeZoneInformation(tz);
    DateTimeToSystemTime(result, systemtime);
    SystemTimeToTzSpecificLocalTime(@tz, systemtime, localtime);
    result:= SystemTimeToDateTime(localtime);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var pFileData : pChar;
begin
  fMMFHandle:= CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, sizeof(longint), 'WR_MM_MAP_0');
  if (fMMFHandle <> 0) then begin
    pFileData := MapViewOfFile(fMMFHandle, FILE_MAP_WRITE, 0, 0, 0);
    if Assigned(pFileData) then try
      pLongint(pFileData)^:= Self.Handle;
    finally UnmapViewOfFile(pFileData); end;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin if (fMMFHandle <> 0) then CloseHandle(fMMFHandle); end;

procedure TForm1.WMCopyData(var msg: TWMCopyData);
begin
  if assigned(msg.CopyDataStruct) then with msg.CopyDataStruct^ do begin
    case dwData of
      $0067 : if (cbData = sizeof(tTickData)) then with pTickData(lpData)^ do
                listbox1.Items.Add(format('%s %d %d %s %.4f %d %.0f %d',
                                          [name, pkttype, unknown0,
                                           FormatDateTime('DD/MM/YYYY HH:NN:SS', UnixTimeToDateTime(datetime, true)),
                                           price, unknown1, quantity, unknown2]));
      else listbox1.Items.Add(format('arrived packet id: %d datalen: %d', [dwData, cbData]));
    end;
  end;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
begin listbox1.Items.Clear; end;

end.
