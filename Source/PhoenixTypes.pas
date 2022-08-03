unit PhoenixTypes;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, System.Generics.Collections, System.DateUtils,
  WTypes;

type

  A12Byte = Array [0 .. 12] of Byte;
  ArrayByte = Array [0 .. 2] of Byte;
  TPrograssEvent = procedure(Sender: TObject; current, all: Integer) of object;

  TTBLPackedRecord = packed record
    Name: Array [0 .. 3] of AnsiChar;
    Null: Byte;
    MTUShort: Smallint;
    MTULong: Longint;
    Identifier: Byte;
    Data: A12Byte;
    constructor Create(const nameValue: string); overload;
    function Value: String;
    procedure SetValue(const v: String);
  end;

  TsData = record
    second: Byte;
    minute: Byte;
    hour: Byte;
    day: Byte;
    month: Byte;
    year: Byte;
    week: Byte;
    century: Byte;
    BoxSN: Smallint;
    NumberofScans: Smallint;
    NumberofChannels: Byte;
    LengthofTS: Byte;
    StatusCodes: Byte;
    SaturationFlag: Byte;
    Reservedbit: Byte;
    Samplelength: Byte;
    SampleRate: Smallint;
    SampleRateUnits: Byte;
    ClockStatus: Byte;
    ClockError: Integer;
    keep: Array [0 .. 5] of Byte;
    datas: Array of Array of ArrayByte;
  private
    function GetData(IndexA, IndexB: Integer): Integer;
    function GetChDataI(Index: Integer): Integer1D;
    function GetChDataD(Index: Integer): Double1D;
    function GetDateTime: TDateTime;
    procedure SetChDataD(Index: Integer; const Value: Double1D);
    procedure SetChDataI(Index: Integer; const Value: Integer1D);
    procedure SetData(IndexA, IndexB: Integer; const Value: Integer);
    procedure SetDateTime(const Value: TDateTime);
  public
    constructor Read(var fs: TFileStream);
    constructor ReadFromStrings(const str: TStrings);
    function ToString: string;
    procedure Write(var fs: TFileStream);
    property Data[IndexA, IndexB: Integer]: Integer read GetData write SetData;
    property ChDataI[Index: Integer]: Integer1D read GetChDataI write SetChDataI;
    property ChDataD[Index: Integer]: Double1D read GetChDataD write SetChDataD;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
  end;

type
  TTsn = class
  private
    FBoxSN: Integer;
    FChannels: Integer;
    FLengthPre: Integer;
    FSamplingRate: Integer;
    FScans: Integer;
    FData: TList<TsData>;
    FOnReadWriteProgress: TPrograssEvent;
    function GetData(Index: Integer): TsData;
    procedure SetData(Index: Integer; const Value: TsData);
  public
    constructor Create(const fileNameV: string); overload;
    constructor Create; overload;
    destructor Destroy; override;
    function Count: Integer;
    procedure ReadBin(const FileName: string);
    procedure ReadText(const FileName: string);
    procedure LoadFrom(const FileName: string);
    procedure WriteBin(const FileName: string);
    procedure WriteText(const FileName: string);
    procedure SaveTo(const FileName: string);
    procedure DeleteData(const Index, len: Integer);
    property BoxSN: Integer read FBoxSN;
    property Channels: Integer read FChannels;
    property SamplingRate: Integer read FSamplingRate;
    property Scans: Integer read FScans;
    property LengthPre: Integer read FLengthPre;
    property Data[Index: Integer]: TsData read GetData write SetData;
    procedure AddData(dat: TsData);
    procedure AppendTs(const ats: TTsn);
  published
    property OnReadWriteProgress: TPrograssEvent read FOnReadWriteProgress write FOnReadWriteProgress;
  end;

type
  TTSNRead = class
  private
    FBoxSN: Integer;
    FChannels: Integer;
    FCount: Integer;
    FFileName: string;
    FLengthPre: Integer;
    FSamplingRate: Integer;
    FScans: Integer;
    FStream: TFileStream;
    function GetData(Index: Integer): TsData;
    procedure SetFileName(const Value: string);
    procedure SetData(Index: Integer; const Value: TsData);
  public
    constructor Create(const fileNameV: string);
    destructor Destroy; override;
    property BoxSN: Integer read FBoxSN;
    property Channels: Integer read FChannels;
    property Count: Integer read FCount;
    property FileName: string read FFileName write SetFileName;
    property SamplingRate: Integer read FSamplingRate;
    property Scans: Integer read FScans;
    property LengthPre: Integer read FLengthPre;
    property Data[Index: Integer]: TsData read GetData write SetData;
  end;

type
  TTSNWrite = class
  private
    FCount: Integer;
    FLengthPre: Integer;
    FFileName: string;
    FStream: TFileStream;
  public
    constructor Create(const fileNameV: string; const LenPre: Integer);
    procedure AppendData(tmp: TsData);
    procedure AppendTsFile(const fileNameV: String);
    destructor Destroy; override;
    property FileName: string read FFileName;
    property LengthPre: Integer read FLengthPre;
    property Count: Integer read FCount;
  end;

function TBLStrToDateTime(const str: String): TDateTime; inline;
function DateTimeToTBLStr(const dt: TDateTime): String; inline;
function bigByte24ToInteger(const bytes: ArrayByte): Integer; inline;
function bigBytes24ToInteger2D(bytes: TBytes; const scs, cnls: Integer): Integer2D; inline;
function IntegerTobigByte24(const lint: Integer): ArrayByte; inline;
procedure Integer2dTobigByte24(const lint: Integer2D; bts: TBytes; bindex: Integer); inline;
function ByteToInt(const bt: Byte): Integer; inline;
function IntToByte(const it: Integer): Byte; inline;
function StringLenthTo(const str: string; const len: Integer): string; inline;

implementation

function TBLStrToDateTime(const str: String): TDateTime; inline;
var
  tstr: String;
  fs: TFormatSettings;
begin
  tstr := str;
  tstr[11] := ' ';
  fs.ShortDateFormat := 'yyyy-MM-dd';
  fs.ShortTimeFormat := 'hh:mm:ss';
  fs.DateSeparator := '-';
  fs.TimeSeparator := ':';
  Result := StrToDateTime(tstr, fs)
end;

function DateTimeToTBLStr(const dt: TDateTime): String; inline;
begin
  Result := FormatDateTime('yyyy-MM-dd HH:mm:ss', dt);
  Result[11] := 'T';
end;

function bigByte24ToInteger(const bytes: ArrayByte): Integer; inline;
begin
  if bytes[2] > $7F then
  begin
    Result := (((bytes[0] and $000000FF) or ((bytes[1] shl 8) and $0000FF00)) or ((bytes[2] shl 16) and $00FF0000)) or
      (($FF shl 24) and $FF000000);
  end
  else
  begin
    Result := ((bytes[0] and $000000FF) or ((bytes[1] shl 8) and $0000FF00)) or ((bytes[2] shl 16) and $00FF0000);
  end;
end;

function bigBytes24ToInteger2D(bytes: TBytes; const scs, cnls: Integer): Integer2D; inline;
var
  I, J, Index0, Index1, Index2: Integer;
begin
  SetLength(Result, cnls);
  for J := 0 to cnls - 1 do
  begin
    SetLength(Result[J], scs);
  end;
  for I := 0 to scs - 1 do
  begin
    for J := 0 to cnls - 1 do
    begin
      Index0 := (I * cnls + J) * 3;
      Index1 := Index0 + 1;
      Index2 := Index1 + 1;
      if bytes[Index2] > $7F then
      begin
        Result[J][I] := (((bytes[Index0] and $000000FF) or ((bytes[Index1] shl 8) and $0000FF00)) or
          ((bytes[Index2] shl 16) and $00FF0000)) or (($FF shl 24) and $FF000000);
      end
      else
      begin
        Result[J][I] := ((bytes[Index0] and $000000FF) or ((bytes[Index1] shl 8) and $0000FF00)) or
          ((bytes[Index2] shl 16) and $00FF0000);
      end;
    end;
  end;
end;

procedure Integer2dTobigByte24(const lint: Integer2D; bts: TBytes; bindex: Integer); inline;
var
  I, J, Index: Integer;
  scs, cnls: Integer;
begin
  cnls := Length(lint);
  scs := Length(lint[0]);
  for I := 0 to scs - 1 do
  begin
    for J := 0 to cnls - 1 do
    begin
      Index := bindex + (I * cnls + J) * 3;
      bts[Index + 2] := (lint[J][I] and $00FF0000) shr 16;
      bts[Index + 1] := (lint[J][I] and $0000FF00) shr 8;
      bts[Index + 0] := lint[J][I] and $000000FF;
    end;
  end;
end;

function IntegerTobigByte24(const lint: Integer): ArrayByte; inline;
begin
  Result[2] := (lint and $00FF0000) shr 16;
  Result[1] := (lint and $0000FF00) shr 8;
  Result[0] := lint and $000000FF;
end;

function ByteToInt(const bt: Byte): Integer; inline;
var
  bts: array [0 .. 3] of Byte;
begin
  bts[0] := $00;
  bts[1] := $00;
  bts[2] := $00;
  bts[3] := bt;
  Move(bts, Result, 4);
end;

function IntToByte(const it: Integer): Byte; inline;
begin
  Result := it and $000000FF;
end;

function StringLenthTo(const str: string; const len: Integer): string; inline;
var
  I, strlen: Integer;
begin
  Result := str;
  strlen := Length(str);
  if strlen < len then
  begin
    for I := 0 to len - strlen - 2 do
      Result := ' ' + Result;
  end
  else if strlen > len then
  begin
    Result := Copy(str, 1, 10);
  end
  else
    Result := str;
end;

constructor TTBLPackedRecord.Create(const nameValue: string);
begin
  Name[0] := AnsiChar(nameValue[1]);
  Name[1] := AnsiChar(nameValue[2]);
  Name[2] := AnsiChar(nameValue[3]);
  Name[3] := AnsiChar(nameValue[4]);
  Null := 0;
  MTUShort := 0;
  MTULong := 0;
  Identifier := 0;
end;

procedure TTBLPackedRecord.SetValue(const v: String);
var
  lint: Longint;
  dou: Double;
  I: Integer;
  dt: TDateTime;
  str: String;
begin
  case Identifier of
    0:
      begin
        lint := StrToInt(v);
        Move(lint, Data, 4);
        for I := 4 to 13 - 1 do
          Data[I] := $00;
      end;
    1:
      begin
        dou := StrToFloat(v);
        Move(dou, Data, 8);
        for I := 8 to 13 - 1 do
          Data[I] := $00;
      end;
    2:
      begin
        for I := 0 to Length(v) - 1 do
          Data[I] := Ord(v[I + 1]);
        for I := Length(v) to 12 do
          Data[I] := $00;
      end;
    3:
      begin
        for I := 0 to Length(v) - 1 do
          Data[I] := Ord(v[I + 1]);
        for I := Length(v) to 12 do
          Data[I] := $00;
      end;
    4:
      begin
        for I := 0 to Length(v) - 1 do
          Data[I] := Ord(v[I + 1]);
        for I := Length(v) to 12 do
          Data[I] := $00;
      end;
    5:
      begin
        for I := 8 to 12 do
          Data[I] := $00;
        str := v;
        dt := TBLStrToDateTime(str);
        if v = '1899-12-30T00:00:00' then
        begin
          Data[0] := 0;
          Data[1] := 0;
          Data[2] := 0;
          Data[3] := 30;
          Data[4] := 12;
          Data[5] := 99;
          Data[6] := 0;
          Data[7] := 18;
        end
        else
        begin
          Data[7] := YearOf(dt) div 100;
          Data[6] := DayOfWeek(dt) - 1;
          Data[5] := YearOf(dt) mod 100;
          Data[4] := MonthOf(dt);
          Data[3] := DayOf(dt);
          Data[2] := HourOf(dt);
          Data[1] := MinuteOf(dt);
          Data[0] := SecondOf(dt);
        end;
      end;
  end;
end;

function TTBLPackedRecord.Value: String;
var
  lint: Longint;
  dou: Double;
  dt: TDateTime;
  I: Integer;
  str: String;
begin
  try
    case Identifier of
      0:
        begin
          Move(Data, lint, 4);
          Result := IntToStr(lint);
        end;
      1:
        begin
          Move(Data, dou, 8);
          Result := FloatToStr(dou);
        end;
      2:
        begin
          str := '';
          for I := 0 to 9 - 1 do
          begin
            if Data[I] = $00 then
              Break;
            str := str + Chr(Data[I]);
          end;
          Result := str;
        end;
      3:
        begin
          str := '';
          for I := 0 to 7 do
          begin
            str := str + Chr(Data[I]);
          end;
          Result := str;
        end;
      4:
        begin
          str := '';
          for I := 0 to 12 do
          begin
            if Data[I] = $00 then
              Break;
            str := str + Chr(Data[I]);
          end;
          Result := str;
        end;
      5:
        begin
          dt := 0;
          if Data[7] <> 0 then
            dt := EncodeDateTime(Data[7] * 100 + Data[5], Data[4], Data[3], Data[2], Data[1], Data[0], 0);
          Result := DateTimeToTBLStr(dt);
        end;
    end;
  except
    raise Exception.Create('TBL Record to Data Error' + Name + IntToStr(Identifier));
  end;
end;

procedure TsData.Write(var fs: TFileStream);
var
  I, J: Integer;
  singleData: ArrayByte;
begin
  fs.Write(second, 1);
  fs.Write(minute, 1);
  fs.Write(hour, 1);
  fs.Write(day, 1);
  fs.Write(month, 1);
  fs.Write(year, 1);
  fs.Write(week, 1);
  fs.Write(century, 1);
  fs.Write(BoxSN, 2);
  fs.Write(NumberofScans, 2);
  fs.Write(NumberofChannels, 1);
  fs.Write(LengthofTS, 1);
  fs.Write(StatusCodes, 1);
  fs.Write(SaturationFlag, 1);
  fs.Write(Reservedbit, 1);
  fs.Write(Samplelength, 1);
  fs.Write(SampleRate, 2);
  fs.Write(SampleRateUnits, 1);
  fs.Write(ClockStatus, 1);
  fs.Write(ClockError, 4);
  fs.Write(keep, 6);
  for I := 0 to NumberofScans - 1 do
  begin
    for J := 0 to NumberofChannels - 1 do
    begin
      singleData := datas[J][I];
      fs.Write(singleData, 3);
    end;
  end;
end;

function TsData.ToString: string;
var
  I, J: Integer;
  singleData: ArrayByte;
begin
  Result := '';
  Result := Result + 'DateTime:       ' + DateTimeToStr(Self.DateTime) + #13#10;
  Result := Result + 'BoxSN:          ' + IntToStr(Self.BoxSN) + #13#10;
  Result := Result + 'Scans:          ' + IntToStr(Self.NumberofScans) + #13#10;
  Result := Result + 'Channels:       ' + IntToStr(Self.NumberofChannels) + #13#10;
  Result := Result + 'LengthofTS:     ' + IntToStr(Self.LengthofTS) + #13#10;
  Result := Result + 'StatusCodes:    ' + IntToStr(Self.StatusCodes) + #13#10;
  Result := Result + 'SaturationFlag: ' + IntToStr(Self.SaturationFlag) + #13#10;
  Result := Result + 'Reservedbit:    ' + IntToStr(Self.Reservedbit) + #13#10;
  Result := Result + 'Samplelength:   ' + IntToStr(Self.Samplelength) + #13#10;
  Result := Result + 'SampleRate:     ' + IntToStr(Self.SampleRate) + #13#10;
  Result := Result + 'SampleRateUnits:' + IntToStr(Self.SampleRateUnits) + #13#10;
  Result := Result + 'ClockStatus:    ' + IntToStr(Self.ClockStatus) + #13#10;
  Result := Result + 'ClockError:     ' + IntToStr(Self.ClockError) + #13#10;
  // Result := Result + IntToStr(Self.keep) + #13#10;
  for I := 0 to NumberofScans - 1 do
  begin
    for J := 0 to NumberofChannels - 1 do
    begin
      singleData := datas[J][I];
      Result := Result + StringLenthTo(IntToStr(bigByte24ToInteger(singleData)), 18) + ' ';
    end;
    Result := Result + #13#10;
  end;
end;

constructor TsData.Read(var fs: TFileStream);
var
  I, J: Integer;
  singleData: ArrayByte;
begin
  fs.Read(second, 1);
  fs.Read(minute, 1);
  fs.Read(hour, 1);
  fs.Read(day, 1);
  fs.Read(month, 1);
  fs.Read(year, 1);
  fs.Read(week, 1);
  fs.Read(century, 1);
  fs.Read(BoxSN, 2);
  fs.Read(NumberofScans, 2);
  fs.Read(NumberofChannels, 1);
  fs.Read(LengthofTS, 1);
  fs.Read(StatusCodes, 1);
  fs.Read(SaturationFlag, 1);
  fs.Read(Reservedbit, 1);
  fs.Read(Samplelength, 1);
  fs.Read(SampleRate, 2);
  fs.Read(SampleRateUnits, 1);
  fs.Read(ClockStatus, 1);
  fs.Read(ClockError, 4);
  fs.Read(keep, 6);
  SetLength(datas, NumberofChannels);
  for I := 0 to NumberofChannels - 1 do
  begin
    SetLength(datas[I], NumberofScans);
  end;
  for I := 0 to NumberofScans - 1 do
  begin
    for J := 0 to NumberofChannels - 1 do
    begin
      fs.Read(singleData, 3);
      datas[J][I] := singleData;
    end;
  end;
end;

constructor TsData.ReadFromStrings(const str: TStrings);
var
  I, J: Integer;
  datastr, tmp: string;
begin
  Self.SetDateTime(StrToDateTime(Copy(str[0], 17, Length(str[0]) - 16)));
  Self.BoxSN := StrToInt(Trim(Copy(str[1], 17, Length(str[0]) - 16)));
  Self.NumberofScans := StrToInt(Trim(Copy(str[2], 17, Length(str[0]) - 16)));
  Self.NumberofChannels := StrToInt(Trim(Copy(str[3], 17, Length(str[0]) - 16)));
  Self.LengthofTS := StrToInt(Trim(Copy(str[4], 17, Length(str[0]) - 16)));
  Self.StatusCodes := StrToInt(Trim(Copy(str[5], 17, Length(str[0]) - 16)));
  Self.SaturationFlag := StrToInt(Trim(Copy(str[6], 17, Length(str[0]) - 16)));
  Self.Reservedbit := StrToInt(Trim(Copy(str[7], 17, Length(str[0]) - 16)));
  Self.Samplelength := StrToInt(Trim(Copy(str[8], 17, Length(str[0]) - 16)));
  Self.SampleRate := StrToInt(Trim(Copy(str[9], 17, Length(str[0]) - 16)));
  Self.SampleRateUnits := StrToInt(Trim(Copy(str[10], 17, Length(str[0]) - 16)));
  Self.ClockStatus := StrToInt(Trim(Copy(str[11], 17, Length(str[0]) - 16)));
  Self.ClockError := StrToInt(Trim(Copy(str[12], 17, Length(str[0]) - 16)));
  Self.keep[0] := $00;
  Self.keep[1] := $00;
  Self.keep[2] := $00;
  Self.keep[3] := $00;
  Self.keep[4] := $00;
  Self.keep[5] := $00;
  SetLength(datas, NumberofChannels);
  for I := 0 to NumberofChannels - 1 do
  begin
    SetLength(datas[I], NumberofScans);
  end;
  for I := 0 to NumberofScans - 1 do
  begin
    datastr := str[13 + I];
    for J := 0 to NumberofChannels - 1 do
    begin
      tmp := Trim(Copy(datastr, J * 18 + 1, 18));
      datas[J][I] := IntegerTobigByte24(StrToInt(tmp));
    end;
  end;
end;

function TsData.GetData(IndexA, IndexB: Integer): Integer;
begin
  Result := bigByte24ToInteger(datas[IndexA][IndexB]);
end;

function TsData.GetChDataI(Index: Integer): Integer1D;
var
  I: Integer;
begin
  SetLength(Result, NumberofScans);
  for I := 0 to NumberofScans - 1 do
  begin
    Result[I] := bigByte24ToInteger(datas[Index][I]);
  end;
end;

function TsData.GetChDataD(Index: Integer): Double1D;
var
  I: Integer;
begin
  SetLength(Result, NumberofScans);
  for I := 0 to NumberofScans - 1 do
  begin
    Result[I] := bigByte24ToInteger(datas[Index][I]);
  end;
end;

function TsData.GetDateTime: TDateTime;
begin
  Result := EncodeDate(century * 100 + year, month, day) + EncodeTime(hour, minute, second, 0);
end;

procedure TsData.SetChDataD(Index: Integer; const Value: Double1D);
var
  I: Integer;
begin
  for I := 0 to NumberofScans - 1 do
  begin
    datas[Index][I] := IntegerTobigByte24(Round(Value[I]));
  end;
end;

procedure TsData.SetChDataI(Index: Integer; const Value: Integer1D);
var
  I: Integer;
begin
  for I := 0 to NumberofScans - 1 do
  begin
    datas[Index][I] := IntegerTobigByte24(Value[I]);
  end;
end;

procedure TsData.SetData(IndexA, IndexB: Integer; const Value: Integer);
begin
  datas[IndexA][IndexB] := IntegerTobigByte24(Value);
end;

procedure TsData.SetDateTime(const Value: TDateTime);
var
  yy, ce, yyy, mon, dd, ww, hh, min, sec, msec: Word;
begin
  DecodeDateFully(Value, yy, mon, dd, ww);
  ce := yy div 100;
  yyy := yy mod 100;
  Move(ce, century, 1);
  Move(yyy, year, 1);
  Move(mon, month, 1);
  Move(dd, day, 1);
  Move(ww, week, 1);
  week := week - 1;
  if week = 0 then
    week := 7;
  DecodeTime(Value, hh, min, sec, msec);
  Move(hh, hour, 1);
  Move(min, minute, 1);
  Move(sec, second, 1);
end;

constructor TTsn.Create(const fileNameV: string);
begin
  FData := TList<TsData>.Create;
  Self.LoadFrom(fileNameV);
end;

procedure TTsn.DeleteData(const Index, len: Integer);
begin
  FData.DeleteRange(index, len);
end;

destructor TTsn.Destroy;
begin
  inherited;
  FData.Free;
end;

procedure TTsn.AppendTs(const ats: TTsn);
begin
  Self.FData.AddRange(ats.FData)
end;

function TTsn.Count: Integer;
begin
  Result := FData.Count;
end;

constructor TTsn.Create;
begin
  FData := TList<TsData>.Create;
end;

procedure TTsn.AddData(dat: TsData);
begin
  FData.Add(dat);
end;

function TTsn.GetData(Index: Integer): TsData;
begin
  Result := FData[Index];
end;

procedure TTsn.LoadFrom(const FileName: string);
begin
  try
    if LowerCase(Copy(FileName, Length(FileName) - 3, 3)) = '.ts' then
      Self.ReadBin(FileName)
    else
      Self.ReadText(FileName);
  except
    raise Exception.Create('Create TSN Error : ' + FileName);
  end;
end;

procedure TTsn.ReadBin(const FileName: string);
var
  firstTs: TsData;
  I, FCount: Integer;
  FStream: TFileStream;
begin
  FStream := TFileStream.Create(FileName, fmOpenRead);
  firstTs := TsData.Read(FStream);
  FBoxSN := firstTs.BoxSN;
  FChannels := firstTs.NumberofChannels;
  FSamplingRate := firstTs.SampleRate;
  FScans := firstTs.NumberofScans;
  FLengthPre := FChannels * FScans * 3 + 32;
  FCount := FStream.Size div FLengthPre;
  for I := 0 to FCount - 1 do
  begin
    FStream.Position := I * FLengthPre;
    FData.Add(TsData.Read(FStream));
    FOnReadWriteProgress(Self, I, FCount);
  end;
  FStream.Free;
end;

procedure TTsn.ReadText(const FileName: string);
var
  I, J, tscount: Integer;
  tfile: TextFile;
  tmp: string;
  tsrecordstr: TStrings;
begin
  AssignFile(tfile, FileName);
  Reset(tfile);
  Readln(tfile, tmp);
  tmp := Copy(tmp, 17, Length(tmp) - 16);
  tscount := StrToInt(Trim(tmp));
  Readln(tfile, tmp);
  Readln(tfile, tmp);
  Readln(tfile, tmp);
  tmp := Copy(tmp, 17, Length(tmp) - 16);
  FBoxSN := StrToInt(Trim(tmp));
  Readln(tfile, tmp);
  tmp := Copy(tmp, 17, Length(tmp) - 16);
  FScans := StrToInt(Trim(tmp));
  Readln(tfile, tmp);
  tmp := Copy(tmp, 17, Length(tmp) - 16);
  FChannels := StrToInt(Trim(tmp));
  Readln(tfile, tmp);
  tmp := Copy(tmp, 17, Length(tmp) - 16);
  FLengthPre := StrToInt(Trim(tmp));
  Readln(tfile, tmp);
  Readln(tfile, tmp);
  Readln(tfile, tmp);
  Readln(tfile, tmp);
  Readln(tfile, tmp);
  tmp := Copy(tmp, 17, Length(tmp) - 16);
  FSamplingRate := StrToInt(Trim(tmp));
  CloseFile(tfile);
  Reset(tfile);
  Readln(tfile, tmp);
  tsrecordstr := TStringList.Create;
  for I := 0 to tscount - 1 do
  begin
    Readln(tfile, tmp);
    tsrecordstr.Clear;
    for J := 0 to 13 + FScans - 1 do
    begin
      Readln(tfile, tmp);
      tsrecordstr.Add(tmp);
    end;
    FData.Add(TsData.ReadFromStrings(tsrecordstr));
    FOnReadWriteProgress(Self, I, tscount);
  end;
  CloseFile(tfile);
end;

procedure TTsn.SetData(Index: Integer; const Value: TsData);
begin
  FData[Index] := Value;
end;

procedure TTsn.SaveTo(const FileName: string);
begin
  try
    if LowerCase(Copy(FileName, Length(FileName) - 3, 3)) = '.ts' then
      Self.WriteBin(FileName)
    else
      Self.WriteText(FileName);
  except
    raise Exception.Create('SaveTo TSN Error : ' + FileName);
  end;
end;

procedure TTsn.WriteBin(const FileName: string);
var
  tsdat: TsData;
  I: Integer;
  FStream: TFileStream;
begin
  FStream := TFileStream.Create(FileName, fmCreate);
  for I := 0 to Self.Count - 1 do
  begin
    tsdat := FData[I];
    tsdat.Write(FStream);
    FOnReadWriteProgress(Self, I, Self.Count);
  end;
  FStream.Free;
end;

procedure TTsn.WriteText(const FileName: string);
var
  tfile: TextFile;
  tsdat: TsData;
  I: Integer;
begin
  AssignFile(tfile, FileName);
  Rewrite(tfile);
  Writeln(tfile, 'Record Count:   ' + IntToStr(Self.Count));
  for I := 0 to Self.Count - 1 do
  begin
    tsdat := FData[I];
    Writeln(tfile, 'RecordNumber:  ' + IntToStr(I + 1));
    Write(tfile, tsdat.ToString);
    FOnReadWriteProgress(Self, I, Self.Count);
  end;
  CloseFile(tfile);
end;

constructor TTSNRead.Create(const fileNameV: string);
var
  firstTs: TsData;
begin
  FFileName := fileNameV;
  FStream := TFileStream.Create(FFileName, fmOpenReadWrite);
  firstTs := TsData.Read(FStream);

  FBoxSN := firstTs.BoxSN;
  FChannels := firstTs.NumberofChannels;
  FSamplingRate := firstTs.SampleRate;
  FScans := firstTs.NumberofScans;
  FLengthPre := FChannels * FScans * 3 + 32;
  FCount := FStream.Size div FLengthPre;
end;

destructor TTSNRead.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TTSNRead.GetData(Index: Integer): TsData;
begin
  FStream.Position := Index * FLengthPre;
  Result := TsData.Read(FStream);
end;

procedure TTSNRead.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TTSNRead.SetData(Index: Integer; const Value: TsData);
begin
  FStream.Position := Index * FLengthPre;
  Value.Write(FStream);
end;

procedure TTSNWrite.AppendData(tmp: TsData);
begin
  FStream.Position := FCount * FLengthPre;
  Inc(FCount);
  tmp.Write(FStream);
end;

procedure TTSNWrite.AppendTsFile(const fileNameV: String);
var
  tmp: TTSNRead;
  I: Integer;
  TsStream: TFileStream;
begin
  TsStream := TFileStream.Create(fileNameV, fmOpenRead or fmShareExclusive);
  FStream.Position := FCount * FLengthPre;
  FStream.CopyFrom(TsStream, TsStream.Size);
  TsStream.Free;
  tmp := TTSNRead.Create(fileNameV);
  FCount := FCount + tmp.Count;
  tmp.Free;
end;

constructor TTSNWrite.Create(const fileNameV: string; const LenPre: Integer);
begin
  FFileName := fileNameV;
  FStream := TFileStream.Create(FFileName, fmCreate);
  FLengthPre := LenPre;
  FCount := 0;
end;

destructor TTSNWrite.Destroy;
begin
  FStream.Free;
  inherited;
end;

end.
