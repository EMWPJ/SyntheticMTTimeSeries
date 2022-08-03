unit TimeSeries;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Generics.Collections, System.DateUtils,
  WTypes, Phoenix, PhoenixB, MathFunctions;

const
  TimeSeries_Phoneix = 0;
  TimeSeries_LEMI = 1;
  TimeSeries_Synthetic = 100;

type

  TSiteTimeSeries = class;
  TTimeSeries = class;
  TContinuousTimeSeries = class;
  TMultipleTimeSeries = class;

  TSiteTimeSeries = class(TObject)
    procedure LoadFrom(fname: string);
    procedure LoadFromPhoneix(fname: string);
    procedure SaveGMTTS(path, name: string; absolutetime: Boolean = True);
    procedure SavePhoenix(path: string; tbl: TTBL; name: string);
  private
    fname: String;
    FTimeSeriess: TList<TTimeSeries>;
    FLongitude: Double;
    FLatitude: Double;
    FAltitude: Double;
    FBeginTime: TDateTime;
    FEndTime: TDateTime;
    FDataType: Byte;
    procedure SetName(const _Value: String);
    procedure SetTimeSeriess(const _Value: TList<TTimeSeries>);
    function GetTimeSeries(Index: Integer): TTimeSeries;
    procedure SetTimeSeries(Index: Integer; const _Value: TTimeSeries);
    procedure SetLongitude(const _Value: Double);
    procedure SetLatitude(const _Value: Double);
    procedure SetAltitude(const _Value: Double);
    procedure SetBeginTime(const _Value: TDateTime);
    procedure SetEndTime(const _Value: TDateTime);
    procedure SetDataType(const _Value: Byte);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTimeSeries(_Value: TTimeSeries);
    function AddNewContinuousTimeSeries: TContinuousTimeSeries;
    function AddNewMultipleTimeSeries: TMultipleTimeSeries;
    procedure TimeSeriesClear;
    function TimeSeriesCount: Integer;
    procedure RemoveTimeSeries(_Value: TTimeSeries);
    procedure DeleteTimeSeries(Index: Integer);
    property Name: String read fname write SetName;
    property TimeSeriess: TList<TTimeSeries> read FTimeSeriess write SetTimeSeriess;
    property TimeSeries[Index: Integer]: TTimeSeries read GetTimeSeries write SetTimeSeries;
    property Longitude: Double read FLongitude write SetLongitude;
    property Latitude: Double read FLatitude write SetLatitude;
    property Altitude: Double read FAltitude write SetAltitude;
    property BeginTime: TDateTime read FBeginTime write SetBeginTime;
    property EndTime: TDateTime read FEndTime write SetEndTime;
    property DataType: Byte read FDataType write SetDataType;
  end;

  TTimeSeries = class abstract(TObject)
    function Fields: Double3D;
    function GetTimes: Double1D;
    procedure SaveGMTTS(path, name: string; absolutetime: Boolean = True);
    procedure ToPhoenixTS(path: string; tbl: TTBL);
  private
    fname: String;
    FBeginTimes: TList<TDateTime>;
    FEndTimes: TList<TDateTime>;
    FSampleRate: Double;
    FExField: Double2D;
    FEyField: Double2D;
    FHxField: Double2D;
    FHyField: Double2D;
    FHzField: Double2D;
    procedure SetName(const _Value: String);
    procedure SetBeginTimes(const _Value: TList<TDateTime>);
    function GetBeginTime(Index: Integer): TDateTime;
    procedure SetBeginTime(Index: Integer; const _Value: TDateTime);
    procedure SetEndTimes(const _Value: TList<TDateTime>);
    function GetEndTime(Index: Integer): TDateTime;
    procedure SetEndTime(Index: Integer; const _Value: TDateTime);
    procedure SetSampleRate(const _Value: Double);
    procedure SetExField(const _Value: Double2D);
    procedure SetEyField(const _Value: Double2D);
    procedure SetHxField(const _Value: Double2D);
    procedure SetHyField(const _Value: Double2D);
    procedure SetHzField(const _Value: Double2D);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddBeginTime(_Value: TDateTime);
    function AddNewBeginTime: TDateTime;
    procedure BeginTimeClear;
    function BeginTimeCount: Integer;
    procedure RemoveBeginTime(_Value: TDateTime);
    procedure DeleteBeginTime(Index: Integer);
    procedure AddEndTime(_Value: TDateTime);
    function AddNewEndTime: TDateTime;
    procedure EndTimeClear;
    function EndTimeCount: Integer;
    procedure RemoveEndTime(_Value: TDateTime);
    procedure DeleteEndTime(Index: Integer);
    function GetData(const bIndex: Integer = 0; const eIndex: Integer = -1): Double2D;
    function IndexOf(const dt: TDateTime): Integer;
    property Name: String read fname write SetName;
    property BeginTimes: TList<TDateTime> read FBeginTimes write SetBeginTimes;
    property BeginTime[Index: Integer]: TDateTime read GetBeginTime write SetBeginTime;
    property EndTimes: TList<TDateTime> read FEndTimes write SetEndTimes;
    property EndTime[Index: Integer]: TDateTime read GetEndTime write SetEndTime;
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property ExField: Double2D read FExField write SetExField;
    property EyField: Double2D read FEyField write SetEyField;
    property HxField: Double2D read FHxField write SetHxField;
    property HyField: Double2D read FHyField write SetHyField;
    property HzField: Double2D read FHzField write SetHzField;
  end;

  TContinuousTimeSeries = class(TTimeSeries)
    procedure FromPhoenixTS(fname: string);
    procedure ToPhoenixTS(path: string; tbl: TTBL);
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TMultipleTimeSeries = class(TTimeSeries)
    procedure FromPhoenixTS(fname: string);
    procedure ToPhoenixTS(path: string; tbl: TTBL);
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ SiteTimeSeries }
constructor TSiteTimeSeries.Create;
begin
  FTimeSeriess := TList<TTimeSeries>.Create;
end;

destructor TSiteTimeSeries.Destroy;
begin
  TimeSeriesClear;
  FTimeSeriess.Free;
  inherited;
end;

procedure TSiteTimeSeries.SetName(const _Value: String);
begin
  fname := _Value;
end;

procedure TSiteTimeSeries.SetTimeSeriess(const _Value: TList<TTimeSeries>);
begin
  TimeSeriesClear;
  FTimeSeriess := _Value;
end;

function TSiteTimeSeries.GetTimeSeries(Index: Integer): TTimeSeries;
begin
  Result := FTimeSeriess[Index];
end;

procedure TSiteTimeSeries.SetTimeSeries(Index: Integer; const _Value: TTimeSeries);
begin
  FTimeSeriess[Index].Free;
  FTimeSeriess[Index] := _Value;
end;

procedure TSiteTimeSeries.AddTimeSeries(_Value: TTimeSeries);
begin;
  FTimeSeriess.Add(_Value);
end;

function TSiteTimeSeries.AddNewContinuousTimeSeries: TContinuousTimeSeries;
var
  ContinuousTimeSeriestmp: TContinuousTimeSeries;
begin;
  ContinuousTimeSeriestmp := TContinuousTimeSeries.Create;
  FTimeSeriess.Add(ContinuousTimeSeriestmp);
  Result := ContinuousTimeSeriestmp;
end;

function TSiteTimeSeries.AddNewMultipleTimeSeries: TMultipleTimeSeries;
var
  MultipleTimeSeriestmp: TMultipleTimeSeries;
begin;
  MultipleTimeSeriestmp := TMultipleTimeSeries.Create;
  FTimeSeriess.Add(MultipleTimeSeriestmp);
  Result := MultipleTimeSeriestmp;
end;

procedure TSiteTimeSeries.TimeSeriesClear;
begin
  while FTimeSeriess.Count > 0 do
  begin
    FTimeSeriess.Items[0].Free;
    FTimeSeriess.Delete(0);
  end;
end;

function TSiteTimeSeries.TimeSeriesCount: Integer;
begin
  Result := FTimeSeriess.Count;
end;

procedure TSiteTimeSeries.RemoveTimeSeries(_Value: TTimeSeries);
begin
  FTimeSeriess.Remove(_Value);
  _Value.Free;
end;

procedure TSiteTimeSeries.DeleteTimeSeries(Index: Integer);
begin
  FTimeSeriess.Items[Index].Free;
  FTimeSeriess.Delete(Index);
end;

procedure TSiteTimeSeries.LoadFrom(fname: string);
var
  ext: string;
begin
  ext := ExtractFileExt(fname).ToLower;
  if ext = '.tbl' then
  begin
    Self.DataType := TimeSeries_Phoneix;
    Self.LoadFromPhoneix(fname);
  end;
end;

procedure TSiteTimeSeries.LoadFromPhoneix(fname: string);
var
  tbl: TTBL;
  cts: TContinuousTimeSeries;
  mts: TMultipleTimeSeries;
begin
  tbl := TTBL.Create;
  tbl.LoadTBL(fname);
  Self.name := tbl.name;
  case tbl.MTMode of
    PNX_AMT:
      begin
        mts := Self.AddNewMultipleTimeSeries;
        mts.FromPhoenixTS(tbl.path + '/' + tbl.TSFileName + '.TS2');
        mts.name := 'TS2';
        mts := Self.AddNewMultipleTimeSeries;
        mts.FromPhoenixTS(tbl.path + '/' + tbl.TSFileName + '.TS3');
        mts.name := 'TS3';
        cts := Self.AddNewContinuousTimeSeries;
        cts.FromPhoenixTS(tbl.path + '/' + tbl.TSFileName + '.TS4');
        cts.name := 'TS4';
      end;
    PNX_MT:
      begin
        mts := Self.AddNewMultipleTimeSeries;
        mts.FromPhoenixTS(tbl.path + '/' + tbl.TSFileName + '.TS3');
        mts.name := 'TS3';
        mts := Self.AddNewMultipleTimeSeries;
        mts.FromPhoenixTS(tbl.path + '/' + tbl.TSFileName + '.TS4');
        mts.name := 'TS4';
        cts := Self.AddNewContinuousTimeSeries;
        cts.FromPhoenixTS(tbl.path + '/' + tbl.TSFileName + '.TS5');
        cts.name := 'TS5';
      end;
  end;
  tbl.Free;
end;

procedure TSiteTimeSeries.SaveGMTTS(path, name: string; absolutetime: Boolean = True);
var
  I: Integer;
begin;
  for I := 0 to Self.TimeSeriesCount - 1 do
  begin
    Self.TimeSeries[I].SaveGMTTS(path, name + '.STS' + (I + 1).ToString, absolutetime);
  end;
end;

procedure TSiteTimeSeries.SavePhoenix(path: string; tbl: TTBL; name: string);
var
  I: Integer;
begin
  tbl.name := name;
  tbl.TSFileName := name;
  tbl.SITE := name;
  tbl.SaveTBL(path + tbl.name + '.TBL');
  for I := 0 to Self.TimeSeriesCount - 1 do
  begin
    Self.TimeSeries[I].ToPhoenixTS(path, tbl);
  end;
end;

procedure TSiteTimeSeries.SetLongitude(const _Value: Double);
begin
  FLongitude := _Value;
end;

procedure TSiteTimeSeries.SetLatitude(const _Value: Double);
begin
  FLatitude := _Value;
end;

procedure TSiteTimeSeries.SetAltitude(const _Value: Double);
begin
  FAltitude := _Value;
end;

procedure TSiteTimeSeries.SetBeginTime(const _Value: TDateTime);
begin
  FBeginTime := _Value;
end;

procedure TSiteTimeSeries.SetEndTime(const _Value: TDateTime);
begin
  FEndTime := _Value;
end;

procedure TSiteTimeSeries.SetDataType(const _Value: Byte);
begin
  FDataType := _Value;
end;

{ TimeSeries }
constructor TTimeSeries.Create;
begin
  FBeginTimes := TList<TDateTime>.Create;
  FEndTimes := TList<TDateTime>.Create;
end;

destructor TTimeSeries.Destroy;
begin
  FBeginTimes.Free;
  FEndTimes.Free;
  FExField := nil;
  FEyField := nil;
  FHxField := nil;
  FHyField := nil;
  FHzField := nil;
  inherited;
end;

procedure TTimeSeries.SetName(const _Value: String);
begin
  fname := _Value;
end;

procedure TTimeSeries.SetBeginTimes(const _Value: TList<TDateTime>);
begin
  FBeginTimes.Clear;
  FBeginTimes := _Value;
end;

function TTimeSeries.GetBeginTime(Index: Integer): TDateTime;
begin
  Result := FBeginTimes[Index];
end;

procedure TTimeSeries.SetBeginTime(Index: Integer; const _Value: TDateTime);
begin
  FBeginTimes[Index] := _Value;
end;

procedure TTimeSeries.AddBeginTime(_Value: TDateTime);
begin;
  FBeginTimes.Add(_Value);
end;

function TTimeSeries.AddNewBeginTime: TDateTime;
var
  BeginTimetmp: TDateTime;
begin;
  FBeginTimes.Add(BeginTimetmp);
  Result := BeginTimetmp;
end;

procedure TTimeSeries.BeginTimeClear;
begin
  FBeginTimes.Clear;
end;

function TTimeSeries.BeginTimeCount: Integer;
begin
  Result := FBeginTimes.Count;
end;

procedure TTimeSeries.RemoveBeginTime(_Value: TDateTime);
begin
  FBeginTimes.Remove(_Value);
end;

procedure TTimeSeries.DeleteBeginTime(Index: Integer);
begin
  FBeginTimes.Delete(Index);
end;

procedure TTimeSeries.SetEndTimes(const _Value: TList<TDateTime>);
begin
  FEndTimes.Clear;
  FEndTimes := _Value;
end;

function TTimeSeries.GetEndTime(Index: Integer): TDateTime;
begin
  Result := FEndTimes[Index];
end;

procedure TTimeSeries.SetEndTime(Index: Integer; const _Value: TDateTime);
begin
  FEndTimes[Index] := _Value;
end;

procedure TTimeSeries.AddEndTime(_Value: TDateTime);
begin;
  FEndTimes.Add(_Value);
end;

function TTimeSeries.AddNewEndTime: TDateTime;
var
  EndTimetmp: TDateTime;
begin;
  FEndTimes.Add(EndTimetmp);
  Result := EndTimetmp;
end;

procedure TTimeSeries.EndTimeClear;
begin
  FEndTimes.Clear;
end;

function TTimeSeries.EndTimeCount: Integer;
begin
  Result := FEndTimes.Count;
end;

procedure TTimeSeries.RemoveEndTime(_Value: TDateTime);
begin
  FEndTimes.Remove(_Value);
end;

procedure TTimeSeries.DeleteEndTime(Index: Integer);
begin
  FEndTimes.Delete(Index);
end;

function TTimeSeries.Fields: Double3D;
var
  I, J, scount: Integer;
begin
  scount := Self.BeginTimeCount;
  SetLength(Result, scount);
  for I := 0 to scount - 1 do
  begin
    SetLength(Result[I], 5);
    Result[I][0] := Self.ExField[I];
    Result[I][1] := Self.EyField[I];
    Result[I][2] := Self.HxField[I];
    Result[I][3] := Self.HyField[I];
    Result[I][4] := Self.HzField[I];
  end;
end;

function TTimeSeries.GetData(const bIndex: Integer = 0; const eIndex: Integer = -1): Double2D;
var
  I, J, beginindex, endIndex, Scans: Integer;
begin
  beginindex := 0;
  endIndex := 0;
  SetLength(Result, 5);
  Scans := Length(Self.ExField[0]);
  if bIndex < 0 then
    beginindex := 0;
  if eIndex = -1 then
    endIndex := Self.BeginTimeCount - 1;
  for I := 0 to 5 - 1 do
  begin
    SetLength(Result[I], (endIndex - beginindex + 1) * Scans);
  end;
  for I := beginindex to endIndex do
  begin
    Move(Self.ExField[I][0], Result[0][(I - beginindex) * Scans], Scans * 8);
    Move(Self.EyField[I][0], Result[1][(I - beginindex) * Scans], Scans * 8);
    Move(Self.HxField[I][0], Result[2][(I - beginindex) * Scans], Scans * 8);
    Move(Self.HyField[I][0], Result[3][(I - beginindex) * Scans], Scans * 8);
    Move(Self.HzField[I][0], Result[4][(I - beginindex) * Scans], Scans * 8);
  end;
end;

function TTimeSeries.GetTimes: Double1D;
var
  I, J, Scans, Count: Integer;
  strs: TStringList;
  str: String;
  inctime: Double;
begin
  inctime := 1 / 24 / 3600 / Self.SampleRate;
  Count := 0;
  for I := 0 to Self.BeginTimeCount - 1 do
  begin
    Scans := Length(Self.ExField[I]);
    SetLength(Result, Count + Scans);
    for J := 0 to Scans - 1 do
    begin
      Result[Count + J] := Self.BeginTime[I] + J * inctime;
    end;
    Count := Count + Scans;
  end;
end;

function TTimeSeries.IndexOf(const dt: TDateTime): Integer;
var
  I, J, tmpIndex: Integer;
begin
  if (dt < Self.BeginTime[0]) or (dt > Self.BeginTime[Self.BeginTimeCount - 1]) then
  begin
    Result := -1;
    Exit;
  end;
  tmpIndex := Round(SecondsBetween(Self.BeginTime[0], dt) * Self.BeginTimeCount / SecondsBetween(Self.BeginTime[0],
    Self.BeginTime[Self.BeginTimeCount - 1]));
  if tmpIndex < 0 then
    tmpIndex := 0;
  if tmpIndex > Self.BeginTimeCount - 1 then
    tmpIndex := Self.BeginTimeCount - 1;
  while True do
  begin
    if Self.BeginTime[tmpIndex] = dt then
    begin
      Result := tmpIndex;
      Exit;
    end
    else if Self.BeginTime[tmpIndex] < dt then
    begin
      Inc(tmpIndex);
    end
    else if Self.BeginTime[tmpIndex] > dt then
    begin
      Dec(tmpIndex);
    end;
    if (tmpIndex < 0) or (tmpIndex >= Self.BeginTimeCount) then
    begin
      Result := -1;
      Exit;
    end;
  end;
end;

procedure TTimeSeries.SaveGMTTS(path, name: string; absolutetime: Boolean = True);
var
  I, J, Scans: Integer;
  strs: TStringList;
  str: String;
  inctime: Double;
const
  GMTDTMFormat: string = 'yyyy-mm-dd"T"hh:nn:ss.zzz';
begin
  strs := TStringList.Create;
  if absolutetime then
  begin
    inctime := 1 / 24 / 3600 / Self.SampleRate;
    for I := 0 to Self.BeginTimeCount - 1 do
    begin
      Scans := Length(Self.ExField[I]);
      // strs.Add('> ');
      for J := 0 to Scans - 1 do
      begin
        str := FormatDateTime(GMTDTMFormat, Self.BeginTime[I] + J * inctime);
        str := str + ' ' + FloatToStrF(Self.ExField[I][J], ffExponent, 10, 5);
        str := str + ' ' + FloatToStrF(Self.EyField[I][J], ffExponent, 10, 5);
        str := str + ' ' + FloatToStrF(Self.HxField[I][J], ffExponent, 10, 5);
        str := str + ' ' + FloatToStrF(Self.HyField[I][J], ffExponent, 10, 5);
        str := str + ' ' + FloatToStrF(Self.HzField[I][J], ffExponent, 10, 5);
        strs.Add(str);
      end;
    end;
  end
  else
  begin
    inctime := 1 / Self.SampleRate;
    for I := 0 to Self.BeginTimeCount - 1 do
    begin
      Scans := Length(Self.ExField[I]);
      strs.Add('> ');
      for J := 0 to Scans - 1 do
      begin
        str := FloatToStrF(I * Scans * inctime + J * inctime, ffExponent, 10, 5);
        str := str + ' ' + FloatToStrF(Self.ExField[I][J], ffExponent, 10, 5);
        str := str + ' ' + FloatToStrF(Self.EyField[I][J], ffExponent, 10, 5);
        str := str + ' ' + FloatToStrF(Self.HxField[I][J], ffExponent, 10, 5);
        str := str + ' ' + FloatToStrF(Self.HyField[I][J], ffExponent, 10, 5);
        str := str + ' ' + FloatToStrF(Self.HzField[I][J], ffExponent, 10, 5);
        strs.Add(str);
      end;
    end;
  end;
  strs.SaveToFile(path + name);
  strs.Free;
end;

procedure TTimeSeries.SetSampleRate(const _Value: Double);
begin
  FSampleRate := _Value;
end;

procedure TTimeSeries.SetExField(const _Value: Double2D);
begin
  FExField := _Value;
end;

procedure TTimeSeries.SetEyField(const _Value: Double2D);
begin
  FEyField := _Value;
end;

procedure TTimeSeries.SetHxField(const _Value: Double2D);
begin
  FHxField := _Value;
end;

procedure TTimeSeries.SetHyField(const _Value: Double2D);
begin
  FHyField := _Value;
end;

procedure TTimeSeries.SetHzField(const _Value: Double2D);
begin
  FHzField := _Value;
end;

{ TTimeSeries }

procedure TTimeSeries.ToPhoenixTS(path: string; tbl: TTBL);
begin
  if Self is TContinuousTimeSeries then
  begin
    TContinuousTimeSeries(Self).ToPhoenixTS(path, tbl);
  end
  else if Self is TMultipleTimeSeries then
  begin
    TMultipleTimeSeries(Self).ToPhoenixTS(path, tbl);
  end;
end;

{ ContinuousTimeSeries }
constructor TContinuousTimeSeries.Create;
begin
  inherited Create;
  
end;

destructor TContinuousTimeSeries.Destroy;
begin
  inherited;
end;

procedure TContinuousTimeSeries.FromPhoenixTS(fname: string);
var
  ts: TTS;
  J, K, Records: Integer;
  tr: TTSRecord;
  dt: Integer2D;
  I: Integer;
  Ex: Double2D;
  Ey: Double2D;
  Hx: Double2D;
  Hy: Double2D;
  Hz: Double2D;
begin
  ts := TTS.Create;
  ts.LoadFile(fname);
  Self.name := ExtractFileName(fname);
  Self.BeginTimeClear;
  Self.EndTimeClear;
  Self.AddBeginTime(ts.BeginTime);
  Self.AddEndTime(ts.EndTime);
  Self.SampleRate := ts.SampleRate;
  SetLength(Ex, 1);
  SetLength(Ey, 1);
  SetLength(Hx, 1);
  SetLength(Hy, 1);
  SetLength(Hz, 1);
  SetLength(Ex[0], ts.TSRecordCount * ts.Scans);
  SetLength(Ey[0], ts.TSRecordCount * ts.Scans);
  SetLength(Hx[0], ts.TSRecordCount * ts.Scans);
  SetLength(Hy[0], ts.TSRecordCount * ts.Scans);
  SetLength(Hz[0], ts.TSRecordCount * ts.Scans);
  for I := 0 to ts.TSRecordCount - 1 do
  begin
    tr := ts.TSRecord[I];
    for K := 0 to ts.Scans - 1 do
    begin
      Ex[0][I * ts.Scans + K] := tr.Data[0][K];
      Ey[0][I * ts.Scans + K] := tr.Data[1][K];
      Hx[0][I * ts.Scans + K] := tr.Data[2][K];
      Hy[0][I * ts.Scans + K] := tr.Data[3][K];
      Hz[0][I * ts.Scans + K] := tr.Data[4][K];
    end;
  end;
  Self.ExField := Ex;
  Self.EyField := Ey;
  Self.HxField := Hx;
  Self.HyField := Hy;
  Self.HzField := Hz;
end;

procedure TContinuousTimeSeries.ToPhoenixTS(path: string; tbl: TTBL);
var
  ts: TTS;
  J, K, Records: Integer;
  tr: TTSRecord;
  dt: Integer2D;
begin
  ts := TTS.Create;
  if Self.name = 'TS4' then
  begin
    ts.BeginTime := Self.BeginTime[0];
    ts.EndTime := Self.EndTime[0];
    ts.Scans := 150;
    ts.Channels := 5;
    ts.Samplelength := 3;
    ts.SampleRate := 150;
    ts.BoxSN := tbl.SNUM.ToInteger;
    ts.SampleRateUnits := '0';
    ts.TSLength := 32;
    ts.fileName := tbl.TSFileName;
  end
  else if Self.name = 'TS5' then
  begin
    ts.BeginTime := Self.BeginTime[0];
    ts.EndTime := Self.EndTime[0];
    ts.Scans := 15;
    ts.Channels := 5;
    ts.Samplelength := 3;
    ts.SampleRate := 15;
    ts.BoxSN := tbl.SNUM.ToInteger;
    ts.SampleRateUnits := '0';
    ts.TSLength := 32;
    ts.fileName := tbl.TSFileName;
  end;

  Records := Length(Self.ExField[0]) div ts.Scans;
  for J := 0 to Records - 1 do
  begin
    tr := ts.AddNewTSRecord;
    tr.DateTime := Self.BeginTime[0] + ts.Scans * J / Self.SampleRate / 3600 / 24;
    tr.BoxSN := ts.BoxSN;
    tr.Scans := ts.Scans;
    tr.Channels := ts.Channels;
    tr.TSLength := ts.TSLength;
    tr.StatusCodes := 0;
    tr.SaturationFlag := 0;
    tr.Reservedbit := 0;
    tr.Samplelength := ts.Samplelength;
    tr.SampleRate := ts.SampleRate;
    tr.SampleRateUnits := 0;
    tr.ClockStatus := 4;
    tr.ClockError := 0;
    dt := MakeInteger2D(ts.Channels, ts.Scans);
    for K := 0 to ts.Scans - 1 do
    begin
      dt[0][K] := Trunc(Self.ExField[0][J * ts.Scans + K]);
      dt[1][K] := Trunc(Self.EyField[0][J * ts.Scans + K]);
      dt[2][K] := Trunc(Self.HxField[0][J * ts.Scans + K]);
      dt[3][K] := Trunc(Self.HyField[0][J * ts.Scans + K]);
      dt[4][K] := Trunc(Self.HzField[0][J * ts.Scans + K]);
    end;
    tr.Data := dt;
  end;
  ts.SaveTo(path + tbl.TSFileName + '.' + Self.name);
  ts.Free;
end;

{ MultipleTimeSeries }
constructor TMultipleTimeSeries.Create;
begin
  inherited Create;
  
end;

destructor TMultipleTimeSeries.Destroy;
begin
  inherited;
end;

{ TMultipleTimeSeries }

procedure TMultipleTimeSeries.FromPhoenixTS(fname: string);
var
  ts: TTS;
  J, K, Records: Integer;
  tr: TTSRecord;
  dt: Integer2D;
  I: Integer;
  Ex: Double2D;
  Ey: Double2D;
  Hx: Double2D;
  Hy: Double2D;
  Hz: Double2D;
begin
  ts := TTS.Create;
  ts.LoadFile(fname);
  Self.name := ExtractFileName(fname);
  Self.BeginTimeClear;
  Self.EndTimeClear;
  Self.SampleRate := ts.SampleRate;
  SetLength(Ex, ts.TSRecordCount);
  SetLength(Ey, ts.TSRecordCount);
  SetLength(Hx, ts.TSRecordCount);
  SetLength(Hy, ts.TSRecordCount);
  SetLength(Hz, ts.TSRecordCount);
  for I := 0 to ts.TSRecordCount - 1 do
  begin
    tr := ts.TSRecord[I];
    SetLength(Ex[I], tr.Scans);
    SetLength(Ey[I], tr.Scans);
    SetLength(Hx[I], tr.Scans);
    SetLength(Hy[I], tr.Scans);
    SetLength(Hz[I], tr.Scans);
    Self.AddBeginTime(tr.DateTime);
    Self.AddEndTime(tr.DateTime + tr.Scans / ts.SampleRate / 3600 / 24);
    for K := 0 to ts.Scans - 1 do
    begin
      Ex[I][K] := tr.Data[0][K];
      Ey[I][K] := tr.Data[1][K];
      Hx[I][K] := tr.Data[2][K];
      Hy[I][K] := tr.Data[3][K];
      Hz[I][K] := tr.Data[4][K];
    end;
  end;
  Self.ExField := Ex;
  Self.EyField := Ey;
  Self.HxField := Hx;
  Self.HyField := Hy;
  Self.HzField := Hz;
end;

{ TMultipleTimeSeries }

procedure TMultipleTimeSeries.ToPhoenixTS(path: string; tbl: TTBL);
var
  ts: TTS;
  I, J, K, perrecords: Integer;
  tr: TTSRecord;
  dt: Integer2D;
begin
  ts := TTS.Create;
  if Self.name = 'TS2' then
  begin
    ts.BeginTime := Self.BeginTime[0];
    ts.EndTime := Self.BeginTime[Self.BeginTimeCount - 1];
    ts.Scans := 2400;
    ts.Channels := 5;
    ts.LengthPre := ts.Channels * ts.Scans * 3 + 32;
    ts.Samplelength := 3;
    ts.SampleRate := 24000;
    ts.BoxSN := tbl.SNUM.ToInteger;
    ts.SampleRateUnits := '0';
    ts.TSLength := 32;
    ts.fileName := tbl.TSFileName;
    perrecords := tbl.L2NS;
  end
  else if Self.name = 'TS3' then
  begin
    ts.BeginTime := Self.BeginTime[0];
    ts.EndTime := Self.BeginTime[Self.BeginTimeCount - 1];
    ts.Scans := 2400;
    ts.Channels := 5;
    ts.LengthPre := ts.Channels * ts.Scans * 3 + 32;
    ts.Samplelength := 3;
    ts.SampleRate := 2400;
    ts.BoxSN := tbl.SNUM.ToInteger;
    ts.SampleRateUnits := '0';
    ts.TSLength := 32;
    ts.fileName := tbl.TSFileName;
    perrecords := tbl.L3NS;
  end
  else if Self.name = 'TS4' then
  begin
    ts.BeginTime := Self.BeginTime[0];
    ts.EndTime := Self.BeginTime[Self.BeginTimeCount - 1];
    ts.Scans := 150;
    ts.Channels := 5;
    ts.LengthPre := ts.Channels * ts.Scans * 3 + 32;
    ts.Samplelength := 3;
    ts.SampleRate := 150;
    ts.BoxSN := tbl.SNUM.ToInteger;
    ts.SampleRateUnits := '0';
    ts.TSLength := 32;
    ts.fileName := tbl.TSFileName;
    perrecords := tbl.L4NS;
  end;

  for I := 0 to Self.BeginTimeCount - 1 do
  begin
    for J := 0 to perrecords - 1 do
    begin
      tr := ts.AddNewTSRecord;
      tr.DateTime := Self.BeginTime[I] + ts.Scans * J / Self.SampleRate / 3600 / 24;
      tr.BoxSN := ts.BoxSN;
      tr.Scans := ts.Scans;
      tr.Channels := ts.Channels;
      ts.LengthPre := ts.Channels * ts.Scans * 3 + 32;
      tr.TSLength := ts.TSLength;
      tr.StatusCodes := 0;
      tr.SaturationFlag := 0;
      tr.Reservedbit := 0;
      tr.Samplelength := ts.Samplelength;
      tr.SampleRate := ts.SampleRate;
      tr.SampleRateUnits := 0;
      tr.ClockStatus := 4;
      tr.ClockError := 0;
      dt := MakeInteger2D(ts.Channels, ts.Scans);
      for K := 0 to ts.Scans - 1 do
      begin
        dt[0][K] := System.Round(Self.ExField[I][J * ts.Scans + K]);
        dt[1][K] := System.Round(Self.EyField[I][J * ts.Scans + K]);
        dt[2][K] := System.Round(Self.HxField[I][J * ts.Scans + K]);
        dt[3][K] := System.Round(Self.HyField[I][J * ts.Scans + K]);
        dt[4][K] := System.Round(Self.HzField[I][J * ts.Scans + K]);
      end;
      tr.Data := dt;
    end;
  end;
  ts.WriteBin(path + tbl.TSFileName + '.' + Self.name);
  ts.Free;
end;

end.
