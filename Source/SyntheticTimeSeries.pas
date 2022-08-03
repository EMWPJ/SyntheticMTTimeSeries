unit SyntheticTimeSeries;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math, PhoenixSite,
  System.Variants, System.Generics.Collections, MathFunctions, System.DateUtils, TimeSeriesWindows,
  WTypes, FreToTime, ForwardSite, TimeSeries, Phoenix, PhoenixB;

type
  SyntheticFunction = (Synthetic_Fix, Synthetic_AverageSegment, Synthetic_AverageSegmentWindowed,
    Synthetic_RandomSegment, Synthetic_RandomSegmentWindowed, Synthetic_RandomSegmentPartiallyWindowed);

const
  SyntheticFunctionName: Array [SyntheticFunction] of string = ('No Segment', 'Segment Length Fixed',
    'Segment Length Fixed & Windowed', 'Random Segment Length', 'Random Segment Length & Windowed',
    'Random Segment Length & Partially Windowed');
  // FixSyntheticPeriods = 1;
  SyntheticTS2FreMin = 10;
  SyntheticTS2FreMax = 12000;
  SyntheticTS3FreMin = 1;
  SyntheticTS3FreMax = 1000;
  SyntheticTS4FreMin = 0.1;
  SyntheticTS4FreMax = 10;
  SyntheticTS5FreMin = 0.000001;
  SyntheticTS5FreMax = 1;

type

  TSyntheticTimeSeries = class;
  TSyntheticSchema = class;

  TSyntheticTimeSeries = class(TObject)
    function SyntheticSites(sites: TForwardSite; const rotate: Double = 0): TSiteTimeSeries;
  private
    FName: String;
    FDescription: String;
    FBeginTime: Double;
    FEndTime: Double;
    FSchemas: TList<TSyntheticSchema>;
    procedure SetName(const _Value: String);
    procedure SetDescription(const _Value: String);
    procedure SetBeginTime(const _Value: Double);
    procedure SetEndTime(const _Value: Double);
    procedure SetSchemas(const _Value: TList<TSyntheticSchema>);
    function GetSchema(Index: Integer): TSyntheticSchema;
    procedure SetSchema(Index: Integer; const _Value: TSyntheticSchema);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSchema(_Value: TSyntheticSchema);
    function AddNewSchema: TSyntheticSchema;
    procedure SchemaClear;
    function SchemaCount: Integer;
    procedure RemoveSchema(_Value: TSyntheticSchema);
    procedure DeleteSchema(Index: Integer);
    property Name: String read FName write SetName;
    property Description: String read FDescription write SetDescription;
    property BeginTime: Double read FBeginTime write SetBeginTime;
    property EndTime: Double read FEndTime write SetEndTime;
    property Schemas: TList<TSyntheticSchema> read FSchemas write SetSchemas;
    property Schema[Index: Integer]: TSyntheticSchema read GetSchema write SetSchema;
  end;

  TSyntheticSchema = class(TObject)
    function SyntheticSite(site: TForwardSite; const btime: TDateTime; etime: TDateTime; const rotate: Double = 0)
      : TTimeSeries;
  private
    FName: String;
    FDescription: String;
    FSyntheticPeriods: Double;
    FContinous: Boolean;
    FSampleRate: Double;
    FMaxFrequency: Double;
    FMinFrequency: Double;
    FIntervals: Double;
    FOffset: Double;
    FSamplingTime: Double;
    FSourceScale: Double;
    FSyntheticFunciton: Integer;
    procedure SetName(const _Value: String);
    procedure SetDescription(const _Value: String);
    procedure SetSyntheticPeriods(const _Value: Double);
    procedure SetContinous(const _Value: Boolean);
    procedure SetSampleRate(const _Value: Double);
    procedure SetMaxFrequency(const _Value: Double);
    procedure SetMinFrequency(const _Value: Double);
    procedure SetIntervals(const _Value: Double);
    procedure SetOffset(const _Value: Double);
    procedure SetSamplingTime(const _Value: Double);
    procedure SetSourceScale(const _Value: Double);
    procedure SetSyntheticFunciton(const _Value: Integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property Name: String read FName write SetName;
    property Description: String read FDescription write SetDescription;
    property SyntheticPeriods: Double read FSyntheticPeriods write SetSyntheticPeriods;
    property Continous: Boolean read FContinous write SetContinous;
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property MaxFrequency: Double read FMaxFrequency write SetMaxFrequency;
    property MinFrequency: Double read FMinFrequency write SetMinFrequency;
    property Intervals: Double read FIntervals write SetIntervals;
    property Offset: Double read FOffset write SetOffset;
    property SamplingTime: Double read FSamplingTime write SetSamplingTime;
    property SourceScale: Double read FSourceScale write SetSourceScale;
    property SyntheticFunciton: Integer read FSyntheticFunciton write SetSyntheticFunciton;
  end;

procedure AverageSegmentSyntheticSeries(const SampleRate, btime, etime, MaxFre, MinFre: Double;
  var ex, ey, hx, hy, hz: Double1D; const freList: Double1D; const Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2,
  Hz2: TComplex1D; const SyntheticPeriods: Double = 8; AllScale: Double = 1);

procedure AverageSegmentWindowedSyntheticSeries(const SampleRate, btime, etime, MaxFre, MinFre: Double;
  var ex, ey, hx, hy, hz: Double1D; const freList: Double1D; const Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2,
  Hz2: TComplex1D; const SyntheticPeriods: Double = 8; AllScale: Double = 1);

procedure RandomSegmentLengthPartiallyWindowedSyntheticSeries(const SampleRate, btime, etime, MaxFre, MinFre: Double;
  var ex, ey, hx, hy, hz: Double1D; const freList: Double1D; const Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2,
  Hz2: TComplex1D; const SyntheticPeriods: Double = 8; const AllScale: Double = 1);

procedure RandomSegmentLengthSyntheticSeries(const SampleRate, btime, etime, MaxFre, MinFre: Double;
  var ex, ey, hx, hy, hz: Double1D; const freList: Double1D; const Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2,
  Hz2: TComplex1D; const SyntheticPeriods: Double = 8; const AllScale: Double = 1);

procedure RandomSegmentLengthWindowedSyntheticSeries(const SampleRate, btime, etime, MaxFre, MinFre: Double;
  var ex, ey, hx, hy, hz: Double1D; const freList: Double1D; const Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2,
  Hz2: TComplex1D; const SyntheticPeriods: Double = 8; const AllScale: Double = 1);

procedure SyntheticSeriesFix(const SampleRate, btime, etime, MaxFre, MinFre: Double; var ex, ey, hx, hy, hz: Double1D;
  const freList: Double1D; const Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2, Hz2: TComplex1D;
  const AllScale: Double = 1);

implementation

procedure AverageSegmentSyntheticSeries(const SampleRate, btime, etime, MaxFre, MinFre: Double;
  var ex, ey, hx, hy, hz: Double1D; const freList: Double1D; const Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2,
  Hz2: TComplex1D; const SyntheticPeriods: Double = 8; AllScale: Double = 1);
var
  Index: Integer;
  timeLength: Double;
  I, J, K, Channels, FCount, TCount: Integer;
  exts1, eyts1, hxts1, hyts1, hzts1, exts2, eyts2, hxts2, hyts2, hzts2: Double1D;
  ex1d1, ey1d1, hx1d1, hy1d1, hz1d1: Double1D;
  ex1d2, ey1d2, hx1d2, hy1d2, hz1d2: Double1D;
  tmpex1d1, tmpey1d1, tmphx1d1, tmphy1d1, tmphz1d1: Double1D;
  tmpex1d2, tmpey1d2, tmphx1d2, tmphy1d2, tmphz1d2: Double1D;
  examps1, eyamps1, hxamps1, hyamps1, hzamps1, exphss1, eyphss1, hxphss1, hyphss1, hzphss1: Double;
  examps2, eyamps2, hxamps2, hyamps2, hzamps2, exphss2, eyphss2, hxphss2, hyphss2, hzphss2: Double;
  tmpexamps1, tmpeyamps1, tmphxamps1, tmphyamps1, tmphzamps1, tmpexphss1, tmpeyphss1, tmphxphss1, tmphyphss1,
    tmphzphss1: Double;
  tmpexamps2, tmpeyamps2, tmphxamps2, tmphyamps2, tmphzamps2, tmpexphss2, tmpeyphss2, tmphxphss2, tmphyphss2,
    tmphzphss2: Double;
  deltatime, deltaPha1, deltaPha2: Double;

  scount, percount, leftcount: Integer;
  da1, da2, dp1, dp2: Double;
begin
  FCount := Length(freList);
  TCount := Trunc(SecondsBetween(btime, etime) * SampleRate);
  ex := MakeDouble1D(TCount, 0);
  ey := MakeDouble1D(TCount, 0);
  hx := MakeDouble1D(TCount, 0);
  hy := MakeDouble1D(TCount, 0);
  hz := MakeDouble1D(TCount, 0);
  exts1 := MakeDouble1D(TCount, 0);
  eyts1 := MakeDouble1D(TCount, 0);
  hxts1 := MakeDouble1D(TCount, 0);
  hyts1 := MakeDouble1D(TCount, 0);
  hzts1 := MakeDouble1D(TCount, 0);
  exts2 := MakeDouble1D(TCount, 0);
  eyts2 := MakeDouble1D(TCount, 0);
  hxts2 := MakeDouble1D(TCount, 0);
  hyts2 := MakeDouble1D(TCount, 0);
  hzts2 := MakeDouble1D(TCount, 0);
  SetLength(ex1d1, TCount);
  SetLength(ey1d1, TCount);
  SetLength(hx1d1, TCount);
  SetLength(hy1d1, TCount);
  SetLength(hz1d1, TCount);
  SetLength(ex1d2, TCount);
  SetLength(ey1d2, TCount);
  SetLength(hx1d2, TCount);
  SetLength(hy1d2, TCount);
  SetLength(hz1d2, TCount);
  deltatime := MilliSecondsBetween(0, btime) / 1000.0;
  for I := 0 to FCount - 1 do // Fre Cycle
  begin
    if (freList[I] < MinFre) or (freList[I] > MaxFre) then
      Continue;
    deltaPha1 := deltatime / (2.0 * System.Pi);
    deltaPha1 := deltaPha1 - (Trunc(deltaPha1 * 180 / System.Pi) div 360) * 2 * System.Pi;
    deltaPha2 := deltaPha1 + System.Pi / 4;
    examps1 := Ex1[I].Module * AllScale;
    eyamps1 := Ey1[I].Module * AllScale;
    hxamps1 := Hx1[I].Module * AllScale;
    hyamps1 := Hy1[I].Module * AllScale;
    hzamps1 := Hz1[I].Module * AllScale;

    exphss1 := Ex1[I].Phase + System.Pi;
    eyphss1 := Ey1[I].Phase + System.Pi;
    hxphss1 := Hx1[I].Phase;
    hyphss1 := Hy1[I].Phase;
    hzphss1 := Hz1[I].Phase;

    exphss1 := exphss1 + deltaPha1;
    eyphss1 := eyphss1 + deltaPha1;
    hxphss1 := hxphss1 + deltaPha1;
    hyphss1 := hyphss1 + deltaPha1;
    hzphss1 := hzphss1 + deltaPha1;

    examps2 := Ex2[I].Module * AllScale;
    eyamps2 := Ey2[I].Module * AllScale;
    hxamps2 := Hx2[I].Module * AllScale;
    hyamps2 := Hy2[I].Module * AllScale;
    hzamps2 := Hz2[I].Module * AllScale;

    exphss2 := Ex2[I].Phase + System.Pi;
    eyphss2 := Ey2[I].Phase + System.Pi;
    hxphss2 := Hx2[I].Phase;
    hyphss2 := Hy2[I].Phase;
    hzphss2 := Hz2[I].Phase;

    exphss2 := exphss2 + deltaPha2;
    eyphss2 := eyphss2 + deltaPha2;
    hxphss2 := hxphss2 + deltaPha2;
    hyphss2 := hyphss2 + deltaPha2;
    hzphss2 := hzphss2 + deltaPha2;

    percount := Round(SampleRate / freList[I] * SyntheticPeriods);
    if percount > TCount then
    begin
      percount := TCount;
    end;
    scount := TCount div percount;
    leftcount := TCount - percount * scount;

    SetLength(tmpex1d1, percount);
    SetLength(tmpey1d1, percount);
    SetLength(tmphx1d1, percount);
    SetLength(tmphy1d1, percount);
    SetLength(tmphz1d1, percount);
    SetLength(tmpex1d2, percount);
    SetLength(tmpey1d2, percount);
    SetLength(tmphx1d2, percount);
    SetLength(tmphy1d2, percount);
    SetLength(tmphz1d2, percount);
    RandSeed := MilliSecondsBetween(0, btime + 1 / freList[I] / 24 / 3600);
    da1 := Random * 2;
    dp1 := Random * System.Pi * 2;
    RandSeed := MilliSecondsBetween(0, btime - 1 / freList[I] / 24 / 3600);
    da2 := Random * 2;
    dp2 := Random * System.Pi * 2;
    for K := 0 to scount - 1 do
    begin
      RandSeed := MilliSecondsBetween(0, btime + 1 / freList[I] / 24 / 3600) + Round(K * percount * SampleRate);
      da1 := Random * 2;
      dp1 := Random * System.Pi * 2;
      RandSeed := MilliSecondsBetween(0, btime - 1 / freList[I] / 24 / 3600) + Round(K * percount * SampleRate);
      da2 := Random * 2;
      dp2 := Random * System.Pi * 2;

      tmpexamps1 := examps1 * da1;
      tmpeyamps1 := eyamps1 * da1;
      tmphxamps1 := hxamps1 * da1;
      tmphyamps1 := hyamps1 * da1;
      tmphzamps1 := hzamps1 * da1;
      tmpexphss1 := exphss1 + dp1;
      tmpeyphss1 := eyphss1 + dp1;
      tmphxphss1 := hxphss1 + dp1;
      tmphyphss1 := hyphss1 + dp1;
      tmphzphss1 := hzphss1 + dp1;

      FreToTimeSeries(tmpexamps1, tmpexphss1, freList[I], SampleRate, percount, tmpex1d1);
      FreToTimeSeries(tmpeyamps1, tmpeyphss1, freList[I], SampleRate, percount, tmpey1d1);
      FreToTimeSeries(tmphxamps1, tmphxphss1, freList[I], SampleRate, percount, tmphx1d1);
      FreToTimeSeries(tmphyamps1, tmphyphss1, freList[I], SampleRate, percount, tmphy1d1);
      FreToTimeSeries(tmphzamps1, tmphzphss1, freList[I], SampleRate, percount, tmphz1d1);
      Move(tmpex1d1[0], ex1d1[K * percount], percount * 8);
      Move(tmpey1d1[0], ey1d1[K * percount], percount * 8);
      Move(tmphx1d1[0], hx1d1[K * percount], percount * 8);
      Move(tmphy1d1[0], hy1d1[K * percount], percount * 8);
      Move(tmphz1d1[0], hz1d1[K * percount], percount * 8);

      tmpexamps2 := examps2 * da2;
      tmpeyamps2 := eyamps2 * da2;
      tmphxamps2 := hxamps2 * da2;
      tmphyamps2 := hyamps2 * da2;
      tmphzamps2 := hzamps2 * da2;
      tmpexphss2 := exphss2 + dp2;
      tmpeyphss2 := eyphss2 + dp2;
      tmphxphss2 := hxphss2 + dp2;
      tmphyphss2 := hyphss2 + dp2;
      tmphzphss2 := hzphss2 + dp2;
      FreToTimeSeries(tmpexamps2, tmpexphss2, freList[I], SampleRate, percount, tmpex1d2);
      FreToTimeSeries(tmpeyamps2, tmpeyphss2, freList[I], SampleRate, percount, tmpey1d2);
      FreToTimeSeries(tmphxamps2, tmphxphss2, freList[I], SampleRate, percount, tmphx1d2);
      FreToTimeSeries(tmphyamps2, tmphyphss2, freList[I], SampleRate, percount, tmphy1d2);
      FreToTimeSeries(tmphzamps2, tmphzphss2, freList[I], SampleRate, percount, tmphz1d2);
      Move(tmpex1d2[0], ex1d2[K * percount], percount * 8);
      Move(tmpey1d2[0], ey1d2[K * percount], percount * 8);
      Move(tmphx1d2[0], hx1d2[K * percount], percount * 8);
      Move(tmphy1d2[0], hy1d2[K * percount], percount * 8);
      Move(tmphz1d2[0], hz1d2[K * percount], percount * 8);
    end;
    if leftcount > 0 then
    begin
      K := scount;

      tmpexamps1 := examps1 * da1;
      tmpeyamps1 := eyamps1 * da1;
      tmphxamps1 := hxamps1 * da1;
      tmphyamps1 := hyamps1 * da1;
      tmphzamps1 := hzamps1 * da1;
      tmpexphss1 := exphss1 + dp1;
      tmpeyphss1 := eyphss1 + dp1;
      tmphxphss1 := hxphss1 + dp1;
      tmphyphss1 := hyphss1 + dp1;
      tmphzphss1 := hzphss1 + dp1;

      FreToTimeSeries(tmpexamps1, tmpexphss1, freList[I], SampleRate, leftcount, tmpex1d1);
      FreToTimeSeries(tmpeyamps1, tmpeyphss1, freList[I], SampleRate, leftcount, tmpey1d1);
      FreToTimeSeries(tmphxamps1, tmphxphss1, freList[I], SampleRate, leftcount, tmphx1d1);
      FreToTimeSeries(tmphyamps1, tmphyphss1, freList[I], SampleRate, leftcount, tmphy1d1);
      FreToTimeSeries(tmphzamps1, tmphzphss1, freList[I], SampleRate, leftcount, tmphz1d1);
      Move(tmpex1d1[0], ex1d1[K * percount], leftcount * 8);
      Move(tmpey1d1[0], ey1d1[K * percount], leftcount * 8);
      Move(tmphx1d1[0], hx1d1[K * percount], leftcount * 8);
      Move(tmphy1d1[0], hy1d1[K * percount], leftcount * 8);
      Move(tmphz1d1[0], hz1d1[K * percount], leftcount * 8);

      tmpexamps2 := examps2 * da2;
      tmpeyamps2 := eyamps2 * da2;
      tmphxamps2 := hxamps2 * da2;
      tmphyamps2 := hyamps2 * da2;
      tmphzamps2 := hzamps2 * da2;
      tmpexphss2 := exphss2 + dp2;
      tmpeyphss2 := eyphss2 + dp2;
      tmphxphss2 := hxphss2 + dp2;
      tmphyphss2 := hyphss2 + dp2;
      tmphzphss2 := hzphss2 + dp2;
      FreToTimeSeries(tmpexamps2, tmpexphss2, freList[I], SampleRate, leftcount, tmpex1d2);
      FreToTimeSeries(tmpeyamps2, tmpeyphss2, freList[I], SampleRate, leftcount, tmpey1d2);
      FreToTimeSeries(tmphxamps2, tmphxphss2, freList[I], SampleRate, leftcount, tmphx1d2);
      FreToTimeSeries(tmphyamps2, tmphyphss2, freList[I], SampleRate, leftcount, tmphy1d2);
      FreToTimeSeries(tmphzamps2, tmphzphss2, freList[I], SampleRate, leftcount, tmphz1d2);
      Move(tmpex1d2[0], ex1d2[K * percount], leftcount * 8);
      Move(tmpey1d2[0], ey1d2[K * percount], leftcount * 8);
      Move(tmphx1d2[0], hx1d2[K * percount], leftcount * 8);
      Move(tmphy1d2[0], hy1d2[K * percount], leftcount * 8);
      Move(tmphz1d2[0], hz1d2[K * percount], leftcount * 8);
    end;

    exts1 := VectorAdd(exts1, ex1d1);
    eyts1 := VectorAdd(eyts1, ey1d1);
    hxts1 := VectorAdd(hxts1, hx1d1);
    hyts1 := VectorAdd(hyts1, hy1d1);
    hzts1 := VectorAdd(hzts1, hz1d1);

    exts2 := VectorAdd(exts2, ex1d2);
    eyts2 := VectorAdd(eyts2, ey1d2);
    hxts2 := VectorAdd(hxts2, hx1d2);
    hyts2 := VectorAdd(hyts2, hy1d2);
    hzts2 := VectorAdd(hzts2, hz1d2);
  end;

  ex := VectorAdd(exts1, exts2);
  ey := VectorAdd(eyts1, eyts2);
  hx := VectorAdd(hxts1, hxts2);
  hy := VectorAdd(hyts1, hyts2);
  hz := VectorAdd(hzts1, hzts2);

  ex1d1 := nil;
  ey1d1 := nil;
  hx1d1 := nil;
  hy1d1 := nil;
  hz1d1 := nil;

  ex1d2 := nil;
  ey1d2 := nil;
  hx1d2 := nil;
  hy1d2 := nil;
  hz1d2 := nil;

  tmpex1d1 := nil;
  tmpey1d1 := nil;
  tmphx1d1 := nil;
  tmphy1d1 := nil;
  tmphz1d1 := nil;

  tmpex1d2 := nil;
  tmpey1d2 := nil;
  tmphx1d2 := nil;
  tmphy1d2 := nil;
  tmphz1d2 := nil;

  exts1 := nil;
  eyts1 := nil;
  hxts1 := nil;
  hyts1 := nil;
  hzts1 := nil;
end;

procedure AverageSegmentWindowedSyntheticSeries(const SampleRate, btime, etime, MaxFre, MinFre: Double;
  var ex, ey, hx, hy, hz: Double1D; const freList: Double1D; const Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2,
  Hz2: TComplex1D; const SyntheticPeriods: Double = 8; AllScale: Double = 1);
var
  Index: Integer;
  timeLength: Double;
  I, J, K, Channels, FCount, TCount: Integer;
  exts1, eyts1, hxts1, hyts1, hzts1, exts2, eyts2, hxts2, hyts2, hzts2: Double1D;
  ex1d1, ey1d1, hx1d1, hy1d1, hz1d1: Double1D;
  ex1d2, ey1d2, hx1d2, hy1d2, hz1d2: Double1D;
  tmpex1d1, tmpey1d1, tmphx1d1, tmphy1d1, tmphz1d1: Double1D;
  tmpex1d2, tmpey1d2, tmphx1d2, tmphy1d2, tmphz1d2: Double1D;
  examps1, eyamps1, hxamps1, hyamps1, hzamps1, exphss1, eyphss1, hxphss1, hyphss1, hzphss1: Double;
  examps2, eyamps2, hxamps2, hyamps2, hzamps2, exphss2, eyphss2, hxphss2, hyphss2, hzphss2: Double;
  tmpexamps1, tmpeyamps1, tmphxamps1, tmphyamps1, tmphzamps1, tmpexphss1, tmpeyphss1, tmphxphss1, tmphyphss1,
    tmphzphss1: Double;
  tmpexamps2, tmpeyamps2, tmphxamps2, tmphyamps2, tmphzamps2, tmpexphss2, tmpeyphss2, tmphxphss2, tmphyphss2,
    tmphzphss2: Double;
  deltatime, deltaPha1, deltaPha2: Double;

  window, windowedTs: Double1D;
  scount, percount, leftcount: Integer;
  da1, da2, dp1, dp2: Double;
begin
  FCount := Length(freList);
  TCount := Trunc(SecondsBetween(btime, etime) * SampleRate);
  ex := MakeDouble1D(TCount, 0);
  ey := MakeDouble1D(TCount, 0);
  hx := MakeDouble1D(TCount, 0);
  hy := MakeDouble1D(TCount, 0);
  hz := MakeDouble1D(TCount, 0);
  exts1 := MakeDouble1D(TCount, 0);
  eyts1 := MakeDouble1D(TCount, 0);
  hxts1 := MakeDouble1D(TCount, 0);
  hyts1 := MakeDouble1D(TCount, 0);
  hzts1 := MakeDouble1D(TCount, 0);
  exts2 := MakeDouble1D(TCount, 0);
  eyts2 := MakeDouble1D(TCount, 0);
  hxts2 := MakeDouble1D(TCount, 0);
  hyts2 := MakeDouble1D(TCount, 0);
  hzts2 := MakeDouble1D(TCount, 0);
  SetLength(ex1d1, TCount);
  SetLength(ey1d1, TCount);
  SetLength(hx1d1, TCount);
  SetLength(hy1d1, TCount);
  SetLength(hz1d1, TCount);
  SetLength(ex1d2, TCount);
  SetLength(ey1d2, TCount);
  SetLength(hx1d2, TCount);
  SetLength(hy1d2, TCount);
  SetLength(hz1d2, TCount);
  deltatime := MilliSecondsBetween(0, btime) / 1000.0;
  for I := 0 to FCount - 1 do // Fre Cycle
  begin
    if (freList[I] < MinFre) or (freList[I] > MaxFre) then
      Continue;
    deltaPha1 := deltatime / (2.0 * System.Pi);
    deltaPha1 := deltaPha1 - (Trunc(deltaPha1 * 180 / System.Pi) div 360) * 2 * System.Pi;
    deltaPha2 := deltaPha1 + System.Pi / 4;
    examps1 := Ex1[I].Module * AllScale;
    eyamps1 := Ey1[I].Module * AllScale;
    hxamps1 := Hx1[I].Module * AllScale;
    hyamps1 := Hy1[I].Module * AllScale;
    hzamps1 := Hz1[I].Module * AllScale;

    exphss1 := Ex1[I].Phase + System.Pi;
    eyphss1 := Ey1[I].Phase + System.Pi;
    hxphss1 := Hx1[I].Phase;
    hyphss1 := Hy1[I].Phase;
    hzphss1 := Hz1[I].Phase;

    exphss1 := exphss1 + deltaPha1;
    eyphss1 := eyphss1 + deltaPha1;
    hxphss1 := hxphss1 + deltaPha1;
    hyphss1 := hyphss1 + deltaPha1;
    hzphss1 := hzphss1 + deltaPha1;

    examps2 := Ex2[I].Module * AllScale;
    eyamps2 := Ey2[I].Module * AllScale;
    hxamps2 := Hx2[I].Module * AllScale;
    hyamps2 := Hy2[I].Module * AllScale;
    hzamps2 := Hz2[I].Module * AllScale;

    exphss2 := Ex2[I].Phase + System.Pi;
    eyphss2 := Ey2[I].Phase + System.Pi;
    hxphss2 := Hx2[I].Phase;
    hyphss2 := Hy2[I].Phase;
    hzphss2 := Hz2[I].Phase;

    exphss2 := exphss2 + deltaPha2;
    eyphss2 := eyphss2 + deltaPha2;
    hxphss2 := hxphss2 + deltaPha2;
    hyphss2 := hyphss2 + deltaPha2;
    hzphss2 := hzphss2 + deltaPha2;

    percount := Round(SampleRate / freList[I] * SyntheticPeriods);
    if percount > TCount then
    begin
      percount := TCount;
    end;
    scount := TCount div percount;
    leftcount := TCount - percount * scount;

    SetLength(tmpex1d1, percount);
    SetLength(tmpey1d1, percount);
    SetLength(tmphx1d1, percount);
    SetLength(tmphy1d1, percount);
    SetLength(tmphz1d1, percount);
    SetLength(tmpex1d2, percount);
    SetLength(tmpey1d2, percount);
    SetLength(tmphx1d2, percount);
    SetLength(tmphy1d2, percount);
    SetLength(tmphz1d2, percount);
    RandSeed := MilliSecondsBetween(0, btime + 1 / freList[I] / 24 / 3600);
    da1 := Random * 2;
    dp1 := Random * System.Pi * 2;
    RandSeed := MilliSecondsBetween(0, btime - 1 / freList[I] / 24 / 3600);
    da2 := Random * 2;
    dp2 := Random * System.Pi * 2;
    window := HanningWin(percount);
    for K := 0 to scount - 1 do
    begin
      RandSeed := MilliSecondsBetween(0, btime + 1 / freList[I] / 24 / 3600) + Round(K * percount * SampleRate);
      da1 := Random * 2;
      dp1 := Random * System.Pi * 2;
      RandSeed := MilliSecondsBetween(0, btime - 1 / freList[I] / 24 / 3600) + Round(K * percount * SampleRate);
      da2 := Random * 2;
      dp2 := Random * System.Pi * 2;

      tmpexamps1 := examps1 * da1;
      tmpeyamps1 := eyamps1 * da1;
      tmphxamps1 := hxamps1 * da1;
      tmphyamps1 := hyamps1 * da1;
      tmphzamps1 := hzamps1 * da1;
      tmpexphss1 := exphss1 + dp1;
      tmpeyphss1 := eyphss1 + dp1;
      tmphxphss1 := hxphss1 + dp1;
      tmphyphss1 := hyphss1 + dp1;
      tmphzphss1 := hzphss1 + dp1;

      FreToTimeSeries(tmpexamps1, tmpexphss1, freList[I], SampleRate, percount, tmpex1d1);
      FreToTimeSeries(tmpeyamps1, tmpeyphss1, freList[I], SampleRate, percount, tmpey1d1);
      FreToTimeSeries(tmphxamps1, tmphxphss1, freList[I], SampleRate, percount, tmphx1d1);
      FreToTimeSeries(tmphyamps1, tmphyphss1, freList[I], SampleRate, percount, tmphy1d1);
      FreToTimeSeries(tmphzamps1, tmphzphss1, freList[I], SampleRate, percount, tmphz1d1);
      windowedTs := VectorMultiply(tmpex1d1, window);
      Move(windowedTs[0], ex1d1[K * percount], percount * 8);
      windowedTs := VectorMultiply(tmpey1d1, window);
      Move(windowedTs[0], ey1d1[K * percount], percount * 8);
      windowedTs := VectorMultiply(tmphx1d1, window);
      Move(windowedTs[0], hx1d1[K * percount], percount * 8);
      windowedTs := VectorMultiply(tmphy1d1, window);
      Move(windowedTs[0], hy1d1[K * percount], percount * 8);
      windowedTs := VectorMultiply(tmphz1d1, window);
      Move(windowedTs[0], hz1d1[K * percount], percount * 8);

      tmpexamps2 := examps2 * da2;
      tmpeyamps2 := eyamps2 * da2;
      tmphxamps2 := hxamps2 * da2;
      tmphyamps2 := hyamps2 * da2;
      tmphzamps2 := hzamps2 * da2;
      tmpexphss2 := exphss2 + dp2;
      tmpeyphss2 := eyphss2 + dp2;
      tmphxphss2 := hxphss2 + dp2;
      tmphyphss2 := hyphss2 + dp2;
      tmphzphss2 := hzphss2 + dp2;
      FreToTimeSeries(tmpexamps2, tmpexphss2, freList[I], SampleRate, percount, tmpex1d2);
      FreToTimeSeries(tmpeyamps2, tmpeyphss2, freList[I], SampleRate, percount, tmpey1d2);
      FreToTimeSeries(tmphxamps2, tmphxphss2, freList[I], SampleRate, percount, tmphx1d2);
      FreToTimeSeries(tmphyamps2, tmphyphss2, freList[I], SampleRate, percount, tmphy1d2);
      FreToTimeSeries(tmphzamps2, tmphzphss2, freList[I], SampleRate, percount, tmphz1d2);
      windowedTs := VectorMultiply(tmpex1d2, window);
      Move(windowedTs[0], ex1d2[K * percount], percount * 8);
      windowedTs := VectorMultiply(tmpey1d2, window);
      Move(windowedTs[0], ey1d2[K * percount], percount * 8);
      windowedTs := VectorMultiply(tmphx1d2, window);
      Move(windowedTs[0], hx1d2[K * percount], percount * 8);
      windowedTs := VectorMultiply(tmphy1d2, window);
      Move(windowedTs[0], hy1d2[K * percount], percount * 8);
      windowedTs := VectorMultiply(tmphz1d2, window);
      Move(windowedTs[0], hz1d2[K * percount], percount * 8);
    end;
    if leftcount > 0 then
    begin
      K := scount;

      tmpexamps1 := examps1 * da1;
      tmpeyamps1 := eyamps1 * da1;
      tmphxamps1 := hxamps1 * da1;
      tmphyamps1 := hyamps1 * da1;
      tmphzamps1 := hzamps1 * da1;
      tmpexphss1 := exphss1 + dp1;
      tmpeyphss1 := eyphss1 + dp1;
      tmphxphss1 := hxphss1 + dp1;
      tmphyphss1 := hyphss1 + dp1;
      tmphzphss1 := hzphss1 + dp1;

      FreToTimeSeries(tmpexamps1, tmpexphss1, freList[I], SampleRate, leftcount, tmpex1d1);
      FreToTimeSeries(tmpeyamps1, tmpeyphss1, freList[I], SampleRate, leftcount, tmpey1d1);
      FreToTimeSeries(tmphxamps1, tmphxphss1, freList[I], SampleRate, leftcount, tmphx1d1);
      FreToTimeSeries(tmphyamps1, tmphyphss1, freList[I], SampleRate, leftcount, tmphy1d1);
      FreToTimeSeries(tmphzamps1, tmphzphss1, freList[I], SampleRate, leftcount, tmphz1d1);
      Move(tmpex1d1[0], ex1d1[K * percount], leftcount * 8);
      Move(tmpey1d1[0], ey1d1[K * percount], leftcount * 8);
      Move(tmphx1d1[0], hx1d1[K * percount], leftcount * 8);
      Move(tmphy1d1[0], hy1d1[K * percount], leftcount * 8);
      Move(tmphz1d1[0], hz1d1[K * percount], leftcount * 8);

      tmpexamps2 := examps2 * da2;
      tmpeyamps2 := eyamps2 * da2;
      tmphxamps2 := hxamps2 * da2;
      tmphyamps2 := hyamps2 * da2;
      tmphzamps2 := hzamps2 * da2;
      tmpexphss2 := exphss2 + dp2;
      tmpeyphss2 := eyphss2 + dp2;
      tmphxphss2 := hxphss2 + dp2;
      tmphyphss2 := hyphss2 + dp2;
      tmphzphss2 := hzphss2 + dp2;
      FreToTimeSeries(tmpexamps2, tmpexphss2, freList[I], SampleRate, leftcount, tmpex1d2);
      FreToTimeSeries(tmpeyamps2, tmpeyphss2, freList[I], SampleRate, leftcount, tmpey1d2);
      FreToTimeSeries(tmphxamps2, tmphxphss2, freList[I], SampleRate, leftcount, tmphx1d2);
      FreToTimeSeries(tmphyamps2, tmphyphss2, freList[I], SampleRate, leftcount, tmphy1d2);
      FreToTimeSeries(tmphzamps2, tmphzphss2, freList[I], SampleRate, leftcount, tmphz1d2);
      Move(tmpex1d2[0], ex1d2[K * percount], leftcount * 8);
      Move(tmpey1d2[0], ey1d2[K * percount], leftcount * 8);
      Move(tmphx1d2[0], hx1d2[K * percount], leftcount * 8);
      Move(tmphy1d2[0], hy1d2[K * percount], leftcount * 8);
      Move(tmphz1d2[0], hz1d2[K * percount], leftcount * 8);
    end;

    exts1 := VectorAdd(exts1, ex1d1);
    eyts1 := VectorAdd(eyts1, ey1d1);
    hxts1 := VectorAdd(hxts1, hx1d1);
    hyts1 := VectorAdd(hyts1, hy1d1);
    hzts1 := VectorAdd(hzts1, hz1d1);

    exts2 := VectorAdd(exts2, ex1d2);
    eyts2 := VectorAdd(eyts2, ey1d2);
    hxts2 := VectorAdd(hxts2, hx1d2);
    hyts2 := VectorAdd(hyts2, hy1d2);
    hzts2 := VectorAdd(hzts2, hz1d2);
  end;

  ex := VectorAdd(exts1, exts2);
  ey := VectorAdd(eyts1, eyts2);
  hx := VectorAdd(hxts1, hxts2);
  hy := VectorAdd(hyts1, hyts2);
  hz := VectorAdd(hzts1, hzts2);

  ex1d1 := nil;
  ey1d1 := nil;
  hx1d1 := nil;
  hy1d1 := nil;
  hz1d1 := nil;

  ex1d2 := nil;
  ey1d2 := nil;
  hx1d2 := nil;
  hy1d2 := nil;
  hz1d2 := nil;

  tmpex1d1 := nil;
  tmpey1d1 := nil;
  tmphx1d1 := nil;
  tmphy1d1 := nil;
  tmphz1d1 := nil;

  tmpex1d2 := nil;
  tmpey1d2 := nil;
  tmphx1d2 := nil;
  tmphy1d2 := nil;
  tmphz1d2 := nil;

  exts1 := nil;
  eyts1 := nil;
  hxts1 := nil;
  hyts1 := nil;
  hzts1 := nil;
end;

procedure RandomSegmentLengthSyntheticSeries(const SampleRate, btime, etime, MaxFre, MinFre: Double;
  var ex, ey, hx, hy, hz: Double1D; const freList: Double1D; const Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2,
  Hz2: TComplex1D; const SyntheticPeriods: Double = 8; const AllScale: Double = 1);
var
  Index: Integer;
  timeLength: Double;
  I, J, K, Channels, FCount, TCount: Integer;
  exts1, eyts1, hxts1, hyts1, hzts1, exts2, eyts2, hxts2, hyts2, hzts2: Double1D;
  ex1d1, ey1d1, hx1d1, hy1d1, hz1d1: Double1D;
  ex1d2, ey1d2, hx1d2, hy1d2, hz1d2: Double1D;
  tmpex1d1, tmpey1d1, tmphx1d1, tmphy1d1, tmphz1d1: Double1D;
  tmpex1d2, tmpey1d2, tmphx1d2, tmphy1d2, tmphz1d2: Double1D;
  examps1, eyamps1, hxamps1, hyamps1, hzamps1, exphss1, eyphss1, hxphss1, hyphss1, hzphss1: Double;
  examps2, eyamps2, hxamps2, hyamps2, hzamps2, exphss2, eyphss2, hxphss2, hyphss2, hzphss2: Double;
  tmpexamps1, tmpeyamps1, tmphxamps1, tmphyamps1, tmphzamps1, tmpexphss1, tmpeyphss1, tmphxphss1, tmphyphss1,
    tmphzphss1: Double;
  tmpexamps2, tmpeyamps2, tmphxamps2, tmphyamps2, tmphzamps2, tmpexphss2, tmpeyphss2, tmphxphss2, tmphyphss2,
    tmphzphss2: Double;
  deltatime, deltaPha1, deltaPha2: Double;
  scount, tmpcount, leftcount: Integer;
  da1, da2, dp1, dp2: Double;
  ran: Double;
begin
  FCount := Length(freList);
  TCount := Trunc(SecondsBetween(btime, etime) * SampleRate);
  ex := MakeDouble1D(TCount, 0);
  ey := MakeDouble1D(TCount, 0);
  hx := MakeDouble1D(TCount, 0);
  hy := MakeDouble1D(TCount, 0);
  hz := MakeDouble1D(TCount, 0);
  exts1 := MakeDouble1D(TCount, 0);
  eyts1 := MakeDouble1D(TCount, 0);
  hxts1 := MakeDouble1D(TCount, 0);
  hyts1 := MakeDouble1D(TCount, 0);
  hzts1 := MakeDouble1D(TCount, 0);
  exts2 := MakeDouble1D(TCount, 0);
  eyts2 := MakeDouble1D(TCount, 0);
  hxts2 := MakeDouble1D(TCount, 0);
  hyts2 := MakeDouble1D(TCount, 0);
  hzts2 := MakeDouble1D(TCount, 0);
  SetLength(ex1d1, TCount);
  SetLength(ey1d1, TCount);
  SetLength(hx1d1, TCount);
  SetLength(hy1d1, TCount);
  SetLength(hz1d1, TCount);
  SetLength(ex1d2, TCount);
  SetLength(ey1d2, TCount);
  SetLength(hx1d2, TCount);
  SetLength(hy1d2, TCount);
  SetLength(hz1d2, TCount);
  deltatime := MilliSecondsBetween(0, btime) / 1000.0;

  for I := 0 to FCount - 1 do // Fre Cycle
  begin
    if (freList[I] <= MinFre) or (freList[I] >= MaxFre) then
      Continue;
    deltaPha1 := deltatime / (2.0 * System.Pi);
    deltaPha1 := deltaPha1 - (Trunc(deltaPha1 * 180 / System.Pi) div 360) * 2 * System.Pi;
    deltaPha2 := deltaPha1 + System.Pi / 4;
    examps1 := Ex1[I].Module * AllScale;
    eyamps1 := Ey1[I].Module * AllScale;
    hxamps1 := Hx1[I].Module * AllScale;
    hyamps1 := Hy1[I].Module * AllScale;
    hzamps1 := Hz1[I].Module * AllScale;

    exphss1 := Ex1[I].Phase + System.Pi;
    eyphss1 := Ey1[I].Phase + System.Pi;
    hxphss1 := Hx1[I].Phase;
    hyphss1 := Hy1[I].Phase;
    hzphss1 := Hz1[I].Phase;

    exphss1 := exphss1 + deltaPha1;
    eyphss1 := eyphss1 + deltaPha1;
    hxphss1 := hxphss1 + deltaPha1;
    hyphss1 := hyphss1 + deltaPha1;
    hzphss1 := hzphss1 + deltaPha1;

    examps2 := Ex2[I].Module * AllScale;
    eyamps2 := Ey2[I].Module * AllScale;
    hxamps2 := Hx2[I].Module * AllScale;
    hyamps2 := Hy2[I].Module * AllScale;
    hzamps2 := Hz2[I].Module * AllScale;

    exphss2 := Ex2[I].Phase + System.Pi;
    eyphss2 := Ey2[I].Phase + System.Pi;
    hxphss2 := Hx2[I].Phase;
    hyphss2 := Hy2[I].Phase;
    hzphss2 := Hz2[I].Phase;

    exphss2 := exphss2 + deltaPha2;
    eyphss2 := eyphss2 + deltaPha2;
    hxphss2 := hxphss2 + deltaPha2;
    hyphss2 := hyphss2 + deltaPha2;
    hzphss2 := hzphss2 + deltaPha2;

    leftcount := TCount;
    while leftcount > 0 do
    begin
      RandSeed := MilliSecondsBetween(0, etime - (leftcount / SampleRate + 1) / freList[I] / 24 / 3600);
      tmpcount := Ceil(Abs(SampleRate / freList[I] * (RandG(SyntheticPeriods, SyntheticPeriods / 2))));
      while tmpcount < 10 do
      begin
        tmpcount := Ceil(Abs(SampleRate / freList[I] * (RandG(SyntheticPeriods, SyntheticPeriods / 2))));
      end;
      da1 := RandG(1, 1);
      dp1 := Random * System.Pi * 2;
      if tmpcount > leftcount then
      begin
        tmpcount := leftcount;
      end;
      if leftcount - tmpcount < 10 then
      begin
        tmpcount := leftcount;
      end;

      SetLength(tmpex1d1, tmpcount);
      SetLength(tmpey1d1, tmpcount);
      SetLength(tmphx1d1, tmpcount);
      SetLength(tmphy1d1, tmpcount);
      SetLength(tmphz1d1, tmpcount);

      tmpexamps1 := examps1 * da1;
      tmpeyamps1 := eyamps1 * da1;
      tmphxamps1 := hxamps1 * da1;
      tmphyamps1 := hyamps1 * da1;
      tmphzamps1 := hzamps1 * da1;
      tmpexphss1 := exphss1 + dp1;
      tmpeyphss1 := eyphss1 + dp1;
      tmphxphss1 := hxphss1 + dp1;
      tmphyphss1 := hyphss1 + dp1;
      tmphzphss1 := hzphss1 + dp1;

      FreToTimeSeries(tmpexamps1, tmpexphss1, freList[I], SampleRate, tmpcount, tmpex1d1);
      FreToTimeSeries(tmpeyamps1, tmpeyphss1, freList[I], SampleRate, tmpcount, tmpey1d1);
      FreToTimeSeries(tmphxamps1, tmphxphss1, freList[I], SampleRate, tmpcount, tmphx1d1);
      FreToTimeSeries(tmphyamps1, tmphyphss1, freList[I], SampleRate, tmpcount, tmphy1d1);
      FreToTimeSeries(tmphzamps1, tmphzphss1, freList[I], SampleRate, tmpcount, tmphz1d1);

      Move(tmpex1d1[0], ex1d1[TCount - leftcount], tmpcount * 8);
      Move(tmpey1d1[0], ey1d1[TCount - leftcount], tmpcount * 8);
      Move(tmphx1d1[0], hx1d1[TCount - leftcount], tmpcount * 8);
      Move(tmphy1d1[0], hy1d1[TCount - leftcount], tmpcount * 8);
      Move(tmphz1d1[0], hz1d1[TCount - leftcount], tmpcount * 8);

      leftcount := leftcount - tmpcount;
    end;

    leftcount := TCount;
    while leftcount > 0 do
    begin
      RandSeed := MilliSecondsBetween(0, etime - (leftcount / SampleRate - 1) / freList[I] / 24 / 3600);
      tmpcount := Ceil(Abs(SampleRate / freList[I] * (RandG(SyntheticPeriods, SyntheticPeriods / 2))));
      while tmpcount < 10 do
      begin
        tmpcount := Ceil(Abs(SampleRate / freList[I] * (RandG(SyntheticPeriods, SyntheticPeriods / 2))));
      end;
      da2 := Random * 2;
      dp2 := Random * System.Pi * 2;
      if tmpcount > leftcount then
      begin
        tmpcount := leftcount;
      end;
      if leftcount - tmpcount < 10 then
      begin
        tmpcount := leftcount;
      end;

      SetLength(tmpex1d2, tmpcount);
      SetLength(tmpey1d2, tmpcount);
      SetLength(tmphx1d2, tmpcount);
      SetLength(tmphy1d2, tmpcount);
      SetLength(tmphz1d2, tmpcount);

      tmpexamps2 := examps2 * da2;
      tmpeyamps2 := eyamps2 * da2;
      tmphxamps2 := hxamps2 * da2;
      tmphyamps2 := hyamps2 * da2;
      tmphzamps2 := hzamps2 * da2;
      tmpexphss2 := exphss2 + dp2;
      tmpeyphss2 := eyphss2 + dp2;
      tmphxphss2 := hxphss2 + dp2;
      tmphyphss2 := hyphss2 + dp2;
      tmphzphss2 := hzphss2 + dp2;

      FreToTimeSeries(tmpexamps2, tmpexphss2, freList[I], SampleRate, tmpcount, tmpex1d2);
      FreToTimeSeries(tmpeyamps2, tmpeyphss2, freList[I], SampleRate, tmpcount, tmpey1d2);
      FreToTimeSeries(tmphxamps2, tmphxphss2, freList[I], SampleRate, tmpcount, tmphx1d2);
      FreToTimeSeries(tmphyamps2, tmphyphss2, freList[I], SampleRate, tmpcount, tmphy1d2);
      FreToTimeSeries(tmphzamps2, tmphzphss2, freList[I], SampleRate, tmpcount, tmphz1d2);

      Move(tmpex1d2[0], ex1d2[TCount - leftcount], tmpcount * 8);
      Move(tmpey1d2[0], ey1d2[TCount - leftcount], tmpcount * 8);
      Move(tmphx1d2[0], hx1d2[TCount - leftcount], tmpcount * 8);
      Move(tmphy1d2[0], hy1d2[TCount - leftcount], tmpcount * 8);
      Move(tmphz1d2[0], hz1d2[TCount - leftcount], tmpcount * 8);

      leftcount := leftcount - tmpcount;
    end;
    exts1 := VectorAdd(exts1, ex1d1);
    eyts1 := VectorAdd(eyts1, ey1d1);
    hxts1 := VectorAdd(hxts1, hx1d1);
    hyts1 := VectorAdd(hyts1, hy1d1);
    hzts1 := VectorAdd(hzts1, hz1d1);

    exts2 := VectorAdd(exts2, ex1d2);
    eyts2 := VectorAdd(eyts2, ey1d2);
    hxts2 := VectorAdd(hxts2, hx1d2);
    hyts2 := VectorAdd(hyts2, hy1d2);
    hzts2 := VectorAdd(hzts2, hz1d2);

  end;

  ex := VectorAdd(exts1, exts2);
  ey := VectorAdd(eyts1, eyts2);
  hx := VectorAdd(hxts1, hxts2);
  hy := VectorAdd(hyts1, hyts2);
  hz := VectorAdd(hzts1, hzts2);

  ex1d1 := nil;
  ey1d1 := nil;
  hx1d1 := nil;
  hy1d1 := nil;
  hz1d1 := nil;

  ex1d2 := nil;
  ey1d2 := nil;
  hx1d2 := nil;
  hy1d2 := nil;
  hz1d2 := nil;

  tmpex1d1 := nil;
  tmpey1d1 := nil;
  tmphx1d1 := nil;
  tmphy1d1 := nil;
  tmphz1d1 := nil;

  tmpex1d2 := nil;
  tmpey1d2 := nil;
  tmphx1d2 := nil;
  tmphy1d2 := nil;
  tmphz1d2 := nil;

  exts1 := nil;
  eyts1 := nil;
  hxts1 := nil;
  hyts1 := nil;
  hzts1 := nil;
end;

procedure RandomSegmentLengthWindowedSyntheticSeries(const SampleRate, btime, etime, MaxFre, MinFre: Double;
  var ex, ey, hx, hy, hz: Double1D; const freList: Double1D; const Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2,
  Hz2: TComplex1D; const SyntheticPeriods: Double = 8; const AllScale: Double = 1);
var
  Index: Integer;
  timeLength: Double;
  I, J, K, Channels, FCount, TCount: Integer;
  exts1, eyts1, hxts1, hyts1, hzts1, exts2, eyts2, hxts2, hyts2, hzts2: Double1D;
  ex1d1, ey1d1, hx1d1, hy1d1, hz1d1: Double1D;
  ex1d2, ey1d2, hx1d2, hy1d2, hz1d2: Double1D;
  tmpex1d1, tmpey1d1, tmphx1d1, tmphy1d1, tmphz1d1: Double1D;
  tmpex1d2, tmpey1d2, tmphx1d2, tmphy1d2, tmphz1d2: Double1D;
  examps1, eyamps1, hxamps1, hyamps1, hzamps1, exphss1, eyphss1, hxphss1, hyphss1, hzphss1: Double;
  examps2, eyamps2, hxamps2, hyamps2, hzamps2, exphss2, eyphss2, hxphss2, hyphss2, hzphss2: Double;
  tmpexamps1, tmpeyamps1, tmphxamps1, tmphyamps1, tmphzamps1, tmpexphss1, tmpeyphss1, tmphxphss1, tmphyphss1,
    tmphzphss1: Double;
  tmpexamps2, tmpeyamps2, tmphxamps2, tmphyamps2, tmphzamps2, tmpexphss2, tmpeyphss2, tmphxphss2, tmphyphss2,
    tmphzphss2: Double;
  deltatime, deltaPha1, deltaPha2: Double;
  window, windowedTs: Double1D;
  scount, tmpcount, leftcount: Integer;
  da1, da2, dp1, dp2: Double;
  ran: Double;
begin
  FCount := Length(freList);
  TCount := Trunc(SecondsBetween(btime, etime) * SampleRate);
  ex := MakeDouble1D(TCount, 0);
  ey := MakeDouble1D(TCount, 0);
  hx := MakeDouble1D(TCount, 0);
  hy := MakeDouble1D(TCount, 0);
  hz := MakeDouble1D(TCount, 0);
  exts1 := MakeDouble1D(TCount, 0);
  eyts1 := MakeDouble1D(TCount, 0);
  hxts1 := MakeDouble1D(TCount, 0);
  hyts1 := MakeDouble1D(TCount, 0);
  hzts1 := MakeDouble1D(TCount, 0);
  exts2 := MakeDouble1D(TCount, 0);
  eyts2 := MakeDouble1D(TCount, 0);
  hxts2 := MakeDouble1D(TCount, 0);
  hyts2 := MakeDouble1D(TCount, 0);
  hzts2 := MakeDouble1D(TCount, 0);
  SetLength(ex1d1, TCount);
  SetLength(ey1d1, TCount);
  SetLength(hx1d1, TCount);
  SetLength(hy1d1, TCount);
  SetLength(hz1d1, TCount);
  SetLength(ex1d2, TCount);
  SetLength(ey1d2, TCount);
  SetLength(hx1d2, TCount);
  SetLength(hy1d2, TCount);
  SetLength(hz1d2, TCount);
  deltatime := MilliSecondsBetween(0, btime) / 1000.0;

  if SampleRate = 15 then
  begin
    deltatime := MilliSecondsBetween(0, btime) / 1000.0;
  end;
  for I := 0 to FCount - 1 do // Fre Cycle
  begin
    if (freList[I] <= MinFre) or (freList[I] >= MaxFre) then
      Continue;
    deltaPha1 := deltatime / (2.0 * System.Pi);
    deltaPha1 := deltaPha1 - (Trunc(deltaPha1 * 180 / System.Pi) div 360) * 2 * System.Pi;
    deltaPha2 := deltaPha1 + System.Pi / 4;
    examps1 := Ex1[I].Module * AllScale;
    eyamps1 := Ey1[I].Module * AllScale;
    hxamps1 := Hx1[I].Module * AllScale;
    hyamps1 := Hy1[I].Module * AllScale;
    hzamps1 := Hz1[I].Module * AllScale;

    exphss1 := Ex1[I].Phase + System.Pi;
    eyphss1 := Ey1[I].Phase + System.Pi;
    hxphss1 := Hx1[I].Phase;
    hyphss1 := Hy1[I].Phase;
    hzphss1 := Hz1[I].Phase;

    exphss1 := exphss1 + deltaPha1;
    eyphss1 := eyphss1 + deltaPha1;
    hxphss1 := hxphss1 + deltaPha1;
    hyphss1 := hyphss1 + deltaPha1;
    hzphss1 := hzphss1 + deltaPha1;

    examps2 := Ex2[I].Module * AllScale;
    eyamps2 := Ey2[I].Module * AllScale;
    hxamps2 := Hx2[I].Module * AllScale;
    hyamps2 := Hy2[I].Module * AllScale;
    hzamps2 := Hz2[I].Module * AllScale;

    exphss2 := Ex2[I].Phase + System.Pi;
    eyphss2 := Ey2[I].Phase + System.Pi;
    hxphss2 := Hx2[I].Phase;
    hyphss2 := Hy2[I].Phase;
    hzphss2 := Hz2[I].Phase;

    exphss2 := exphss2 + deltaPha2;
    eyphss2 := eyphss2 + deltaPha2;
    hxphss2 := hxphss2 + deltaPha2;
    hyphss2 := hyphss2 + deltaPha2;
    hzphss2 := hzphss2 + deltaPha2;

    leftcount := TCount;
    while leftcount > 0 do
    begin
      RandSeed := MilliSecondsBetween(0, etime - (leftcount / SampleRate + 1) / freList[I] / 24 / 3600);
      tmpcount := Ceil(Abs(SampleRate / freList[I] * (RandG(SyntheticPeriods, SyntheticPeriods / 2))));
      while tmpcount < 10 do
      begin
        tmpcount := Ceil(Abs(SampleRate / freList[I] * (RandG(SyntheticPeriods, SyntheticPeriods / 2))));
      end;
      da1 := RandG(1, 1);
      dp1 := Random * System.Pi * 2;
      if tmpcount > leftcount then
      begin
        tmpcount := leftcount;
      end;
      if leftcount - tmpcount < 10 then
      begin
        tmpcount := leftcount;
      end;

      SetLength(tmpex1d1, tmpcount);
      SetLength(tmpey1d1, tmpcount);
      SetLength(tmphx1d1, tmpcount);
      SetLength(tmphy1d1, tmpcount);
      SetLength(tmphz1d1, tmpcount);

      window := HanningWin(tmpcount);

      tmpexamps1 := examps1 * da1;
      tmpeyamps1 := eyamps1 * da1;
      tmphxamps1 := hxamps1 * da1;
      tmphyamps1 := hyamps1 * da1;
      tmphzamps1 := hzamps1 * da1;
      tmpexphss1 := exphss1 + dp1;
      tmpeyphss1 := eyphss1 + dp1;
      tmphxphss1 := hxphss1 + dp1;
      tmphyphss1 := hyphss1 + dp1;
      tmphzphss1 := hzphss1 + dp1;

      FreToTimeSeries(tmpexamps1, tmpexphss1, freList[I], SampleRate, tmpcount, tmpex1d1);
      FreToTimeSeries(tmpeyamps1, tmpeyphss1, freList[I], SampleRate, tmpcount, tmpey1d1);
      FreToTimeSeries(tmphxamps1, tmphxphss1, freList[I], SampleRate, tmpcount, tmphx1d1);
      FreToTimeSeries(tmphyamps1, tmphyphss1, freList[I], SampleRate, tmpcount, tmphy1d1);
      FreToTimeSeries(tmphzamps1, tmphzphss1, freList[I], SampleRate, tmpcount, tmphz1d1);

      windowedTs := VectorMultiply(tmpex1d1, window);
      Move(windowedTs[0], ex1d1[TCount - leftcount], tmpcount * 8);
      windowedTs := VectorMultiply(tmpey1d1, window);
      Move(windowedTs[0], ey1d1[TCount - leftcount], tmpcount * 8);
      windowedTs := VectorMultiply(tmphx1d1, window);
      Move(windowedTs[0], hx1d1[TCount - leftcount], tmpcount * 8);
      windowedTs := VectorMultiply(tmphy1d1, window);
      Move(windowedTs[0], hy1d1[TCount - leftcount], tmpcount * 8);
      windowedTs := VectorMultiply(tmphz1d1, window);
      Move(windowedTs[0], hx1d1[TCount - leftcount], tmpcount * 8);

      leftcount := leftcount - tmpcount;
    end;

    leftcount := TCount;
    while leftcount > 0 do
    begin
      RandSeed := MilliSecondsBetween(0, etime - (leftcount / SampleRate - 1) / freList[I] / 24 / 3600);
      tmpcount := Ceil(Abs(SampleRate / freList[I] * (RandG(SyntheticPeriods, SyntheticPeriods / 2))));
      while tmpcount < 10 do
      begin
        tmpcount := Ceil(Abs(SampleRate / freList[I] * (RandG(SyntheticPeriods, SyntheticPeriods / 2))));
      end;
      da2 := Random * 2;
      dp2 := Random * System.Pi * 2;
      if tmpcount > leftcount then
      begin
        tmpcount := leftcount;
      end;
      if leftcount - tmpcount < 10 then
      begin
        tmpcount := leftcount;
      end;

      SetLength(tmpex1d2, tmpcount);
      SetLength(tmpey1d2, tmpcount);
      SetLength(tmphx1d2, tmpcount);
      SetLength(tmphy1d2, tmpcount);
      SetLength(tmphz1d2, tmpcount);
      window := HanningWin(tmpcount);

      tmpexamps2 := examps2 * da2;
      tmpeyamps2 := eyamps2 * da2;
      tmphxamps2 := hxamps2 * da2;
      tmphyamps2 := hyamps2 * da2;
      tmphzamps2 := hzamps2 * da2;
      tmpexphss2 := exphss2 + dp2;
      tmpeyphss2 := eyphss2 + dp2;
      tmphxphss2 := hxphss2 + dp2;
      tmphyphss2 := hyphss2 + dp2;
      tmphzphss2 := hzphss2 + dp2;

      FreToTimeSeries(tmpexamps2, tmpexphss2, freList[I], SampleRate, tmpcount, tmpex1d2);
      FreToTimeSeries(tmpeyamps2, tmpeyphss2, freList[I], SampleRate, tmpcount, tmpey1d2);
      FreToTimeSeries(tmphxamps2, tmphxphss2, freList[I], SampleRate, tmpcount, tmphx1d2);
      FreToTimeSeries(tmphyamps2, tmphyphss2, freList[I], SampleRate, tmpcount, tmphy1d2);
      FreToTimeSeries(tmphzamps2, tmphzphss2, freList[I], SampleRate, tmpcount, tmphz1d2);

      windowedTs := VectorMultiply(tmpex1d2, window);
      Move(windowedTs[0], ex1d2[TCount - leftcount], tmpcount * 8);
      windowedTs := VectorMultiply(tmpey1d2, window);
      Move(windowedTs[0], ey1d2[TCount - leftcount], tmpcount * 8);
      windowedTs := VectorMultiply(tmphx1d2, window);
      Move(windowedTs[0], hx1d2[TCount - leftcount], tmpcount * 8);
      windowedTs := VectorMultiply(tmphy1d2, window);
      Move(windowedTs[0], hy1d2[TCount - leftcount], tmpcount * 8);
      windowedTs := VectorMultiply(tmphz1d2, window);
      Move(windowedTs[0], hx1d2[TCount - leftcount], tmpcount * 8);

      leftcount := leftcount - tmpcount;
    end;
    exts1 := VectorAdd(exts1, ex1d1);
    eyts1 := VectorAdd(eyts1, ey1d1);
    hxts1 := VectorAdd(hxts1, hx1d1);
    hyts1 := VectorAdd(hyts1, hy1d1);
    hzts1 := VectorAdd(hzts1, hz1d1);

    exts2 := VectorAdd(exts2, ex1d2);
    eyts2 := VectorAdd(eyts2, ey1d2);
    hxts2 := VectorAdd(hxts2, hx1d2);
    hyts2 := VectorAdd(hyts2, hy1d2);
    hzts2 := VectorAdd(hzts2, hz1d2);

  end;

  ex := VectorAdd(exts1, exts2);
  ey := VectorAdd(eyts1, eyts2);
  hx := VectorAdd(hxts1, hxts2);
  hy := VectorAdd(hyts1, hyts2);
  hz := VectorAdd(hzts1, hzts2);

  ex1d1 := nil;
  ey1d1 := nil;
  hx1d1 := nil;
  hy1d1 := nil;
  hz1d1 := nil;

  ex1d2 := nil;
  ey1d2 := nil;
  hx1d2 := nil;
  hy1d2 := nil;
  hz1d2 := nil;

  tmpex1d1 := nil;
  tmpey1d1 := nil;
  tmphx1d1 := nil;
  tmphy1d1 := nil;
  tmphz1d1 := nil;

  tmpex1d2 := nil;
  tmpey1d2 := nil;
  tmphx1d2 := nil;
  tmphy1d2 := nil;
  tmphz1d2 := nil;

  exts1 := nil;
  eyts1 := nil;
  hxts1 := nil;
  hyts1 := nil;
  hzts1 := nil;
end;

procedure RandomSegmentLengthPartiallyWindowedSyntheticSeries(const SampleRate, btime, etime, MaxFre, MinFre: Double;
  var ex, ey, hx, hy, hz: Double1D; const freList: Double1D; const Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2,
  Hz2: TComplex1D; const SyntheticPeriods: Double = 8; const AllScale: Double = 1);
var
  Index: Integer;
  timeLength: Double;
  I, J, K, Channels, FCount, TCount: Integer;
  exts1, eyts1, hxts1, hyts1, hzts1, exts2, eyts2, hxts2, hyts2, hzts2: Double1D;
  ex1d1, ey1d1, hx1d1, hy1d1, hz1d1: Double1D;
  ex1d2, ey1d2, hx1d2, hy1d2, hz1d2: Double1D;
  tmpex1d1, tmpey1d1, tmphx1d1, tmphy1d1, tmphz1d1: Double1D;
  tmpex1d2, tmpey1d2, tmphx1d2, tmphy1d2, tmphz1d2: Double1D;
  examps1, eyamps1, hxamps1, hyamps1, hzamps1, exphss1, eyphss1, hxphss1, hyphss1, hzphss1: Double;
  examps2, eyamps2, hxamps2, hyamps2, hzamps2, exphss2, eyphss2, hxphss2, hyphss2, hzphss2: Double;
  tmpexamps1, tmpeyamps1, tmphxamps1, tmphyamps1, tmphzamps1, tmpexphss1, tmpeyphss1, tmphxphss1, tmphyphss1,
    tmphzphss1: Double;
  tmpexamps2, tmpeyamps2, tmphxamps2, tmphyamps2, tmphzamps2, tmpexphss2, tmpeyphss2, tmphxphss2, tmphyphss2,
    tmphzphss2: Double;
  deltatime, deltaPha1, deltaPha2: Double;
  window: Double1D;
  wcount: Integer;
  J1, J2: Integer;
  tmpd: Double;
  scount, tmpcount, leftcount: Integer;
  da1, da2, dp1, dp2: Double;
  ran: Double;
begin
  FCount := Length(freList);
  TCount := Trunc(SecondsBetween(btime, etime) * SampleRate);
  ex := MakeDouble1D(TCount, 0);
  ey := MakeDouble1D(TCount, 0);
  hx := MakeDouble1D(TCount, 0);
  hy := MakeDouble1D(TCount, 0);
  hz := MakeDouble1D(TCount, 0);
  exts1 := MakeDouble1D(TCount, 0);
  eyts1 := MakeDouble1D(TCount, 0);
  hxts1 := MakeDouble1D(TCount, 0);
  hyts1 := MakeDouble1D(TCount, 0);
  hzts1 := MakeDouble1D(TCount, 0);
  exts2 := MakeDouble1D(TCount, 0);
  eyts2 := MakeDouble1D(TCount, 0);
  hxts2 := MakeDouble1D(TCount, 0);
  hyts2 := MakeDouble1D(TCount, 0);
  hzts2 := MakeDouble1D(TCount, 0);
  SetLength(ex1d1, TCount);
  SetLength(ey1d1, TCount);
  SetLength(hx1d1, TCount);
  SetLength(hy1d1, TCount);
  SetLength(hz1d1, TCount);
  SetLength(ex1d2, TCount);
  SetLength(ey1d2, TCount);
  SetLength(hx1d2, TCount);
  SetLength(hy1d2, TCount);
  SetLength(hz1d2, TCount);
  deltatime := MilliSecondsBetween(0, btime) / 1000.0;

  deltatime := MilliSecondsBetween(0, btime) / 1000.0;
  for I := 0 to FCount - 1 do // Fre Cycle
  begin
    if (freList[I] <= MinFre) or (freList[I] >= MaxFre) then
      Continue;
    deltaPha1 := deltatime / (2.0 * System.Pi);
    deltaPha1 := deltaPha1 - (Trunc(deltaPha1 * 180 / System.Pi) div 360) * 2 * System.Pi;
    deltaPha2 := deltaPha1 + System.Pi / 4;
    examps1 := Ex1[I].Module * AllScale;
    eyamps1 := Ey1[I].Module * AllScale;
    hxamps1 := Hx1[I].Module * AllScale;
    hyamps1 := Hy1[I].Module * AllScale;
    hzamps1 := Hz1[I].Module * AllScale;

    exphss1 := Ex1[I].Phase + System.Pi;
    eyphss1 := Ey1[I].Phase + System.Pi;
    hxphss1 := Hx1[I].Phase;
    hyphss1 := Hy1[I].Phase;
    hzphss1 := Hz1[I].Phase;

    exphss1 := exphss1 + deltaPha1;
    eyphss1 := eyphss1 + deltaPha1;
    hxphss1 := hxphss1 + deltaPha1;
    hyphss1 := hyphss1 + deltaPha1;
    hzphss1 := hzphss1 + deltaPha1;

    examps2 := Ex2[I].Module * AllScale;
    eyamps2 := Ey2[I].Module * AllScale;
    hxamps2 := Hx2[I].Module * AllScale;
    hyamps2 := Hy2[I].Module * AllScale;
    hzamps2 := Hz2[I].Module * AllScale;

    exphss2 := Ex2[I].Phase + System.Pi;
    eyphss2 := Ey2[I].Phase + System.Pi;
    hxphss2 := Hx2[I].Phase;
    hyphss2 := Hy2[I].Phase;
    hzphss2 := Hz2[I].Phase;

    exphss2 := exphss2 + deltaPha2;
    eyphss2 := eyphss2 + deltaPha2;
    hxphss2 := hxphss2 + deltaPha2;
    hyphss2 := hyphss2 + deltaPha2;
    hzphss2 := hzphss2 + deltaPha2;

    wcount := Trunc(SampleRate / 2 / freList[I]) * 2;
    if wcount <= 2 then
    begin
      wcount := 2;
    end;
    window := InvHanningWin(wcount);
    leftcount := TCount;
    while leftcount > 0 do
    begin
      RandSeed := MilliSecondsBetween(0, etime - (leftcount / SampleRate + 1) / freList[I] / 24 / 3600);
      tmpcount := Ceil(Abs(SampleRate / freList[I] * (RandG(SyntheticPeriods, SyntheticPeriods / 2))));
      while tmpcount < 10 do
      begin
        tmpcount := Ceil(Abs(SampleRate / freList[I] * (RandG(SyntheticPeriods, SyntheticPeriods / 2))));
      end;
      da1 := RandG(1, 1);
      dp1 := Random * System.Pi * 2;
      if tmpcount > leftcount then
      begin
        tmpcount := leftcount;
      end;
      if leftcount - tmpcount < 10 then
      begin
        tmpcount := leftcount;
      end;

      SetLength(tmpex1d1, tmpcount);
      SetLength(tmpey1d1, tmpcount);
      SetLength(tmphx1d1, tmpcount);
      SetLength(tmphy1d1, tmpcount);
      SetLength(tmphz1d1, tmpcount);

      tmpexamps1 := examps1 * da1;
      tmpeyamps1 := eyamps1 * da1;
      tmphxamps1 := hxamps1 * da1;
      tmphyamps1 := hyamps1 * da1;
      tmphzamps1 := hzamps1 * da1;
      tmpexphss1 := exphss1 + dp1;
      tmpeyphss1 := eyphss1 + dp1;
      tmphxphss1 := hxphss1 + dp1;
      tmphyphss1 := hyphss1 + dp1;
      tmphzphss1 := hzphss1 + dp1;

      FreToTimeSeries(tmpexamps1, tmpexphss1, freList[I], SampleRate, tmpcount, tmpex1d1);
      FreToTimeSeries(tmpeyamps1, tmpeyphss1, freList[I], SampleRate, tmpcount, tmpey1d1);
      FreToTimeSeries(tmphxamps1, tmphxphss1, freList[I], SampleRate, tmpcount, tmphx1d1);
      FreToTimeSeries(tmphyamps1, tmphyphss1, freList[I], SampleRate, tmpcount, tmphy1d1);
      FreToTimeSeries(tmphzamps1, tmphzphss1, freList[I], SampleRate, tmpcount, tmphz1d1);

      Move(tmpex1d1[0], ex1d1[TCount - leftcount], tmpcount * 8);
      Move(tmpey1d1[0], ey1d1[TCount - leftcount], tmpcount * 8);
      Move(tmphx1d1[0], hx1d1[TCount - leftcount], tmpcount * 8);
      Move(tmphy1d1[0], hy1d1[TCount - leftcount], tmpcount * 8);
      Move(tmphz1d1[0], hz1d1[TCount - leftcount], tmpcount * 8);

      if tmpcount > wcount / 2 then
      begin
        J1 := TCount - leftcount - wcount div 2;
        J2 := TCount - leftcount + wcount div 2 - 1;
        for J := J1 to J2 do
        begin
          if J < 0 then
          begin
            break;
          end;
          if J > TCount - 1 then
          begin
            Continue;
          end;
          ex1d1[J] := ex1d1[J] * window[J - J1];
          ey1d1[J] := ey1d1[J] * window[J - J1];
          hx1d1[J] := hx1d1[J] * window[J - J1];
          hy1d1[J] := hy1d1[J] * window[J - J1];
          hz1d1[J] := hz1d1[J] * window[J - J1];

        end;
      end;

      leftcount := leftcount - tmpcount;
    end;

    leftcount := TCount;
    while leftcount > 0 do
    begin
      RandSeed := MilliSecondsBetween(0, etime - (leftcount / SampleRate - 1) / freList[I] / 24 / 3600);
      tmpcount := Ceil(Abs(SampleRate / freList[I] * (RandG(SyntheticPeriods, SyntheticPeriods / 2))));
      while tmpcount < 10 do
      begin
        tmpcount := Ceil(Abs(SampleRate / freList[I] * (RandG(SyntheticPeriods, SyntheticPeriods / 2))));
      end;
      da2 := Random * 2;
      dp2 := Random * System.Pi * 2;
      if tmpcount > leftcount then
      begin
        tmpcount := leftcount;
      end;
      if leftcount - tmpcount < 10 then
      begin
        tmpcount := leftcount;
      end;

      SetLength(tmpex1d2, tmpcount);
      SetLength(tmpey1d2, tmpcount);
      SetLength(tmphx1d2, tmpcount);
      SetLength(tmphy1d2, tmpcount);
      SetLength(tmphz1d2, tmpcount);

      tmpexamps2 := examps2 * da2;
      tmpeyamps2 := eyamps2 * da2;
      tmphxamps2 := hxamps2 * da2;
      tmphyamps2 := hyamps2 * da2;
      tmphzamps2 := hzamps2 * da2;
      tmpexphss2 := exphss2 + dp2;
      tmpeyphss2 := eyphss2 + dp2;
      tmphxphss2 := hxphss2 + dp2;
      tmphyphss2 := hyphss2 + dp2;
      tmphzphss2 := hzphss2 + dp2;

      FreToTimeSeries(tmpexamps2, tmpexphss2, freList[I], SampleRate, tmpcount, tmpex1d2);
      FreToTimeSeries(tmpeyamps2, tmpeyphss2, freList[I], SampleRate, tmpcount, tmpey1d2);
      FreToTimeSeries(tmphxamps2, tmphxphss2, freList[I], SampleRate, tmpcount, tmphx1d2);
      FreToTimeSeries(tmphyamps2, tmphyphss2, freList[I], SampleRate, tmpcount, tmphy1d2);
      FreToTimeSeries(tmphzamps2, tmphzphss2, freList[I], SampleRate, tmpcount, tmphz1d2);

      Move(tmpex1d2[0], ex1d2[TCount - leftcount], tmpcount * 8);
      Move(tmpey1d2[0], ey1d2[TCount - leftcount], tmpcount * 8);
      Move(tmphx1d2[0], hx1d2[TCount - leftcount], tmpcount * 8);
      Move(tmphy1d2[0], hy1d2[TCount - leftcount], tmpcount * 8);
      Move(tmphz1d2[0], hz1d2[TCount - leftcount], tmpcount * 8);

      if tmpcount > wcount / 2 then
      begin
        J1 := TCount - leftcount - wcount div 2;
        J2 := TCount - leftcount + wcount div 2 - 1;
        for J := J1 to J2 do
        begin
          if J < 0 then
          begin
            break;
          end;
          if J > TCount - 1 then
          begin
            Continue;
          end;
          ex1d2[J] := ex1d2[J] * window[J - J1];
          ey1d2[J] := ey1d2[J] * window[J - J1];
          hx1d2[J] := hx1d2[J] * window[J - J1];
          hy1d2[J] := hy1d2[J] * window[J - J1];
          hz1d2[J] := hz1d2[J] * window[J - J1];

        end;
      end;
      leftcount := leftcount - tmpcount;
    end;
    // vdAdd(TCount, @exts1[0], @ex1d1[0], @exts1[0]);
    exts1 := VectorAdd(exts1, ex1d1);
    eyts1 := VectorAdd(eyts1, ey1d1);
    hxts1 := VectorAdd(hxts1, hx1d1);
    hyts1 := VectorAdd(hyts1, hy1d1);
    hzts1 := VectorAdd(hzts1, hz1d1);

    exts2 := VectorAdd(exts2, ex1d2);
    eyts2 := VectorAdd(eyts2, ey1d2);
    hxts2 := VectorAdd(hxts2, hx1d2);
    hyts2 := VectorAdd(hyts2, hy1d2);
    hzts2 := VectorAdd(hzts2, hz1d2);

  end;

  ex := VectorAdd(exts1, exts2);
  ey := VectorAdd(eyts1, eyts2);
  hx := VectorAdd(hxts1, hxts2);
  hy := VectorAdd(hyts1, hyts2);
  hz := VectorAdd(hzts1, hzts2);

  ex1d1 := nil;
  ey1d1 := nil;
  hx1d1 := nil;
  hy1d1 := nil;
  hz1d1 := nil;

  ex1d2 := nil;
  ey1d2 := nil;
  hx1d2 := nil;
  hy1d2 := nil;
  hz1d2 := nil;

  tmpex1d1 := nil;
  tmpey1d1 := nil;
  tmphx1d1 := nil;
  tmphy1d1 := nil;
  tmphz1d1 := nil;

  tmpex1d2 := nil;
  tmpey1d2 := nil;
  tmphx1d2 := nil;
  tmphy1d2 := nil;
  tmphz1d2 := nil;

  exts1 := nil;
  eyts1 := nil;
  hxts1 := nil;
  hyts1 := nil;
  hzts1 := nil;
end;

procedure SyntheticSeriesFix(const SampleRate, btime, etime, MaxFre, MinFre: Double; var ex, ey, hx, hy, hz: Double1D;
  const freList: Double1D; const Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2, Hz2: TComplex1D;
  const AllScale: Double = 1);
var
  Ex12, Ey12, Hx12, Hy12, Hz12: TComplex;
  Index: Integer;
  timeLength: Double;
  I, J, K, Channels, FCount, TCount: Integer;
  exts1, eyts1, hxts1, hyts1, hzts1: Double1D;
begin
  FCount := Length(freList);
  TCount := Trunc(SecondsBetween(btime, etime) * SampleRate);
  ex := MakeDouble1D(TCount, 0);
  ey := MakeDouble1D(TCount, 0);
  hx := MakeDouble1D(TCount, 0);
  hy := MakeDouble1D(TCount, 0);
  hz := MakeDouble1D(TCount, 0);
  exts1 := MakeDouble1D(TCount, 0);
  eyts1 := MakeDouble1D(TCount, 0);
  hxts1 := MakeDouble1D(TCount, 0);
  hyts1 := MakeDouble1D(TCount, 0);
  hzts1 := MakeDouble1D(TCount, 0);
  for I := 0 to FCount - 1 do // Fre Cycle
  begin
    if (freList[I] < MinFre) or (freList[I] > MaxFre) then
      Continue;
    Ex12 := -(Ex1[I] + Ex2[I]) * AllScale;
    Ey12 := -(Ey1[I] + Ey2[I]) * AllScale;
    Hx12 := (Hx1[I] + Hx2[I]) * AllScale;
    Hy12 := (Hy1[I] + Hy2[I]) * AllScale;
    Hz12 := (Hz1[I] + Hz2[I]) * AllScale;

    FreToTimeSeries(Ex12.Module, Ex12.Phase, freList[I], SampleRate, TCount, exts1);
    FreToTimeSeries(Ey12.Module, Ey12.Phase, freList[I], SampleRate, TCount, eyts1);
    FreToTimeSeries(Hx12.Module, Hx12.Phase, freList[I], SampleRate, TCount, hxts1);
    FreToTimeSeries(Hy12.Module, Hy12.Phase, freList[I], SampleRate, TCount, hyts1);
    FreToTimeSeries(Hz12.Module, Hz12.Phase, freList[I], SampleRate, TCount, hzts1);

    ex := VectorAdd(exts1, ex);
    ey := VectorAdd(eyts1, ex);
    hx := VectorAdd(hxts1, ex);
    hy := VectorAdd(hyts1, ex);
    hz := VectorAdd(hzts1, ex);
  end;

  exts1 := nil;
  eyts1 := nil;
  hxts1 := nil;
  hyts1 := nil;
  hzts1 := nil;
end;

{ SyntheticTimeSeries }
constructor TSyntheticTimeSeries.Create;
begin
  FSchemas := TList<TSyntheticSchema>.Create;
end;

destructor TSyntheticTimeSeries.Destroy;
begin
  SchemaClear;
  FSchemas.Free;
  inherited;
end;

procedure TSyntheticTimeSeries.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TSyntheticTimeSeries.SetDescription(const _Value: String);
begin
  FDescription := _Value;
end;

procedure TSyntheticTimeSeries.SetBeginTime(const _Value: Double);
begin
  FBeginTime := _Value;
end;

procedure TSyntheticTimeSeries.SetEndTime(const _Value: Double);
begin
  FEndTime := _Value;
end;

procedure TSyntheticTimeSeries.SetSchemas(const _Value: TList<TSyntheticSchema>);
begin
  SchemaClear;
  FSchemas := _Value;
end;

function TSyntheticTimeSeries.GetSchema(Index: Integer): TSyntheticSchema;
begin
  Result := FSchemas[Index];
end;

procedure TSyntheticTimeSeries.SetSchema(Index: Integer; const _Value: TSyntheticSchema);
begin
  FSchemas[Index].Free;
  FSchemas[Index] := _Value;
end;

procedure TSyntheticTimeSeries.AddSchema(_Value: TSyntheticSchema);
begin;
  FSchemas.Add(_Value);
end;

function TSyntheticTimeSeries.AddNewSchema: TSyntheticSchema;
var
  Schematmp: TSyntheticSchema;
begin;
  Schematmp := TSyntheticSchema.Create;
  FSchemas.Add(Schematmp);
  Result := Schematmp;
end;

procedure TSyntheticTimeSeries.SchemaClear;
begin
  while FSchemas.Count > 0 do
  begin
    FSchemas.Items[0].Free;
    FSchemas.Delete(0);
  end;
end;

function TSyntheticTimeSeries.SchemaCount: Integer;
begin
  Result := FSchemas.Count;
end;

procedure TSyntheticTimeSeries.RemoveSchema(_Value: TSyntheticSchema);
begin
  FSchemas.Remove(_Value);
  _Value.Free;
end;

procedure TSyntheticTimeSeries.DeleteSchema(Index: Integer);
begin
  FSchemas.Items[Index].Free;
  FSchemas.Delete(Index);
end;

function TSyntheticTimeSeries.SyntheticSites(sites: TForwardSite; const rotate: Double = 0): TSiteTimeSeries;
var
  I, J, scount: Integer;
begin
  Result := TSiteTimeSeries.Create;
  for I := 0 to Self.SchemaCount - 1 do
  begin
    Result.AddTimeSeries(Self.Schema[I].SyntheticSite(sites, Self.BeginTime, Self.EndTime, rotate));
  end;
end;

{ SyntheticSchema }
constructor TSyntheticSchema.Create;
begin
end;

destructor TSyntheticSchema.Destroy;
begin
  inherited;
end;

procedure TSyntheticSchema.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TSyntheticSchema.SetDescription(const _Value: String);
begin
  FDescription := _Value;
end;

procedure TSyntheticSchema.SetSyntheticPeriods(const _Value: Double);
begin
  FSyntheticPeriods := _Value;
end;

procedure TSyntheticSchema.SetContinous(const _Value: Boolean);
begin
  FContinous := _Value;
end;

procedure TSyntheticSchema.SetSampleRate(const _Value: Double);
begin
  FSampleRate := _Value;
end;

procedure TSyntheticSchema.SetMaxFrequency(const _Value: Double);
begin
  FMaxFrequency := _Value;
end;

procedure TSyntheticSchema.SetMinFrequency(const _Value: Double);
begin
  FMinFrequency := _Value;
end;

procedure TSyntheticSchema.SetIntervals(const _Value: Double);
begin
  FIntervals := _Value;
end;

procedure TSyntheticSchema.SetOffset(const _Value: Double);
begin
  FOffset := _Value;
end;

procedure TSyntheticSchema.SetSamplingTime(const _Value: Double);
begin
  FSamplingTime := _Value;
end;

procedure TSyntheticSchema.SetSourceScale(const _Value: Double);
begin
  FSourceScale := _Value;
end;

procedure TSyntheticSchema.SetSyntheticFunciton(const _Value: Integer);
begin
  FSyntheticFunciton := _Value;
end;

function TSyntheticSchema.SyntheticSite(site: TForwardSite; const btime: TDateTime; etime: TDateTime;
  const rotate: Double = 0): TTimeSeries;
var
  scount: Integer;
  I: Integer;
  BeginTime, EndTime: TDateTime;
  TsEx, TsEy, TsHx, TsHy, TsHz: Double2D;
  TsExtmp, TsEytmp, TsHxtmp, TsHytmp, TsHztmp: Double1D;
  fre: Double1D;
  Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2, Hz2: TComplex1D;
  SF: SyntheticFunction;
begin
  site.GetFEH(fre, Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2, Hz2);
  for I := 0 to Length(fre) - 1 do
  begin
    EHRotate(Ex1[I], Ey1[I], Hx1[I], Hy1[I], rotate);
    EHRotate(Ex2[I], Ey2[I], Hx2[I], Hy2[I], rotate);
  end;
  if Self.Continous then
  begin
    Result := TContinuousTimeSeries.Create;
    scount := 1;
    Result.AddBeginTime(btime);
    Result.AddEndTime(IncSecond(etime));
  end
  else
  begin
    Result := TMultipleTimeSeries.Create;
    scount := Trunc((SecondsBetween(btime, etime) - Self.Offset) / Self.Intervals) + 1;
    for I := 0 to scount - 1 do
    begin
      BeginTime := IncSecond(btime, Trunc(Self.Intervals * I + Self.Offset));
      EndTime := IncMilliSecond(BeginTime, Round(Self.SamplingTime * 1000));
      Result.AddBeginTime(BeginTime);
      Result.AddEndTime(EndTime);
    end;
  end;

  SetLength(TsEx, scount);
  SetLength(TsEy, scount);
  SetLength(TsHx, scount);
  SetLength(TsHy, scount);
  SetLength(TsHz, scount);
  SF := SyntheticFunction(Self.SyntheticFunciton);
  for I := 0 to scount - 1 do
  begin
    case SF of
      Synthetic_Fix:
        begin
          SyntheticSeriesFix(Self.SampleRate, Result.BeginTime[I], Result.EndTime[I], Self.MaxFrequency,
            Self.MinFrequency, TsExtmp, TsEytmp, TsHxtmp, TsHytmp, TsHztmp, fre, Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2,
            Hy2, Hz2, Self.SourceScale);
        end;
      Synthetic_AverageSegment:
        begin
          AverageSegmentSyntheticSeries(Self.SampleRate, Result.BeginTime[I], Result.EndTime[I], Self.MaxFrequency,
            Self.MinFrequency, TsExtmp, TsEytmp, TsHxtmp, TsHytmp, TsHztmp, fre, Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2,
            Hy2, Hz2, Self.SyntheticPeriods, Self.SourceScale);
        end;
      Synthetic_AverageSegmentWindowed:
        begin
          AverageSegmentWindowedSyntheticSeries(Self.SampleRate, Result.BeginTime[I], Result.EndTime[I],
            Self.MaxFrequency, Self.MinFrequency, TsExtmp, TsEytmp, TsHxtmp, TsHytmp, TsHztmp, fre, Ex1, Ey1, Hx1, Hy1,
            Hz1, Ex2, Ey2, Hx2, Hy2, Hz2, Self.SyntheticPeriods, Self.SourceScale);
        end;
      Synthetic_RandomSegment:
        begin
          RandomSegmentLengthSyntheticSeries(Self.SampleRate, Result.BeginTime[I], Result.EndTime[I], Self.MaxFrequency,
            Self.MinFrequency, TsExtmp, TsEytmp, TsHxtmp, TsHytmp, TsHztmp, fre, Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2,
            Hy2, Hz2, Self.SyntheticPeriods, Self.SourceScale);
        end;
      Synthetic_RandomSegmentWindowed:
        begin
          RandomSegmentLengthWindowedSyntheticSeries(Self.SampleRate, Result.BeginTime[I], Result.EndTime[I],
            Self.MaxFrequency, Self.MinFrequency, TsExtmp, TsEytmp, TsHxtmp, TsHytmp, TsHztmp, fre, Ex1, Ey1, Hx1, Hy1,
            Hz1, Ex2, Ey2, Hx2, Hy2, Hz2, Self.SyntheticPeriods, Self.SourceScale);
        end;
      Synthetic_RandomSegmentPartiallyWindowed:
        begin
          RandomSegmentLengthPartiallyWindowedSyntheticSeries(Self.SampleRate, Result.BeginTime[I], Result.EndTime[I],
            Self.MaxFrequency, Self.MinFrequency, TsExtmp, TsEytmp, TsHxtmp, TsHytmp, TsHztmp, fre, Ex1, Ey1, Hx1, Hy1,
            Hz1, Ex2, Ey2, Hx2, Hy2, Hz2, Self.SyntheticPeriods, Self.SourceScale);
        end;
    end;
    TsEx[I] := TsExtmp;
    TsEy[I] := TsEytmp;
    TsHx[I] := TsHxtmp;
    TsHy[I] := TsHytmp;
    TsHz[I] := TsHztmp;
  end;
  Result.ExField := TsEx;
  Result.EyField := TsEy;
  Result.HxField := TsHx;
  Result.HyField := TsHy;
  Result.HzField := TsHz;
  Result.SampleRate := Self.SampleRate;
  Result.Name := Self.Name;
end;

end.
