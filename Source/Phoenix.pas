unit Phoenix;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Generics.Collections, WTypes;

type

  TTBL = class;
  TTBLRecord = class;
  TTS = class;
  TTSRecord = class;
  TCLB = class;
  TCLC = class;
  TCLRecord = class;

  TTBL = class(TObject)
  private
    FName: String;
    FPath: String;
    FTBLRecords: TList<TTBLRecord>;
    procedure SetName(const _Value: String);
    procedure SetPath(const _Value: String);
    procedure SetTBLRecords(const _Value: TList<TTBLRecord>);
    function GetTBLRecord(Index: Integer): TTBLRecord;
    procedure SetTBLRecord(Index: Integer; const _Value: TTBLRecord);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTBLRecord(_Value: TTBLRecord);
    function AddNewTBLRecord: TTBLRecord;
    procedure TBLRecordClear;
    function TBLRecordCount: Integer;
    procedure RemoveTBLRecord(_Value: TTBLRecord);
    procedure DeleteTBLRecord(Index: Integer);
    property Name: String read FName write SetName;
    property Path: String read FPath write SetPath;
    property TBLRecords: TList<TTBLRecord> read FTBLRecords write SetTBLRecords;
    property TBLRecord[Index: Integer]: TTBLRecord read GetTBLRecord write SetTBLRecord;
  end;

  TTBLRecord = class(TObject)
  private
    FName: String;
    FValue: String;
    FIdentifier: Integer;
    FShort: Integer;
    FLong: Integer;
    procedure SetName(const _Value: String);
    procedure SetValue(const _Value: String);
    procedure SetIdentifier(const _Value: Integer);
    procedure SetShort(const _Value: Integer);
    procedure SetLong(const _Value: Integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property Name: String read FName write SetName;
    property Value: String read FValue write SetValue;
    property Identifier: Integer read FIdentifier write SetIdentifier;
    property Short: Integer read FShort write SetShort;
    property Long: Integer read FLong write SetLong;
  end;

  TTS = class(TObject)
  private
    FName: String;
    FPath: String;
    FBeginTime: TDateTime;
    FEndTime: TDateTime;
    FScans: Integer;
    FChannels: Integer;
    FSamplelength: Integer;
    FSampleRate: Integer;
    FBoxSN: Integer;
    FSampleRateUnits: String;
    FTSLength: Integer;
    FFileName: String;
    FLengthPre: Integer;
    FTSRecords: TList<TTSRecord>;
    procedure SetName(const _Value: String);
    procedure SetPath(const _Value: String);
    procedure SetBeginTime(const _Value: TDateTime);
    procedure SetEndTime(const _Value: TDateTime);
    procedure SetScans(const _Value: Integer);
    procedure SetChannels(const _Value: Integer);
    procedure SetSamplelength(const _Value: Integer);
    procedure SetSampleRate(const _Value: Integer);
    procedure SetBoxSN(const _Value: Integer);
    procedure SetSampleRateUnits(const _Value: String);
    procedure SetTSLength(const _Value: Integer);
    procedure SetFileName(const _Value: String);
    procedure SetLengthPre(const _Value: Integer);
    procedure SetTSRecords(const _Value: TList<TTSRecord>);
    function GetTSRecord(Index: Integer): TTSRecord;
    procedure SetTSRecord(Index: Integer; const _Value: TTSRecord);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTSRecord(_Value: TTSRecord);
    function AddNewTSRecord: TTSRecord;
    procedure TSRecordClear;
    function TSRecordCount: Integer;
    procedure RemoveTSRecord(_Value: TTSRecord);
    procedure DeleteTSRecord(Index: Integer);
    property Name: String read FName write SetName;
    property Path: String read FPath write SetPath;
    property BeginTime: TDateTime read FBeginTime write SetBeginTime;
    property EndTime: TDateTime read FEndTime write SetEndTime;
    property Scans: Integer read FScans write SetScans;
    property Channels: Integer read FChannels write SetChannels;
    property Samplelength: Integer read FSamplelength write SetSamplelength;
    property SampleRate: Integer read FSampleRate write SetSampleRate;
    property BoxSN: Integer read FBoxSN write SetBoxSN;
    property SampleRateUnits: String read FSampleRateUnits write SetSampleRateUnits;
    property TSLength: Integer read FTSLength write SetTSLength;
    property fileName: String read FFileName write SetFileName;
    property LengthPre: Integer read FLengthPre write SetLengthPre;
    property TSRecords: TList<TTSRecord> read FTSRecords write SetTSRecords;
    property TSRecord[Index: Integer]: TTSRecord read GetTSRecord write SetTSRecord;
  end;

  TTSRecord = class(TObject)
  private
    FDateTime: TDateTime;
    FBoxSN: Smallint;
    FScans: Smallint;
    FChannels: Byte;
    FTSLength: Byte;
    FStatusCodes: Byte;
    FSaturationFlag: Byte;
    FReservedbit: Byte;
    FSamplelength: Byte;
    FSampleRate: Smallint;
    FSampleRateUnits: Byte;
    FClockStatus: Byte;
    FClockError: Integer;
    FData: Integer2D;
    procedure SetDateTime(const _Value: TDateTime);
    procedure SetBoxSN(const _Value: Smallint);
    procedure SetScans(const _Value: Smallint);
    procedure SetChannels(const _Value: Byte);
    procedure SetTSLength(const _Value: Byte);
    procedure SetStatusCodes(const _Value: Byte);
    procedure SetSaturationFlag(const _Value: Byte);
    procedure SetReservedbit(const _Value: Byte);
    procedure SetSamplelength(const _Value: Byte);
    procedure SetSampleRate(const _Value: Smallint);
    procedure SetSampleRateUnits(const _Value: Byte);
    procedure SetClockStatus(const _Value: Byte);
    procedure SetClockError(const _Value: Integer);
    procedure SetData(const _Value: Integer2D);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property DateTime: TDateTime read FDateTime write SetDateTime;
    property BoxSN: Smallint read FBoxSN write SetBoxSN;
    property Scans: Smallint read FScans write SetScans;
    property Channels: Byte read FChannels write SetChannels;
    property TSLength: Byte read FTSLength write SetTSLength;
    property StatusCodes: Byte read FStatusCodes write SetStatusCodes;
    property SaturationFlag: Byte read FSaturationFlag write SetSaturationFlag;
    property Reservedbit: Byte read FReservedbit write SetReservedbit;
    property Samplelength: Byte read FSamplelength write SetSamplelength;
    property SampleRate: Smallint read FSampleRate write SetSampleRate;
    property SampleRateUnits: Byte read FSampleRateUnits write SetSampleRateUnits;
    property ClockStatus: Byte read FClockStatus write SetClockStatus;
    property ClockError: Integer read FClockError write SetClockError;
    property Data: Integer2D read FData write SetData;
  end;

  TCLB = class(TObject)
  private
    FName: String;
    FPath: String;
    FDateTime: TDateTime;
    FSNUM: Smallint;
    FNCHN: Smallint;
    Funknow1: Smallint;
    FSTAT: Smallint;
    Funknow2: Smallint;
    Funknow3: Smallint;
    FHW: String;
    FVER: String;
    FTCMB: Integer;
    FTALS: Integer;
    FLFRQ: Integer;
    FV5SR: Integer;
    FLPFR: Integer;
    Funknow4: String;
    FCLBRecords: TList<TCLRecord>;
    procedure SetName(const _Value: String);
    procedure SetPath(const _Value: String);
    procedure SetDateTime(const _Value: TDateTime);
    procedure SetSNUM(const _Value: Smallint);
    procedure SetNCHN(const _Value: Smallint);
    procedure Setunknow1(const _Value: Smallint);
    procedure SetSTAT(const _Value: Smallint);
    procedure Setunknow2(const _Value: Smallint);
    procedure Setunknow3(const _Value: Smallint);
    procedure SetHW(const _Value: String);
    procedure SetVER(const _Value: String);
    procedure SetTCMB(const _Value: Integer);
    procedure SetTALS(const _Value: Integer);
    procedure SetLFRQ(const _Value: Integer);
    procedure SetV5SR(const _Value: Integer);
    procedure SetLPFR(const _Value: Integer);
    procedure Setunknow4(const _Value: String);
    procedure SetCLBRecords(const _Value: TList<TCLRecord>);
    function GetCLBRecord(Index: Integer): TCLRecord;
    procedure SetCLBRecord(Index: Integer; const _Value: TCLRecord);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCLBRecord(_Value: TCLRecord);
    function AddNewCLBRecord: TCLRecord;
    procedure CLBRecordClear;
    function CLBRecordCount: Integer;
    procedure RemoveCLBRecord(_Value: TCLRecord);
    procedure DeleteCLBRecord(Index: Integer);
    property Name: String read FName write SetName;
    property Path: String read FPath write SetPath;
    property DateTime: TDateTime read FDateTime write SetDateTime;
    property SNUM: Smallint read FSNUM write SetSNUM;
    property NCHN: Smallint read FNCHN write SetNCHN;
    property unknow1: Smallint read Funknow1 write Setunknow1;
    property STAT: Smallint read FSTAT write SetSTAT;
    property unknow2: Smallint read Funknow2 write Setunknow2;
    property unknow3: Smallint read Funknow3 write Setunknow3;
    property HW: String read FHW write SetHW;
    property VER: String read FVER write SetVER;
    property TCMB: Integer read FTCMB write SetTCMB;
    property TALS: Integer read FTALS write SetTALS;
    property LFRQ: Integer read FLFRQ write SetLFRQ;
    property V5SR: Integer read FV5SR write SetV5SR;
    property LPFR: Integer read FLPFR write SetLPFR;
    property unknow4: String read Funknow4 write Setunknow4;
    property CLBRecords: TList<TCLRecord> read FCLBRecords write SetCLBRecords;
    property CLBRecord[Index: Integer]: TCLRecord read GetCLBRecord write SetCLBRecord;
  end;

  TCLC = class(TObject)
  private
    FName: String;
    FPath: String;
    FDateTime: TDateTime;
    FSNUM: Smallint;
    FNCHN: Smallint;
    Funknow1: Smallint;
    FSTAT: Smallint;
    Funknow2: Smallint;
    Funknow3: Smallint;
    FHW: String;
    FVER: String;
    FTCMB: Integer;
    FTALS: Integer;
    FLFRQ: Integer;
    FV5SR: Integer;
    FLPFR: Integer;
    FHSN: String;
    FHATT: Double;
    FHNOM: Double;
    FCPHC: Double;
    Funknow4: String;
    FCLCRecords: TList<TCLRecord>;
    procedure SetName(const _Value: String);
    procedure SetPath(const _Value: String);
    procedure SetDateTime(const _Value: TDateTime);
    procedure SetSNUM(const _Value: Smallint);
    procedure SetNCHN(const _Value: Smallint);
    procedure Setunknow1(const _Value: Smallint);
    procedure SetSTAT(const _Value: Smallint);
    procedure Setunknow2(const _Value: Smallint);
    procedure Setunknow3(const _Value: Smallint);
    procedure SetHW(const _Value: String);
    procedure SetVER(const _Value: String);
    procedure SetTCMB(const _Value: Integer);
    procedure SetTALS(const _Value: Integer);
    procedure SetLFRQ(const _Value: Integer);
    procedure SetV5SR(const _Value: Integer);
    procedure SetLPFR(const _Value: Integer);
    procedure SetHSN(const _Value: String);
    procedure SetHATT(const _Value: Double);
    procedure SetHNOM(const _Value: Double);
    procedure SetCPHC(const _Value: Double);
    procedure Setunknow4(const _Value: String);
    procedure SetCLCRecords(const _Value: TList<TCLRecord>);
    function GetCLCRecord(Index: Integer): TCLRecord;
    procedure SetCLCRecord(Index: Integer; const _Value: TCLRecord);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCLCRecord(_Value: TCLRecord);
    function AddNewCLCRecord: TCLRecord;
    procedure CLCRecordClear;
    function CLCRecordCount: Integer;
    procedure RemoveCLCRecord(_Value: TCLRecord);
    procedure DeleteCLCRecord(Index: Integer);
    property Name: String read FName write SetName;
    property Path: String read FPath write SetPath;
    property DateTime: TDateTime read FDateTime write SetDateTime;
    property SNUM: Smallint read FSNUM write SetSNUM;
    property NCHN: Smallint read FNCHN write SetNCHN;
    property unknow1: Smallint read Funknow1 write Setunknow1;
    property STAT: Smallint read FSTAT write SetSTAT;
    property unknow2: Smallint read Funknow2 write Setunknow2;
    property unknow3: Smallint read Funknow3 write Setunknow3;
    property HW: String read FHW write SetHW;
    property VER: String read FVER write SetVER;
    property TCMB: Integer read FTCMB write SetTCMB;
    property TALS: Integer read FTALS write SetTALS;
    property LFRQ: Integer read FLFRQ write SetLFRQ;
    property V5SR: Integer read FV5SR write SetV5SR;
    property LPFR: Integer read FLPFR write SetLPFR;
    property HSN: String read FHSN write SetHSN;
    property HATT: Double read FHATT write SetHATT;
    property HNOM: Double read FHNOM write SetHNOM;
    property CPHC: Double read FCPHC write SetCPHC;
    property unknow4: String read Funknow4 write Setunknow4;
    property CLCRecords: TList<TCLRecord> read FCLCRecords write SetCLCRecords;
    property CLCRecord[Index: Integer]: TCLRecord read GetCLCRecord write SetCLCRecord;
  end;

  TCLRecord = class(TObject)
  private
    Ffcn: Smallint;
    Fbaseband: Single;
    Fharmonic: Smallint;
    Frawreal: Double1D;
    Frawimag: Double1D;
    Ferr: Double1D;
    FFC: Double1D;
    Freal: Double1D;
    Fimag: Double1D;
    procedure Setfcn(const _Value: Smallint);
    procedure Setbaseband(const _Value: Single);
    procedure Setharmonic(const _Value: Smallint);
    procedure Setrawreal(const _Value: Double1D);
    procedure Setrawimag(const _Value: Double1D);
    procedure Seterr(const _Value: Double1D);
    procedure SetFC(const _Value: Double1D);
    procedure Setreal(const _Value: Double1D);
    procedure Setimag(const _Value: Double1D);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property fcn: Smallint read Ffcn write Setfcn;
    property baseband: Single read Fbaseband write Setbaseband;
    property harmonic: Smallint read Fharmonic write Setharmonic;
    property rawreal: Double1D read Frawreal write Setrawreal;
    property rawimag: Double1D read Frawimag write Setrawimag;
    property err: Double1D read Ferr write Seterr;
    property FC: Double1D read FFC write SetFC;
    property real: Double1D read Freal write Setreal;
    property imag: Double1D read Fimag write Setimag;
  end;

implementation

{ TBL }
constructor TTBL.Create;
begin
  FTBLRecords := TList<TTBLRecord>.Create;
end;

destructor TTBL.Destroy;
begin
  TBLRecordClear;
  FTBLRecords.Free;
  inherited;
end;

procedure TTBL.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TTBL.SetPath(const _Value: String);
begin
  FPath := _Value;
end;

procedure TTBL.SetTBLRecords(const _Value: TList<TTBLRecord>);
begin
  TBLRecordClear;
  FTBLRecords := _Value;
end;

function TTBL.GetTBLRecord(Index: Integer): TTBLRecord;
begin
  Result := FTBLRecords[Index];
end;

procedure TTBL.SetTBLRecord(Index: Integer; const _Value: TTBLRecord);
begin
  FTBLRecords[Index].Free;
  FTBLRecords[Index] := _Value;
end;

procedure TTBL.AddTBLRecord(_Value: TTBLRecord);
begin;
  FTBLRecords.Add(_Value);
end;

function TTBL.AddNewTBLRecord: TTBLRecord;
var
  TBLRecordtmp: TTBLRecord;
begin;
  TBLRecordtmp := TTBLRecord.Create;
  FTBLRecords.Add(TBLRecordtmp);
  Result := TBLRecordtmp;
end;

procedure TTBL.TBLRecordClear;
begin
  while FTBLRecords.Count > 0 do
  begin
    FTBLRecords.Items[0].Free;
    FTBLRecords.Delete(0);
  end;
end;

function TTBL.TBLRecordCount: Integer;
begin
  Result := FTBLRecords.Count;
end;

procedure TTBL.RemoveTBLRecord(_Value: TTBLRecord);
begin
  FTBLRecords.Remove(_Value);
  _Value.Free;
end;

procedure TTBL.DeleteTBLRecord(Index: Integer);
begin
  FTBLRecords.Items[Index].Free;
  FTBLRecords.Delete(Index);
end;

{ TBLRecord }
constructor TTBLRecord.Create;
begin
end;

destructor TTBLRecord.Destroy;
begin
  inherited;
end;

procedure TTBLRecord.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TTBLRecord.SetValue(const _Value: String);
begin
  FValue := _Value;
end;

procedure TTBLRecord.SetIdentifier(const _Value: Integer);
begin
  FIdentifier := _Value;
end;

procedure TTBLRecord.SetShort(const _Value: Integer);
begin
  FShort := _Value;
end;

procedure TTBLRecord.SetLong(const _Value: Integer);
begin
  FLong := _Value;
end;

{ TS }
constructor TTS.Create;
begin
  FTSRecords := TList<TTSRecord>.Create;
end;

destructor TTS.Destroy;
begin
  TSRecordClear;
  FTSRecords.Free;
  inherited;
end;

procedure TTS.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TTS.SetPath(const _Value: String);
begin
  FPath := _Value;
end;

procedure TTS.SetBeginTime(const _Value: TDateTime);
begin
  FBeginTime := _Value;
end;

procedure TTS.SetEndTime(const _Value: TDateTime);
begin
  FEndTime := _Value;
end;

procedure TTS.SetScans(const _Value: Integer);
begin
  FScans := _Value;
end;

procedure TTS.SetChannels(const _Value: Integer);
begin
  FChannels := _Value;
end;

procedure TTS.SetSamplelength(const _Value: Integer);
begin
  FSamplelength := _Value;
end;

procedure TTS.SetSampleRate(const _Value: Integer);
begin
  FSampleRate := _Value;
end;

procedure TTS.SetBoxSN(const _Value: Integer);
begin
  FBoxSN := _Value;
end;

procedure TTS.SetSampleRateUnits(const _Value: String);
begin
  FSampleRateUnits := _Value;
end;

procedure TTS.SetTSLength(const _Value: Integer);
begin
  FTSLength := _Value;
end;

procedure TTS.SetFileName(const _Value: String);
begin
  FFileName := _Value;
end;

procedure TTS.SetLengthPre(const _Value: Integer);
begin
  FLengthPre := _Value;
end;

procedure TTS.SetTSRecords(const _Value: TList<TTSRecord>);
begin
  TSRecordClear;
  FTSRecords := _Value;
end;

function TTS.GetTSRecord(Index: Integer): TTSRecord;
begin
  Result := FTSRecords[Index];
end;

procedure TTS.SetTSRecord(Index: Integer; const _Value: TTSRecord);
begin
  FTSRecords[Index].Free;
  FTSRecords[Index] := _Value;
end;

procedure TTS.AddTSRecord(_Value: TTSRecord);
begin;
  FTSRecords.Add(_Value);
end;

function TTS.AddNewTSRecord: TTSRecord;
var
  TSRecordtmp: TTSRecord;
begin;
  TSRecordtmp := TTSRecord.Create;
  FTSRecords.Add(TSRecordtmp);
  Result := TSRecordtmp;
end;

procedure TTS.TSRecordClear;
var
  I: Integer;
begin
  // while FTSRecords.Count > 0 do
  // begin
  // FTSRecords.Items[0].Free;
  // FTSRecords.Delete(0);
  // end;
  for I := 0 to FTSRecords.Count - 1 do
  begin
    FTSRecords[I].Free;
  end;
  FTSRecords.Clear;
end;

function TTS.TSRecordCount: Integer;
begin
  Result := FTSRecords.Count;
end;

procedure TTS.RemoveTSRecord(_Value: TTSRecord);
begin
  FTSRecords.Remove(_Value);
  _Value.Free;
end;

procedure TTS.DeleteTSRecord(Index: Integer);
begin
  FTSRecords.Items[Index].Free;
  FTSRecords.Delete(Index);
end;

{ TSRecord }
constructor TTSRecord.Create;
begin
end;

destructor TTSRecord.Destroy;
begin
  FData := nil;
  inherited;
end;

procedure TTSRecord.SetDateTime(const _Value: TDateTime);
begin
  FDateTime := _Value;
end;

procedure TTSRecord.SetBoxSN(const _Value: Smallint);
begin
  FBoxSN := _Value;
end;

procedure TTSRecord.SetScans(const _Value: Smallint);
begin
  FScans := _Value;
end;

procedure TTSRecord.SetChannels(const _Value: Byte);
begin
  FChannels := _Value;
end;

procedure TTSRecord.SetTSLength(const _Value: Byte);
begin
  FTSLength := _Value;
end;

procedure TTSRecord.SetStatusCodes(const _Value: Byte);
begin
  FStatusCodes := _Value;
end;

procedure TTSRecord.SetSaturationFlag(const _Value: Byte);
begin
  FSaturationFlag := _Value;
end;

procedure TTSRecord.SetReservedbit(const _Value: Byte);
begin
  FReservedbit := _Value;
end;

procedure TTSRecord.SetSamplelength(const _Value: Byte);
begin
  FSamplelength := _Value;
end;

procedure TTSRecord.SetSampleRate(const _Value: Smallint);
begin
  FSampleRate := _Value;
end;

procedure TTSRecord.SetSampleRateUnits(const _Value: Byte);
begin
  FSampleRateUnits := _Value;
end;

procedure TTSRecord.SetClockStatus(const _Value: Byte);
begin
  FClockStatus := _Value;
end;

procedure TTSRecord.SetClockError(const _Value: Integer);
begin
  FClockError := _Value;
end;

procedure TTSRecord.SetData(const _Value: Integer2D);
begin
  FData := _Value;
end;

{ CLB }
constructor TCLB.Create;
begin
  FCLBRecords := TList<TCLRecord>.Create;
end;

destructor TCLB.Destroy;
begin
  CLBRecordClear;
  FCLBRecords.Free;
  inherited;
end;

procedure TCLB.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TCLB.SetPath(const _Value: String);
begin
  FPath := _Value;
end;

procedure TCLB.SetDateTime(const _Value: TDateTime);
begin
  FDateTime := _Value;
end;

procedure TCLB.SetSNUM(const _Value: Smallint);
begin
  FSNUM := _Value;
end;

procedure TCLB.SetNCHN(const _Value: Smallint);
begin
  FNCHN := _Value;
end;

procedure TCLB.Setunknow1(const _Value: Smallint);
begin
  Funknow1 := _Value;
end;

procedure TCLB.SetSTAT(const _Value: Smallint);
begin
  FSTAT := _Value;
end;

procedure TCLB.Setunknow2(const _Value: Smallint);
begin
  Funknow2 := _Value;
end;

procedure TCLB.Setunknow3(const _Value: Smallint);
begin
  Funknow3 := _Value;
end;

procedure TCLB.SetHW(const _Value: String);
begin
  FHW := _Value;
end;

procedure TCLB.SetVER(const _Value: String);
begin
  FVER := _Value;
end;

procedure TCLB.SetTCMB(const _Value: Integer);
begin
  FTCMB := _Value;
end;

procedure TCLB.SetTALS(const _Value: Integer);
begin
  FTALS := _Value;
end;

procedure TCLB.SetLFRQ(const _Value: Integer);
begin
  FLFRQ := _Value;
end;

procedure TCLB.SetV5SR(const _Value: Integer);
begin
  FV5SR := _Value;
end;

procedure TCLB.SetLPFR(const _Value: Integer);
begin
  FLPFR := _Value;
end;

procedure TCLB.Setunknow4(const _Value: String);
begin
  Funknow4 := _Value;
end;

procedure TCLB.SetCLBRecords(const _Value: TList<TCLRecord>);
begin
  CLBRecordClear;
  FCLBRecords := _Value;
end;

function TCLB.GetCLBRecord(Index: Integer): TCLRecord;
begin
  Result := FCLBRecords[Index];
end;

procedure TCLB.SetCLBRecord(Index: Integer; const _Value: TCLRecord);
begin
  FCLBRecords[Index].Free;
  FCLBRecords[Index] := _Value;
end;

procedure TCLB.AddCLBRecord(_Value: TCLRecord);
begin;
  FCLBRecords.Add(_Value);
end;

function TCLB.AddNewCLBRecord: TCLRecord;
var
  CLBRecordtmp: TCLRecord;
begin;
  CLBRecordtmp := TCLRecord.Create;
  FCLBRecords.Add(CLBRecordtmp);
  Result := CLBRecordtmp;
end;

procedure TCLB.CLBRecordClear;
begin
  while FCLBRecords.Count > 0 do
  begin
    FCLBRecords.Items[0].Free;
    FCLBRecords.Delete(0);
  end;
end;

function TCLB.CLBRecordCount: Integer;
begin
  Result := FCLBRecords.Count;
end;

procedure TCLB.RemoveCLBRecord(_Value: TCLRecord);
begin
  FCLBRecords.Remove(_Value);
  _Value.Free;
end;

procedure TCLB.DeleteCLBRecord(Index: Integer);
begin
  FCLBRecords.Items[Index].Free;
  FCLBRecords.Delete(Index);
end;

{ CLC }
constructor TCLC.Create;
begin
  FCLCRecords := TList<TCLRecord>.Create;
end;

destructor TCLC.Destroy;
begin
  CLCRecordClear;
  FCLCRecords.Free;
  inherited;
end;

procedure TCLC.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TCLC.SetPath(const _Value: String);
begin
  FPath := _Value;
end;

procedure TCLC.SetDateTime(const _Value: TDateTime);
begin
  FDateTime := _Value;
end;

procedure TCLC.SetSNUM(const _Value: Smallint);
begin
  FSNUM := _Value;
end;

procedure TCLC.SetNCHN(const _Value: Smallint);
begin
  FNCHN := _Value;
end;

procedure TCLC.Setunknow1(const _Value: Smallint);
begin
  Funknow1 := _Value;
end;

procedure TCLC.SetSTAT(const _Value: Smallint);
begin
  FSTAT := _Value;
end;

procedure TCLC.Setunknow2(const _Value: Smallint);
begin
  Funknow2 := _Value;
end;

procedure TCLC.Setunknow3(const _Value: Smallint);
begin
  Funknow3 := _Value;
end;

procedure TCLC.SetHW(const _Value: String);
begin
  FHW := _Value;
end;

procedure TCLC.SetVER(const _Value: String);
begin
  FVER := _Value;
end;

procedure TCLC.SetTCMB(const _Value: Integer);
begin
  FTCMB := _Value;
end;

procedure TCLC.SetTALS(const _Value: Integer);
begin
  FTALS := _Value;
end;

procedure TCLC.SetLFRQ(const _Value: Integer);
begin
  FLFRQ := _Value;
end;

procedure TCLC.SetV5SR(const _Value: Integer);
begin
  FV5SR := _Value;
end;

procedure TCLC.SetLPFR(const _Value: Integer);
begin
  FLPFR := _Value;
end;

procedure TCLC.SetHSN(const _Value: String);
begin
  FHSN := _Value;
end;

procedure TCLC.SetHATT(const _Value: Double);
begin
  FHATT := _Value;
end;

procedure TCLC.SetHNOM(const _Value: Double);
begin
  FHNOM := _Value;
end;

procedure TCLC.SetCPHC(const _Value: Double);
begin
  FCPHC := _Value;
end;

procedure TCLC.Setunknow4(const _Value: String);
begin
  Funknow4 := _Value;
end;

procedure TCLC.SetCLCRecords(const _Value: TList<TCLRecord>);
begin
  CLCRecordClear;
  FCLCRecords := _Value;
end;

function TCLC.GetCLCRecord(Index: Integer): TCLRecord;
begin
  Result := FCLCRecords[Index];
end;

procedure TCLC.SetCLCRecord(Index: Integer; const _Value: TCLRecord);
begin
  FCLCRecords[Index].Free;
  FCLCRecords[Index] := _Value;
end;

procedure TCLC.AddCLCRecord(_Value: TCLRecord);
begin;
  FCLCRecords.Add(_Value);
end;

function TCLC.AddNewCLCRecord: TCLRecord;
var
  CLCRecordtmp: TCLRecord;
begin;
  CLCRecordtmp := TCLRecord.Create;
  FCLCRecords.Add(CLCRecordtmp);
  Result := CLCRecordtmp;
end;

procedure TCLC.CLCRecordClear;
begin
  while FCLCRecords.Count > 0 do
  begin
    FCLCRecords.Items[0].Free;
    FCLCRecords.Delete(0);
  end;
end;

function TCLC.CLCRecordCount: Integer;
begin
  Result := FCLCRecords.Count;
end;

procedure TCLC.RemoveCLCRecord(_Value: TCLRecord);
begin
  FCLCRecords.Remove(_Value);
  _Value.Free;
end;

procedure TCLC.DeleteCLCRecord(Index: Integer);
begin
  FCLCRecords.Items[Index].Free;
  FCLCRecords.Delete(Index);
end;

{ CLRecord }
constructor TCLRecord.Create;
begin
end;

destructor TCLRecord.Destroy;
begin
  Frawreal := nil;
  Frawimag := nil;
  Ferr := nil;
  FFC := nil;
  Freal := nil;
  Fimag := nil;
  inherited;
end;

procedure TCLRecord.Setfcn(const _Value: Smallint);
begin
  Ffcn := _Value;
end;

procedure TCLRecord.Setbaseband(const _Value: Single);
begin
  Fbaseband := _Value;
end;

procedure TCLRecord.Setharmonic(const _Value: Smallint);
begin
  Fharmonic := _Value;
end;

procedure TCLRecord.Setrawreal(const _Value: Double1D);
begin
  Frawreal := _Value;
end;

procedure TCLRecord.Setrawimag(const _Value: Double1D);
begin
  Frawimag := _Value;
end;

procedure TCLRecord.Seterr(const _Value: Double1D);
begin
  Ferr := _Value;
end;

procedure TCLRecord.SetFC(const _Value: Double1D);
begin
  FFC := _Value;
end;

procedure TCLRecord.Setreal(const _Value: Double1D);
begin
  Freal := _Value;
end;

procedure TCLRecord.Setimag(const _Value: Double1D);
begin
  Fimag := _Value;
end;

end.
