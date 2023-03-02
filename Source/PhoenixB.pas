unit PhoenixB;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, System.Generics.Collections,
  System.Generics.Defaults,
  WTypes, System.DateUtils, FMX.Menus,
  Phoenix, PhoenixTypes, MathFunctions;

type
  Phoenix_Mode = (PNX_AMT, PNX_MT, PNX_DAMT, PNX_ACMT);

const
  Phoenix_MTUA_MTC30_AMT_Octave2: array [0 .. 39] of Double = (10666.67, 8000, 5333.33, 4000, 2666.67, 2000, 1333.33,
    1000, 666.67, 500, 320, 240, 160, 120, 80, 60, 40, 30, 20, 15, 10, 7.5, 6, 4.5, 3, 2.25, 1.5, 1.125, 0.75, 0.5625,
    0.375, 0.28125, 0.1875, 0.140625, 0.09375, 0.070313, 0.046875, 0.035156, 0.023438, 0.017578);
  Phoenix_MTUA_MTC30_AMT_Octave2Band: array [0 .. 39] of Integer = (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5);
  Phoenix_MTUA_MTC30_AMT_Octave2File: array [0 .. 39] of Integer = (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5);
  Phoenix_MTUA_MTC30_AMT_Octave4: array [0 .. 79] of Double = (10400, 8800, 7200, 6000, 5200, 4400, 3600, 3000, 2600,
    2200, 1800, 1500, 1300, 1100, 900, 776.47, 635.29, 529.41, 458.82, 388.235, 317.645, 264.705, 229.41, 194.1175,
    158.8225, 132.3525, 114.705, 97.0588, 79.4113, 66.1763, 57.3525, 48.5294, 39.7056, 32.5, 27.5, 22.5, 18.75, 16.25,
    13.75, 11.25, 9.375, 8.125, 6.875, 5.625, 4.6875, 4.0625, 3.4375, 2.8125, 2.34375, 2.03125, 1.71875, 1.40625,
    1.17188, 1.01563, 0.85938, 0.70313, 0.58594, 0.50781, 0.42969, 0.35156, 0.2929688, 0.2539063, 0.2148438, 0.1757813,
    0.1464844, 0.1269532, 0.1074219, 0.0878907, 0.0732422, 0.0634766, 0.053711, 0.0439453, 0.0366211, 0.0317383,
    0.0268555, 0.0219727, 0.0183106, 0.0158691, 0.0134277, 0.0109863);
  Phoenix_MTUA_MTC30_AMT_Octave4Band: array [0 .. 79] of Integer = (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6);
  Phoenix_MTUA_MTC30_AMT_Octave4File: array [0 .. 79] of Integer = (2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5);
  Phoenix_MTUA_MTC50_MT_Octave2: array [0 .. 49] of Double = (3.200000000000000E+02, 2.400000000000000E+02,
    1.600000000000000E+02, 1.200000000000000E+02, 8.000000000000000E+01, 6.000000000000000E+01, 4.000000000000000E+01,
    3.000000000000000E+01, 2.000000000000000E+01, 1.500000000000000E+01, 1.000000000000000E+01, 7.500000000000000E+00,
    6.000000000000000E+00, 4.500000000000000E+00, 3.000000000000000E+00, 2.250000000000000E+00, 1.500000000000000E+00,
    1.125000000000000E+00, 7.500000000000000E-01, 5.625000000000000E-01, 3.750000000000000E-01, 2.812500000000000E-01,
    1.875000000000000E-01, 1.406250000000000E-01, 9.375000000000000E-02, 7.031250000000000E-02, 4.687500000000000E-02,
    3.515625000000000E-02, 2.343750000000000E-02, 1.757812500000000E-02, 1.171875000000000E-02, 8.789062500000000E-03,
    5.859375000000000E-03, 4.394531250000000E-03, 2.929687500000000E-03, 2.197265625000000E-03, 1.464843750000000E-03,
    1.098632812500000E-03, 7.324218750000000E-04, 5.493164062500000E-04, 3.662109375000000E-04, 2.746582031250000E-04,
    1.831054687500000E-04, 1.373291015625000E-04, 9.155273437500000E-05, 6.866455078125000E-05, 4.577636718750000E-05,
    3.433227539062500E-05, 2.288818359375000E-05, 1.716613769531250E-05);
  Phoenix_MTUA_MTC50_MT_Octave2Band: array [0 .. 49] of Integer = (3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4,
    4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7);
  Phoenix_MTUA_MTC50_MT_Octave2File: array [0 .. 49] of Integer = (3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4,
    4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5);
  Phoenix_MTUA_MTC50_MT_Octave4: array [0 .. 99] of Double = (3.176450000000000E+02, 2.647050000000000E+02,
    2.294100000000000E+02, 1.941176500000000E+02, 1.588225000000000E+02, 1.323525000000000E+02, 1.147050000000000E+02,
    9.705882500000000E+01, 7.941125000000000E+01, 6.617625000000000E+01, 5.735250000000000E+01, 4.852941250000000E+01,
    3.970562500000000E+01, 3.250000000000000E+01, 2.750000000000000E+01, 2.250000000000000E+01, 1.875000000000000E+01,
    1.625000000000000E+01, 1.375000000000000E+01, 1.125000000000000E+01, 9.375000000000000E+00, 8.125000000000000E+00,
    6.875000000000000E+00, 5.625000000000000E+00, 4.687500000000000E+00, 4.062500000000000E+00, 3.437500000000000E+00,
    2.812500000000000E+00, 2.343750000000000E+00, 2.031250000000000E+00, 1.718750000000000E+00, 1.406250000000000E+00,
    1.171875000000000E+00, 1.015625000000000E+00, 8.593750000000000E-01, 7.031250000000000E-01, 5.859375000000000E-01,
    5.078125000000000E-01, 4.296875000000000E-01, 3.515625000000000E-01, 2.929687500000000E-01, 2.539062500000000E-01,
    2.148437500000000E-01, 1.757812500000000E-01, 1.464843750000000E-01, 1.269531250000000E-01, 1.074218750000000E-01,
    8.789062500000000E-02, 7.324218750000000E-02, 6.347656250000000E-02, 5.371093750000000E-02, 4.394531250000000E-02,
    3.662109375000000E-02, 3.173828125000000E-02, 2.685546875000000E-02, 2.197265625000000E-02, 1.831054687500000E-02,
    1.586914062500000E-02, 1.342773437500000E-02, 1.098632812500000E-02, 9.155273437500000E-03, 7.934570312500000E-03,
    6.713867187500000E-03, 5.493164062500000E-03, 4.577636718750000E-03, 3.967285156250000E-03, 3.356933593750000E-03,
    2.746582031250000E-03, 2.288818359375000E-03, 1.983642578125000E-03, 1.678466796875000E-03, 1.373291015625000E-03,
    1.144409179687500E-03, 9.918212890625000E-04, 8.392333984375000E-04, 6.866455078125000E-04, 5.722045898437500E-04,
    4.959106445312500E-04, 4.196166992187500E-04, 3.433227539062500E-04, 2.861022949218750E-04, 2.479553222656250E-04,
    2.098083496093750E-04, 1.716613769531250E-04, 1.430511474609370E-04, 1.239776611328120E-04, 1.049041748046870E-04,
    8.583068847656250E-05, 7.152557373046870E-05, 6.198883056640620E-05, 5.245208740234370E-05, 4.291534423828120E-05,
    3.576278686523440E-05, 3.099441528320310E-05, 2.622604370117190E-05, 2.145767211914060E-05, 1.788139343261720E-05,
    1.549720764160160E-05, 1.311302185058590E-05, 1.072883605957030E-05);
  Phoenix_MTUA_MTC50_MT_Octave4Band: array [0 .. 99] of Integer = (3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8);
  Phoenix_MTUA_MTC50_MT_Octave4File: array [0 .. 99] of Integer = (3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5);
  Phoenix_MT_Octave2_Frequency: array [0 .. 39] of Double = (320, 240, 160, 120, 80, 60, 40, 30, 20, 15, 10, 7.5, 6.0,
    4.5, 3.0, 2.25, 1.5, 1.125, 0.75, 0.5625, 0.375, 0.28125, 0.1875, 0.140625, 0.09375, 0.0703125, 0.046875,
    0.03515625, 0.0234375, 0.017578125, 0.01171875, 0.0087890625, 0.005859375, 0.00439453125, 0.0029296875,
    0.002197265625, 0.00146484375, 0.0010986328125, 0.000732421875, 0.00054931640625);
  Phoenix_MT_Octave2_FrequencyBand: array [0 .. 39] of Integer = (3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4,
    4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6);
  Phoenix_MT_Octave2_FrequencyFile: array [0 .. 39] of Integer = (3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4,
    4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5);
  Phoenix_LMT_Octave2_Frequency: array [0 .. 39] of Double = (10, 7.5, 6.0, 4.5, 3.0, 2.25, 1.5, 1.125, 0.75, 0.5625,
    0.375, 0.28125, 0.1875, 0.140625, 0.09375, 0.0703125, 0.046875, 0.03515625, 0.0234375, 0.017578125, 0.01171875,
    0.0087890625, 0.005859375, 0.00439453125, 0.0029296875, 0.002197265625, 0.00146484375, 0.0010986328125,
    0.000732421875, 0.00054931640625, 0.0003662109375, 0.000274658203125, 0.00018310546875, 0.0001373291015625,
    0.000091552734375, 0.00006866455078125, 0.0000457763671875, 0.000034332275390625, 0.00002288818359375,
    0.0000171661376953125);
  Phoenix_LMT_Octave2_FrequencyBand: array [0 .. 39] of Integer = (3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7);
  Phoenix_LMT_Octave2_FrequencyFile: array [0 .. 39] of Integer = (3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5);

type

  TTBLHelper = class helper for TTBL
  private
    function GetBADR: Integer;
    function GetCHEX: Integer;
    function GetCHEY: Integer;
    function GetCHHX: Integer;
    function GetCHHY: Integer;
    function GetCHHZ: Integer;
    function GetTBLRecordOf(rname: String): TTBLRecord;
    function GetEAZM: Double;
    function GetEGN: Integer;
    function GetELEV: Double;
    function GetETIM: TDateTime;
    function GetETMH: TDateTime;
    function GetEXLN: Double;
    function GetEXR: Integer;
    function GetEYLN: Double;
    function GetEYR: Integer;
    function GetFSCV: Double;
    function GetFTIM: TDateTime;
    function GetHATT: Double;
    function GetHAZM: Double;
    function GetHGN: Integer;
    function GetHSMP: Integer;
    function GetHTIM: TDateTime;
    function GetHXSN: string;
    function GetHYSN: string;
    function GetHZSN: string;
    function GetL2NS: Integer;
    function GetL3NS: Integer;
    function GetL4NS: Integer;
    function GetLATG: string;
    function GetLFRQ: Integer;
    function GetLNGG: string;
    function GetLTIM: TDateTime;
    function GetMode: String;
    function GetSATR: Integer;
    function GetSITE: String;
    function GetSNUM: String;
    function GetSTIM: TDateTime;
    function GetTOTL: Integer;
    function GetTSFileName: String;
    procedure SetCHEX(const Value: Integer);
    procedure SetCHEY(const Value: Integer);
    procedure SetCHHX(const Value: Integer);
    procedure SetCHHY(const Value: Integer);
    procedure SetCHHZ(const Value: Integer);
    procedure SetEAZM(const Value: Double);
    procedure SetEGN(const Value: Integer);
    procedure SetELEV(const Value: Double);
    procedure SetETIM(const Value: TDateTime);
    procedure SetETMH(const Value: TDateTime);
    procedure SetEXLN(const Value: Double);
    procedure SetEXR(const Value: Integer);
    procedure SetEYLN(const Value: Double);
    procedure SetEYR(const Value: Integer);
    procedure SetFSCV(const Value: Double);
    procedure SetFTIM(const Value: TDateTime);
    procedure SetHATT(const Value: Double);
    procedure SetHAZM(const Value: Double);
    procedure SetHGN(const Value: Integer);
    procedure SetHSMP(const Value: Integer);
    procedure SetHTIM(const Value: TDateTime);
    procedure SetHXSN(const Value: string);
    procedure SetHYSN(const Value: string);
    procedure SetHZSN(const Value: string);
    procedure SetL2NS(const Value: Integer);
    procedure SetL3NS(const Value: Integer);
    procedure SetL4NS(const Value: Integer);
    procedure SetLATG(const Value: string);
    procedure SetLFRQ(const Value: Integer);
    procedure SetLNGG(const Value: string);
    procedure SetLTIM(const Value: TDateTime);
    procedure SetMode(const Value: String);
    procedure SetSATR(const Value: Integer);
    procedure SetSITE(const Value: String);
    procedure SetSNUM(const Value: String);
    procedure SetSTIM(const Value: TDateTime);
    procedure SetTOTL(const Value: Integer);
    procedure SetTSFileName(const Value: String);
    procedure CoorFix;
    function GetLFIX: TDateTime;
    function GetTSS: TObjectList<TTS>;
    procedure SetLFIX(const Value: TDateTime);
    property TBLRecordOf[rname: String]: TTBLRecord read GetTBLRecordOf;
  protected
  public
    function MTMode: Phoenix_Mode;
    function ExCoefficient: Double; // (V/m)
    function EyCoefficient: Double; // (V/m)
    function HCoefficient: Double; // (T)
    function HCoefficientAM: Double; // (A/m)
    function Latitude: Double;
    function Longitude: Double;
    procedure LoadTBL(const fname: String);
    procedure SaveTBL(const fname: String);
    procedure CopyTo(target: TTBL);
    property BADR: Integer read GetBADR;
    property CHEX: Integer read GetCHEX write SetCHEX;
    property CHEY: Integer read GetCHEY write SetCHEY;
    property CHHX: Integer read GetCHHX write SetCHHX;
    property CHHY: Integer read GetCHHY write SetCHHY;
    property CHHZ: Integer read GetCHHZ write SetCHHZ;
    property EAZM: Double read GetEAZM write SetEAZM;
    property EGN: Integer read GetEGN write SetEGN;
    property ELEV: Double read GetELEV write SetELEV;
    property ETIM: TDateTime read GetETIM write SetETIM;
    property ETMH: TDateTime read GetETMH write SetETMH;
    property EXLN: Double read GetEXLN write SetEXLN;
    property EXR: Integer read GetEXR write SetEXR;
    property EYLN: Double read GetEYLN write SetEYLN;
    property EYR: Integer read GetEYR write SetEYR;
    property FSCV: Double read GetFSCV write SetFSCV;
    property FTIM: TDateTime read GetFTIM write SetFTIM;
    property LFIX: TDateTime read GetLFIX write SetLFIX;
    property HATT: Double read GetHATT write SetHATT;
    property HAZM: Double read GetHAZM write SetHAZM;
    property HGN: Integer read GetHGN write SetHGN;
    property HSMP: Integer read GetHSMP write SetHSMP;
    property HTIM: TDateTime read GetHTIM write SetHTIM;
    property HXSN: string read GetHXSN write SetHXSN;
    property HYSN: string read GetHYSN write SetHYSN;
    property HZSN: string read GetHZSN write SetHZSN;
    property L2NS: Integer read GetL2NS write SetL2NS;
    property L3NS: Integer read GetL3NS write SetL3NS;
    property L4NS: Integer read GetL4NS write SetL4NS;
    property LATG: string read GetLATG write SetLATG;
    property LFRQ: Integer read GetLFRQ write SetLFRQ;
    property LNGG: string read GetLNGG write SetLNGG;
    property LTIM: TDateTime read GetLTIM write SetLTIM;
    property Mode: String read GetMode write SetMode;
    property SATR: Integer read GetSATR write SetSATR;
    property SITE: String read GetSITE write SetSITE;
    property SNUM: String read GetSNUM write SetSNUM;
    property STIM: TDateTime read GetSTIM write SetSTIM;
    property TOTL: Integer read GetTOTL write SetTOTL;
    property TSFileName: String read GetTSFileName write SetTSFileName;
    property TSS: TObjectList<TTS> read GetTSS;
  end;

  TTBLRecordHelper = class helper for TTBLRecord
  private
  protected
  public
    procedure CopyTo(target: TTBLRecord);
    procedure LoadRecord(const rec: TTBLPackedRecord);
    procedure SaveRecord(var rec: TTBLPackedRecord);
  end;

  TTSHelper = class helper for TTS
  private
  protected
  public
    function GetData(const bIndex: Integer = 0; const eIndex: Integer = -1; const ExIndex: Integer = 0;
      const EyIndex: Integer = 1; const HxIndex: Integer = 2; const HyIndex: Integer = 3; const HzIndex: Integer = 4)
      : Double2D;
    function GetTime(const bIndex: Integer = 0; const eIndex: Integer = -1): Double1D;
    function GetRecordTime(const bIndex: Integer = 0; const eIndex: Integer = -1): Double1D;
    function IndexOf(const dt: TDateTime): Integer;
    procedure ValueToDegree(values: Double2D; tbl: TTBL; btime, etime: TDateTime;
      intervals, offset, sr, st, box: Integer);
    procedure DataReplace(values: Double2D; tbl: TTBL);
    procedure DataAdd(values: Double2D; tbl: TTBL);
    procedure LoadFile(const fileName: String);
    procedure LoadFrom(const fileName: string);
    procedure ReadBin(const fileName: string);
    procedure ReadText(const fileName: string);
    procedure SaveFile(const fileName: String);
    procedure SaveTo(const fileName: string);
    procedure AppendTs(appts: TTS);
    procedure AppendTsRecord(apptsr: TTSRecord);
    function GetAllData: Integer2D;
    procedure GetData3D(out MData: Double3D; out times: TDateTime1D; const bIndex: Integer = 0;
      const eIndex: Integer = -1; recordsOfPerSegment: Integer = 1; const ExIndex: Integer = 0;
      const EyIndex: Integer = 1; const HxIndex: Integer = 2; const HyIndex: Integer = 3; const HzIndex: Integer = 4);
    procedure SetAllData(const Value: Integer2D);
    procedure WriteBin2(const fileName: string);
    procedure WriteBin(const fileName: string);
    procedure WriteText(const fileName: string);
    procedure WriteGMTText(const fileName: string);
    function GetAllTimes: TDateTime1D;
    function ToGMTStrs: TStringList;
    procedure ReadPhoenixText(const fileName: string);
    procedure ReplaceRecord(const rec: TTSRecord);
    procedure SortDateTime;
    property AllData: Integer2D read GetAllData write SetAllData;
  end;

  TTSRecordHelper = class helper for TTSRecord
  private
    function GetChDataD(Index: Integer): Double1D;
    function GetChDataI(Index: Integer): Integer1D;
    procedure SetChDataD(Index: Integer; const Value: Double1D);
    procedure SetChDataI(Index: Integer; const Value: Integer1D);
  protected
  public
    procedure SuperimposeTSRecord(stsr: TTSRecord; scale: Double1D);
    procedure CopyFrom(const source: TTSRecord);
    procedure DataReplace(values: Integer2D);
    procedure DataAdd(values: Integer2D; scale: Double1D); overload;
    procedure DataAdd(values: Integer2D); overload;
    procedure SetRecord(values: Integer2D; tm: TDateTime; len: Integer; box: smallint; sr: Integer);
    function Read(bts: TBytes; const beginindex: Int64; const first: TTSRecord = nil): Boolean;
    procedure ReadFromStrings(const str: TStrings);
    function ReadFromPhoenixStrings(strs: TStrings; var Index: Integer): Boolean;
    function ToString: string;
    procedure Write(var fs: TFileStream);
    function ToPhoenixBytes: TBytes;
    function ToGMTStrs: TStringList;
    property ChDataD[Index: Integer]: Double1D read GetChDataD write SetChDataD;
    property ChDataI[Index: Integer]: Integer1D read GetChDataI write SetChDataI;
  end;

  TCLBHelper = class helper for TCLB
  private
  protected
  public
    procedure LoadFile(const fileName: String);
    procedure ReadCLB(const fileName: string);
    procedure SaveFile(const fileName: String);
    procedure WriteCLB(const fileName: string);
    procedure SetAsZero;
  end;

  TCLCHelper = class helper for TCLC
  private
  protected
  public
    procedure LoadFile(const fileName: String);
    procedure ReadCLC(const fileName: string);
    procedure SaveFile(const fileName: String);
    procedure WriteCLC(const fileName: string);
    procedure SetAsZero;
  end;

  TCLRecordHelper = class helper for TCLRecord
  private
  protected
  public
    procedure Read(var fs: TFileStream; count: Integer);
    procedure Write(var fs: TFileStream; count: Integer);
    procedure SetAsZero;
  end;

procedure RenamePhoenixSite(path, oldname, newname: String);
implementation

procedure RenamePhoenixSite(path, oldname, newname: String);
var
  newtbl: TTBL;
  I: Integer;
begin
  newtbl := TTBL.Create;
  newtbl.LoadTBL(path + '\' + oldname + '.TBL');
  newtbl.Name := newname;
  newtbl.TSFileName := newname;
  newtbl.SITE := newname;
  newtbl.SaveTBL(path + '\' + newname + '.TBL');
  for I := 2 to 5 do
  begin
    if FileExists(path + '\' + oldname + '.TS' + I.ToString) then
    begin
      RenameFile(path + '\' + oldname + '.TS' + I.ToString, path + '\' + newname + '.TS' + I.ToString);
    end;
  end;
  RenameFile(path + '\' + oldname + '.TBL', path + '\' + newname + '.TBO');
  if FileExists(path + '\' + oldname + '.TBL') then
  begin
    DeleteFile(path + '\' + oldname + '.TBL');
  end;
end;

function TTBLHelper.GetBADR: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['BADR'].Value, 0);
end;

function TTBLHelper.GetCHEX: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['CHEX'].Value, 0);
end;

function TTBLHelper.GetCHEY: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['CHEY'].Value, 0);
end;

function TTBLHelper.GetCHHX: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['CHHX'].Value, 0);
end;

function TTBLHelper.GetCHHY: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['CHHY'].Value, 0);
end;

function TTBLHelper.GetCHHZ: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['CHHZ'].Value, 0);
end;

function TTBLHelper.GetTBLRecordOf(rname: String): TTBLRecord;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.TBLRecordCount - 1 do
  begin
    if TBLRecord[I].Name = rname then
    begin
      Result := TTBLRecord(TBLRecord[I]);
      Exit;
    end;
  end;
end;

function TTBLHelper.GetEAZM: Double;
begin
  Result := StrToFloatDef(TBLRecordOf['EAZM'].Value, 0);
end;

function TTBLHelper.GetEGN: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['EGN'].Value, 0);
end;

function TTBLHelper.GetELEV: Double;
begin
  Result := StrToFloatDef(TBLRecordOf['ELEV'].Value, 0);
end;

function TTBLHelper.GetETIM: TDateTime;
begin
  Result := TBLStrToDateTime(TBLRecordOf['ETIM'].Value);
end;

function TTBLHelper.GetETMH: TDateTime;
begin
  Result := TBLStrToDateTime(TBLRecordOf['ETMH'].Value);
end;

function TTBLHelper.GetEXLN: Double;
begin
  Result := StrToFloatDef(TBLRecordOf['EXLN'].Value, 0);
end;

function TTBLHelper.GetEXR: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['EXR'].Value, 0);
end;

function TTBLHelper.GetEYLN: Double;
begin
  Result := StrToFloatDef(TBLRecordOf['EYLN'].Value, 0);
end;

function TTBLHelper.GetEYR: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['EYR'].Value, 0);
end;

function TTBLHelper.GetFSCV: Double;
begin
  Result := StrToFloatDef(TBLRecordOf['FSCV'].Value, 0);
end;

function TTBLHelper.GetFTIM: TDateTime;
begin
  Result := TBLStrToDateTime(TBLRecordOf['FTIM'].Value);
end;

function TTBLHelper.GetHATT: Double;
begin
  Result := StrToFloatDef(TBLRecordOf['HATT'].Value, 0);
end;

function TTBLHelper.GetHAZM: Double;
begin
  Result := StrToFloatDef(TBLRecordOf['HAZM'].Value, 0);
end;

function TTBLHelper.GetHGN: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['HGN'].Value, 0);
end;

function TTBLHelper.GetHSMP: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['HSMP'].Value, 0);
end;

function TTBLHelper.GetHTIM: TDateTime;
begin
  Result := TBLStrToDateTime(TBLRecordOf['HTIM'].Value);
end;

function TTBLHelper.GetHXSN: string;
begin
  Result := TBLRecordOf['HXSN'].Value;
end;

function TTBLHelper.GetHYSN: string;
begin
  Result := TBLRecordOf['HYSN'].Value;
end;

function TTBLHelper.GetHZSN: string;
begin
  Result := TBLRecordOf['HZSN'].Value;
end;

function TTBLHelper.GetL2NS: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['L2NS'].Value, 0);
end;

function TTBLHelper.GetL3NS: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['L3NS'].Value, 0);
end;

function TTBLHelper.GetL4NS: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['L4NS'].Value, 0);
end;

function TTBLHelper.GetLATG: string;
begin
  Result := TBLRecordOf['LATG'].Value;
end;

function TTBLHelper.GetLFRQ: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['LFRQ'].Value, 0);
end;

function TTBLHelper.GetLNGG: string;
begin
  Result := TBLRecordOf['LNGG'].Value;
end;

function TTBLHelper.GetLTIM: TDateTime;
begin
  Result := TBLStrToDateTime(TBLRecordOf['LTIM'].Value);
end;

function TTBLHelper.GetMode: String;
begin
  Result := TBLRecordOf['Mode'].Value;
end;

function TTBLHelper.GetSATR: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['SATR'].Value, 0);
end;

function TTBLHelper.GetSITE: String;
begin
  Result := TBLRecordOf['SITE'].Value;
end;

function TTBLHelper.GetSNUM: String;
begin
  Result := TBLRecordOf['SNUM'].Value;
end;

function TTBLHelper.GetSTIM: TDateTime;
begin
  Result := TBLStrToDateTime(TBLRecordOf['STIM'].Value);
end;

function TTBLHelper.GetTOTL: Integer;
begin
  Result := StrToIntDef(TBLRecordOf['TOTL'].Value, 0);
end;

function TTBLHelper.GetTSFileName: String;
begin
  Result := TBLRecordOf['FILE'].Value;
end;

procedure TTBLHelper.SetCHEX(const Value: Integer);
begin
  TBLRecordOf['CHEX'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetCHEY(const Value: Integer);
begin
  TBLRecordOf['CHEY'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetCHHX(const Value: Integer);
begin
  TBLRecordOf['CHHX'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetCHHY(const Value: Integer);
begin
  TBLRecordOf['CHHY('].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetCHHZ(const Value: Integer);
begin
  TBLRecordOf['CHHZ'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetEAZM(const Value: Double);
begin
  TBLRecordOf['EAZM'].Value := FloatToStr(Value);
end;

procedure TTBLHelper.SetEGN(const Value: Integer);
begin
  TBLRecordOf['EGN'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetELEV(const Value: Double);
begin
  TBLRecordOf['ELEV'].Value := FloatToStr(Value);
end;

procedure TTBLHelper.SetETIM(const Value: TDateTime);
begin
  TBLRecordOf['ETIM'].Value := DateTimeToTBLStr(Value);
end;

procedure TTBLHelper.SetETMH(const Value: TDateTime);
begin
  TBLRecordOf['ETMH'].Value := DateTimeToTBLStr(Value);
end;

procedure TTBLHelper.SetEXLN(const Value: Double);
begin
  TBLRecordOf['EXLN'].Value := FloatToStr(Value);
end;

procedure TTBLHelper.SetEXR(const Value: Integer);
begin
  TBLRecordOf['EXR'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetEYLN(const Value: Double);
begin
  TBLRecordOf['EYLN'].Value := FloatToStr(Value);
end;

procedure TTBLHelper.SetEYR(const Value: Integer);
begin
  TBLRecordOf['EYR'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetFSCV(const Value: Double);
begin
  TBLRecordOf['FSCV'].Value := FloatToStr(Value);
end;

procedure TTBLHelper.SetFTIM(const Value: TDateTime);
begin
  TBLRecordOf['FTIM'].Value := DateTimeToTBLStr(Value);
end;

procedure TTBLHelper.SetHATT(const Value: Double);
begin
  TBLRecordOf['HATT'].Value := FloatToStr(Value);
end;

procedure TTBLHelper.SetHAZM(const Value: Double);
begin
  TBLRecordOf['HAZM'].Value := FloatToStr(Value);
end;

procedure TTBLHelper.SetHGN(const Value: Integer);
begin
  TBLRecordOf['HGN'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetHSMP(const Value: Integer);
begin
  TBLRecordOf['HSMP'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetHTIM(const Value: TDateTime);
begin
  TBLRecordOf['HTIM'].Value := DateTimeToTBLStr(Value);
end;

procedure TTBLHelper.SetHXSN(const Value: string);
begin
  TBLRecordOf['HXSN'].Value := Value;
end;

procedure TTBLHelper.SetHYSN(const Value: string);
begin
  TBLRecordOf['HYSN'].Value := Value;
end;

procedure TTBLHelper.SetHZSN(const Value: string);
begin
  TBLRecordOf['HZSN'].Value := Value;
end;

procedure TTBLHelper.SetL2NS(const Value: Integer);
begin
  TBLRecordOf['L2NS'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetL3NS(const Value: Integer);
begin
  TBLRecordOf['L3NS'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetL4NS(const Value: Integer);
begin
  TBLRecordOf['L4NS'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetLATG(const Value: string);
begin
  TBLRecordOf['LATG'].Value := Value;
end;

procedure TTBLHelper.SetLFRQ(const Value: Integer);
begin
  TBLRecordOf['LFRQ'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetLNGG(const Value: string);
begin
  TBLRecordOf['LNGG'].Value := Value;
end;

procedure TTBLHelper.SetLTIM(const Value: TDateTime);
begin
  TBLRecordOf['LTIM'].Value := DateTimeToTBLStr(Value);
end;

procedure TTBLHelper.SetMode(const Value: String);
begin
  TBLRecordOf['Mode'].Value := Value;
end;

procedure TTBLHelper.SetSATR(const Value: Integer);
begin
  TBLRecordOf['SATR'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetSITE(const Value: String);
begin
  TBLRecordOf['SITE'].Value := Value;
end;

procedure TTBLHelper.SetSNUM(const Value: String);
begin
  TBLRecordOf['SNUM'].Value := Value;
end;

procedure TTBLHelper.SetSTIM(const Value: TDateTime);
begin
  TBLRecordOf['STIM'].Value := DateTimeToTBLStr(Value);
end;

procedure TTBLHelper.SetTOTL(const Value: Integer);
begin
  TBLRecordOf['TOTL'].Value := IntToStr(Value);
end;

procedure TTBLHelper.SetTSFileName(const Value: String);
begin
  TBLRecordOf['FILE'].Value := Value;
end;

{ TTBLHelper }
procedure TTBLHelper.CoorFix;
var
  lng: string;
begin
  if LNGG[1] = 'N' then
  begin
    LATG := LATG + ',N';
    lng := LNGG;
    LNGG := lng.Remove(0, 2) + ',E';
  end;
end;

function TTBLHelper.GetLFIX: TDateTime;
begin
  Result := TBLStrToDateTime(TBLRecordOf['LFIX'].Value);
end;

function TTBLHelper.GetTSS: TObjectList<TTS>;
var
  I: Integer;
  ts: TTS;
begin
  Result := TObjectList<TTS>.Create;
  for I := 2 to 5 do
  begin
    if FileExists(Self.path + Self.Name + '.TS' + I.ToString) then
    begin
      ts := TTS.Create;
      ts.LoadFile(Self.path + Self.Name + '.TS' + I.ToString);
      Result.Add(ts);
    end;
  end;
end;

procedure TTBLHelper.SetLFIX(const Value: TDateTime);
begin
  TBLRecordOf['LFIX'].Value := DateTimeToTBLStr(Value);
end;

function TTBLHelper.MTMode: Phoenix_Mode;
begin
  if FileExists(Self.path + Self.Name + '.TS3') and FileExists(Self.path + Self.Name + '.TS4') and
    FileExists(Self.path + Self.Name + '.TS5') then
  begin
    Result := Phoenix_Mode.PNX_MT;
    Exit;
  end;
  if FileExists(Self.path + Self.Name + '.TS2') and FileExists(Self.path + Self.Name + '.TS3') and
    FileExists(Self.path + Self.Name + '.TS4') then
  begin
    Result := Phoenix_Mode.PNX_AMT;
    Exit;
  end;
  if FileExists(Self.path + Self.Name + '.TS2') and FileExists(Self.path + Self.Name + '.TS3') then
  begin
    Result := Phoenix_Mode.PNX_DAMT;
    Exit;
  end;
end;

function TTBLHelper.ExCoefficient: Double;
begin
  Result := FSCV / 8388608 / EGN / EXLN;
  // E-Channel (V/m) = I3dat * (FSCV/2^23) * (1/EGN) * (1/E_LN)
  // (V/m) =  du   *  (V / du)   *   1     *   (1/m)
end;

function TTBLHelper.EyCoefficient: Double;
begin
  Result := FSCV / 8388608 / EGN / EYLN;
  // E-Channel (V/m) = I3dat * (FSCV/2^23) * (1/EGN) * (1/E_LN)
  // (V/m) =  du   *  (V / du)   *   1     *   (1/m)
end;

function TTBLHelper.HCoefficient: Double;
begin
  Result := FSCV / 8388608 / HGN / HATT * 1000 / 1000 * 1E-9;
  // HNUM  =	100 (mV/nT) for AMT sensor.	  The Scale factor for AMTC-30
  // =	1000(mV/nT) for MT sensor	  The Scale factor for MTC-50
  // H-Channel (T) =  I3dat * (FSCV/2^23) * (1/HGN) * (1/HATT) * (1000/HNUM) * 1E-09
  // (T) =   du   *  (V / du)   *   1    * (1/0.233) * (mV/V / mV/nT) * (T/nT)
end;

function TTBLHelper.HCoefficientAM: Double;
begin
  Result := FSCV / 8388608 / HGN / HATT * 1000 / 1000 / (4 * Pi * 100);
  // HNUM  =	100 (mV/nT) for AMT sensor.	  The Scale factor for AMTC-30
  // =	1000(mV/nT) for MT sensor	  The Scale factor for MTC-50
  // H-Channel (nT) =  I3dat * (FSCV/2^23) * (1/HGN) * (1/HATT) * (1000/HNUM)
  // (nT) =   du   *  (V / du)   *   1    * (1/0.233) * (mV/V / mV/nT)
end;

function TTBLHelper.Latitude: Double;
var
  str, strDeg, strMin: String;
  Index: Integer;
begin
  str := Self.LATG;
  str := str.Replace(',', '');
  str := str.Replace('N', '');
  str := str.Replace('S', '');
  Index := str.IndexOf('.');
  Index := Index - 2;
  strDeg := str.Substring(0, Index);
  strMin := str.Substring(Index, str.Length - Index);
  Result := strDeg.ToDouble + strMin.ToDouble / 60.0;
end;

function TTBLHelper.Longitude: Double;
var
  str, strDeg, strMin: String;
  Index: Integer;
begin
  str := Self.LNGG;
  str := str.Replace(',', '');
  str := str.Replace('W', '');
  str := str.Replace('E', '');
  Index := str.IndexOf('.');
  Index := Index - 2;
  strDeg := str.Substring(0, Index);
  strMin := str.Substring(Index, str.Length - Index);
  Result := strDeg.ToDouble + strMin.ToDouble / 60.0;
end;

procedure TTBLHelper.LoadTBL(const fname: String);
var
  tblFile: file of TTBLPackedRecord;
  rec: TTBLPackedRecord;
  dt: TTBLRecord;
  ext: string;
begin
  try
    Self.Path := ExtractFilePath(fname);
    Name := ExtractFileName(fname);
    ext := ExtractFileExt(Name);
    Name := Copy(Name, 1, Length(Name) - Length(ext));
    AssignFile(tblFile, fname);
    Reset(tblFile);
    Self.TBLRecordClear;
    while not Eof(tblFile) do
    begin
      Read(tblFile, rec);
      if rec.Name[0] = #03 then
        Break;
      dt := TTBLRecord(AddNewTBLRecord);
      dt.LoadRecord(rec);
    end;
    CloseFile(tblFile);
    CoorFix;
    if not FileExists(path + Self.TSFileName) then
    begin
      Self.TSFileName := Self.Name;
    end;
  except
    raise Exception.Create('Load TBL Error : ' + fname);
  end;
end;

procedure TTBLHelper.SaveTBL(const fname: String);
var
  rec: TTBLPackedRecord;
  tblFile: file of TTBLPackedRecord;
  I: Integer;
begin
  AssignFile(tblFile, fname);
  Rewrite(tblFile);
  for I := 0 to TBLRecordCount - 1 do
  begin
    TBLRecord[I].SaveRecord(rec);
    Write(tblFile, rec);
  end;
  rec := TTBLPackedRecord.Create('0000');
  rec.Name[0] := #03;
  rec.Name[1] := #00;
  rec.Name[2] := #00;
  rec.Name[3] := #00;
  Write(tblFile, rec);
  Close(tblFile);
end;

procedure TTBLHelper.CopyTo(target: TTBL);
var
  I: Integer;
  nr: TTBLRecord;
begin
  try
    target.TBLRecordClear;
    for I := 0 to Self.TBLRecordCount - 1 do
    begin
      nr := target.AddNewTBLRecord;
      Self.TBLRecord[I].CopyTo(nr);
    end;
  except
    raise Exception.Create('TTBL CopyTo Error!'#13#10);
  end;
end;

{ TTBLRecordHelper }

procedure TTBLRecordHelper.CopyTo(target: TTBLRecord);
begin
  target.Name := Self.Name;
  target.Value := Self.Value;
  target.Identifier := Self.Identifier;
  target.Short := Self.Short;
  target.Long := Self.Long;
end;

procedure TTBLRecordHelper.LoadRecord(const rec: TTBLPackedRecord);
begin
  Name := String(rec.Name);
  Value := rec.Value;
  Identifier := rec.Identifier;
  Short := rec.MTUShort;
  Long := rec.MTULong;
end;

procedure TTBLRecordHelper.SaveRecord(var rec: TTBLPackedRecord);
var
  PC: PChar;
begin
  PC := PChar(Name);
  rec.Name[0] := AnsiChar(PC[0]);
  rec.Name[1] := AnsiChar(PC[1]);
  rec.Name[2] := AnsiChar(PC[2]);
  rec.Name[3] := AnsiChar(PC[3]);
  rec.Null := $00;
  rec.Identifier := Identifier;
  rec.MTUShort := Short;
  rec.MTULong := Long;
  rec.SetValue(Value);
end;

procedure TTSRecordHelper.CopyFrom(const source: TTSRecord);
var
  tmp: Integer2D;
begin
  DateTime := source.DateTime;
  BoxSN := source.BoxSN;
  Scans := source.Scans;
  Channels := source.Channels;
  TSLength := source.TSLength;
  StatusCodes := source.StatusCodes;
  SaturationFlag := source.SaturationFlag;
  SampleLength := source.SampleLength;
  samplerate := source.samplerate;
  SampleRateUnits := source.SampleRateUnits;
  ClockStatus := source.ClockStatus;
  ClockError := source.ClockError;
  tmp := source.Data;
  Data := ArrayCopy(tmp);
end;

function TTSRecordHelper.Read(bts: TBytes; const beginindex: Int64; const first: TTSRecord = nil): Boolean;
var
  I, J, LengthPre: Integer;
  singleData: TBytes;
  second: Byte;
  minute: Byte;
  hour: Byte;
  day: Byte;
  month: Byte;
  year: Byte;
  week: Byte;
  century: Byte;

  TBoxSN: smallint;
  TScans: smallint;
  TChannels: Byte;
  TTSLength: Byte;
  TStatusCodes: Byte;
  TSaturationFlag: Byte;
  TReservedbit: Byte;
  TSampleLength: Byte;
  TSampleRate: smallint;
  TSampleRateUnits: Byte;
  TClockStatus: Integer;
  TClockError: Byte;
  dt, tm: TDateTime;
begin
  try
    Result := False;
    Move(bts[beginindex], second, 1);
    Move(bts[beginindex + 1], minute, 1);
    Move(bts[beginindex + 2], hour, 1);
    Move(bts[beginindex + 3], day, 1);
    Move(bts[beginindex + 4], month, 1);
    Move(bts[beginindex + 5], year, 1);
    Move(bts[beginindex + 6], week, 1);
    Move(bts[beginindex + 7], century, 1);

    Result := TryEncodeDate(century * 100 + year, month, day, dt);
    Result := Result and TryEncodeTime(hour, minute, second, 0, tm);
    Self.DateTime := dt + tm;

    Move(bts[beginindex + 8], TBoxSN, 2);
    BoxSN := TBoxSN;
    Move(bts[beginindex + 10], TScans, 2);
    Scans := TScans;
    Move(bts[beginindex + 12], TChannels, 1);
    Channels := TChannels;
    Move(bts[beginindex + 13], TTSLength, 1);
    TSLength := TTSLength;
    Move(bts[beginindex + 14], TStatusCodes, 1);
    StatusCodes := TStatusCodes;
    Move(bts[beginindex + 15], TSaturationFlag, 1);
    SaturationFlag := TSaturationFlag;
    Move(bts[beginindex + 16], TReservedbit, 1);
    Reservedbit := TReservedbit;
    Move(bts[beginindex + 17], TSampleLength, 1);
    SampleLength := TSampleLength;
    Move(bts[beginindex + 18], TSampleRate, 2);
    samplerate := TSampleRate;
    Move(bts[beginindex + 20], TSampleRateUnits, 1);
    SampleRateUnits := TSampleRateUnits;
    Move(bts[beginindex + 21], TClockStatus, 1);
    ClockStatus := TClockStatus;
    Move(bts[beginindex + 22], TClockError, 4);
    ClockError := TClockError;

    // LengthPre := Channels * Scans * 3 + 32;
    Result := True;
    if first <> nil then
    begin
      if (Scans <> first.Scans) or (Channels <> first.Channels) then
      begin
        Result := False;
        BoxSN := first.BoxSN;
        Scans := first.Scans;
        Channels := first.Channels;
        TSLength := first.TSLength;
        StatusCodes := first.StatusCodes;
        SaturationFlag := first.SaturationFlag;
        Reservedbit := first.Reservedbit;
        SampleLength := first.SampleLength;
        samplerate := first.samplerate;
        SampleRateUnits := first.SampleRateUnits;
        ClockStatus := first.ClockStatus;
        ClockError := first.ClockError;
        case first.samplerate of
          15, 150, 2400:
            begin
              Self.DateTime := IncMilliSecond(first.DateTime, 1000);
            end;
          24000:
            begin
              Self.DateTime := IncMilliSecond(first.DateTime, 500);
            end;
        end;
      end;
    end;
    SetLength(singleData, 3 * Scans * Channels);
    Move(bts[beginindex + 32], singleData[0], 3 * Scans * Channels);
    Data := bigBytes24ToInteger2D(singleData, Scans, Channels);
    singleData := nil;
  finally
    // raise Exception.Create('Read TS Record Error!'#13#10 + 'century ' + century.ToString + #13#10 + 'year ' +
    // year.ToString + #13#10 + 'month ' + month.ToString + #13#10 + 'day ' + day.ToString + #13#10 + 'hour ' +
    // hour.ToString + #13#10 + 'minute ' + minute.ToString + #13#10 + 'second ' + second.ToString + #13#10 + 'week ' +
    // week.ToString);
  end;
end;

procedure TTSRecordHelper.ReadFromStrings(const str: TStrings);
var
  I, J: Integer;
  datastr, tmp: string;
  tmpData: Integer2D;
begin
  Self.DateTime := StrToDateTime(Copy(str[0], 17, Length(str[0]) - 16));
  Self.BoxSN := StrToInt(Trim(Copy(str[1], 17, Length(str[0]) - 16)));
  Self.Scans := StrToInt(Trim(Copy(str[2], 17, Length(str[0]) - 16)));
  Self.Channels := StrToInt(Trim(Copy(str[3], 17, Length(str[0]) - 16)));
  Self.TSLength := StrToInt(Trim(Copy(str[4], 17, Length(str[0]) - 16)));
  Self.StatusCodes := StrToInt(Trim(Copy(str[5], 17, Length(str[0]) - 16)));
  Self.SaturationFlag := StrToInt(Trim(Copy(str[6], 17, Length(str[0]) - 16)));
  Self.Reservedbit := StrToInt(Trim(Copy(str[7], 17, Length(str[0]) - 16)));
  Self.SampleLength := StrToInt(Trim(Copy(str[8], 17, Length(str[0]) - 16)));
  Self.samplerate := StrToInt(Trim(Copy(str[9], 17, Length(str[0]) - 16)));
  Self.SampleRateUnits := StrToInt(Trim(Copy(str[10], 17, Length(str[0]) - 16)));
  Self.ClockStatus := StrToInt(Trim(Copy(str[11], 17, Length(str[0]) - 16)));
  Self.ClockError := StrToInt(Trim(Copy(str[12], 17, Length(str[0]) - 16)));
  SetLength(tmpData, Channels);
  for I := 0 to Channels - 1 do
  begin
    SetLength(tmpData[I], Scans);
  end;
  for I := 0 to Scans - 1 do
  begin
    datastr := str[13 + I];
    for J := 0 to Channels - 1 do
    begin
      tmp := Trim(Copy(datastr, J * 18 + 1, 18));
      tmpData[J][I] := StrToInt(tmp);
    end;
  end;
  Data := tmpData;
end;

procedure TTSRecordHelper.DataReplace(values: Integer2D);
var
  I, J, chns, len: Integer;
  dt: Integer2D;
begin
  chns := Self.Channels;
  len := Self.Scans;
  dt := Self.Data;
  for I := 0 to chns - 1 do
  begin
    for J := 0 to Scans - 1 do
    begin
      dt[I][J] := values[I][J];
    end;
  end;
  Self.Data := dt;
end;

procedure TTSRecordHelper.DataAdd(values: Integer2D; scale: Double1D);
var
  I, J, chns, len: Integer;
  dt: Integer2D;
begin
  chns := Self.Channels;
  len := Self.Scans;
  dt := Self.Data;
  for I := 0 to chns - 1 do
  begin
    for J := 0 to Scans - 1 do
    begin
      dt[I][J] := dt[I][J] + Round(values[I][J] * scale[I]);
    end;
  end;
  Self.Data := dt;
end;

procedure TTSRecordHelper.DataAdd(values: Integer2D);
var
  I, J, chns, len: Integer;
  dt: Integer2D;
begin
  chns := Self.Channels;
  len := Self.Scans;
  dt := Self.Data;
  for I := 0 to chns - 1 do
  begin
    for J := 0 to Scans - 1 do
    begin
      dt[I][J] := dt[I][J] + Round(values[I][J]);
    end;
  end;
  Self.Data := dt;
end;

function TTSRecordHelper.GetChDataD(Index: Integer): Double1D;
var
  I: Integer;
begin
  SetLength(Result, Scans);
  for I := 0 to Scans - 1 do
  begin
    Result[I] := Data[Index][I];
  end;
end;

function TTSRecordHelper.GetChDataI(Index: Integer): Integer1D;
var
  I: Integer;
begin
  SetLength(Result, Scans);
  for I := 0 to Scans - 1 do
  begin
    Result[I] := Data[Index][I];
  end;
end;

function TTSRecordHelper.ReadFromPhoenixStrings(strs: TStrings; var Index: Integer): Boolean;
var
  I, J: Integer;
  datastr, tmp, str: string;
  tmpData: Integer2D;
  NormalDateTimeFormat: TFormatSettings;
begin
  Result := False;
  str := strs[Index];
  Inc(Index);

  NormalDateTimeFormat.LongDateFormat := 'yyyy/MM/dd';
  NormalDateTimeFormat.LongTimeFormat := 'hh:mm:ss';
  NormalDateTimeFormat.ShortDateFormat := 'yyyy/MM/dd';
  NormalDateTimeFormat.ShortTimeFormat := 'hh:mm:ss';
  NormalDateTimeFormat.DateSeparator := '/';
  NormalDateTimeFormat.TimeSeparator := ':';
  Self.DateTime := StrToDate(Copy(str, 23, 10), NormalDateTimeFormat) +
    StrToTime(Copy(str, 34, 8), NormalDateTimeFormat);
  str := strs[Index];
  Inc(Index);
  Self.BoxSN := StrToInt(Trim(Copy(str, 23, 5)));
  str := strs[Index];
  Inc(Index);
  Self.Channels := StrToInt(Trim(Copy(str, 23, 5)));
  str := strs[Index];
  Inc(Index);
  Self.Scans := StrToInt(Trim(Copy(str, 23, 5)));
  str := strs[Index];
  Inc(Index);
  Self.samplerate := Trunc(StrToFloat(Trim(Copy(str, 23, 10))));
  Self.TSLength := 32;
  Self.StatusCodes := 0;
  Self.SaturationFlag := 0;
  Self.Reservedbit := 0;
  Self.SampleLength := 3;
  Self.SampleRateUnits := 0;
  Self.ClockStatus := 4;
  Self.ClockError := 0;
  SetLength(tmpData, Channels);
  for I := 0 to Channels - 1 do
  begin
    SetLength(tmpData[I], Scans);
  end;
  for I := 0 to Scans - 1 do
  begin
    datastr := strs[Index];
    Inc(Index);
    for J := 0 to Channels - 1 do
    begin
      tmp := Trim(Copy(datastr, J * 18 + 1, 18));
      tmpData[J][I] := StrToInt(tmp);
    end;
  end;
  Data := tmpData;
  Result := True;
end;

procedure TTSRecordHelper.SetChDataD(Index: Integer; const Value: Double1D);
var
  I: Integer;
begin
  for I := 0 to Scans - 1 do
  begin
    Data[Index][I] := Round(Value[I]);
  end;
end;

procedure TTSRecordHelper.SetChDataI(Index: Integer; const Value: Integer1D);
var
  I: Integer;
begin
  for I := 0 to Scans - 1 do
  begin
    Data[Index][I] := Value[I];
  end;
end;

function TTSRecordHelper.ToGMTStrs: TStringList;
var
  I, J: Integer;
  dt: Integer2D;
  str: String;
  inctime: Double;
const
  GMTDTMFormat: string = 'yyyy-mm-dd"T"hh:nn:ss.zzz';
begin
  Result := TStringList.Create;
  dt := Self.Data;
  inctime := 1 / 24 / 3600 / Self.samplerate;
  for I := 0 to Self.Scans - 1 do
  begin
    str := FormatDateTime(GMTDTMFormat, Self.DateTime + I * inctime);
    for J := 0 to Self.Channels - 1 do
    begin
      str := str + ' ' + StringLenthTo(IntToStr(dt[J][I]), 18);
    end;
    Result.Add(str);
  end;
end;

function TTSRecordHelper.ToPhoenixBytes: TBytes;
var
  yy, ce, yyy, mon, dd, ww, hh, min, sec, msec: Word;
  I, J: Integer;
begin
  try
    SetLength(Result, Channels * Scans * 3 + 32);

    DecodeTime(Self.DateTime, hh, min, sec, msec);
    Move(sec, Result[0], 1);
    Move(min, Result[1], 1);
    Move(hh, Result[2], 1);

    DecodeDateFully(Self.DateTime, yy, mon, dd, ww);
    ce := yy div 100;
    yyy := yy mod 100;
    Move(dd, Result[3], 1);
    Move(mon, Result[4], 1);
    Move(yyy, Result[5], 1);
    Move(ce, Result[7], 1);
    ww := ww - 1;
    if ww = 0 then
      ww := 7;
    Move(ww, Result[6], 1);

    Move(BoxSN, Result[8], 2);
    Move(Scans, Result[10], 2);
    Move(Channels, Result[12], 1);
    Move(TSLength, Result[13], 1);
    Move(StatusCodes, Result[14], 1);
    Move(SaturationFlag, Result[15], 1);
    Move(Reservedbit, Result[16], 1);
    Move(SampleLength, Result[17], 1);
    Move(samplerate, Result[18], 2);
    Move(SampleRateUnits, Result[20], 1);
    Move(ClockStatus, Result[21], 1);
    Move(ClockError, Result[22], 4);
    Result[26] := $00;
    Result[27] := $00;
    Result[28] := $00;
    Result[29] := $00;
    Result[30] := $00;
    Result[32] := $00;

    Integer2dTobigByte24(Self.Data, Result, 32);
  except
    raise Exception.Create('Write TS Record Error!'#13#10);
  end;
end;

function TTSRecordHelper.ToString: string;
var
  I, J: Integer;
begin
  Result := '';
  Result := Result + 'DateTime:       ' + DateTimeToStr(Self.DateTime) + #13#10;
  Result := Result + 'BoxSN:          ' + IntToStr(Self.BoxSN) + #13#10;
  Result := Result + 'Scans:          ' + IntToStr(Self.Scans) + #13#10;
  Result := Result + 'Channels:       ' + IntToStr(Self.Channels) + #13#10;
  Result := Result + 'LengthofTS:     ' + IntToStr(Self.TSLength) + #13#10;
  Result := Result + 'StatusCodes:    ' + IntToStr(Self.StatusCodes) + #13#10;
  Result := Result + 'SaturationFlag: ' + IntToStr(Self.SaturationFlag) + #13#10;
  Result := Result + 'Reservedbit:    ' + IntToStr(Self.Reservedbit) + #13#10;
  Result := Result + 'Samplelength:   ' + IntToStr(Self.SampleLength) + #13#10;
  Result := Result + 'SampleRate:     ' + IntToStr(Self.samplerate) + #13#10;
  Result := Result + 'SampleRateUnits:' + IntToStr(Self.SampleRateUnits) + #13#10;
  Result := Result + 'ClockStatus:    ' + IntToStr(Self.ClockStatus) + #13#10;
  Result := Result + 'ClockError:     ' + IntToStr(Self.ClockError) + #13#10;
  // Result := Result + IntToStr(Self.keep) + #13#10;
  for I := 0 to Scans - 1 do
  begin
    for J := 0 to Channels - 1 do
    begin
      Result := Result + StringLenthTo(IntToStr(Data[J][I]), 18) + ' ';
    end;
    Result := Result + #13#10;
  end;
end;

procedure TTSRecordHelper.SetRecord(values: Integer2D; tm: TDateTime; len: Integer; box: smallint; sr: Integer);
var
  dt: Integer2D;
  I: Integer;
begin
  Self.DateTime := tm;
  Self.BoxSN := box;
  Self.Scans := len;
  Self.Channels := 5;
  Self.TSLength := 32;
  Self.StatusCodes := 0;
  Self.SaturationFlag := 0;
  Self.Reservedbit := 0;
  Self.SampleLength := 3;
  Self.samplerate := sr;
  Self.SampleRateUnits := 0;
  Self.ClockStatus := 4;
  Self.ClockError := 0;
  SetLength(dt, 5);
  for I := 0 to 4 do
  begin
    SetLength(dt[I], Scans);
    dt[I] := Copy(values[I], 0, Scans);
  end;
  Self.Data := dt;
end;

procedure TTSRecordHelper.SuperimposeTSRecord(stsr: TTSRecord; scale: Double1D);
var
  I, J: Integer;
begin
  if Self.DateTime <> stsr.DateTime then
  begin
    raise Exception.Create('DateTime Error! ');
  end;
  Self.DataAdd(stsr.Data, scale);
end;

procedure TTSRecordHelper.Write(var fs: TFileStream);
var
  I, J: Integer;
  singleData: ArrayByte;
  yy, ce, yyy, mon, dd, ww, hh, min, sec, msec: Word;
  second: Byte;
  minute: Byte;
  hour: Byte;
  day: Byte;
  month: Byte;
  year: Byte;
  week: Byte;
  century: Byte;
  keep: Array [0 .. 5] of Byte;
  ab: ArrayByte;
begin
  DecodeDateFully(Self.DateTime, yy, mon, dd, ww);
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
  DecodeTime(Self.DateTime, hh, min, sec, msec);
  Move(hh, hour, 1);
  Move(min, minute, 1);
  Move(sec, second, 1);
  fs.Write(second, 1);
  fs.Write(minute, 1);
  fs.Write(hour, 1);
  fs.Write(day, 1);
  fs.Write(month, 1);
  fs.Write(year, 1);
  fs.Write(week, 1);
  fs.Write(century, 1);
  fs.Write(BoxSN, 2);
  fs.Write(Scans, 2);
  fs.Write(Channels, 1);
  fs.Write(TSLength, 1);
  fs.Write(StatusCodes, 1);
  fs.Write(SaturationFlag, 1);
  fs.Write(Reservedbit, 1);
  fs.Write(SampleLength, 1);
  fs.Write(samplerate, 2);
  fs.Write(SampleRateUnits, 1);
  fs.Write(ClockStatus, 1);
  fs.Write(ClockError, 4);
  keep[0] := $00;
  keep[1] := $00;
  keep[2] := $00;
  keep[3] := $00;
  keep[4] := $00;
  keep[5] := $00;
  fs.Write(keep, 6);
  for I := 0 to Scans - 1 do
  begin
    for J := 0 to Channels - 1 do
    begin
      ab := IntegerTobigByte24(Data[J][I]);
      fs.Write(ab, 3);
    end;
  end;
end;

function TTSHelper.GetData(const bIndex: Integer = 0; const eIndex: Integer = -1; const ExIndex: Integer = 0;
  const EyIndex: Integer = 1; const HxIndex: Integer = 2; const HyIndex: Integer = 3; const HzIndex: Integer = 4)
  : Double2D;
var
  I, J, beginindex, endIndex: Integer;
begin
  beginindex := 0;
  endIndex := 0;
  SetLength(Result, 5);
  if bIndex < 0 then
    beginindex := 0;
  if eIndex = -1 then
    endIndex := Self.TSRecordCount - 1;
  for I := 0 to 5 - 1 do
  begin
    SetLength(Result[I], (endIndex - beginindex + 1) * Self.Scans);
  end;
  for I := beginindex to endIndex do
  begin
    Move(Self.TSRecord[I].GetChDataD(ExIndex)[0], Result[0][(I - beginindex) * Self.Scans], Self.Scans * 8);
    Move(Self.TSRecord[I].GetChDataD(EyIndex)[0], Result[1][(I - beginindex) * Self.Scans], Self.Scans * 8);
    Move(Self.TSRecord[I].GetChDataD(HxIndex)[0], Result[2][(I - beginindex) * Self.Scans], Self.Scans * 8);
    Move(Self.TSRecord[I].GetChDataD(HyIndex)[0], Result[3][(I - beginindex) * Self.Scans], Self.Scans * 8);
    Move(Self.TSRecord[I].GetChDataD(HzIndex)[0], Result[4][(I - beginindex) * Self.Scans], Self.Scans * 8);
  end;
end;

function TTSHelper.GetTime(const bIndex, eIndex: Integer): Double1D;
var
  I, J, beginindex, endIndex, Index: Integer;
begin
  if bIndex < 0 then
    beginindex := 0;
  if eIndex = -1 then
    endIndex := Self.TSRecordCount - 1;
  SetLength(Result, (endIndex - beginindex + 1) * Self.Scans);
  for I := beginindex to endIndex do
  begin
    index := (I - beginindex) * Self.Scans;
    Result[index] := Self.TSRecord[I].DateTime;
    for J := 1 to Self.Scans - 1 do
    begin
      Result[index + J] := Result[index + J - 1] + 1.0 / Self.samplerate / 24 / 3600;
    end;
  end;
end;

function TTSHelper.GetRecordTime(const bIndex, eIndex: Integer): Double1D;
var
  I, J, beginindex, endIndex, Index: Integer;
begin
  if bIndex < 0 then
    beginindex := 0;
  if eIndex = -1 then
    endIndex := Self.TSRecordCount - 1;
  SetLength(Result, (endIndex - beginindex + 1));
  for I := beginindex to endIndex do
  begin
    Result[I] := Self.TSRecord[I].DateTime;
  end;
end;

function TTSHelper.IndexOf(const dt: TDateTime): Integer;
var
  I, J, tmpIndex: Integer;
begin
  if (dt < Self.BeginTime) or (dt > Self.EndTime) then
  begin
    Result := -1;
    Exit;
  end;
  tmpIndex := Round(SecondsBetween(Self.BeginTime, dt) * Self.TSRecordCount / SecondsBetween(Self.BeginTime,
    Self.EndTime));
  if tmpIndex < 0 then
    tmpIndex := 0;
  if tmpIndex > Self.TSRecordCount - 1 then
    tmpIndex := Self.TSRecordCount - 1;
  while True do
  begin
    if Self.TSRecord[tmpIndex].DateTime = dt then
    begin
      Result := tmpIndex;
      Exit;
    end
    else if Self.TSRecord[tmpIndex].DateTime < dt then
    begin
      Inc(tmpIndex);
    end
    else if Self.TSRecord[tmpIndex].DateTime > dt then
    begin
      Dec(tmpIndex);
    end;
    if (tmpIndex < 0) or (tmpIndex >= Self.TSRecordCount) then
    begin
      Result := -1;
      Exit;
    end;
  end;
end;

procedure TTSHelper.ValueToDegree(values: Double2D; tbl: TTBL; btime, etime: TDateTime;
  intervals, offset, sr, st, box: Integer);
var
  exc, eyc, hc, scale: Double;
  I, J, K, count, rcount, Channels: Integer;
  tsdata: Integer2D;
  tsrec: TTSRecord;
  tmptm: TDateTime;
begin
  Self.TSRecordClear;
  exc := tbl.ExCoefficient;
  eyc := tbl.EyCoefficient;
  hc := tbl.HCoefficientAM;
  Channels := Length(values) + 1;
  count := Length(values[0]);
  SetLength(tsdata, Channels);
  tmptm := IncSecond(btime, offset);
  for I := 0 to Channels - 1 do
  begin
    SetLength(tsdata[I], sr);
  end;
  rcount := count div sr;
  for K := 0 to sr - 1 do
  begin
    tsdata[Channels - 1][K] := Random(10);
  end;
  scale := 1000000;
  for I := 0 to rcount - 1 do
  begin
    for K := 0 to sr - 1 do
    begin
      tsdata[0][K] := Trunc(values[0][I * sr + K] / exc / scale);
      // tsdata[0][k] := Random(1000);
    end;
    for K := 0 to sr - 1 do
    begin
      tsdata[1][K] := Trunc(values[1][I * sr + K] / eyc / scale);
      // tsdata[1][k] := Random(1000);
    end;
    for J := 2 to Channels - 2 do
    begin
      for K := 0 to sr - 1 do
      begin
        tsdata[J][K] := Trunc(values[J][I * sr + K] / hc / scale);
        // tsdata[J][k] := Random(1000);
      end;
    end;
    tsrec := Self.AddNewTSRecord;
    tsrec.SetRecord(tsdata, tmptm, sr, box, sr);
    if st = 0 then
    begin
      tmptm := IncSecond(tmptm, 1);
    end
    else
    begin
      if (I + 1) mod st = 0 then
      begin
        tmptm := IncSecond(tmptm, intervals - st + 1);
      end
      else
      begin
        tmptm := IncSecond(tmptm, 1);
      end;
    end;
  end;
  //
end;

procedure TTSHelper.DataReplace(values: Double2D; tbl: TTBL);

var
  exc, eyc, hc, scale: Double;
  I, J, K, count, rcount, Channels, sr: Integer;
  tsdata: Integer2D;
  tsrec: TTSRecord;
begin
  scale := 1;
  exc := tbl.ExCoefficient * scale;
  eyc := tbl.EyCoefficient * scale;
  hc := tbl.HCoefficientAM * scale;
  Channels := Self.Channels;
  sr := Self.Scans;
  count := Length(values[0]);
  SetLength(tsdata, Channels);
  for I := 0 to Channels - 1 do
  begin
    SetLength(tsdata[I], sr);
  end;
  rcount := Self.TSRecordCount;
  for K := 0 to sr - 1 do
  begin
    tsdata[Channels - 1][K] := Random(1000);
  end;
  for I := 0 to rcount - 1 do
  begin
    for K := 0 to sr - 1 do
    begin
      tsdata[0][K] := Trunc(values[0][I * sr + K] / exc);
    end;
    for K := 0 to sr - 1 do
    begin
      tsdata[1][K] := Trunc(values[1][I * sr + K] / eyc);
    end;
    for J := 2 to Channels - 2 do
    begin
      for K := 0 to sr - 1 do
      begin
        tsdata[J][K] := Trunc(values[J][I * sr + K] / hc);
      end;
    end;
    Self.TSRecord[I].DataReplace(tsdata);
  end;
end;

procedure TTSHelper.AppendTsRecord(apptsr: TTSRecord);
var
  newRecord: TTSRecord;
begin
  newRecord := Self.AddNewTSRecord;
  newRecord.CopyFrom(apptsr);
  Self.EndTime := apptsr.DateTime;
end;

procedure TTSHelper.DataAdd(values: Double2D; tbl: TTBL);

var
  exc, eyc, hc, scale: Double;
  I, J, K, count, rcount, Channels, sr: Integer;
  tsdata: Integer2D;
  tsrec: TTSRecord;
begin
  exc := tbl.ExCoefficient;
  eyc := tbl.EyCoefficient;
  hc := tbl.HCoefficientAM;
  Channels := Self.Channels;
  sr := Self.Scans;
  count := Length(values[0]);
  SetLength(tsdata, Channels);
  for I := 0 to Channels - 1 do
  begin
    SetLength(tsdata[I], sr);
  end;
  rcount := Self.TSRecordCount;
  for K := 0 to sr - 1 do
  begin
    tsdata[Channels - 1][K] := Random(1000);
  end;
  scale := 1000000;
  for I := 0 to rcount - 1 do
  begin
    for K := 0 to sr - 1 do
    begin
      tsdata[0][K] := Trunc(values[0][I * sr + K] / exc / scale);
    end;
    for K := 0 to sr - 1 do
    begin
      tsdata[1][K] := Trunc(values[1][I * sr + K] / eyc / scale);
    end;
    for J := 2 to Channels - 2 do
    begin
      for K := 0 to sr - 1 do
      begin
        tsdata[J][K] := Trunc(values[J][I * sr + K] / hc / scale);
      end;
    end;
    Self.TSRecord[I].DataAdd(tsdata);
  end;
end;

procedure TTSHelper.LoadFile(const fileName: String);
var
  ext: string;
begin
  path := ExtractFilePath(fileName);
  Name := ExtractFileName(fileName);
  ext := ExtractFileExt(Name);
  Name := Copy(Name, 1, Length(Name) - Length(ext));
  if (ext.ToLower = '.ts2') or (ext.ToLower = '.ts3') or (ext.ToLower = '.ts4') or (ext.ToLower = '.ts5') then
  begin
    ReadBin(fileName);
  end
  else if (ext.ToLower = '.ts2dat') or (ext.ToLower = '.ts3dat') or (ext.ToLower = '.ts4dat') or
    (ext.ToLower = '.ts5dat') then
  begin
    ReadText(fileName);
  end
  else
  begin
    raise Exception.Create('It is not a TS File!');
  end;
end;

procedure TTSHelper.LoadFrom(const fileName: string);
begin
  try
    if LowerCase(Copy(fileName, Length(fileName) - 3, 3)) = '.ts' then
      Self.ReadBin(fileName)
    else
      Self.ReadText(fileName);
  except
    raise Exception.Create('Create TSN Error : ' + fileName);
  end;
end;

procedure TTSHelper.ReadBin(const fileName: string);
var
  tmpts: TTSRecord;
  I, FCount: Integer;
  fsize: Int64;
  FStream: TFileStream;
  bts: TBytes;
begin
  FStream := TFileStream.Create(fileName, fmShareDenyNone);
  FStream.Position := 0;
  fsize := FStream.Size;
  SetLength(bts, fsize);
  FStream.Read(bts, fsize);
  FreeAndNil(FStream);

  tmpts := AddNewTSRecord;
  tmpts.Read(bts, 0, NIL);
  BoxSN := tmpts.BoxSN;
  Channels := tmpts.Channels;
  samplerate := tmpts.samplerate;
  Scans := tmpts.Scans;
  SampleRateUnits := tmpts.SampleRateUnits.ToString;
  TSLength := tmpts.TSLength;
  LengthPre := Channels * Scans * 3 + 32;
  FCount := fsize div LengthPre;
  for I := 1 to FCount - 1 do
  begin
    tmpts := AddNewTSRecord;
    if not tmpts.Read(bts, I * LengthPre, Self.TSRecord[I - 1]) then
    begin
      Log.d('Read Ts ERROR at record of ' + I.ToString + #13#10 + 'While the records count are ' + FCount.ToString);
    end;
  end;
  bts := nil;
  BeginTime := TSRecord[0].DateTime;
  EndTime := TSRecord[TSRecordCount - 1].DateTime;
end;

procedure TTSHelper.ReadText(const fileName: string);
var
  I, J, tscount: Integer;
  tfile: TextFile;
  tmp: string;
  tsrecordstr: TStrings;
  tmpts: TTSRecord;
begin
  AssignFile(tfile, fileName);
  Reset(tfile);
  ReadLn(tfile, tmp);
  tmp := Copy(tmp, 17, Length(tmp) - 16);
  tscount := StrToInt(Trim(tmp));
  ReadLn(tfile, tmp);
  ReadLn(tfile, tmp);
  ReadLn(tfile, tmp);
  tmp := Copy(tmp, 17, Length(tmp) - 16);
  BoxSN := StrToInt(Trim(tmp));
  ReadLn(tfile, tmp);
  tmp := Copy(tmp, 17, Length(tmp) - 16);
  Scans := StrToInt(Trim(tmp));
  ReadLn(tfile, tmp);
  tmp := Copy(tmp, 17, Length(tmp) - 16);
  Channels := StrToInt(Trim(tmp));
  ReadLn(tfile, tmp);
  tmp := Copy(tmp, 17, Length(tmp) - 16);
  LengthPre := StrToInt(Trim(tmp));
  ReadLn(tfile, tmp);
  ReadLn(tfile, tmp);
  ReadLn(tfile, tmp);
  ReadLn(tfile, tmp);
  ReadLn(tfile, tmp);
  tmp := Copy(tmp, 17, Length(tmp) - 16);
  samplerate := StrToInt(Trim(tmp));
  CloseFile(tfile);
  Reset(tfile);
  ReadLn(tfile, tmp);
  tsrecordstr := TStringList.Create;
  for I := 0 to tscount - 1 do
  begin
    ReadLn(tfile, tmp);
    tsrecordstr.Clear;
    for J := 0 to 13 + Scans - 1 do
    begin
      ReadLn(tfile, tmp);
      tsrecordstr.Add(tmp);
    end;
    tmpts := AddNewTSRecord;
    tmpts.ReadFromStrings(tsrecordstr);
  end;
  CloseFile(tfile);
  BeginTime := TSRecord[0].DateTime;
  EndTime := TSRecord[TSRecordCount - 1].DateTime;
  SampleRateUnits := TSRecord[0].SampleRateUnits.ToString;
  TSLength := TSRecord[0].TSLength;
end;

procedure TTSHelper.ReplaceRecord(const rec: TTSRecord);
var
  I: Integer;
  Index1, Index2, Index: Integer;
begin
  Index1 := 0;
  Index2 := Self.TSRecordCount - 1;
  if rec.DateTime < Self.TSRecord[0].DateTime then
  begin
    Exit;
  end;
  if rec.DateTime > Self.TSRecord[Index2].DateTime then
  begin
    Exit;
  end;
  Index := (Index2 + Index1) div 2;
  while Self.TSRecord[Index].DateTime <> rec.DateTime do
  begin
    if rec.DateTime > Self.TSRecord[Index].DateTime then
    begin
      Index1 := Index;
    end
    else
    begin
      Index2 := Index;
    end;
    Index := (Index2 + Index1) div 2;
    if Self.TSRecord[Index2].DateTime = rec.DateTime then
    begin
      Index := Index2;
      Break;
    end;
  end;
  Self.TSRecords[Index].Free;
  Self.TSRecords[Index] := rec;
  //
end;

procedure TTSHelper.SaveFile(const fileName: String);
var
  ext: string;
begin
  path := ExtractFilePath(fileName);
  Name := ExtractFileName(fileName);
  ext := ExtractFileExt(Name);
  Name := Copy(Name, 1, Length(Name) - Length(ext));
  if (ext.ToLower = '.ts2') or (ext.ToLower = '.ts3') or (ext.ToLower = '.ts4') or (ext.ToLower = '.ts5') then
  begin
    WriteBin(fileName);
  end
  else if (ext.ToLower = '.ts2dat') or (ext.ToLower = '.ts3dat') or (ext.ToLower = '.ts4dat') or
    (ext.ToLower = '.ts5dat') then
  begin
    WriteText(fileName);
  end
  else
  begin
    raise Exception.Create('It is not a TS File Name!');
  end;
end;

procedure TTSHelper.SaveTo(const fileName: string);
begin
  try
    if LowerCase(Copy(fileName, Length(fileName) - 3, 3)) = '.ts' then
      Self.WriteBin(fileName)
    else
      Self.WriteText(fileName);
  except
    raise Exception.Create('SaveTo TSN Error : ' + fileName);
  end;
end;

procedure TTSHelper.AppendTs(appts: TTS);
var
  I: Integer;
begin
  // for I := 0 to appts.TSRecordCount - 1 do
  // begin
  // Self.AppendTsRecord(appts.TSRecord[I]);
  // end;
  Self.TSRecords.AddRange(appts.TSRecords);
  appts.TSRecords.Clear;
end;

function TTSHelper.GetAllData: Integer2D;
var
  I, J, tcount: Integer;
begin
  SetLength(Result, Self.Channels);
  tcount := Self.TSRecordCount * Self.Scans;
  for I := 0 to Self.Channels - 1 do
  begin
    SetLength(Result[I], tcount);
  end;
  for I := 0 to Self.TSRecordCount - 1 do
  begin
    for J := 0 to Self.Channels - 1 do
    begin
      Move(Self.TSRecord[I].Data[J][0], Result[J][I * Self.Scans], 4 * Self.Scans);
    end;
  end;
end;

function TTSHelper.GetAllTimes: TDateTime1D;
var
  I, J, tcount: Integer;
begin
  tcount := Self.TSRecordCount * Self.Scans;
  SetLength(Result, tcount);
  for I := 0 to Self.TSRecordCount - 1 do
  begin
    for J := 0 to Self.Scans - 1 do
    begin
      Result[I * Self.Scans + J] := Self.TSRecord[I].DateTime + J / Self.samplerate / 24 / 3600;
    end;
  end;
end;

procedure TTSHelper.GetData3D(out MData: Double3D; out times: TDateTime1D; const bIndex: Integer = 0;
  const eIndex: Integer = -1; recordsOfPerSegment: Integer = 1; const ExIndex: Integer = 0; const EyIndex: Integer = 1;
  const HxIndex: Integer = 2; const HyIndex: Integer = 3; const HzIndex: Integer = 4);
var
  I, J, K, beginindex, endIndex, rcount, scount: Integer;
  shift: Boolean;
begin
  beginindex := 0;
  endIndex := 0;
  if bIndex < 0 then
    beginindex := 0;
  // 解决分段采集数据，起始时间不是整点的问题
  // 检测并偏移
  shift := False;
  while abs(Self.TSRecord[beginindex + 1].DateTime - Self.TSRecord[beginindex].DateTime -
    Self.TSRecord[beginindex + recordsOfPerSegment].DateTime + Self.TSRecord[beginindex + recordsOfPerSegment - 1]
    .DateTime) <= 0.00000001 do
  begin
    Inc(beginindex);
    shift := True;
  end;
  if shift then
  begin
    Inc(beginindex);
  end;
  // 偏移完毕
  if eIndex = -1 then
    endIndex := Self.TSRecordCount - 1;
  rcount := endIndex - beginindex + 1;
  scount := rcount div recordsOfPerSegment;
  SetLength(MData, scount);
  SetLength(times, scount);
  for K := 0 to scount - 1 do
  begin
    SetLength(MData[K], 5);
    times[K] := Self.TSRecord[beginindex + K * recordsOfPerSegment].DateTime;
    for I := 0 to 5 - 1 do
    begin
      SetLength(MData[K][I], recordsOfPerSegment * Self.Scans);
    end;
    for I := 0 to recordsOfPerSegment - 1 do
    begin
      Move(Self.TSRecord[beginindex + K * recordsOfPerSegment + I].GetChDataD(ExIndex)[0], MData[K][0][I * Self.Scans],
        Self.Scans * 8);
      Move(Self.TSRecord[beginindex + K * recordsOfPerSegment + I].GetChDataD(EyIndex)[0], MData[K][1][I * Self.Scans],
        Self.Scans * 8);
      Move(Self.TSRecord[beginindex + K * recordsOfPerSegment + I].GetChDataD(HxIndex)[0], MData[K][2][I * Self.Scans],
        Self.Scans * 8);
      Move(Self.TSRecord[beginindex + K * recordsOfPerSegment + I].GetChDataD(HyIndex)[0], MData[K][3][I * Self.Scans],
        Self.Scans * 8);
      Move(Self.TSRecord[beginindex + K * recordsOfPerSegment + I].GetChDataD(HzIndex)[0], MData[K][4][I * Self.Scans],
        Self.Scans * 8);
    end;
  end;
end;

procedure TTSHelper.ReadPhoenixText(const fileName: string);
var
  Index, tscount: Integer;
  tsrecordstr: TStrings;
  tmpts: TTSRecord;

begin
  tsrecordstr := TStringList.Create;
  tsrecordstr.LoadFromFile(fileName);
  Index := 0;
  tscount := tsrecordstr.count;
  while Index < tscount do
  begin
    tmpts := AddNewTSRecord;
    if not tmpts.ReadFromPhoenixStrings(tsrecordstr, index) then
    begin
      Self.RemoveTSRecord(tmpts);
      Break;
    end;
  end;
  tsrecordstr.Free;
  Name := ExtractFileName(fileName);
  path := ExtractFileDir(fileName);
  BeginTime := TSRecord[0].DateTime;
  EndTime := TSRecord[TSRecordCount - 1].DateTime;
  Scans := TSRecord[0].Scans;
  Channels := TSRecord[0].Channels;
  SampleLength := TSRecord[0].SampleLength;
  samplerate := TSRecord[0].samplerate;
  BoxSN := TSRecord[0].BoxSN;
  SampleRateUnits := TSRecord[0].SampleRateUnits.ToString;
  TSLength := TSRecord[0].TSLength;
  Self.fileName := ExtractFileName(fileName);
  LengthPre := Channels * Scans * 3 + 32;
end;

procedure TTSHelper.SetAllData(const Value: Integer2D);
var
  I, J, tcount: Integer;
begin
  for I := 0 to Self.TSRecordCount - 1 do
  begin
    for J := 0 to Self.Channels - 1 do
    begin
      Move(Value[J][I * Self.Scans], Self.TSRecord[I].Data[J][0], 4 * Self.Scans);
    end;
  end;
end;

procedure TTSHelper.SortDateTime;
var
  I, J: Integer;
begin
  for I := 0 to Self.TSRecordCount - 2 do
  begin
    for J := I + 1 to Self.TSRecordCount - 1 do
    begin
      if Self.TSRecord[I].DateTime > Self.TSRecord[J].DateTime then
      begin
        Self.TSRecords.Exchange(I, J);
      end;
    end;
  end;
end;

function TTSHelper.ToGMTStrs: TStringList;
var
  I: Integer;
  tmp: TStringList;
begin
  Result := TStringList.Create;
  for I := 0 to Self.TSRecordCount - 1 do
  begin
    tmp := Self.TSRecord[I].ToGMTStrs;
    Result.AddStrings(tmp);
    tmp.Free;
  end;
end;

procedure TTSHelper.WriteBin2(const fileName: string);
var
  tsdat: TTSRecord;
  I: Integer;
  FStream: TFileStream;
begin
  FStream := TFileStream.Create(fileName, fmCreate);
  for I := 0 to Self.TSRecordCount - 1 do
  begin
    tsdat := TSRecord[I];
    tsdat.Write(FStream);
  end;
  FStream.Free;
end;

procedure TTSHelper.WriteBin(const fileName: string);
var
  I: Integer;
  FStream: TFileStream;
  bts: TBytes;
begin
  FStream := TFileStream.Create(fileName, fmCreate);
  for I := 0 to Self.TSRecordCount - 1 do
  begin
    bts.Append(TSRecord[I].ToPhoenixBytes);
  end;
  FStream.WriteData(bts, bts.count);
  FStream.Free;
  bts := nil;
end;

procedure TTSHelper.WriteGMTText(const fileName: string);
var
  strs: TStringList;
begin
  strs := Self.ToGMTStrs;
  strs.SaveToFile(fileName);
  strs.Free;
end;

procedure TTSHelper.WriteText(const fileName: string);
var
  tfile: TextFile;
  tsdat: TTSRecord;
  I: Integer;
begin
  AssignFile(tfile, fileName);
  Rewrite(tfile);
  Writeln(tfile, 'Record Count:   ' + IntToStr(Self.TSRecordCount));
  for I := 0 to Self.TSRecordCount - 1 do
  begin
    tsdat := TSRecord[I];
    Writeln(tfile, 'RecordNumber:  ' + IntToStr(I + 1));
    Write(tfile, tsdat.ToString);
  end;
  CloseFile(tfile);
end;

{ TCLRecordHelper }

procedure TCLRecordHelper.Read(var fs: TFileStream; count: Integer);
var
  Tfcn: smallint;
  Tbaseband: Single;
  Tharmonic: smallint;
  Trawreal: Double1D;
  Trawimag: Double1D;
  Terr: Double1D;
  TFC: Double1D;
  Treal: Double1D;
  Timag: Double1D;
  tmpD: Single;
  I: Integer;
begin
  fs.Read(Tfcn, 2);
  fcn := Tfcn;
  fs.Read(Tbaseband, 4);
  baseband := Tbaseband;
  fs.Read(Tharmonic, 2);
  harmonic := Tharmonic;
  SetLength(Trawreal, count);
  SetLength(Trawimag, count);
  SetLength(Terr, count);
  SetLength(TFC, count);
  SetLength(Treal, count);
  SetLength(Timag, count);
  for I := 0 to count - 1 do
  begin
    fs.Read(tmpD, 4);
    Trawreal[I] := tmpD;
    fs.Read(tmpD, 4);
    Trawimag[I] := tmpD;
    fs.Read(tmpD, 4);
    Terr[I] := tmpD;
    fs.Read(tmpD, 4);
    TFC[I] := tmpD;
    fs.Read(tmpD, 4);
    Treal[I] := tmpD;
    fs.Read(tmpD, 4);
    Timag[I] := tmpD;
  end;
  rawreal := Trawreal;
  rawimag := Trawimag;
  err := Terr;
  FC := TFC;
  real := Treal;
  imag := Timag;
end;

procedure TCLRecordHelper.SetAsZero;
var
  I, count: Integer;
begin
  count := Length(rawreal);
  for I := 0 to count - 1 do
  begin
    rawreal[I] := 1;
    rawimag[I] := 0;
    err[I] := 0;
    FC[I] := 0.0000001;
    real[I] := 1;
    imag[I] := 0;
  end;
end;

procedure TCLRecordHelper.Write(var fs: TFileStream; count: Integer);
var
  I: Integer;
  s: Single;
begin
  fs.Write(fcn, 2);
  fs.Write(baseband, 4);
  fs.Write(harmonic, 2);
  for I := 0 to count - 1 do
  begin
    s := rawreal[I];
    fs.Write(s, 4);
    s := rawimag[I];
    fs.Write(s, 4);
    s := err[I];
    fs.Write(s, 4);
    s := FC[I];
    fs.Write(s, 4);
    s := real[I];
    fs.Write(s, 4);
    s := imag[I];
    fs.Write(s, 4);
  end;
end;

procedure TCLBHelper.LoadFile(const fileName: String);
var
  ext: string;
begin
  path := ExtractFilePath(fileName);
  Name := ExtractFileName(fileName);
  ext := ExtractFileExt(Name);
  Name := Copy(Name, 1, Length(Name) - Length(ext));
  if ext.ToLower = '.clb' then
  begin
    ReadCLB(fileName);
  end
  else
  begin
    raise Exception.Create('It is not a CLB File!');
  end;
end;

{ TCLBHelper }

procedure TCLBHelper.ReadCLB(const fileName: string);
var
  second: Byte;
  minute: Byte;
  hour: Byte;
  day: Byte;
  month: Byte;
  year: Byte;
  week: Byte;
  century: Byte;
  fs: TFileStream;
  TSNUM: smallint;
  TNCHN: smallint;
  Tunknow1: smallint;
  TSTAT: smallint;
  Tunknow2: smallint;
  Tunknow3: smallint;
  THW: String;
  TVER: String;
  TTCMB: Integer;
  TTALS: Integer;
  TLFRQ: Integer;
  TV5SR: Integer;
  TLPFR: Integer;
  str: String;
  I: Integer;
  strbyte: Array [0 .. 11] of Byte;
  unknow4byte: Array [0 .. 67] of Byte;
  tmpRec: TCLRecord;
begin
  fs := TFileStream.Create(fileName, fmOpenRead);
  fs.Read(second, 1);
  fs.Read(minute, 1);
  fs.Read(hour, 1);
  fs.Read(day, 1);
  fs.Read(month, 1);
  fs.Read(year, 1);
  fs.Read(week, 1);
  fs.Read(century, 1);
  Self.DateTime := EncodeDate(century * 100 + year, month, day) + EncodeTime(hour, minute, second, 0);
  fs.Read(TSNUM, 2);
  SNUM := TSNUM;
  fs.Read(TNCHN, 2);
  NCHN := TNCHN;
  fs.Read(Tunknow1, 2);
  unknow1 := Tunknow1;
  fs.Read(TSTAT, 2);
  STAT := TSTAT;
  fs.Read(Tunknow2, 2);
  unknow2 := Tunknow2;
  fs.Read(Tunknow3, 2);
  unknow3 := Tunknow3;
  fs.Read(strbyte, 12);
  str := '';
  for I := 0 to 12 - 1 do
  begin
    str := str + Chr(strbyte[I]);
  end;
  HW := str;
  fs.Read(strbyte, 12);
  str := '';
  for I := 0 to 12 - 1 do
  begin
    str := str + Chr(strbyte[I]);
  end;
  VER := str;
  fs.Read(TTCMB, 4);
  TCMB := TTCMB;
  fs.Read(TTALS, 4);
  TALS := TTALS;
  fs.Read(TLFRQ, 4);
  LFRQ := TLFRQ;
  fs.Read(TV5SR, 4);
  V5SR := TV5SR;
  fs.Read(TLPFR, 4);
  LPFR := TLPFR;
  fs.Read(unknow4byte, 68);
  str := '';
  for I := 0 to 68 - 1 do
  begin
    str := str + Chr(unknow4byte[I]);
  end;
  unknow4 := str;
  while fs.Position < fs.Size - 1 do
  begin
    tmpRec := AddNewCLBRecord;
    tmpRec.Read(fs, NCHN);
  end;
  fs.Free;
end;

procedure TCLBHelper.SaveFile(const fileName: String);
var
  ext: string;
begin
  path := ExtractFilePath(fileName);
  Name := ExtractFileName(fileName);
  ext := ExtractFileExt(Name);
  Name := Copy(Name, 1, Length(Name) - Length(ext));
  if ext.ToLower = '.clb' then
  begin
    WriteCLB(fileName);
  end
  else
  begin
    raise Exception.Create('It is not a CLB File Name!');
  end;
end;

procedure TCLBHelper.SetAsZero;
var
  I: Integer;
begin
  for I := 0 to CLBRecordCount - 1 do
  begin
    CLBRecord[I].SetAsZero;
  end;
  SNUM := 9999;
end;

procedure TCLBHelper.WriteCLB(const fileName: string);
var
  yy, ce, yyy, mon, dd, ww, hh, min, sec, msec: Word;
  second: Byte;
  minute: Byte;
  hour: Byte;
  day: Byte;
  month: Byte;
  year: Byte;
  week: Byte;
  century: Byte;
  I: Integer;
  strbyte: Array [0 .. 11] of Byte;
  unknow4byte: Array [0 .. 67] of Byte;
  tmpRec: TCLRecord;
  fs: TFileStream;
begin
  fs := TFileStream.Create(fileName, fmCreate);

  DecodeDateFully(Self.DateTime, yy, mon, dd, ww);
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
  DecodeTime(Self.DateTime, hh, min, sec, msec);
  Move(hh, hour, 1);
  Move(min, minute, 1);
  Move(sec, second, 1);
  fs.Write(second, 1);
  fs.Write(minute, 1);
  fs.Write(hour, 1);
  fs.Write(day, 1);
  fs.Write(month, 1);
  fs.Write(year, 1);
  fs.Write(week, 1);
  fs.Write(century, 1);

  fs.Write(SNUM, 2);
  fs.Write(NCHN, 2);
  fs.Write(unknow1, 2);
  fs.Write(STAT, 2);
  fs.Write(unknow2, 2);
  fs.Write(unknow3, 2);

  for I := 0 to Length(HW) - 1 do
    strbyte[I] := Ord(HW[I + 1]);
  for I := Length(HW) to 12 - 1 do
    strbyte[I] := $00;
  fs.Write(strbyte, 12);

  for I := 0 to Length(VER) - 1 do
    strbyte[I] := Ord(VER[I + 1]);
  for I := Length(VER) to 12 - 1 do
    strbyte[I] := $00;
  fs.Write(strbyte, 12);

  fs.Write(TCMB, 4);
  fs.Write(TALS, 4);
  fs.Write(LFRQ, 4);
  fs.Write(V5SR, 4);
  fs.Write(LPFR, 4);

  for I := 0 to Length(unknow4) - 1 do
    unknow4byte[I] := Ord(unknow4[I + 1]);
  for I := Length(unknow4) to 68 - 1 do
    unknow4byte[I] := $00;
  fs.Write(unknow4byte, 68);

  for I := 0 to CLBRecordCount - 1 do
  begin
    CLBRecord[I].Write(fs, NCHN);
  end;

  fs.Free;
end;

procedure TCLCHelper.LoadFile(const fileName: String);
var
  ext: string;
begin
  path := ExtractFilePath(fileName);
  Name := ExtractFileName(fileName);
  ext := ExtractFileExt(Name);
  Name := Copy(Name, 1, Length(Name) - Length(ext));
  if ext.ToLower = '.clc' then
  begin
    ReadCLC(fileName);
  end
  else
  begin
    raise Exception.Create('It is not a CLC File!');
  end;
end;

{ TCLCHelper }

procedure TCLCHelper.ReadCLC(const fileName: string);

var
  second: Byte;
  minute: Byte;
  hour: Byte;
  day: Byte;
  month: Byte;
  year: Byte;
  week: Byte;
  century: Byte;
  fs: TFileStream;
  TSNUM: smallint;
  TNCHN: smallint;
  Tunknow1: smallint;
  TSTAT: smallint;
  Tunknow2: smallint;
  Tunknow3: smallint;
  TTCMB: Integer;
  TTALS: Integer;
  TLFRQ: Integer;
  TV5SR: Integer;
  TLPFR: Integer;
  THATT: Double;
  THNOM: Double;
  TCPHC: Double;
  str: String;
  I: Integer;
  strbyte: Array [0 .. 11] of Byte;
  unknow4byte: Array [0 .. 31] of Byte;
  tmpRec: TCLRecord;
begin
  fs := TFileStream.Create(fileName, fmOpenRead);
  fs.Read(second, 1);
  fs.Read(minute, 1);
  fs.Read(hour, 1);
  fs.Read(day, 1);
  fs.Read(month, 1);
  fs.Read(year, 1);
  fs.Read(week, 1);
  fs.Read(century, 1);
  Self.DateTime := EncodeDate(century * 100 + year, month, day) + EncodeTime(hour, minute, second, 0);
  fs.Read(TSNUM, 2);
  SNUM := TSNUM;
  fs.Read(TNCHN, 2);
  NCHN := TNCHN;
  fs.Read(Tunknow1, 2);
  unknow1 := Tunknow1;
  fs.Read(TSTAT, 2);
  STAT := TSTAT;
  fs.Read(Tunknow2, 2);
  unknow2 := Tunknow2;
  fs.Read(Tunknow3, 2);
  unknow3 := Tunknow3;
  fs.Read(strbyte, 12);
  str := '';
  for I := 0 to 12 - 1 do
  begin
    str := str + Chr(strbyte[I]);
  end;
  HW := str;
  fs.Read(strbyte, 12);
  str := '';
  for I := 0 to 12 - 1 do
  begin
    str := str + Chr(strbyte[I]);
  end;
  VER := str;
  fs.Read(TTCMB, 4);
  TCMB := TTCMB;
  fs.Read(TTALS, 4);
  TALS := TTALS;
  fs.Read(TLFRQ, 4);
  LFRQ := TLFRQ;
  fs.Read(TV5SR, 4);
  V5SR := TV5SR;
  fs.Read(TLPFR, 4);
  LPFR := TLPFR;
  fs.Read(strbyte, 12);
  str := '';
  for I := 0 to 12 - 1 do
  begin
    str := str + Chr(strbyte[I]);
  end;
  HSN := str;
  fs.Read(THATT, 8);
  HATT := THATT;
  fs.Read(THNOM, 8);
  HNOM := THNOM;
  fs.Read(TCPHC, 8);
  CPHC := TCPHC;
  fs.Read(unknow4byte, 32);
  str := '';
  for I := 0 to 32 - 1 do
  begin
    str := str + Chr(unknow4byte[I]);
  end;
  unknow4 := str;
  while fs.Position < fs.Size - 1 do
  begin
    tmpRec := AddNewCLCRecord;
    tmpRec.Read(fs, NCHN);
  end;
  fs.Free;
end;

procedure TCLCHelper.SaveFile(const fileName: String);
var
  ext: string;
begin
  path := ExtractFilePath(fileName);
  Name := ExtractFileName(fileName);
  ext := ExtractFileExt(Name);
  Name := Copy(Name, 1, Length(Name) - Length(ext));
  if ext.ToLower = '.clc' then
  begin
    WriteCLC(fileName);
  end
  else
  begin
    raise Exception.Create('It is not a CLC File Name!');
  end;
end;

procedure TCLCHelper.SetAsZero;
var
  I: Integer;
begin
  for I := 0 to CLCRecordCount - 1 do
  begin
    CLCRecord[I].SetAsZero;
  end;
  SNUM := 9999;
  HSN := 'COIL9999';
end;

procedure TCLCHelper.WriteCLC(const fileName: string);

var
  yy, ce, yyy, mon, dd, ww, hh, min, sec, msec: Word;
  second: Byte;
  minute: Byte;
  hour: Byte;
  day: Byte;
  month: Byte;
  year: Byte;
  week: Byte;
  century: Byte;
  I: Integer;
  strbyte: Array [0 .. 11] of Byte;
  unknow4byte: Array [0 .. 31] of Byte;
  tmpRec: TCLRecord;
  fs: TFileStream;
begin
  fs := TFileStream.Create(fileName, fmCreate);

  DecodeDateFully(Self.DateTime, yy, mon, dd, ww);
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
  DecodeTime(Self.DateTime, hh, min, sec, msec);
  Move(hh, hour, 1);
  Move(min, minute, 1);
  Move(sec, second, 1);
  fs.Write(second, 1);
  fs.Write(minute, 1);
  fs.Write(hour, 1);
  fs.Write(day, 1);
  fs.Write(month, 1);
  fs.Write(year, 1);
  fs.Write(week, 1);
  fs.Write(century, 1);

  fs.Write(SNUM, 2);
  fs.Write(NCHN, 2);
  fs.Write(unknow1, 2);
  fs.Write(STAT, 2);
  fs.Write(unknow2, 2);
  fs.Write(unknow3, 2);

  for I := 0 to Length(HW) - 1 do
    strbyte[I] := Ord(HW[I + 1]);
  for I := Length(HW) to 12 - 1 do
    strbyte[I] := $00;
  fs.Write(strbyte, 12);

  for I := 0 to Length(VER) - 1 do
    strbyte[I] := Ord(VER[I + 1]);
  for I := Length(VER) to 12 - 1 do
    strbyte[I] := $00;
  fs.Write(strbyte, 12);

  fs.Write(TCMB, 4);
  fs.Write(TALS, 4);
  fs.Write(LFRQ, 4);
  fs.Write(V5SR, 4);
  fs.Write(LPFR, 4);

  for I := 0 to Length(HSN) - 1 do
    strbyte[I] := Ord(HSN[I + 1]);
  for I := Length(HSN) to 12 - 1 do
    strbyte[I] := $00;
  fs.Write(strbyte, 12);

  fs.Write(HATT, 8);
  fs.Write(HNOM, 8);
  fs.Write(CPHC, 8);

  for I := 0 to Length(unknow4) - 1 do
    unknow4byte[I] := Ord(unknow4[I + 1]);
  for I := Length(unknow4) to 32 - 1 do
    unknow4byte[I] := $00;
  fs.Write(unknow4byte, 32);

  for I := 0 to CLCRecordCount - 1 do
  begin
    CLCRecord[I].Write(fs, NCHN);
  end;

  fs.Free;
end;

end.
