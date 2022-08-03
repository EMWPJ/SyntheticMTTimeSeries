unit ForwardSite;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  System.Variants, System.Generics.Collections,
  WTypes;

type

  TForwardSite = class;
  TForwardFreqencyFields = class;

  TForwardSite = class(TObject)
  private
    FName: String;
    FX: Double;
    FY: Double;
    FFieldss: TList<TForwardFreqencyFields>;
    procedure SetName(const _Value: String);
    procedure SetX(const _Value: Double);
    procedure SetY(const _Value: Double);
    procedure SetFieldss(const _Value: TList<TForwardFreqencyFields>);
    function GetFields(Index: Integer): TForwardFreqencyFields;
    procedure SetFields(Index: Integer; const _Value: TForwardFreqencyFields);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCalibration(responds: TComplex2D);
    procedure AddFields(_Value: TForwardFreqencyFields);
    function AddNewFields: TForwardFreqencyFields;
    procedure FieldsClear;
    function FieldsCount: Integer;
    procedure RemoveFields(_Value: TForwardFreqencyFields);
    procedure DeleteFields(Index: Integer);
    function GetAllFrequencies: Double1D;
    procedure GetFEH(out fre: Double1D; out Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2, Hz2: TComplex1D);
    procedure GetFEH1(out fre: Double1D; out Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2, Hz2: TComplex1D);
    function GetFrequencies: Double1D;
    procedure GetImpendence(out fre: Double1D; out zxx, zxy, zyx, zyy: TComplex1D);
    procedure Interpolation(percount: Integer);
    procedure NegativeHarmonicFactor;
    procedure UpdateNatureMagneticAmplitude(escale, hscale: Double1D);
    procedure CopyTo(target: TForwardSite);
    property Name: String read FName write SetName;
    property X: Double read FX write SetX;
    property Y: Double read FY write SetY;
    property Fieldss: TList<TForwardFreqencyFields> read FFieldss write SetFieldss;
    property Fields[Index: Integer]: TForwardFreqencyFields read GetFields write SetFields;
  end;

  TForwardFreqencyFields = class(TObject)
  private
    FFrequency: Double;
    FZxx: TComplex;
    FZxy: TComplex;
    FZyx: TComplex;
    FZyy: TComplex;
    FTzx: TComplex;
    FTzy: TComplex;
    FEx1: TComplex;
    FEy1: TComplex;
    FHx1: TComplex;
    FHy1: TComplex;
    FHz1: TComplex;
    FEx2: TComplex;
    FEy2: TComplex;
    FHx2: TComplex;
    FHy2: TComplex;
    FHz2: TComplex;
    procedure SetFrequency(const _Value: Double);
    procedure SetZxx(const _Value: TComplex);
    procedure SetZxy(const _Value: TComplex);
    procedure SetZyx(const _Value: TComplex);
    procedure SetZyy(const _Value: TComplex);
    procedure SetTzx(const _Value: TComplex);
    procedure SetTzy(const _Value: TComplex);
    procedure SetEx1(const _Value: TComplex);
    procedure SetEy1(const _Value: TComplex);
    procedure SetHx1(const _Value: TComplex);
    procedure SetHy1(const _Value: TComplex);
    procedure SetHz1(const _Value: TComplex);
    procedure SetEx2(const _Value: TComplex);
    procedure SetEy2(const _Value: TComplex);
    procedure SetHx2(const _Value: TComplex);
    procedure SetHy2(const _Value: TComplex);
    procedure SetHz2(const _Value: TComplex);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function LoadModEMMTRespond(const FName: string): TList<TForwardSite>;
    procedure CopyTo(target: TForwardFreqencyFields);
    property Frequency: Double read FFrequency write SetFrequency;
    property zxx: TComplex read FZxx write SetZxx;
    property zxy: TComplex read FZxy write SetZxy;
    property zyx: TComplex read FZyx write SetZyx;
    property zyy: TComplex read FZyy write SetZyy;
    property Tzx: TComplex read FTzx write SetTzx;
    property Tzy: TComplex read FTzy write SetTzy;
    property Ex1: TComplex read FEx1 write SetEx1;
    property Ey1: TComplex read FEy1 write SetEy1;
    property Hx1: TComplex read FHx1 write SetHx1;
    property Hy1: TComplex read FHy1 write SetHy1;
    property Hz1: TComplex read FHz1 write SetHz1;
    property Ex2: TComplex read FEx2 write SetEx2;
    property Ey2: TComplex read FEy2 write SetEy2;
    property Hx2: TComplex read FHx2 write SetHx2;
    property Hy2: TComplex read FHy2 write SetHy2;
    property Hz2: TComplex read FHz2 write SetHz2;
  end;

function LoadModEMMTRespond(const FName: string): TObjectList<TForwardSite>;

implementation

function LoadModEMMTRespond(const FName: string): TObjectList<TForwardSite>;
var
  I, J, K, scount, fcount: Int64;
  TXT: TextFile;
  str: string;
  strarr: TArray<string>;
  tmp: TForwardSite;
  ffield: TForwardFreqencyFields;
  fres: Double1D;
begin
  AssignFile(TXT, FName);
  Reset(TXT);
  Result := TObjectList<TForwardSite>.Create;
  while not Eof(TXT) do
  begin
    ReadLn(TXT, str);
    if str.StartsWith('#') then
    begin
      Continue;
    end;
    if str.StartsWith('>  Full_Impedance') then
    begin
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
      fcount := strarr[1].ToInteger;
      scount := strarr[2].ToInteger;
      SetLength(fres, fcount);
      for J := 0 to scount - 1 do
      begin
        tmp := TForwardSite.Create;
        Result.Add(tmp);
        tmp.Name := 'ModEM' + J.ToString;
        for K := 0 to fcount - 1 do
        begin
          ReadLn(TXT, str); // [mv/Km]/[nT] ->    [V/m]/[A/m]
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield := tmp.AddNewFields;
          ffield.Frequency := 1 / strarr[0].ToDouble;
          ffield.zxx := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) / (1E6 / 4E2 / System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.zxy := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) / (1E6 / 4E2 / System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.zyx := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) / (1E6 / 4E2 / System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.zyy := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) / (1E6 / 4E2 / System.Pi);
        end;
        tmp.X := strarr[4].ToDouble;
        tmp.Y := strarr[5].ToDouble;
      end;
      Continue;
    end;
    if str.StartsWith('>  Full_Vertical_Components') then
    begin
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      for J := 0 to scount - 1 do
      begin
        tmp := Result[J];
        for K := 0 to fcount - 1 do
        begin
          ffield := tmp.Fields[K];
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Tzx := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Tzy := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble);
        end;
      end;
    end;
    if str.StartsWith('>  EM_Fields') then
    begin
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      for J := 0 to scount - 1 do
      begin
        tmp := Result[J];
        for K := 0 to fcount - 1 do
        begin
          ffield := tmp.Fields[K];
          ReadLn(TXT, str); // [mv/Km]/[nT] -> [V/m]/[A/m]
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Ex1 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * (4E2 * System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Ey1 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * (4E2 * System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Hx1 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * 1E9;
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Hy1 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * 1E9;
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Hz1 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * 1E9;
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Ex2 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * (4E2 * System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Ey2 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * (4E2 * System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Hx2 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * 1E9;
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Hy2 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * 1E9;
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Hz2 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * 1E9;
        end;
      end;
      Break;
    end;
  end;
  fres := nil;
  strarr := nil;
  CloseFile(TXT);
end;

{ Site }
procedure TForwardSite.CopyTo(target: TForwardSite);
var
  n: TForwardFreqencyFields;
  I: Integer;
begin
  target.FName := Self.FName;
  target.FX := Self.FX;
  target.FY := Self.FY;
  target.FieldsClear;
  for I := 0 to Self.FieldsCount - 1 do
  begin
    n := target.AddNewFields;
    Self.Fields[I].CopyTo(n);
  end;
end;

constructor TForwardSite.Create;
begin
  FFieldss := TList<TForwardFreqencyFields>.Create;
end;

destructor TForwardSite.Destroy;
begin
  FieldsClear;
  FFieldss.Free;
  inherited;
end;

procedure TForwardSite.AddCalibration(responds: TComplex2D);
var
  I: Integer;
begin
  for I := 0 to Self.FieldsCount - 1 do
  begin
    Self.Fields[I].Ex1 := Self.Fields[I].Ex1 / responds[0][I];
    Self.Fields[I].Ex2 := Self.Fields[I].Ex2 / responds[0][I];
    Self.Fields[I].Ey1 := Self.Fields[I].Ey1 / responds[1][I];
    Self.Fields[I].Ey2 := Self.Fields[I].Ey2 / responds[1][I];
    Self.Fields[I].Hx1 := Self.Fields[I].Hx1 / responds[2][I] * 4E-7 * System.Pi;
    Self.Fields[I].Hx2 := Self.Fields[I].Hx2 / responds[2][I] * 4E-7 * System.Pi;
    Self.Fields[I].Hy1 := Self.Fields[I].Hy1 / responds[3][I] * 4E-7 * System.Pi;
    Self.Fields[I].Hy2 := Self.Fields[I].Hy2 / responds[3][I] * 4E-7 * System.Pi;
    Self.Fields[I].Hz1 := Self.Fields[I].Hz1 / responds[4][I] * 4E-7 * System.Pi;
    Self.Fields[I].Hz2 := Self.Fields[I].Hz2 / responds[4][I] * 4E-7 * System.Pi;
  end;
end;

procedure TForwardSite.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TForwardSite.SetX(const _Value: Double);
begin
  FX := _Value;
end;

procedure TForwardSite.SetY(const _Value: Double);
begin
  FY := _Value;
end;

procedure TForwardSite.SetFieldss(const _Value: TList<TForwardFreqencyFields>);
begin
  FieldsClear;
  FFieldss := _Value;
end;

function TForwardSite.GetFields(Index: Integer): TForwardFreqencyFields;
begin
  Result := FFieldss[Index];
end;

procedure TForwardSite.SetFields(Index: Integer; const _Value: TForwardFreqencyFields);
begin
  FFieldss[Index].Free;
  FFieldss[Index] := _Value;
end;

procedure TForwardSite.AddFields(_Value: TForwardFreqencyFields);
begin;
  FFieldss.Add(_Value);
end;

function TForwardSite.AddNewFields: TForwardFreqencyFields;
var
  Fieldstmp: TForwardFreqencyFields;
begin;
  Fieldstmp := TForwardFreqencyFields.Create;
  FFieldss.Add(Fieldstmp);
  Result := Fieldstmp;
end;

procedure TForwardSite.FieldsClear;
begin
  while FFieldss.Count > 0 do
  begin
    FFieldss.Items[0].Free;
    FFieldss.Delete(0);
  end;
end;

function TForwardSite.FieldsCount: Integer;
begin
  Result := FFieldss.Count;
end;

procedure TForwardSite.RemoveFields(_Value: TForwardFreqencyFields);
begin
  FFieldss.Remove(_Value);
  _Value.Free;
end;

procedure TForwardSite.DeleteFields(Index: Integer);
begin
  FFieldss.Items[Index].Free;
  FFieldss.Delete(Index);
end;

function TForwardSite.GetAllFrequencies: Double1D;
var
  I: Integer;
begin
  SetLength(Result, Self.FieldsCount);
  for I := 0 to Self.FieldsCount - 1 do
  begin
    Result[I] := Self.Fields[I].Frequency;
  end;
end;

{ TForwardSite }

procedure TForwardSite.GetFEH(out fre: Double1D; out Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2, Hz2: TComplex1D);
var
  I, Count: Integer;
begin
  Count := 0;
  for I := 0 to Self.FieldsCount - 1 do
  begin
    Inc(Count);
    SetLength(fre, Count);
    SetLength(Ex1, Count);
    SetLength(Ey1, Count);
    SetLength(Hx1, Count);
    SetLength(Hy1, Count);
    SetLength(Hz1, Count);
    SetLength(Ex2, Count);
    SetLength(Ey2, Count);
    SetLength(Hx2, Count);
    SetLength(Hy2, Count);
    SetLength(Hz2, Count);
    fre[Count - 1] := Self.Fields[I].Frequency;
    Ex1[Count - 1] := Self.Fields[I].Ex1;
    Ey1[Count - 1] := Self.Fields[I].Ey1;
    Hx1[Count - 1] := Self.Fields[I].Hx1;
    Hy1[Count - 1] := Self.Fields[I].Hy1;
    Hz1[Count - 1] := Self.Fields[I].Hz1;
    Ex2[Count - 1] := Self.Fields[I].Ex2;
    Ey2[Count - 1] := Self.Fields[I].Ey2;
    Hx2[Count - 1] := Self.Fields[I].Hx2;
    Hy2[Count - 1] := Self.Fields[I].Hy2;
    Hz2[Count - 1] := Self.Fields[I].Hz2;
  end;
end;

{ TForwardSite }

procedure TForwardSite.GetFEH1(out fre: Double1D; out Ex1, Ey1, Hx1, Hy1, Hz1, Ex2, Ey2, Hx2, Hy2, Hz2: TComplex1D);
var
  I, Count: Integer;
begin
  Count := 0;
  for I := 0 to Self.FieldsCount - 1 do
  begin
    Inc(Count);
    SetLength(fre, Count);
    SetLength(Ex1, Count);
    SetLength(Ey1, Count);
    SetLength(Hx1, Count);
    SetLength(Hy1, Count);
    SetLength(Hz1, Count);
    SetLength(Ex2, Count);
    SetLength(Ey2, Count);
    SetLength(Hx2, Count);
    SetLength(Hy2, Count);
    SetLength(Hz2, Count);
    fre[Count - 1] := Self.Fields[I].Frequency;
    Ex1[Count - 1] := Self.Fields[I].Ex1 * 1E6;
    Ey1[Count - 1] := Self.Fields[I].Ey1 * 1E6;
    Hx1[Count - 1] := Self.Fields[I].Hx1 * 4E2 * System.Pi;
    Hy1[Count - 1] := Self.Fields[I].Hy1 * 4E2 * System.Pi;
    Hz1[Count - 1] := Self.Fields[I].Hz1 * 4E2 * System.Pi;
    Ex2[Count - 1] := Self.Fields[I].Ex2 * 1E6;
    Ey2[Count - 1] := Self.Fields[I].Ey2 * 1E6;
    Hx2[Count - 1] := Self.Fields[I].Hx2 * 4E2 * System.Pi;
    Hy2[Count - 1] := Self.Fields[I].Hy2 * 4E2 * System.Pi;
    Hz2[Count - 1] := Self.Fields[I].Hz2 * 4E2 * System.Pi;
  end;
end;

function TForwardSite.GetFrequencies: Double1D;
var
  I, Count: Integer;
begin
  Count := 0;
  for I := 0 to Self.FieldsCount - 1 do
  begin
    Inc(Count);
    SetLength(Result, Count);
    Result[Count - 1] := Self.Fields[I].Frequency;
  end;
end;

procedure TForwardSite.GetImpendence(out fre: Double1D; out zxx, zxy, zyx, zyy: TComplex1D);
var
  I, Count: Integer;
begin
  Count := 0;
  for I := 0 to Self.FieldsCount - 1 do
  begin
    Inc(Count);
    SetLength(fre, Count);
    SetLength(zxx, Count);
    SetLength(zxy, Count);
    SetLength(zyx, Count);
    SetLength(zyy, Count);
    fre[Count - 1] := Self.Fields[I].Frequency;
    zxx[Count - 1] := Self.Fields[I].zxx;
    zxy[Count - 1] := Self.Fields[I].zxy;
    zyx[Count - 1] := Self.Fields[I].zyx;
    zyy[Count - 1] := Self.Fields[I].zyy;
  end;
end;

procedure TForwardSite.Interpolation(percount: Integer);
var
  fcount, I, J, K: Integer;
  scaleA, scaleB: Double;
  tmpF1, tmpF2, tmpNewF: TForwardFreqencyFields;
  tmpFs: TList<TForwardFreqencyFields>;
begin
  tmpFs := TList<TForwardFreqencyFields>.Create;
  for I := 0 to Self.FieldsCount - 1 do
  begin
  end;
  for I := 0 to tmpFs.Count - 2 do
  begin
    for J := I + 1 to tmpFs.Count - 1 do
    begin
      if tmpFs[I].Frequency < tmpFs[J].Frequency then
      begin
        tmpFs.Exchange(I, J);
      end;
    end;
  end;

  for I := 0 to tmpFs.Count - 2 do
  begin
    tmpF1 := tmpFs[I];
    tmpF2 := tmpFs[I + 1];
    for J := 0 to percount - 1 do
    begin
      tmpNewF := TForwardFreqencyFields.Create;
      scaleA := (J + 1) / (percount + 1);
      scaleB := 1 - scaleA;
      tmpNewF.Frequency := power(10, scaleA * log10(tmpF1.Frequency) + scaleB * log10(tmpF2.Frequency));
      tmpNewF.Ex1 := scaleA * tmpF1.Ex1 + scaleB * tmpF2.Ex1;
      tmpNewF.Ey1 := scaleA * tmpF1.Ey1 + scaleB * tmpF2.Ey1;
      tmpNewF.Hx1 := scaleA * tmpF1.Hx1 + scaleB * tmpF2.Hx1;
      tmpNewF.Hy1 := scaleA * tmpF1.Hy1 + scaleB * tmpF2.Hy1;
      tmpNewF.Hz1 := scaleA * tmpF1.Hz1 + scaleB * tmpF2.Hz1;
      tmpNewF.Ex2 := scaleA * tmpF1.Ex2 + scaleB * tmpF2.Ex2;
      tmpNewF.Ey2 := scaleA * tmpF1.Ey2 + scaleB * tmpF2.Ey2;
      tmpNewF.Hx2 := scaleA * tmpF1.Hx2 + scaleB * tmpF2.Hx2;
      tmpNewF.Hy2 := scaleA * tmpF1.Hy2 + scaleB * tmpF2.Hy2;
      tmpNewF.Hz2 := scaleA * tmpF1.Hz2 + scaleB * tmpF2.Hz2;

      tmpNewF.zxx := scaleA * tmpF1.zxx + scaleB * tmpF2.zxx;
      tmpNewF.zxy := scaleA * tmpF1.zxy + scaleB * tmpF2.zxy;
      tmpNewF.zyx := scaleA * tmpF1.zyx + scaleB * tmpF2.zyx;
      tmpNewF.zyy := scaleA * tmpF1.zyy + scaleB * tmpF2.zyy;
      tmpNewF.Tzx := scaleA * tmpF1.Tzx + scaleB * tmpF2.Tzx;
      tmpNewF.Tzy := scaleA * tmpF1.Tzy + scaleB * tmpF2.Tzy;

      Self.AddFields(tmpNewF);
    end;
  end;
  tmpFs.Free;
end;

procedure TForwardSite.NegativeHarmonicFactor;
var
  I: Integer;
begin
  for I := 0 to Self.FieldsCount - 1 do
  begin
    Self.Fields[I].Ex1 := Self.Fields[I].Ex1.Conjugate;
    Self.Fields[I].Ey1 := Self.Fields[I].Ey1.Conjugate;
    Self.Fields[I].Hx1 := Self.Fields[I].Hx1.Conjugate;
    Self.Fields[I].Hy1 := Self.Fields[I].Hy1.Conjugate;
    Self.Fields[I].Hz1 := Self.Fields[I].Hz1.Conjugate;
    Self.Fields[I].Ex2 := Self.Fields[I].Ex2.Conjugate;
    Self.Fields[I].Ey2 := Self.Fields[I].Ey2.Conjugate;
    Self.Fields[I].Hx2 := Self.Fields[I].Hx2.Conjugate;
    Self.Fields[I].Hy2 := Self.Fields[I].Hy2.Conjugate;
    Self.Fields[I].Hz2 := Self.Fields[I].Hz2.Conjugate;
    Self.Fields[I].zxx := Self.Fields[I].zxx.Conjugate;
    Self.Fields[I].zxy := Self.Fields[I].zxy.Conjugate;
    Self.Fields[I].zyx := Self.Fields[I].zyx.Conjugate;
    Self.Fields[I].zyy := Self.Fields[I].zyy.Conjugate;
    Self.Fields[I].Tzx := Self.Fields[I].Tzx.Conjugate;
    Self.Fields[I].Tzy := Self.Fields[I].Tzy.Conjugate;
  end;
end;

procedure TForwardSite.UpdateNatureMagneticAmplitude(escale, hscale: Double1D);
var
  I, Index: Integer;
begin
  index := 0;
  for I := 0 to Self.FieldsCount - 1 do
  begin
    Self.Fields[I].Ex1 := Self.Fields[I].Ex1 * escale[index];
    Self.Fields[I].Ex2 := Self.Fields[I].Ex2 * hscale[index];
    Self.Fields[I].Ey1 := Self.Fields[I].Ey1 * escale[index];
    Self.Fields[I].Ey2 := Self.Fields[I].Ey2 * hscale[index];
    Self.Fields[I].Hx1 := Self.Fields[I].Hx1 * escale[index];
    Self.Fields[I].Hx2 := Self.Fields[I].Hx2 * hscale[index];
    Self.Fields[I].Hy1 := Self.Fields[I].Hy1 * escale[index];
    Self.Fields[I].Hy2 := Self.Fields[I].Hy2 * hscale[index];
    Self.Fields[I].Hz1 := Self.Fields[I].Hz1 * escale[index];
    Self.Fields[I].Hz2 := Self.Fields[I].Hz2 * hscale[index];
    Inc(index);
  end;
end;

{ Fields }
procedure TForwardFreqencyFields.CopyTo(target: TForwardFreqencyFields);
begin
  target.FFrequency := Self.FFrequency;
  target.FZxx := Self.FZxx;
  target.FZxy := Self.FZxy;
  target.FZyx := Self.FZyx;
  target.FZyy := Self.FZyy;
  target.FTzx := Self.FTzx;
  target.FTzy := Self.FTzy;
  target.FEx1 := Self.FEx1;
  target.FEy1 := Self.FEy1;
  target.FHx1 := Self.FHx1;
  target.FHy1 := Self.FHy1;
  target.FHz1 := Self.FHz1;
  target.FEx2 := Self.FEx2;
  target.FEy2 := Self.FEy2;
  target.FHx2 := Self.FHx2;
  target.FHy2 := Self.FHy2;
  target.FHz2 := Self.FHz2;
end;

constructor TForwardFreqencyFields.Create;
begin
end;

destructor TForwardFreqencyFields.Destroy;
begin
  inherited;
end;

function TForwardFreqencyFields.LoadModEMMTRespond(const FName: string): TList<TForwardSite>;
var
  I, J, K, scount, fcount: Int64;
  TXT: TextFile;
  str: string;
  strarr: TArray<string>;
  tmp: TForwardSite;
  ffield: TForwardFreqencyFields;
  fres: Double1D;
begin
  AssignFile(TXT, FName);
  Reset(TXT);
  Result := TList<TForwardSite>.Create;
  while not Eof(TXT) do
  begin
    ReadLn(TXT, str);
    if str.StartsWith('#') then
    begin
      Continue;
    end;
    if str.StartsWith('>  Full_Impedance') then
    begin
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
      fcount := strarr[1].ToInteger;
      scount := strarr[2].ToInteger;
      SetLength(fres, fcount);
      for J := 0 to scount - 1 do
      begin
        tmp := TForwardSite.Create;
        Result.Add(tmp);
        tmp.Name := 'ModEM' + J.ToString;
        for K := 0 to fcount - 1 do
        begin
          ReadLn(TXT, str); // [mv/Km]/[nT] ->    [V/m]/[A/m]
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield := tmp.AddNewFields;
          ffield.Frequency := 1 / strarr[0].ToDouble;
          ffield.zxx := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) / (1E6 / 4E2 / System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.zxy := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) / (1E6 / 4E2 / System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.zyx := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) / (1E6 / 4E2 / System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.zyy := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) / (1E6 / 4E2 / System.Pi);
        end;
        tmp.X := strarr[4].ToDouble;
        tmp.Y := strarr[5].ToDouble;
      end;
      Continue;
    end;
    if str.StartsWith('>  Full_Vertical_Components') then
    begin
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      for J := 0 to scount - 1 do
      begin
        tmp := Result[J];
        for K := 0 to fcount - 1 do
        begin
          ffield := tmp.Fields[K];
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Tzx := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Tzy := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble);
        end;
      end;
    end;
    if str.StartsWith('>  EM_Fields') then
    begin
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      ReadLn(TXT, str);
      for J := 0 to scount - 1 do
      begin
        tmp := Result[J];
        for K := 0 to fcount - 1 do
        begin
          ffield := tmp.Fields[K];
          ReadLn(TXT, str); // [mv/Km]/[nT] -> [V/m]/[A/m]
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Ex1 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * (4E2 * System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Ey1 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * (4E2 * System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Hx1 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * 1E9;
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Hy1 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * 1E9;
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Hz1 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * 1E9;
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Ex2 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * (4E2 * System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Ey2 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * (4E2 * System.Pi);
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Hx2 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * 1E9;
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Hy2 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * 1E9;
          ReadLn(TXT, str);
          strarr := str.Split([' '], TStringSplitOptions.ExcludeEmpty);
          ffield.Hz2 := TComplex.Create(strarr[8].ToDouble, strarr[9].ToDouble) * 1E9;
        end;
      end;
      Break;
    end;
  end;
  fres := nil;
  strarr := nil;
  CloseFile(TXT);
end;

procedure TForwardFreqencyFields.SetFrequency(const _Value: Double);
begin
  FFrequency := _Value;
end;

procedure TForwardFreqencyFields.SetZxx(const _Value: TComplex);
begin
  FZxx := _Value;
end;

procedure TForwardFreqencyFields.SetZxy(const _Value: TComplex);
begin
  FZxy := _Value;
end;

procedure TForwardFreqencyFields.SetZyx(const _Value: TComplex);
begin
  FZyx := _Value;
end;

procedure TForwardFreqencyFields.SetZyy(const _Value: TComplex);
begin
  FZyy := _Value;
end;

procedure TForwardFreqencyFields.SetTzx(const _Value: TComplex);
begin
  FTzx := _Value;
end;

procedure TForwardFreqencyFields.SetTzy(const _Value: TComplex);
begin
  FTzy := _Value;
end;

procedure TForwardFreqencyFields.SetEx1(const _Value: TComplex);
begin
  FEx1 := _Value;
end;

procedure TForwardFreqencyFields.SetEy1(const _Value: TComplex);
begin
  FEy1 := _Value;
end;

procedure TForwardFreqencyFields.SetHx1(const _Value: TComplex);
begin
  FHx1 := _Value;
end;

procedure TForwardFreqencyFields.SetHy1(const _Value: TComplex);
begin
  FHy1 := _Value;
end;

procedure TForwardFreqencyFields.SetHz1(const _Value: TComplex);
begin
  FHz1 := _Value;
end;

procedure TForwardFreqencyFields.SetEx2(const _Value: TComplex);
begin
  FEx2 := _Value;
end;

procedure TForwardFreqencyFields.SetEy2(const _Value: TComplex);
begin
  FEy2 := _Value;
end;

procedure TForwardFreqencyFields.SetHx2(const _Value: TComplex);
begin
  FHx2 := _Value;
end;

procedure TForwardFreqencyFields.SetHy2(const _Value: TComplex);
begin
  FHy2 := _Value;
end;

procedure TForwardFreqencyFields.SetHz2(const _Value: TComplex);
begin
  FHz2 := _Value;
end;

end.
