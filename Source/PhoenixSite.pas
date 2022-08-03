unit PhoenixSite;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Generics.Collections,
  Winapi.ShellAPI, Winapi.Windows,
  WTypes, Phoenix, PhoenixB;

type

  TPhoenixSite = class;
  TPhoenixSystemResponds = class;
  TPhoenixSystemRespond = class;

  TPhoenixSite = class(TObject)
  private
    FTBL: String;
    FCLB: String;
    FCLCs: TList<String>;
    FTSs: TList<String>;
    FName: String;
    FSystemResponds: TPhoenixSystemResponds;
    FSystemRespondsExsit: Boolean;
    procedure SetTBL(const _Value: String);
    procedure SetCLB(const _Value: String);
    procedure SetCLCs(const _Value: TList<String>);
    function GetCLC(Index: Integer): String;
    procedure SetCLC(Index: Integer; const _Value: String);
    procedure SetTSs(const _Value: TList<String>);
    function GetTS(Index: Integer): String;
    procedure SetTS(Index: Integer; const _Value: String);
    procedure SetName(const _Value: String);
    procedure SetSystemResponds(const _Value: TPhoenixSystemResponds);
    procedure SetSystemRespondsExsit(const Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCLC(_Value: String);
    function AddNewCLC: String;
    procedure CLCClear;
    function CLCCount: Integer;
    procedure RemoveCLC(_Value: String);
    procedure DeleteCLC(Index: Integer);
    procedure AddTS(_Value: String);
    function AddNewTS: String;
    procedure TSClear;
    function TSCount: Integer;
    procedure RemoveTS(_Value: String);
    procedure DeleteTS(Index: Integer);
    function AddSystemResponds: TPhoenixSystemResponds;
    procedure GetSystemRespond(var freList: Double1D; var responds: TComplex2D; const ExIndex: Integer = 0;
      const EyIndex: Integer = 1; const HxIndex: Integer = 2; const HyIndex: Integer = 3; const HzIndex: Integer = 4);
    procedure LoadFromTBL(tmptbl: TTBL; clbPath, clcPath: string);
    procedure Set2OctaveFrequencyBand;
    procedure Set4OctaveFrequencyBand;
    procedure SetProcessFrequency(const fre: Double1D);
    procedure SetProcessFrequencyBand(const fre: Double1D; const band: Integer1D);
    procedure SysCal(handle: THandle; tmpdir, SysCalFileName: string);
    procedure SystemRespondsRemove;
    property TBL: String read FTBL write SetTBL;
    property CLB: String read FCLB write SetCLB;
    property CLCs: TList<String> read FCLCs write SetCLCs;
    property CLC[Index: Integer]: String read GetCLC write SetCLC;
    property TSs: TList<String> read FTSs write SetTSs;
    property TS[Index: Integer]: String read GetTS write SetTS;
    property Name: String read FName write SetName;
    property SystemResponds: TPhoenixSystemResponds read FSystemResponds write SetSystemResponds;
    property SystemRespondsExsit: Boolean read FSystemRespondsExsit write SetSystemRespondsExsit;
  end;

  TPhoenixSystemResponds = class(TObject)
  private
    FSystemResponds: TList<TPhoenixSystemRespond>;
    FisBox: Boolean;
    procedure SetSystemResponds(const _Value: TList<TPhoenixSystemRespond>);
    function GetSystemRespond(Index: Integer): TPhoenixSystemRespond;
    procedure SetSystemRespond(Index: Integer; const _Value: TPhoenixSystemRespond);
    procedure SetisBox(const _Value: Boolean);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSystemRespond(_Value: TPhoenixSystemRespond);
    function AddNewSystemRespond: TPhoenixSystemRespond;
    procedure SystemRespondClear;
    function SystemRespondCount: Integer;
    procedure RemoveSystemRespond(_Value: TPhoenixSystemRespond);
    procedure DeleteSystemRespond(Index: Integer);
    procedure ReadCTS(const ctsfilename: string = '.\temp.CTS');
    procedure SavePFC(const pfcfilename: string = '.\temp.PFC');
    property SystemResponds: TList<TPhoenixSystemRespond> read FSystemResponds write SetSystemResponds;
    property SystemRespond[Index: Integer]: TPhoenixSystemRespond read GetSystemRespond write SetSystemRespond;
    property isBox: Boolean read FisBox write SetisBox;
  end;

  TPhoenixSystemRespond = class(TObject)
  private
    FFrequency: Double;
    FBand: Integer;
    FResponds: TComplex1D;
    procedure SetFrequency(const _Value: Double);
    procedure SetBand(const _Value: Integer);
    procedure SetResponds(const _Value: TComplex1D);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    property Frequency: Double read FFrequency write SetFrequency;
    property band: Integer read FBand write SetBand;
    property responds: TComplex1D read FResponds write SetResponds;
  end;

procedure RunSysCal(handle: THandle; syscalfile, pfcfile, tblfile, clbPath, clcPath: string; var ctsfile: String);
//
// var
// SysCalFileName: string = 'C:\EMT-SW\SYSCAL\SysCal_V7.exe';

implementation

procedure RunSysCal(handle: THandle; syscalfile, pfcfile, tblfile, clbPath, clcPath: string; var ctsfile: String);
var
  parameters: String;
begin
  parameters := pfcfile + ' ' + tblfile + ' ' + clbPath + ' ' + clcPath + ' ' + ctsfile;

  if ShellExecute(handle, 'open', PChar(syscalfile), PChar(parameters), nil, 0) > -1 then
  begin
    Exit;
  end;
end;

{ Site }
constructor TPhoenixSite.Create;
begin
  FCLCs := TList<String>.Create;
  FTSs := TList<String>.Create;
end;

destructor TPhoenixSite.Destroy;
begin
  FCLCs.Free;
  FTSs.Free;
  if FSystemRespondsExsit then
    FSystemResponds.Free;
  inherited;
end;

procedure TPhoenixSite.SetTBL(const _Value: String);
begin
  FTBL := _Value;
end;

procedure TPhoenixSite.SetCLB(const _Value: String);
begin
  FCLB := _Value;
end;

procedure TPhoenixSite.SetCLCs(const _Value: TList<String>);
begin
  FCLCs.Clear;
  FCLCs := _Value;
end;

function TPhoenixSite.GetCLC(Index: Integer): String;
begin
  Result := FCLCs[Index];
end;

procedure TPhoenixSite.SetCLC(Index: Integer; const _Value: String);
begin
  FCLCs[Index] := _Value;
end;

procedure TPhoenixSite.AddCLC(_Value: String);
begin;
  FCLCs.Add(_Value);
end;

function TPhoenixSite.AddNewCLC: String;
var
  CLCtmp: String;
begin;
  FCLCs.Add(CLCtmp);
  Result := CLCtmp;
end;

procedure TPhoenixSite.CLCClear;
begin
  FCLCs.Clear;
end;

function TPhoenixSite.CLCCount: Integer;
begin
  Result := FCLCs.Count;
end;

procedure TPhoenixSite.RemoveCLC(_Value: String);
begin
  FCLCs.Remove(_Value);
end;

procedure TPhoenixSite.DeleteCLC(Index: Integer);
begin
  FCLCs.Delete(Index);
end;

procedure TPhoenixSite.SetTSs(const _Value: TList<String>);
begin
  FTSs.Clear;
  FTSs := _Value;
end;

function TPhoenixSite.GetTS(Index: Integer): String;
begin
  Result := FTSs[Index];
end;

procedure TPhoenixSite.SetTS(Index: Integer; const _Value: String);
begin
  FTSs[Index] := _Value;
end;

procedure TPhoenixSite.AddTS(_Value: String);
begin;
  FTSs.Add(_Value);
end;

function TPhoenixSite.AddNewTS: String;
var
  TStmp: String;
begin;
  FTSs.Add(TStmp);
  Result := TStmp;
end;

procedure TPhoenixSite.TSClear;
begin
  FTSs.Clear;
end;

function TPhoenixSite.TSCount: Integer;
begin
  Result := FTSs.Count;
end;

procedure TPhoenixSite.RemoveTS(_Value: String);
begin
  FTSs.Remove(_Value);
end;

procedure TPhoenixSite.DeleteTS(Index: Integer);
begin
  FTSs.Delete(Index);
end;

procedure TPhoenixSite.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TPhoenixSite.SetSystemResponds(const _Value: TPhoenixSystemResponds);
begin
  if FSystemRespondsExsit then
    FSystemResponds.Free;
  FSystemRespondsExsit := True;
  FSystemResponds := _Value;
end;

procedure TPhoenixSite.SetSystemRespondsExsit(const Value: Boolean);
begin
  FSystemRespondsExsit := Value;
end;

function TPhoenixSite.AddSystemResponds: TPhoenixSystemResponds;
begin;
  if not FSystemRespondsExsit then
    FSystemResponds := TPhoenixSystemResponds.Create;
  Result := FSystemResponds;
  FSystemRespondsExsit := True;
end;

{ Site }

{ TPhoenixSite }

procedure TPhoenixSite.GetSystemRespond(var freList: Double1D; var responds: TComplex2D; const ExIndex: Integer = 0;
  const EyIndex: Integer = 1; const HxIndex: Integer = 2; const HyIndex: Integer = 3; const HzIndex: Integer = 4);
var
  J, fcount, ccount: Integer;
begin
  fcount := Self.SystemResponds.SystemRespondCount;
  SetLength(freList, fcount);
  if fcount <= 0 then
    Exit;
  // ccount := Length(Self.SystemResponds.SystemRespond[0].responds);
  ccount := 5;
  SetLength(responds, ccount);
  SetLength(responds[0], fcount);
  SetLength(responds[1], fcount);
  SetLength(responds[2], fcount);
  SetLength(responds[3], fcount);
  SetLength(responds[4], fcount);
  for J := 0 to fcount - 1 do
  begin
    // 频率域 每格值（Unit）对应的系统响应（V/m  T）
    responds[0][J] := 1.0 / (Self.SystemResponds.SystemRespond[J].responds[ExIndex] * 8388608);
    responds[1][J] := 1.0 / (Self.SystemResponds.SystemRespond[J].responds[EyIndex] * 8388608);
    responds[2][J] := 1.0 / (Self.SystemResponds.SystemRespond[J].responds[HxIndex] * 8388608);
    responds[3][J] := 1.0 / (Self.SystemResponds.SystemRespond[J].responds[HyIndex] * 8388608);
    responds[4][J] := 1.0 / (Self.SystemResponds.SystemRespond[J].responds[HzIndex] * 8388608);
  end;
  for J := 0 to fcount - 1 do
  begin
    freList[J] := Self.SystemResponds.SystemRespond[J].Frequency;
  end;
end;

procedure TPhoenixSite.LoadFromTBL(tmptbl: TTBL; clbPath, clcPath: string);
var
  J: Integer;
begin
  Name := tmptbl.Name;
  Self.TBL := tmptbl.Path + tmptbl.Name + '.tbl';
  Self.CLB := clbPath + '\' + tmptbl.SNUM + '.CLB';
  Self.AddNewCLC;
  Self.AddNewCLC;
  Self.AddNewCLC;
  Self.CLC[0] := clcPath + '\' + tmptbl.HXSN + '.CLC';
  Self.CLC[1] := clcPath + '\' + tmptbl.HYSN + '.CLC';
  Self.CLC[2] := clcPath + '\' + tmptbl.HZSN + '.CLC';
  for J := 2 to 5 do
  begin
    if FileExists(tmptbl.Path + tmptbl.TSFileName + '.TS' + J.ToString) then
    begin
      Self.AddTS(tmptbl.Path + tmptbl.TSFileName + '.TS' + J.ToString);
    end;
  end;
end;

procedure TPhoenixSite.Set2OctaveFrequencyBand;
var
  fre: Double1D;
  band: Integer1D;
begin
  SetLength(fre, 50);
  SetLength(band, 50);
  Move(Phoenix_MTUA_MTC50_MT_Octave2[0], fre[0], 50 * 8);
  Move(Phoenix_MTUA_MTC50_MT_Octave2File[0], band[0], 50 * 4);
  SetProcessFrequencyBand(fre, band);
  SetLength(fre, 0);
  SetLength(band, 0);
end;

procedure TPhoenixSite.Set4OctaveFrequencyBand;
var
  fre: Double1D;
  band: Integer1D;
begin
  SetLength(fre, 100);
  SetLength(band, 100);
  Move(Phoenix_MTUA_MTC50_MT_Octave4[0], fre[0], 100 * 8);
  Move(Phoenix_MTUA_MTC50_MT_Octave4File[0], band[0], 100 * 4);
  SetProcessFrequencyBand(fre, band);
  SetLength(fre, 0);
  SetLength(band, 0);
end;

procedure TPhoenixSite.SetProcessFrequency(const fre: Double1D);
var
  I: Integer;
  tmpRes: TPhoenixSystemRespond;
begin
  Self.AddSystemResponds;
  Self.SystemResponds.SystemRespondClear;
  for I := 0 to Length(fre) - 1 do
  begin
    tmpRes := Self.SystemResponds.AddNewSystemRespond;
    tmpRes.Frequency := fre[I];
    if fre[I] > 800 then
    begin
      tmpRes.band := 2;
    end
    else if fre[I] > 6 then
    begin
      tmpRes.band := 3;
    end
    else if fre[I] > 0.4 then
    begin
      tmpRes.band := 4;
    end
    else
    begin
      tmpRes.band := 5;
    end;
  end;
end;

procedure TPhoenixSite.SetProcessFrequencyBand(const fre: Double1D; const band: Integer1D);
var
  I: Integer;
  tmpRes: TPhoenixSystemRespond;
begin
  Self.AddSystemResponds;
  Self.SystemResponds.SystemRespondClear;
  for I := 0 to Length(fre) - 1 do
  begin
    tmpRes := Self.SystemResponds.AddNewSystemRespond;
    tmpRes.Frequency := fre[I];
    tmpRes.band := band[I];
  end;
end;

procedure TPhoenixSite.SysCal(handle: THandle; tmpdir, SysCalFileName: string);
var
  pfcfile, ctsfile, clbPath, clcPath, sysfile, tmpguid: string;
  Index: Integer;
begin
  tmpguid := GUIDStr + '\';
  tmpguid := '';
  System.SysUtils.ForceDirectories(tmpdir + tmpguid);
  clbPath := ExtractFilePath(Self.CLB) + '*.CLB';
  clcPath := ExtractFilePath(Self.CLC[0]) + '*.CLC';
  pfcfile := tmpdir + tmpguid + Self.Name + '.PFC';
  ctsfile := tmpdir + tmpguid + Self.Name + '.CTS';
  Self.SystemResponds.SavePFC(pfcfile);
  RunSysCal(handle, SysCalFileName, pfcfile, Self.TBL, clbPath, clcPath, ctsfile);
  Index := 0;
  while not FileExists(ctsfile) do
  begin
    Sleep(500);
    Inc(Index);
    if Index > 5 then
    begin
      raise Exception.Create('Syscal Error');
      Break;
    end;
  end;
  Self.SystemResponds.ReadCTS(ctsfile);
end;

procedure TPhoenixSite.SystemRespondsRemove;
begin
  if FSystemRespondsExsit then
  begin
    FSystemResponds.Free;
    FSystemRespondsExsit := False;
  end;
end;

{ SystemResponds }
constructor TPhoenixSystemResponds.Create;
begin
  FSystemResponds := TList<TPhoenixSystemRespond>.Create;
end;

destructor TPhoenixSystemResponds.Destroy;
begin
  SystemRespondClear;
  FSystemResponds.Free;
  inherited;
end;

procedure TPhoenixSystemResponds.SetSystemResponds(const _Value: TList<TPhoenixSystemRespond>);
begin
  SystemRespondClear;
  FSystemResponds := _Value;
end;

function TPhoenixSystemResponds.GetSystemRespond(Index: Integer): TPhoenixSystemRespond;
begin
  Result := FSystemResponds[Index];
end;

procedure TPhoenixSystemResponds.SetSystemRespond(Index: Integer; const _Value: TPhoenixSystemRespond);
begin
  FSystemResponds[Index].Free;
  FSystemResponds[Index] := _Value;
end;

procedure TPhoenixSystemResponds.AddSystemRespond(_Value: TPhoenixSystemRespond);
begin;
  FSystemResponds.Add(_Value);
end;

function TPhoenixSystemResponds.AddNewSystemRespond: TPhoenixSystemRespond;
var
  SystemRespondtmp: TPhoenixSystemRespond;
begin;
  SystemRespondtmp := TPhoenixSystemRespond.Create;
  FSystemResponds.Add(SystemRespondtmp);
  Result := SystemRespondtmp;
end;

procedure TPhoenixSystemResponds.SystemRespondClear;
begin
  while FSystemResponds.Count > 0 do
  begin
    FSystemResponds.Items[0].Free;
    FSystemResponds.Delete(0);
  end;
end;

function TPhoenixSystemResponds.SystemRespondCount: Integer;
begin
  Result := FSystemResponds.Count;
end;

procedure TPhoenixSystemResponds.RemoveSystemRespond(_Value: TPhoenixSystemRespond);
begin
  FSystemResponds.Remove(_Value);
  _Value.Free;
end;

procedure TPhoenixSystemResponds.DeleteSystemRespond(Index: Integer);
begin
  FSystemResponds.Items[Index].Free;
  FSystemResponds.Delete(Index);
end;

{ TPhoenixSystemResponds }

procedure TPhoenixSystemResponds.ReadCTS(const ctsfilename: string);
var
  strs: TStrings;
  tmp: String1D;
  I, J, channels: Integer;
  cmplx1D: TComplex1D;
begin
  if not FileExists(ctsfilename) then
  begin
    raise Exception.Create('CTS File Not Exist! ' + ctsfilename);
  end;
  strs := TStringList.Create;
  strs.LoadFromFile(ctsfilename);
  tmp := strs[1].Split([' ', ','], TStringSplitOptions.ExcludeEmpty);
  channels := (Length(tmp) - 2) div 2;
  for I := 1 to strs.Count - 1 do
  begin
    tmp := strs[I].Split([' ', ','], TStringSplitOptions.ExcludeEmpty);
    SetLength(cmplx1D, channels);
    for J := 0 to channels - 1 do
    begin
      cmplx1D[J] := TComplex.Create(tmp[J * 2 + 2].ToDouble, tmp[J * 2 + 3].ToDouble);
    end;
    Self.SystemRespond[I - 1].responds := cmplx1D;
  end;
  FreeAndNil(strs);
end;

procedure TPhoenixSystemResponds.SavePFC(const pfcfilename: string);
var
  strs: TStrings;
  I: Integer;
begin
  strs := TStringList.Create;
  if Self.isBox then
  begin
    strs.Add('FldType,     0,	    SYSTEM CAL MTU-A (AMT mode) Frequency set for 50 or 60Hz area. ');
  end
  else
  begin
    strs.Add('FldType,     1,	    SYSTEM CAL MTU-A (AMT mode) Frequency set for 50 or 60Hz area. ');
  end;
  for I := 0 to Self.SystemRespondCount - 1 do
  begin
    strs.Add('Frequency,   ' + Self.SystemRespond[I].band.ToString + ', ' + Self.SystemRespond[I].Frequency.ToString);
  end;
  strs.SaveToFile(pfcfilename);
  FreeAndNil(strs);
end;

procedure TPhoenixSystemResponds.SetisBox(const _Value: Boolean);
begin
  FisBox := _Value;
end;

{ SystemRespond }
constructor TPhoenixSystemRespond.Create;
begin
end;

destructor TPhoenixSystemRespond.Destroy;
begin
  inherited;
end;

procedure TPhoenixSystemRespond.SetFrequency(const _Value: Double);
begin
  FFrequency := _Value;
end;

procedure TPhoenixSystemRespond.SetBand(const _Value: Integer);
begin
  FBand := _Value;
end;

procedure TPhoenixSystemRespond.SetResponds(const _Value: TComplex1D);
begin
  FResponds := _Value;
end;

end.
