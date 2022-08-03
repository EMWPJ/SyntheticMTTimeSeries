unit SyntheticTimeSeriesForm;



interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.TabControl, FMX.Platform.Win,
  ForwardSite,
  FMX.ListBox, FMX.NumberBox, FMX.Edit, FMX.EditBox, FMX.SpinBox, FMX.Layouts, FMX.Controls.Presentation,
  FMX.DateTimeCtrls, Phoenix, PhoenixB, FMX.Menus, SyntheticTimeSeries, WTypes, PhoenixSite, TimeSeries, FreToTime;

type
  TFormSyntheticTimeseries = class(TForm)
    PanelSite: TPanel;
    ListBoxSites: TListBox;
    PanelRotate: TPanel;
    LabelRotate: TLabel;
    SpinBoxRotate: TSpinBox;
    PanelSegmentLength: TPanel;
    LabelSegmentLength: TLabel;
    NumberBoxSegmentLength: TNumberBox;
    PanelSourceScale: TPanel;
    LabelSourceScale: TLabel;
    NumberBoxSourceScale: TNumberBox;
    Panel2: TPanel;
    LabelSyntheticFunction: TLabel;
    ComboBoxSyntheticFunction: TComboBox;
    PanelTBLEdit: TPanel;
    TabControlTBLSite: TTabControl;
    TBLTab: TTabItem;
    PaneltblControl: TPanel;
    CheckBoxPhoneixTimeseries: TCheckBox;
    ButtonGenerate: TButton;
    Splitter4: TSplitter;
    GridPanelLayoutParameter: TGridPanelLayout;
    LabelSiteName: TLabel;
    EditSiteName: TEdit;
    LabelMTAMT: TLabel;
    SwitchMTAMT: TSwitch;
    LabelBox: TLabel;
    EditBox: TEdit;
    LabelHx: TLabel;
    EditHx: TEdit;
    LabelHy: TLabel;
    EditHy: TEdit;
    LabelHz: TLabel;
    EditHz: TEdit;
    LabelEXLength: TLabel;
    EditExLength: TEdit;
    LabelEyLength: TLabel;
    EditEyLength: TEdit;
    LabelInterval: TLabel;
    EditInterval: TEdit;
    LabelL2Records: TLabel;
    EditL2Records: TEdit;
    LabelL3Records: TLabel;
    EditL3Records: TEdit;
    LabelL4Records: TLabel;
    EditL4Records: TEdit;
    LabelBeginEnd: TLabel;
    GridBeginEnd: TGridPanelLayout;
    DateEditBegin: TDateEdit;
    DateEditEnd: TDateEdit;
    TimeEditBegin: TTimeEdit;
    TimeEditEnd: TTimeEdit;
    LabelBegin: TLabel;
    LabelEnd: TLabel;
    LabelSites: TLabel;
    PopupMenuSites: TPopupMenu;
    MenuLoadModEMSites: TMenuItem;
    MenuClearSites: TMenuItem;
    OpenDialogModem: TOpenDialog;
    procedure FormDestroy(Sender: TObject);
    procedure ButtonGenerateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuClearSitesClick(Sender: TObject);
    procedure MenuLoadModEMSitesClick(Sender: TObject);
  private
    FTBL: TTBL;
    FSites: TObjectList<TForwardSite>;
    FSTS: TSyntheticTimeSeries;
    FScaleE, FScaleB: Double1D;
    FResponds: TComplex2D;
    FSyntheticPeriods, FScale: Double;
    FRot: Double;
    FFre: Double1D;
    FCount: Integer;
    procedure UpdateTBL;
    procedure UpdateSiteList;
    procedure UpdateSTS;
  public
  end;

var
  FormSyntheticTimeseries: TFormSyntheticTimeseries;

implementation

{$R *.fmx}

procedure TFormSyntheticTimeseries.FormDestroy(Sender: TObject);
begin
  FSTS.DisposeOf;
  FTBL.DisposeOf;
  FSites.DisposeOf;
end;

procedure TFormSyntheticTimeseries.ButtonGenerateClick(Sender: TObject);
var
  I: Integer;
  tmpsite: TForwardSite;
  stimeseries: TSiteTimeSeries;
  dir: String;
begin
  if FSites.Count = 0 then
  begin
    Exit;
  end;
  if SelectDirectory('Save to', '', dir) then
  begin
    dir := dir + '\';
    UpdateTBL;
    UpdateSTS;
    tmpsite := TForwardSite.Create;
    for I := 0 to FSites.Count - 1 do
    begin
      if ListBoxSites.ListItems[I].IsChecked then
      begin
        FSites[I].CopyTo(tmpsite);
        tmpsite.UpdateNatureMagneticAmplitude(FScaleE, FScaleB);
        tmpsite.AddCalibration(FResponds);
        stimeseries := FSTS.SyntheticSites(tmpsite, FRot);
        if CheckBoxPhoneixTimeseries.IsChecked then
        begin
          stimeseries.SavePhoenix(dir, FTBL, tmpsite.Name);
        end
        else
        begin
          stimeseries.SaveGMTTS(dir, tmpsite.Name, true);
        end;
        stimeseries.Free;
      end;
    end;
    tmpsite.DisposeOf;
  end;
end;

procedure TFormSyntheticTimeseries.FormCreate(Sender: TObject);
var
  sf: SyntheticFunction;
begin
  FSTS := TSyntheticTimeSeries.Create;
  FTBL := TTBL.Create;
  FTBL.LoadTBL('.\Config\TEST0000.TBL');
  FSites := TObjectList<TForwardSite>.Create;
  ComboBoxSyntheticFunction.Clear;
  for sf := Low(SyntheticFunction) to High(SyntheticFunction) do
  begin
    ComboBoxSyntheticFunction.Items.Add(SyntheticFunctionName[sf]);
  end;
  ComboBoxSyntheticFunction.ItemIndex := 5;
  TimeEditBegin.Time := 0;
end;

procedure TFormSyntheticTimeseries.MenuClearSitesClick(Sender: TObject);
begin
  FSites.Clear;
  UpdateSiteList;
end;

procedure TFormSyntheticTimeseries.MenuLoadModEMSitesClick(Sender: TObject);
begin
  if OpenDialogModem.Execute then
  begin
    FSites.DisposeOf;
    FSites := LoadModEMMTRespond(OpenDialogModem.FileName);
    UpdateSiteList;
  end;
end;

procedure TFormSyntheticTimeseries.UpdateSiteList;
var
  I: Integer;
  sitenames: TStringList;
begin
  ListBoxSites.Clear;
  sitenames := TStringList.Create;
  for I := 0 to FSites.Count - 1 do
  begin
    sitenames.Add(FSites.Items[I].Name + ' X=' + FSites.Items[I].X.ToString + ' Y=' + FSites.Items[I].Y.ToString);
  end;
  ListBoxSites.Items := sitenames;
end;

procedure TFormSyntheticTimeseries.UpdateSTS;
var
  ts2, ts3, ts4, ts5: TSyntheticSchema;
  I, J: Integer;
  psite: TPhoenixSite;
  tmpts: TSiteTimeSeries;
  respondsite: TForwardSite;
  tmpsts: TSyntheticTimeSeries;
begin
  FRot := SpinBoxRotate.Value * System.Pi / 180;
  FSyntheticPeriods := NumberBoxSegmentLength.Value;
  FScale := NumberBoxSourceScale.Value;

  FSTS.BeginTime := FTBL.FTIM;
  FSTS.EndTime := FTBL.LTIM;
  FSTS.SchemaClear;
  ts3 := FSTS.AddNewSchema;
  ts3.Name := 'TS3';
  ts3.Continous := false;
  ts3.SyntheticPeriods := FSyntheticPeriods;
  ts3.SourceScale := FScale;
  ts3.SampleRate := 2400;
  ts3.MaxFrequency := SyntheticTS3FreMax;
  ts3.MinFrequency := SyntheticTS3FreMin;
  ts3.Intervals := FTBL.HSMP * 2;
  ts3.Offset := FTBL.HSMP;
  ts3.SamplingTime := FTBL.L3NS;
  ts3.SyntheticFunciton := ComboBoxSyntheticFunction.ItemIndex;
  ts4 := FSTS.AddNewSchema;
  ts4.Name := 'TS4';
  ts4.Continous := false;
  ts4.SyntheticPeriods := FSyntheticPeriods;
  ts4.SourceScale := FScale;
  ts4.SampleRate := 150;
  ts4.MaxFrequency := SyntheticTS4FreMax;
  ts4.MinFrequency := SyntheticTS4FreMin;
  ts4.Intervals := FTBL.HSMP * 2;
  ts4.Offset := 0;
  ts4.SamplingTime := FTBL.L4NS;
  ts4.SyntheticFunciton := ComboBoxSyntheticFunction.ItemIndex;
  ts5 := FSTS.AddNewSchema;
  ts5.Name := 'TS5';
  ts5.Continous := true;
  ts5.SyntheticPeriods := FSyntheticPeriods;
  ts5.SourceScale := FScale;
  ts5.SampleRate := 15;
  ts5.MaxFrequency := SyntheticTS5FreMax;
  ts5.MinFrequency := SyntheticTS5FreMin;
  ts5.Intervals := 0;
  ts5.Offset := 0;
  ts5.SamplingTime := 0;
  ts5.SyntheticFunciton := ComboBoxSyntheticFunction.ItemIndex;
  FFre := FSites[0].GetFrequencies;
  FCount := Length(FFre);
  SetLength(FScaleE, FCount);
  SetLength(FScaleB, FCount);
  for I := 0 to FCount - 1 do
  begin
    FScaleE[I] := NatureMagneticAmplitude(FFre[I]);
    FScaleB[I] := FScaleE[I];
    FScaleE[I] := FScaleE[I] / System.Sqrt(FSites[0].Fields[I].Hx1.Module2 + FSites[0].Fields[I].Hy1.Module2);
    FScaleB[I] := FScaleB[I] / System.Sqrt(FSites[0].Fields[I].Hx2.Module2 + FSites[0].Fields[I].Hy2.Module2);
  end;
  if CheckBoxPhoneixTimeseries.IsChecked then
  begin
    psite := TPhoenixSite.Create;
    psite.LoadFromTBL(FTBL, '.\Config', '.\Config');
    psite.SetProcessFrequency(FFre);
    psite.SysCal(WindowHandleToPlatform(Self.Handle).wnd, '.\tmp\', '.\Config\SysCal_V7.exe');
    psite.GetSystemRespond(FFre, FResponds, FTBL.CHEX - 1, FTBL.CHEY - 1, FTBL.CHHX - 1, FTBL.CHHY - 1, FTBL.CHHZ - 1);
    psite.Free;
  end
  else
  begin
    SetLength(FResponds, 5);
    SetLength(FResponds[0], FCount);
    SetLength(FResponds[1], FCount);
    SetLength(FResponds[2], FCount);
    SetLength(FResponds[3], FCount);
    SetLength(FResponds[4], FCount);
    for J := 0 to FCount - 1 do
    begin
      FResponds[0][J] := TComplex.Create(1E-6, 0);
      FResponds[1][J] := TComplex.Create(1E-6, 0);
      FResponds[2][J] := TComplex.Create(1E-9, 0);
      FResponds[3][J] := TComplex.Create(1E-9, 0);
      FResponds[4][J] := TComplex.Create(1E-9, 0);
    end;
  end;
end;

procedure TFormSyntheticTimeseries.UpdateTBL;
begin
  FTBL.Name := EditSiteName.Text;
  FTBL.Site := EditSiteName.Text;
  FTBL.TSFileName := EditSiteName.Text;
  FTBL.SNUM := EditBox.Text;
  FTBL.HXSN := EditHx.Text;
  FTBL.HYSN := EditHy.Text;
  FTBL.HZSN := EditHz.Text;
  FTBL.EXLN := EditExLength.Text.ToDouble;
  FTBL.EYLN := EditEyLength.Text.ToDouble;
  FTBL.HSMP := EditInterval.Text.ToInteger;
  FTBL.L2NS := EditL2Records.Text.ToInteger;
  FTBL.L3NS := EditL3Records.Text.ToInteger;
  FTBL.L4NS := EditL4Records.Text.ToInteger;
  FTBL.STIM := DateEditBegin.Date + TimeEditBegin.Time;
  FTBL.ETIM := DateEditEnd.Date + TimeEditEnd.Time;
  FTBL.HTIM := FTBL.STIM;
  FTBL.ETMH := FTBL.ETIM;
  FTBL.LFIX := FTBL.ETIM;
  FTBL.FTIM := FTBL.STIM;
  FTBL.LTIM := FTBL.ETIM;
  FTBL.SaveTBL(FTBL.path + '\' + FTBL.Name + '.TBL');
end;

end.
