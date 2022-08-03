program Synthesize_MT_Timeseries;

uses
  System.StartUpCopy,
  FMX.Forms,
  SyntheticTimeSeriesForm in 'SyntheticTimeSeriesForm.pas' {FormSyntheticTimeseries},
  ForwardSite in 'Source\ForwardSite.pas',
  FreToTime in 'Source\FreToTime.pas',
  MathFunctions in 'Source\MathFunctions.pas',
  Phoenix in 'Source\Phoenix.pas',
  PhoenixB in 'Source\PhoenixB.pas',
  PhoenixSite in 'Source\PhoenixSite.pas',
  PhoenixTypes in 'Source\PhoenixTypes.pas',
  SyntheticTimeSeries in 'Source\SyntheticTimeSeries.pas',
  TimeSeries in 'Source\TimeSeries.pas',
  TimeSeriesWindows in 'Source\TimeSeriesWindows.pas',
  WTypes in 'Source\WTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSyntheticTimeseries, FormSyntheticTimeseries);
  Application.Run;

end.
