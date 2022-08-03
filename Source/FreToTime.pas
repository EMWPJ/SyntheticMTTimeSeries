unit FreToTime;

interface

uses
  WTypes, System.Math, MathFunctions;

procedure FreToTimeSeries(const amp, pha, fre, samplerate: Double; const count: Int64; out fields: Double1D); overload;
function NatureMagneticAmplitude(const fre: Double): Double;

implementation

procedure FreToTimeSeries(const amp, pha, fre, samplerate: Double; const count: Int64; out fields: Double1D);
var
  I: Integer;
begin
  SetLength(fields, count);
  for I := 0 to count - 1 do
  begin
    fields[I] := amp * System.Cos(2.0 * Pi * fre * I / samplerate + pha);
  end;
end;

function NatureMagneticAmplitude(const fre: Double): Double;
const
  a1 = 1.846;
  b1 = 0.3578;
  c1 = -2.302;
  a2 = 0.4855;
  b2 = 1.466;
  c2 = -0.6232;
  a3 = 0.07102;
  b3 = 2.435;
  c3 = -2.001;
  a4 = 0.05518;
  b4 = 3.765;
  c4 = -0.8136;
var
  x: Double;
begin
  x := System.Math.log10(fre);
  if x > 4 then
  begin
    x := 4;
  end;
  if x < -4 then
  begin
    x := -4;
  end;
  Result := a1 * sin(b1 * x + c1) + a2 * sin(b2 * x + c2) + a3 * sin(b3 * x + c3) + a4 * sin(b4 * x + c4); // nT
  Result := Power(10, Result) / (400 * System.Pi) / 10; // A/m
end;

end.
