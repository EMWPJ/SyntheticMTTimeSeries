unit TimeSeriesWindows;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, WTypes,
  System.Variants, FMX.Types, System.Generics.Collections, System.Math, MathFunctions;

type
  FFTWindows = (RectangularWindow, HammingWindow, HanningWindow, BlackmanWindow, FlattopWindow, GaussianWindow,
    BarlettWindow);
procedure WindowSeries(var value: Double1D; var sw: Double; const win: FFTWindows); overload;
function RectangularWin(const count: Integer): Double1D;
function HammingWin(const count: Integer): Double1D;
function HanningWin(const count: Integer): Double1D;
function InvHanningWin(const count: Integer): Double1D;
function BlackmanWin(const count: Integer): Double1D;
function FlattopWin(const count: Integer): Double1D;
function GaussianWin(const count: Integer; const sigma: Double = 0.5): Double1D;
function BarlettWin(const count: Integer): Double1D;

implementation

procedure WindowSeries(var value: Double1D; var sw: Double; const win: FFTWindows);
var
  I, count: Integer;
  c: Double1D;
begin
  count := length(value);
  case win of
    RectangularWindow:
      c := RectangularWin(count);
    HammingWindow:
      c := HammingWin(count);
    HanningWindow:
      c := HanningWin(count);
    BlackmanWindow:
      c := BlackmanWin(count);
    FlattopWindow:
      c := FlattopWin(count);
    GaussianWindow:
      c := GaussianWin(count);
    BarlettWindow:
      c := BarlettWin(count);
  end;
  sw := Sum(c);
  value := VectorMultiply(value, c);
  SetLength(c, 0);
end;

function RectangularWin(const count: Integer): Double1D;
var
  I: Integer;
begin
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := 1;
  end;
end;

function HammingWin(const count: Integer): Double1D;
var
  I: Integer;
begin
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := 0.53836 - 0.46164 * cos(2.0 * Pi * I / (count - 1.0));
  end;
end;

function HanningWin(const count: Integer): Double1D;
var
  I: Integer;
begin
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := 0.5 * (1.0 - cos(2.0 * Pi * I / (count - 1.0)));
  end;
end;

function InvHanningWin(const count: Integer): Double1D;
var
  I: Integer;
  index: Integer;
begin
  SetLength(Result, count);
  index := count div 2;
  for I := 0 to count - 1 do
  begin
    Result[index] := 0.5 * (1.0 - cos(2.0 * Pi * I / (count - 1.0)));
    Inc(index);
    if Index > count - 1 then
    begin
      Index := 0;
    end;
  end;
end;

function BlackmanWin(const count: Integer): Double1D;
var
  I: Integer;
begin
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := 0.42 - 0.5 * cos(2.0 * Pi * I / (count - 1.0)) + //
      0.08 * cos(4.0 * Pi * I / (count - 1.0));
  end;
end;

function FlattopWin(const count: Integer): Double1D;
var
  I: Integer;
begin
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := 1.0 - 1.93 * cos(2.0 * Pi * I / (count - 1.0)) + //
      1.29 * cos(4.0 * Pi * I / (count - 1.0)) - //
      0.388 * cos(6.0 * Pi * I / (count - 1.0)) + //
      0.0322 * cos(8.0 * Pi * I / (count - 1.0));
  end;
end;

function GaussianWin(const count: Integer; const sigma: Double): Double1D;
var
  I: Integer;
begin
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := exp((-0.5) * Power((I - (count - 1.0) / 2.0) / (sigma * (count - 1.0) / 2.0), 2.0));
  end;
end;

function BarlettWin(const count: Integer): Double1D;
var
  I: Integer;
begin
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := 1.0 - (2.0 * Abs(I - (count - 1.0) / 2.0)) / (count - 1.0);
  end;
end;

end.
