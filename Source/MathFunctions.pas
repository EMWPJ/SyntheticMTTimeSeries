unit MathFunctions;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, WTypes,
   System.Variants, FMX.Types, System.Generics.Collections,
  System.Generics.Defaults, System.Math;

const
  PI = 3.1415926535897932384626;
  Mu0 = 1.2566370614359E-6;
  Epsilon0 = 8.841941282883E-12;

procedure FieldRotate(var ax, ay: TComplex; const theta: Double); inline;
procedure EHRotate(var ex, ey, hx, hy: TComplex; const theta: Double); inline;
function RealEqual(a, b: Double): Boolean; inline;
function Larger(a, b: Integer): Integer; overload; inline;
function Smaller(a, b: Integer): Integer; overload; inline;
function Larger(a, b: Double): Double; overload; inline;
function Smaller(a, b: Double): Double; overload; inline;
function NewDouble1D(const icount: Integer; const value: Double = 0): Double1D; inline;
function NewDouble2D(const icount, jcount: Integer; const value: Double = 0): Double2D; inline;
function NewDouble3D(const icount, jcount, kcount: Integer; const value: Double = 0): Double3D; inline;
procedure FillDouble1D(var values: Double1D; const value: Double = 0); inline;
procedure FillDouble2D(var values: Double2D; const value: Double = 0); inline;
procedure FillDouble3D(var values: Double3D; const value: Double = 0); inline;

function Average(values: Double1D): Double; overload; inline;

function Average(values: Integer1D): Integer; overload; inline;

procedure QuickSort(var a: Integer1D; l, r: Integer); overload; inline;

procedure QuickSort(var a: Double1D; l, r: Integer); overload; inline;

procedure QuickSort(var a, ai: Integer1D; l, r: Integer); overload; inline;

procedure QuickSort(var a: Double1D; var ai: Integer1D; l, r: Integer); overload; inline;

procedure QuickSort(var a, ai: Integer1D); overload; inline;

procedure QuickSort(var a: Double1D; var ai: Integer1D); overload; inline;

procedure Sort(var a: Double1D; var ai: Integer1D); overload; inline;

procedure Sort(var a: Integer1D; var ai: Integer1D); overload; inline;

function Average(values: Double1D; choosed: Boolean1D): Double; overload; inline;

function Average(values: Integer1D; choosed: Boolean1D): Integer; overload; inline;

function StandardDeviation(values: Double1D; mean: Double): Double; overload; inline;

function VectorAdd(a, b: Double1D): Double1D; overload; inline;
function VectorAdd(a, b: Integer1D): Integer1D; overload; inline;
function VectorSub(a, b: Double1D): Double1D; overload; inline;
function VectorSub(a, b: Integer1D): Integer1D; overload; inline;
function VectorMultiply(a, b: Double1D): Double1D; overload; inline;
function VectorMultiply(a, b: Integer1D): Integer1D; overload; inline;
function VectorMultiply(a: Integer1D; b: Double1D): Integer1D; overload; inline;
function VectorMultiplyValue(a: Double1D; scale: Double): Double1D; overload; inline;
function VectorMultiplyValue(a: Integer1D; scale: Integer): Integer1D; overload; inline;
function MartixAdd(a, b: Double2D): Double2D; overload; inline;
function MartixAdd(a: Integer2D; b: Double2D): Integer2D; overload; inline;

function VectorMerge(a, b: Double1D): Double1D; overload; inline;
function VectorMerge(a, b: TComplex1D): TComplex1D; overload; inline;
function VectorMerge(a, b: Integer1D): Double1D; overload; inline;

procedure VectorCombine(var a: Double1D; const b: Double1D); overload; inline;
procedure VectorCombine(var a: TComplex1D; const b: TComplex1D); overload; inline;
procedure VectorCombine(var a: Integer1D; const b: Integer1D); overload; inline;

function Martrix2Vector(const value: Double2D): Double1D; overload; inline;
function Martrix2Vector(const value: Integer2D): Integer1D; overload; inline;

function MartrixMultiply(const a, b: Double2D): Double2D; overload; inline;
function MartrixMultiply(const a: Double2D; b: TComplex2D): TComplex2D; overload; inline;
function MartrixMultiply(const a: TComplex2D; b: Double2D): TComplex2D; overload; inline;
function MartrixMultiply(const a, b: TComplex2D): TComplex2D; overload; inline;

function TransportMartrix(const a: Double2D): Double2D; overload; inline;

procedure SortXY(var X, Y: Double1D); overload; inline;
procedure SortXYI(var X, Y: Double1D); overload; inline;
procedure SortXY(var X: Double1D; var Y: Double2D); overload; inline;
procedure SortXYI(var X: Double1D; var Y: Double2D); overload; inline;

function ArrayCopy(const src: Integer1D): Integer1D; overload; inline;
function ArrayCopy(const src: Integer2D): Integer2D; overload; inline;
function ArrayCopy(const src: Integer3D): Integer3D; overload; inline;
function ArrayCopy(const src: Double1D): Double1D; overload; inline;
function ArrayCopy(const src: Double2D): Double2D; overload; inline;
function ArrayCopy(const src: Double3D): Double3D; overload; inline;
function ArrayCopy(const src: String1D): String1D; overload; inline;
function ArrayCopy(const src: String2D): String2D; overload; inline;
function ArrayCopy(const src: String3D): String3D; overload; inline;
function ArrayCopy(const src: TComplex1D): TComplex1D; overload; inline;
function ArrayCopy(const src: TComplex2D): TComplex2D; overload; inline;
function ArrayCopy(const src: TComplex3D): TComplex3D; overload; inline;
function ArrayCopy(const src: Boolean1D): Boolean1D; overload; inline;
function ArrayCopy(const src: Boolean2D): Boolean2D; overload; inline;
function ArrayCopy(const src: Boolean3D): Boolean3D; overload; inline;

function Bytes2String(var bts: Byte1D): String; inline;
function String2Bytes(var str: String): Byte1D; inline;

function Bytes2Double(var bts: Byte1D): Double; inline;
function Double2Bytes(var db: Double): Byte1D; inline;

function MakeDouble1D(const count: Integer; const value: Double = 0): Double1D; inline;

function MakeDouble2D(const icount, jcount: Integer; const value: Double = 0): Double2D; inline;

function MakeDouble3D(const icount, jcount, kcount: Integer; const value: Double = 0): Double3D; overload; inline;

function MakeComplex1D(const count: Integer; const value: TComplex): TComplex1D; inline;

function MakeComplex2D(const icount, jcount: Integer; const value: TComplex): TComplex2D; inline;

function MakeComplex3D(const icount, jcount, kcount: Integer; const value: TComplex): TComplex3D; overload; inline;

function MakeInteger1D(const count: Integer; const value: Integer = 0): Integer1D; inline;

function MakeInteger2D(const icount, jcount: Integer; const value: Integer = 0): Integer2D; inline;

function MakeInteger3D(const icount, jcount, kcount: Integer; const value: Integer = 0): Integer3D; overload; inline;

function MakeInt641D(const count: Integer; const value: Int64 = 0): Int641D; inline;

function MakeInt642D(const icount, jcount: Integer; const value: Int64 = 0): Int642D; inline;

function MakeInt643D(const icount, jcount, kcount: Integer; const value: Int64 = 0): Int643D; overload; inline;

function MakeBoolean1D(const count: Integer; const value: Boolean = True): Boolean1D; inline;

function MakeBoolean2D(const icount, jcount: Integer; const value: Boolean = True): Boolean2D; inline;

function MakeBoolean3D(const icount, jcount, kcount: Integer; const value: Boolean = True): Boolean3D; overload; inline;

function SumArray(const value: Double1D): Double;

function MatrixMerge(var a: Double2D; const b: Double2D): Double2D; inline;

implementation

procedure FieldRotate(var ax, ay: TComplex; const theta: Double); inline;
var
  costheta, sintheta: Double;
  ax0, ay0: TComplex;
begin
  costheta := cos(theta);
  sintheta := sin(theta);
  ax0 := ax;
  ay0 := ay;
  ax := ax0 * costheta + ay0 * sintheta;
  ay := -ax0 * sintheta + ay0 * costheta;
end;

procedure EHRotate(var ex, ey, hx, hy: TComplex; const theta: Double); inline;
var
  costheta, sintheta: Double;
  ex0, ey0, hx0, hy0: TComplex;
begin
  costheta := cos(theta);
  sintheta := sin(theta);
  ex0 := ex;
  ey0 := ey;
  ex := ex0 * costheta + ey0 * sintheta;
  ey := -ex0 * sintheta + ey0 * costheta;
  hx0 := hx;
  hy0 := hy;
  hx := hx0 * costheta + hy0 * sintheta;
  hy := -hx0 * sintheta + hy0 * costheta;
end;

function RealEqual(a, b: Double): Boolean; inline;
begin
  if a = b then
  begin
    Result := True;
    Exit;
  end;
  if a = 0 then
  begin
    Result := Abs((a - b) / b) < 1E-5;
  end
  else
  begin
    Result := Abs((a - b) / a) < 1E-5;
  end;
end;

function Larger(a, b: Double): Double; inline;
begin
  if a > b then
  begin
    Result := a;
  end
  else
  begin
    Result := b;
  end;
end;

function Smaller(a, b: Double): Double; inline;
begin
  if a < b then
  begin
    Result := a;
  end
  else
  begin
    Result := b;
  end;
end;

function Larger(a, b: Integer): Integer; inline;
begin
  if a > b then
  begin
    Result := a;
  end
  else
  begin
    Result := b;
  end;
end;

function Smaller(a, b: Integer): Integer; inline;
begin
  if a < b then
  begin
    Result := a;
  end
  else
  begin
    Result := b;
  end;
end;

function MakeDouble2D(const icount, jcount: Integer; const value: Double = 0): Double2D; inline;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := MakeDouble1D(jcount, value);
  end;
end;

function MakeComplex2D(const icount, jcount: Integer; const value: TComplex): TComplex2D;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := MakeComplex1D(jcount, value);
  end;
end;

function MakeInteger2D(const icount, jcount: Integer; const value: Integer = 0): Integer2D; inline;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := MakeInteger1D(jcount, value);
  end;
end;

function MakeInt642D(const icount, jcount: Integer; const value: Int64 = 0): Int642D; inline;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := MakeInt641D(jcount, value);
  end;
end;

function MakeBoolean2D(const icount, jcount: Integer; const value: Boolean = True): Boolean2D; inline;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := MakeBoolean1D(jcount, value);
  end;
end;

function NewDouble1D(const icount: Integer; const value: Double = 0): Double1D; inline;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := value;
  end;
end;

function NewDouble2D(const icount, jcount: Integer; const value: Double = 0): Double2D; inline;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := NewDouble1D(jcount, value);
  end;
end;

function NewDouble3D(const icount, jcount, kcount: Integer; const value: Double = 0): Double3D; inline;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := NewDouble2D(jcount, kcount, value);
  end;
end;

procedure FillDouble1D(var values: Double1D; const value: Double = 0); inline;
var
  I, count: Integer;
begin
  count := Length(values);
  for I := 0 to count - 1 do
  begin
    values[I] := value;
  end;
end;

procedure FillDouble2D(var values: Double2D; const value: Double = 0); inline;
var
  I, count: Integer;
begin
  count := Length(values);
  for I := 0 to count - 1 do
  begin
    FillDouble1D(values[I], value);
  end;
end;

procedure FillDouble3D(var values: Double3D; const value: Double = 0); inline;
var
  I, count: Integer;
begin
  count := Length(values);
  for I := 0 to count - 1 do
  begin
    FillDouble2D(values[I], value);
  end;
end;

function Average(values: Double1D): Double; inline;
var
  I, count: Integer;
begin
  count := Length(values);
  Result := 0;
  for I := 0 to count - 1 do
  begin
    Result := Result + values[I];
  end;
  Result := Result / count;
end;

function Average(values: Integer1D): Integer; overload; inline;
var
  I, count: Integer;
begin
  count := Length(values);
  Result := 0;
  for I := 0 to count - 1 do
  begin
    Result := Result + values[I];
  end;
  Result := Result div count;
end;

function StandardDeviation(values: Double1D; mean: Double): Double; overload; inline;
var
  I, count: Integer;
begin
  count := Length(values);
  Result := 0;
  for I := 0 to count - 1 do
  begin
    Result := Result + Abs(values[I] - mean);
  end;
  Result := Result / count;
end;

procedure QuickSort(var a: Integer1D; l, r: Integer); overload; inline;
var
  I, J, Temp: Integer;
begin
  if l < r then
  begin
    I := l;
    J := r;
    Temp := a[I];
    while I < J do
    begin
      while (I < J) and (a[J] >= Temp) do
      begin
        J := J - 1;
      end;
      if I < J then
        a[I] := a[J];
      while (I < J) and (a[I] <= Temp) do
      begin
        I := I + 1;
      end;
      if I < J then
        a[J] := a[I];
    end;
    a[I] := Temp;
    QuickSort(a, l, I - 1);
    QuickSort(a, I + 1, r);
  end;
end;

procedure QuickSort(var a: Double1D; l, r: Integer); overload; inline;
var
  I, J: Integer;
  Temp: Double;
begin
  if l < r then
  begin
    I := l;
    J := r;
    Temp := a[I];
    while I < J do
    begin
      while (I < J) and (a[J] >= Temp) do
      begin
        J := J - 1;
      end;
      if I < J then
        a[I] := a[J];
      while (I < J) and (a[I] <= Temp) do
      begin
        I := I + 1;
      end;
      if I < J then
        a[J] := a[I];
    end;
    a[I] := Temp;
    QuickSort(a, l, I - 1);
    QuickSort(a, I + 1, r);
  end;
end;

procedure QuickSort(var a, ai: Integer1D; l, r: Integer); overload; inline;
var
  I, J, Temp, TempI: Integer;
begin
  if l < r then
  begin
    I := l;
    J := r;
    Temp := a[I];
    TempI := ai[I];
    while I < J do
    begin
      while (I < J) and (a[J] >= Temp) do
      begin
        J := J - 1;
      end;
      if I < J then
      begin
        a[I] := a[J];
        ai[I] := ai[J];
      end;
      while (I < J) and (a[I] <= Temp) do
      begin
        I := I + 1;
      end;
      if I < J then
      begin
        a[J] := a[I];
        ai[J] := ai[I];
      end;
    end;
    a[I] := Temp;
    ai[I] := TempI;
    QuickSort(a, ai, l, I - 1);
    QuickSort(a, ai, I + 1, r);
  end;
end;

procedure QuickSort(var a: Double1D; var ai: Integer1D; l, r: Integer); overload; inline;
var
  I, J, TempI: Integer;
  Temp: Double;
begin
  if l < r then
  begin
    I := l;
    J := r;
    Temp := a[I];
    TempI := ai[I];
    while I < J do
    begin
      while (I < J) and (a[J] >= Temp) do
      begin
        J := J - 1;
      end;
      if I < J then
      begin
        a[I] := a[J];
        ai[I] := ai[J];
      end;
      while (I < J) and (a[I] <= Temp) do
      begin
        I := I + 1;
      end;
      if I < J then
      begin
        a[J] := a[I];
        ai[J] := ai[I];
      end;
    end;
    a[I] := Temp;
    ai[I] := TempI;
    QuickSort(a, ai, l, I - 1);
    QuickSort(a, ai, I + 1, r);
  end;
end;

procedure QuickSort(var a, ai: Integer1D); overload; inline;
var
  l, r: Integer;
begin
  l := 0;
  r := Length(a) - 1;
  QuickSort(a, ai, l, r);
end;

procedure QuickSort(var a: Double1D; var ai: Integer1D); overload; inline;
var
  l, r: Integer;
begin
  l := 0;
  r := Length(a) - 1;
  QuickSort(a, ai, l, r);
end;

procedure Sort(var a: Double1D; var ai: Integer1D); overload; inline;
var
  I, J, tmpI, count: Integer;
  tmpd: Double;
begin
  count := Length(a);
  for I := 0 to count - 2 do
  begin
    for J := I + 1 to count - 1 do
    begin
      if a[I] < a[J] then
      begin
        tmpd := a[I];
        a[I] := a[J];
        a[J] := tmpd;

        tmpI := ai[I];
        ai[I] := ai[J];
        ai[J] := tmpI;
      end;
    end;
  end;
end;

procedure Sort(var a: Integer1D; var ai: Integer1D); overload; inline;
var
  I, J, tmpd, tmpI, count: Integer;
begin
  count := Length(a);
  for I := 0 to count - 2 do
  begin
    for J := I + 1 to count - 1 do
    begin
      if a[I] < a[J] then
      begin
        tmpd := a[I];
        a[I] := a[J];
        a[J] := tmpd;

        tmpI := ai[I];
        ai[I] := ai[J];
        ai[J] := tmpI;
      end;
    end;
  end;
end;

function Average(values: Double1D; choosed: Boolean1D): Double; overload; inline;
var
  I, count, count1: Integer;
  tmpv: Double1D;
begin
  count := Length(values);
  count1 := 0;
  for I := 0 to count - 1 do
  begin
    if choosed[I] then
    begin
      inc(count1);
      SetLength(tmpv, count1);
      tmpv[count1 - 1] := values[I];
    end;
  end;
  Result := Average(tmpv);
  tmpv := nil;
end;

function Average(values: Integer1D; choosed: Boolean1D): Integer; overload; inline;
var
  I, count, count1: Integer;
  tmpv: Integer1D;
begin
  count := Length(values);
  count1 := 0;
  for I := 0 to count - 1 do
  begin
    if choosed[I] then
    begin
      inc(count1);
      SetLength(tmpv, count1);
      tmpv[count1 - 1] := values[I];
    end;
  end;
  Result := Average(tmpv);
  tmpv := nil;
end;

function VectorAdd(a, b: Double1D): Double1D; inline;
var
  I, count: Integer;
begin
  count := Length(a);
  // if Length(b) <> count then
  // raise Exception.Create('VectorAdd Error ' + Length(a).ToString + '  ' +
  // Length(b).ToString);
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := a[I] + b[I];
  end;
end;

function MartixAdd(a, b: Double2D): Double2D; inline;
var
  I, J, count, count1: Integer;
begin
  count := Length(a);
  count1 := Length(a[0]);
  // if Length(b) <> count then
  // raise Exception.Create('MartixAdd Error ' + Length(a).ToString + '  ' +
  // Length(b).ToString);
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    SetLength(Result[I], count1);
    for J := 0 to count1 - 1 do
    begin
      Result[I][J] := a[I][J] + b[I][J];
    end;
  end;
end;

function MartixAdd(a: Integer2D; b: Double2D): Integer2D; overload; inline;
var
  I, J, count, count1: Integer;
begin
  count := Length(a);
  count1 := Length(a[0]);
  // if Length(b) <> count then
  // raise Exception.Create('MartixAdd Error ' + Length(a).ToString + '  ' +
  // Length(b).ToString);
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    SetLength(Result[I], count1);
    for J := 0 to count1 - 1 do
    begin
      Result[I][J] := a[I][J] + Round(b[I][J]);
    end;
  end;
end;

function VectorAdd(a, b: Integer1D): Integer1D; inline;
var
  I, count: Integer;
begin
  count := Length(a);
  if Length(b) <> count then
    raise Exception.Create('VectorAdd Error ' + Length(a).ToString + '  ' + Length(b).ToString);
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := a[I] + b[I];
  end;
end;

function VectorSub(a, b: Double1D): Double1D; inline;
var
  I, count: Integer;
begin
  count := Length(a);
  if Length(b) <> count then
    raise Exception.Create('Vector Sub Error ' + Length(a).ToString + '  ' + Length(b).ToString);
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := a[I] - b[I];
  end;
end;

function VectorSub(a, b: Integer1D): Integer1D; inline;
var
  I, count: Integer;
begin
  count := Length(a);
  if Length(b) <> count then
    raise Exception.Create('Vector Sub Error ' + Length(a).ToString + '  ' + Length(b).ToString);
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := a[I] - b[I];
  end;
end;

function VectorMultiply(a, b: Double1D): Double1D; inline;
var
  I, count: Integer;
begin
  count := Length(a);
  if Length(b) <> count then
    raise Exception.Create('Vector Multiply Error ' + Length(a).ToString + '  ' + Length(b).ToString);
  SetLength(Result, count);
   for I := 0 to count - 1 do
   begin
   Result[I] := a[I] * b[I];
  end;
end;

function VectorMultiply(a, b: Integer1D): Integer1D; inline;
var
  I, count: Integer;
begin
  count := Length(a);
  if Length(b) <> count then
    raise Exception.Create('Vector Multiply Error ' + Length(a).ToString + '  ' + Length(b).ToString);
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := a[I] * b[I];
  end;
end;

function VectorMultiply(a: Integer1D; b: Double1D): Integer1D; overload; inline;
var
  I, count: Integer;
begin
  count := Length(a);
  if Length(b) <> count then
    raise Exception.Create('Vector Multiply Error ' + Length(a).ToString + '  ' + Length(b).ToString);
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := Round(a[I] * b[I]);
  end;
end;

function VectorMultiplyValue(a: Double1D; scale: Double): Double1D; inline;
var
  I, count: Integer;
begin
  count := Length(a);
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := a[I] * scale;
  end;
end;

function VectorMultiplyValue(a: Integer1D; scale: Integer): Integer1D; inline;
var
  I, count: Integer;
begin
  count := Length(a);
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := a[I] * scale;
  end;
end;

function VectorMerge(a, b: Double1D): Double1D; inline;
var
  I, acount, bcount: Integer;
begin
  acount := Length(a);
  bcount := Length(b);
  SetLength(Result, acount + bcount);
  Move(a[0], Result[0], acount * 8);
  Move(b[0], Result[acount], bcount * 8);
end;

function VectorMerge(a, b: TComplex1D): TComplex1D; inline;
var
  I, acount, bcount: Integer;
begin
  acount := Length(a);
  bcount := Length(b);
  SetLength(Result, acount + bcount);
  Move(a[0], Result[0], acount * 16);
  Move(b[0], Result[acount], bcount * 16);
end;

function VectorMerge(a, b: Integer1D): Double1D; inline;
var
  I, acount, bcount: Integer;
begin
  acount := Length(a);
  bcount := Length(b);
  SetLength(Result, acount + bcount);
  Move(a[0], Result[0], acount * 4);
  Move(b[0], Result[acount], bcount * 4);
end;

procedure VectorCombine(var a: Double1D; const b: Double1D);
var
  alen, blen: Integer;
begin
  alen := Length(a);
  blen := Length(b);
  SetLength(a, alen + Length(b));
  Move(b[0], a[alen], 16 * blen);
end;

procedure VectorCombine(var a: TComplex1D; const b: TComplex1D);
var
  alen, blen: Integer;
begin
  alen := Length(a);
  blen := Length(b);
  SetLength(a, alen + Length(b));
  Move(b[0], a[alen], 8 * blen);
end;

procedure VectorCombine(var a: Integer1D; const b: Integer1D);
var
  alen, blen: Integer;
begin
  alen := Length(a);
  blen := Length(b);
  SetLength(a, alen + Length(b));
  Move(b[0], a[alen], 4 * blen);
end;

function Martrix2Vector(const value: Double2D): Double1D; inline;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to Length(value) - 1 do
  begin
    VectorCombine(Result, value[I]);
  end;
end;

function Martrix2Vector(const value: Integer2D): Integer1D; inline;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to Length(value) - 1 do
  begin
    VectorCombine(Result, value[I]);
  end;
end;

function MartrixMultiply(const a, b: Double2D): Double2D; overload; inline;
var
  I, J, K, len1, len2, len3: Integer;
begin
  len1 := Length(a);
  len2 := Length(a[0]);
  if len2 <> Length(b) then
  begin
    raise Exception.Create('MartrixMultiply Error!');
  end;
  len3 := Length(b[0]);
  SetLength(Result, len1);
  for I := 0 to len1 - 1 do
  begin
    SetLength(Result[I], len3);
    for J := 0 to len3 - 1 do
    begin
      Result[I][J] := 0;
      for K := 0 to len2 - 1 do
      begin
        Result[I][J] := Result[I][J] + a[I][K] * b[K][J];
      end;
    end;
  end;
end;

function MartrixMultiply(const a: Double2D; b: TComplex2D): TComplex2D; overload; inline;
var
  I, J, K, len1, len2, len3: Integer;
begin
  len1 := Length(a);
  len2 := Length(a[0]);
  if len2 <> Length(b) then
  begin
    raise Exception.Create('MartrixMultiply Error!');
  end;
  len3 := Length(b[0]);
  SetLength(Result, len1);
  for I := 0 to len1 - 1 do
  begin
    SetLength(Result[I], len3);
    for J := 0 to len3 - 1 do
    begin
      Result[I][J] := TComplex.Create(0, 0);
      for K := 0 to len2 - 1 do
      begin
        Result[I][J] := Result[I][J] + a[I][K] * b[K][J];
      end;
    end;
  end;
end;

function MartrixMultiply(const a: TComplex2D; b: Double2D): TComplex2D; overload; inline;
var
  I, J, K, len1, len2, len3: Integer;
begin
  len1 := Length(a);
  len2 := Length(a[0]);
  if len2 <> Length(b) then
  begin
    raise Exception.Create('MartrixMultiply Error!');
  end;
  len3 := Length(b[0]);
  SetLength(Result, len1);
  for I := 0 to len1 - 1 do
  begin
    SetLength(Result[I], len3);
    for J := 0 to len3 - 1 do
    begin
      Result[I][J] := TComplex.Create(0, 0);
      for K := 0 to len2 - 1 do
      begin
        Result[I][J] := Result[I][J] + a[I][K] * b[K][J];
      end;
    end;
  end;
end;

function MartrixMultiply(const a, b: TComplex2D): TComplex2D; overload; inline;
var
  I, J, K, len1, len2, len3: Integer;
begin
  len1 := Length(a);
  len2 := Length(a[0]);
  if len2 <> Length(b) then
  begin
    raise Exception.Create('MartrixMultiply Error!');
  end;
  len3 := Length(b[0]);
  SetLength(Result, len1);
  for I := 0 to len1 - 1 do
  begin
    SetLength(Result[I], len3);
    for J := 0 to len3 - 1 do
    begin
      Result[I][J] := TComplex.Create(0, 0);
      for K := 0 to len2 - 1 do
      begin
        Result[I][J] := Result[I][J] + a[I][K] * b[K][J];
      end;
    end;
  end;
end;

function TransportMartrix(const a: Double2D): Double2D; overload; inline;
var
  I, J, len1, len2: Integer;
begin
  len1 := Length(a);
  len2 := Length(a[0]);
  SetLength(Result, len2);
  for I := 0 to len2 - 1 do
  begin
    SetLength(Result[I], len1);
    for J := 0 to len1 - 1 do
    begin
      Result[I][J] := a[J][I];
    end;
  end;
end;

procedure SortXY(var X, Y: Double1D); inline;
var
  tmpx, tmpy: Double;
  I, count: Integer;
  J: Integer;
begin
  count := Length(X);
  for I := 0 to count - 2 do
  begin
    for J := I + 1 to count - 1 do
    begin
      if X[I] < X[J] then
      begin
        tmpx := X[I];
        X[I] := X[J];
        X[J] := tmpx;
        tmpy := Y[I];
        Y[I] := Y[J];
        Y[J] := tmpy;
      end;
    end;
  end;
end;

procedure SortXYI(var X, Y: Double1D); inline;
var
  tmpx, tmpy: Double;
  I, count: Integer;
  J: Integer;
begin
  count := Length(X);
  for I := 0 to count - 2 do
  begin
    for J := I + 1 to count - 1 do
    begin
      if X[I] > X[J] then
      begin
        tmpx := X[I];
        X[I] := X[J];
        X[J] := tmpx;
        tmpy := Y[I];
        Y[I] := Y[J];
        Y[J] := tmpy;
      end;
    end;
  end;
end;

procedure SortXY(var X: Double1D; var Y: Double2D); inline;
var
  tmpx, tmpy: Double;
  I, J, K, count, count1: Integer;
begin
  count := Length(X);
  count1 := Length(Y);
  for I := 0 to count - 2 do
  begin
    for J := I + 1 to count - 1 do
    begin
      if X[I] < X[J] then
      begin
        tmpx := X[I];
        X[I] := X[J];
        X[J] := tmpx;
        for K := 0 to count1 - 1 do
        begin
          tmpy := Y[K][I];
          Y[K][I] := Y[K][J];
          Y[K][J] := tmpy;
        end;
      end;
    end;
  end;
end;

procedure SortXYI(var X: Double1D; var Y: Double2D); inline;
var
  tmpx, tmpy: Double;
  I, J, K, count, count1: Integer;
begin
  count := Length(X);
  count1 := Length(Y);
  for I := 0 to count - 2 do
  begin
    for J := I + 1 to count - 1 do
    begin
      if X[I] > X[J] then
      begin
        tmpx := X[I];
        X[I] := X[J];
        X[J] := tmpx;
        for K := 0 to count1 - 1 do
        begin
          tmpy := Y[K][I];
          Y[K][I] := Y[K][J];
          Y[K][J] := tmpy;
        end;
      end;
    end;
  end;
end;

function ArrayCopy(const src: Integer1D): Integer1D; inline;
begin
  SetLength(Result, Length(src));
  Result := Copy(src, 0, Length(src));
end;

function ArrayCopy(const src: Integer2D): Integer2D; inline;
var
  I: Integer;
begin
  try
    SetLength(Result, Length(src));
    for I := 0 to Length(src) - 1 do
    begin
      Result[I] := ArrayCopy(src[I]);
    end;
  except
    raise Exception.Create('Integer2D Array Copy Error At ' + I.ToString);
  end;
end;

function ArrayCopy(const src: Integer3D): Integer3D; inline;
var
  I: Integer;
begin
  try
    SetLength(Result, Length(src));
    for I := 0 to Length(src) - 1 do
    begin
      Result[I] := ArrayCopy(src[I]);
    end;
  except
    raise Exception.Create('Integer3D Array Copy Error At ' + I.ToString);
  end;
end;

function ArrayCopy(const src: Double1D): Double1D; inline;
begin
  SetLength(Result, Length(src));
  Result := Copy(src, 0, Length(src));
end;

function ArrayCopy(const src: Double2D): Double2D; inline;
var
  I: Integer;
begin
  try
    SetLength(Result, Length(src));
    for I := 0 to Length(src) - 1 do
    begin
      Result[I] := ArrayCopy(src[I]);
    end;
  except
    raise Exception.Create('Double2D Array Copy Error At ' + I.ToString);
  end;
end;

function ArrayCopy(const src: Double3D): Double3D; inline;
var
  I: Integer;
begin
  try
    SetLength(Result, Length(src));
    for I := 0 to Length(src) - 1 do
    begin
      Result[I] := ArrayCopy(src[I]);
    end;
  except
    raise Exception.Create('Double3D Array Copy Error At ' + I.ToString);
  end;
end;

function ArrayCopy(const src: TComplex1D): TComplex1D; inline;
begin
  SetLength(Result, Length(src));
  Result := Copy(src, 0, Length(src));
end;

function ArrayCopy(const src: TComplex2D): TComplex2D; inline;
var
  I: Integer;
begin
  try
    SetLength(Result, Length(src));
    for I := 0 to Length(src) - 1 do
    begin
      Result[I] := ArrayCopy(src[I]);
    end;
  except
    raise Exception.Create('Double2D Array Copy Error At ' + I.ToString);
  end;
end;

function ArrayCopy(const src: TComplex3D): TComplex3D; inline;
var
  I: Integer;
begin
  try
    SetLength(Result, Length(src));
    for I := 0 to Length(src) - 1 do
    begin
      Result[I] := ArrayCopy(src[I]);
    end;
  except
    raise Exception.Create('Double3D Array Copy Error At ' + I.ToString);
  end;
end;

function ArrayCopy(const src: String1D): String1D; inline;
begin
  SetLength(Result, Length(src));
  Result := Copy(src, 0, Length(src));
end;

function ArrayCopy(const src: String2D): String2D; inline;
var
  I: Integer;
begin
  try
    SetLength(Result, Length(src));
    for I := 0 to Length(src) - 1 do
    begin
      Result[I] := ArrayCopy(src[I]);
    end;
  except
    raise Exception.Create('String2D Array Copy Error At ' + I.ToString);
  end;
end;

function ArrayCopy(const src: String3D): String3D; inline;
var
  I: Integer;
begin
  try
    SetLength(Result, Length(src));
    for I := 0 to Length(src) - 1 do
    begin
      Result[I] := ArrayCopy(src[I]);
    end;
  except
    raise Exception.Create('String3D Array Copy Error At ' + I.ToString);
  end;
end;

function ArrayCopy(const src: Boolean1D): Boolean1D; inline;
begin
  SetLength(Result, Length(src));
  Result := Copy(src, 0, Length(src));
end;

function ArrayCopy(const src: Boolean2D): Boolean2D; inline;
var
  I: Integer;
begin
  try
    SetLength(Result, Length(src));
    for I := 0 to Length(src) - 1 do
    begin
      Result[I] := ArrayCopy(src[I]);
    end;
  except
    raise Exception.Create('Boolean2D Array Copy Error At ' + I.ToString);
  end;
end;

function ArrayCopy(const src: Boolean3D): Boolean3D; inline;
var
  I: Integer;
begin
  try
    SetLength(Result, Length(src));
    for I := 0 to Length(src) - 1 do
    begin
      Result[I] := ArrayCopy(src[I]);
    end;
  except
    raise Exception.Create('Boolean3D Array Copy Error At ' + I.ToString);
  end;
end;

function Bytes2String(var bts: Byte1D): String; inline;
var
  I, count: Integer;
begin
  count := Length(bts);
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I + 1] := Char(bts[I]);
  end;
end;

function String2Bytes(var str: String): Byte1D; inline;
var
  I, count: Integer;
begin
  count := Length(str);
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := Ord(str[I + 1]);
  end;
end;

function Bytes2Double(var bts: Byte1D): Double; inline;
begin
  Result := PDouble(@bts[0])^;
end;

function Double2Bytes(var db: Double): Byte1D; inline;
begin
  SetLength(Result, 8);
  Move(Result, db, 8);
end;

function MakeDouble1D(const count: Integer; const value: Double = 0): Double1D; inline;
var
  I: Integer;
begin
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := value;
  end;
end;

function MakeComplex1D(const count: Integer; const value: TComplex): TComplex1D; inline;
var
  I: Integer;
begin
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := value;
  end;
end;

function MakeInteger1D(const count: Integer; const value: Integer = 0): Integer1D; inline;
var
  I: Integer;
begin
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := value;
  end;
end;

function MakeInt641D(const count: Integer; const value: Int64 = 0): Int641D; inline;
var
  I: Int64;
begin
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := value;
  end;
end;

function MakeBoolean1D(const count: Integer; const value: Boolean = True): Boolean1D; inline;
var
  I: Integer;
begin
  SetLength(Result, count);
  for I := 0 to count - 1 do
  begin
    Result[I] := value;
  end;
end;

function MakeDouble3D(const icount, jcount, kcount: Integer; const value: Double = 0): Double3D; inline;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := MakeDouble2D(jcount, kcount, value);
  end;
end;

function MakeComplex3D(const icount, jcount, kcount: Integer; const value: TComplex): TComplex3D;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := MakeComplex2D(jcount, kcount, value);
  end;
end;

function MakeInteger3D(const icount, jcount, kcount: Integer; const value: Integer = 0): Integer3D; overload; inline;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := MakeInteger2D(jcount, kcount, value);
  end;
end;

function MakeInt643D(const icount, jcount, kcount: Integer; const value: Int64 = 0): Int643D; overload; inline;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := MakeInt642D(jcount, kcount, value);
  end;
end;

function MakeBoolean3D(const icount, jcount, kcount: Integer; const value: Boolean = True): Boolean3D; overload; inline;
var
  I: Integer;
begin
  SetLength(Result, icount);
  for I := 0 to icount - 1 do
  begin
    Result[I] := MakeBoolean2D(jcount, kcount, value);
  end;
end;

function SumArray(const value: Double1D): Double;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(value) - 1 do
  begin
    Result := Result + value[I];
  end;
end;

function MatrixMerge(var a: Double2D; const b: Double2D): Double2D; inline;
var
  I, acount, bcount, count: Integer;
begin
  acount := Length(a);
  bcount := Length(b);
  SetLength(a, acount + bcount);
  for I := 0 to bcount - 1 do
  begin
    count := Length(b[I]);
    SetLength(a[I + acount], count);
    Move(b[I][0], a[I + acount][0], count * 8);
  end;
  Result := a;
end;

end.
