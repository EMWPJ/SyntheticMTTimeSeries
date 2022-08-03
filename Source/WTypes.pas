unit WTypes;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  System.Variants, FMX.Types, System.Generics.Collections, System.JSON;

type

  TComplex = record
    Real: Double;
    Imag: Double;
  Public
    constructor Create(rvalue, ivalue: Double);
    constructor CreateAP(Avalue, Pvalue: Double);
    constructor CreateFromString(str: String);
    constructor CreateFromXML(str: String);
    Class Operator Negative(z: TComplex): TComplex; OverLoad;
    Class Operator Add(Z1, Z2: TComplex): TComplex; OverLoad;
    Class Operator Add(z: TComplex; R: Double): TComplex; OverLoad;
    Class Operator Add(R: Double; z: TComplex): TComplex; OverLoad;
    Class Operator Subtract(Z1, Z2: TComplex): TComplex; OverLoad;
    Class Operator Subtract(z: TComplex; R: Double): TComplex; OverLoad;
    Class Operator Subtract(R: Double; z: TComplex): TComplex; OverLoad;
    Class Operator Multiply(Z1, Z2: TComplex): TComplex; OverLoad;
    Class Operator Multiply(z: TComplex; R: Double): TComplex; OverLoad;
    Class Operator Multiply(R: Double; z: TComplex): TComplex; OverLoad;
    Class Operator Divide(Z1, Z2: TComplex): TComplex; OverLoad;
    Class Operator Divide(z: TComplex; R: Double): TComplex; OverLoad;
    Class Operator Divide(R: Double; Z2: TComplex): TComplex; OverLoad;
    Class operator Equal(Z1, Z2: TComplex): Boolean; OverLoad;
    Class Operator Equal(z: TComplex; R: Double): Boolean; OverLoad;
    Class operator NotEqual(Z1, Z2: TComplex): Boolean; OverLoad;
    Class Operator NotEqual(z: TComplex; R: Double): Boolean; OverLoad;
    function Supplementary: TComplex;
    Procedure ConjugateSelf;
    Function Conjugate: TComplex;
    function ConjugateUnitAngle: TComplex;
    Function Module: Double;
    Function Module2: Double;
    Function Phase: Double; { 以弧度为单位,-Pi~Pi之间 }
    Function PhaseDeg: Double;
    Function PhaseDegFirst: Double;
    Function Sin: TComplex;
    Function Cos: TComplex;
    Function Tan: TComplex;
    Function Sinh: TComplex;
    Function Cosh: TComplex;
    Function Tanh: TComplex;
    Function Sqr: TComplex;
    Function Sqrt: TComplex;
    Function Ln: TComplex;
    Function Exp: TComplex;
    Function ToString: String;
    Function ToXML: String;
    Procedure FromString(str: String);
    Procedure FromXML(str: String);
    procedure SupplementarySelf;
  End;

  PComplex = ^TComplex;


  ShortInt1D = TArray<ShortInt>;
  Byte1D = TArray<Byte>;
  SmallInt1D = TArray<SmallInt>;
  Word1D = TArray<Word>;
  Integer1D = TArray<Integer>;
  Cardinal1D = TArray<Cardinal>;
  Int641D = TArray<Int64>;
  UInt641D = TArray<UInt64>;
  Double1D = TArray<Double>;
  Single1D = TArray<Single>;
  Boolean1D = TArray<Boolean>;
  String1D = TArray<String>;
  TDateTime1D = TArray<TDateTime>;

  PShortInt1D = ^ShortInt1D;
  PByte1D = ^Byte1D;
  PSmallInt1D = ^SmallInt1D;
  PWord1D = ^Word1D;
  PInteger1D = ^Integer1D;
  PCardinal1D = ^Cardinal1D;
  PInt641D = ^Int641D;
  PUInt641D = ^UInt641D;
  PDouble1D = ^Double1D;
  PSingle1D = ^Single1D;
  PBoolean1D = ^Boolean1D;
  PString1D = ^String1D;

  ShortInt2D = TArray<ShortInt1D>;
  Byte2D = TArray<Byte1D>;
  SmallInt2D = TArray<SmallInt1D>;
  Word2D = TArray<Word1D>;
  Integer2D = TArray<Integer1D>;
  Cardinal2D = TArray<Cardinal1D>;
  Int642D = TArray<Int641D>;
  UInt642D = TArray<UInt641D>;
  Double2D = TArray<Double1D>;
  Single2D = TArray<Single1D>;
  Boolean2D = TArray<Boolean1D>;
  String2D = TArray<String1D>;
  TDateTime2D = TArray<TDateTime1D>;

  PShortInt2D = ^ShortInt2D;
  PByte2D = ^Byte2D;
  PSmallInt2D = ^SmallInt2D;
  PWord2D = ^Word2D;
  PInteger2D = ^Integer2D;
  PCardinal2D = ^Cardinal2D;
  PInt642D = ^Int642D;
  PUInt642D = ^UInt642D;
  PDouble2D = ^Double2D;
  PSingle2D = ^Single2D;
  PBoolean2D = ^Boolean2D;
  PString2D = ^String2D;

  ShortInt3D = TArray<ShortInt2D>;
  Byte3D = TArray<Byte2D>;
  SmallInt3D = TArray<SmallInt2D>;
  Word3D = TArray<Word2D>;
  Integer3D = TArray<Integer2D>;
  Cardinal3D = TArray<Cardinal2D>;
  Int643D = TArray<Int642D>;
  UInt643D = TArray<UInt642D>;
  Double3D = TArray<Double2D>;
  Single3D = TArray<Single2D>;
  Boolean3D = TArray<Boolean2D>;
  String3D = TArray<String2D>;
  TDateTime3D = TArray<TDateTime2D>;

  ShortInt4D = TArray<ShortInt3D>;
  Byte4D = TArray<Byte3D>;
  SmallInt4D = TArray<SmallInt3D>;
  Word4D = TArray<Word3D>;
  Integer4D = TArray<Integer3D>;
  Cardinal4D = TArray<Cardinal3D>;
  Int644D = TArray<Int643D>;
  UInt644D = TArray<UInt643D>;
  Double4D = TArray<Double3D>;
  Single4D = TArray<Single3D>;
  Boolean4D = TArray<Boolean3D>;
  String4D = TArray<String3D>;
  TDateTime4D = TArray<TDateTime3D>;

  PShortInt3D = ^ShortInt3D;
  PByte3D = ^Byte3D;
  PSmallInt3D = ^SmallInt3D;
  PWord3D = ^Word3D;
  PInteger3D = ^Integer3D;
  PCardinal3D = ^Cardinal3D;
  PInt643D = ^Int643D;
  PUInt643D = ^UInt643D;
  PDouble3D = ^Double3D;
  PSingle3D = ^Single3D;
  PBoolean3D = ^Boolean3D;
  PString3D = ^String3D;

  DoubleP2D = TArray<PDouble1D>;
  DoubleP3D = TArray<DoubleP2D>;

  TComplex1D = TArray<TComplex>;
  TComplex2D = TArray<TComplex1D>;
  TComplex3D = TArray<TComplex2D>;
  TComplex4D = TArray<TComplex3D>;

  Integer1DHelper = record helper for Integer1D
    procedure Clear;
    function Count: Integer;
    procedure Append(app: Integer);
  end;

  Double1DHelper = record helper for Double1D
    procedure SaveTo(const fileName: String);
  end;

type
  TBytesHelper = record helper for TBytes
    procedure Append(bts: TBytes);
    function Count: Int64;
  end;

function GUIDStr: string;

const
  CMPLX_I: TComplex = (Real: 0; Imag: 1);
  CMPLX_0: TComplex = (Real: 0; Imag: 0);
  CMPLX_1: TComplex = (Real: 1; Imag: 0);
  CMPLX_2: TComplex = (Real: 2; Imag: 0);
  CMPLX_00: TComplex = (Real: 1E-20; Imag: 0);
  CMPLX_Max: TComplex = (Real: MaxDouble; Imag: MaxDouble);
  CMPLX_NMax: TComplex = (Real: - MaxDouble; Imag: - MaxDouble);

implementation

function GUIDStr: string;
var
  gid: TGUID;
begin
  CreateGUID(gid);
  Result := GUIDToString(gid);
end;

class Operator TComplex.Add(Z1, Z2: TComplex): TComplex;
begin
  Result.Real := Z1.Real + Z2.Real;
  Result.Imag := Z1.Imag + Z2.Imag;
end;

class Operator TComplex.Add(z: TComplex; R: Double): TComplex;
begin
  Result.Real := R + z.Real;
  Result.Imag := z.Imag;
end;

class Operator TComplex.Add(R: Double; z: TComplex): TComplex;
begin
  Result := z + R;
end;

function TComplex.Conjugate: TComplex;
begin
  Result.Real := Real;
  Result.Imag := -Imag;
end;

procedure TComplex.ConjugateSelf;
begin
  Self.Imag := -Self.Imag;
end;

function TComplex.Cos: TComplex;
Var
  Z1, Z2: TComplex;
  Eip, Ein: TComplex;
Begin
  Eip := TComplex.Create(0, 1);
  Ein := TComplex.Create(0, -1);
  Z1 := Eip * Self;
  Z2 := Ein * Self;
  Result := Z1.Exp + Z2.Exp;
  With Result Do
  Begin
    Real := Real / 2;
    Imag := Imag / 2;
  end;
End;

function TComplex.Cosh: TComplex;
begin
  Result := (Self.Exp + (-Self).Exp) / 2;
end;

constructor TComplex.Create(rvalue, ivalue: Double);
begin
  Real := rvalue;
  Imag := ivalue;
end;

constructor TComplex.CreateAP(Avalue, Pvalue: Double);
begin
  Real := Avalue * System.Cos(Pvalue);
  Imag := Avalue * System.Sin(Pvalue);
end;

constructor TComplex.CreateFromString(str: String);
Var
  strs: TArray<string>;
begin
  Try
    strs := str.Split([' ']);
    Self.Real := strs[0].ToDouble;
    Self.Imag := strs[1].ToDouble;
  Except
    Raise;
  end;
end;

constructor TComplex.CreateFromXML(str: String);
Var
  strs: TArray<string>;
begin
  Try
    strs := str.Split([',', ' ']);
    Real := strs[0].ToDouble;
    Imag := strs[1].ToDouble;
  Except
    Raise;
  end;
end;

function TComplex.ConjugateUnitAngle: TComplex;
var
  m: Double;
begin
  m := Self.Module;
  Result.Real := Real / m;
  Result.Imag := -Imag / m;
end;

class Operator TComplex.Divide(Z1, Z2: TComplex): TComplex;

Var
  z: TComplex;
  Mode_Z2: Double;
Begin
  Try
    Mode_Z2 := System.Sqr(Z2.Module);
    z := Z2.Conjugate;
    Result := Z1 * z;
    With Result Do
    Begin
      Real := Real / Mode_Z2;
      Imag := Imag / Mode_Z2;
    End;
  Except
    Raise;
  End;
End;

class Operator TComplex.Divide(z: TComplex; R: Double): TComplex;
begin
  Try
    With Result Do
    Begin
      Real := z.Real / R;
      Imag := z.Imag / R;
    End;
  Except
    Raise;
  End;
end;

class Operator TComplex.Divide(R: Double; Z2: TComplex): TComplex;

Var
  Z1: TComplex;
Begin
  Try
    Z1.Real := R;
    Z1.Imag := 0;
    Result := Z1 / Z2;
  Except
    Raise;
  End;
End;

class operator TComplex.Equal(z: TComplex; R: Double): Boolean;
begin
  Result := (z.Imag = 0) and (z.Real = R);
end;

class operator TComplex.Equal(Z1, Z2: TComplex): Boolean;
begin
  Result := (Z1.Real = Z2.Real) and (Z1.Imag = Z2.Imag);
end;

function TComplex.Exp: TComplex;
// var
// tmp: Double;
begin
  Result.Real := System.Exp(Real) * System.Cos(Imag);
  Result.Imag := System.Exp(Real) * System.Sin(Imag);
  // if (Real < 10) and (Imag < 10) then
  // Begin
  // Result.Real := System.Exp(Real) * System.Cos(Imag);
  // Result.Imag := System.Exp(Real) * System.Sin(Imag);
  // end
  // Else
  // Begin
  // Result.Real := System.Exp(Real + System.Ln(System.Cos(Imag)));
  // Result.Imag := System.Exp(Real + System.Ln(System.Sin(Imag)));
  // end;
end;

procedure TComplex.FromString(str: String);
Var
  strs: TArray<string>;
begin
  Try
    strs := str.Split(['(', ',', ')']);
    Real := strs[1].ToDouble;
    Imag := strs[2].ToDouble;
  Except
    Raise;
  end;
end;

procedure TComplex.FromXML(str: String);
Var
  strs: TArray<string>;
begin
  Try
    strs := str.Split([',', ' ']);
    Real := strs[0].ToDouble;
    Imag := strs[1].ToDouble;
  Except
    Raise;
  end;
end;

function TComplex.Ln: TComplex;
begin
  With Result Do
  Begin
    Real := System.Ln(Self.Module);
    Imag := Self.Phase;
  End;
end;

class Operator TComplex.Multiply(Z1, Z2: TComplex): TComplex;
begin
  Result.Real := Z1.Real * Z2.Real - Z1.Imag * Z2.Imag;
  Result.Imag := Z1.Imag * Z2.Real + Z1.Real * Z2.Imag;
end;

class Operator TComplex.Multiply(z: TComplex; R: Double): TComplex;
begin
  Result.Real := R * z.Real;
  Result.Imag := R * z.Imag;
end;

function TComplex.Module: Double;
begin
  Result := System.Sqrt(System.Sqr(Real) + System.Sqr(Imag));
end;

function TComplex.Module2: Double;
begin
  Result := System.Sqr(Real) + System.Sqr(Imag);
end;

class Operator TComplex.Multiply(R: Double; z: TComplex): TComplex;
begin
  Result.Real := R * z.Real;
  Result.Imag := R * z.Imag;
end;

class Operator TComplex.Negative(z: TComplex): TComplex;
begin
  With Result Do
  Begin
    Real := -z.Real;
    Imag := -z.Imag;
  end;
end;

class operator TComplex.NotEqual(z: TComplex; R: Double): Boolean;
begin
  Result := (z.Real <> R) or (z.Imag <> 0);
end;

class operator TComplex.NotEqual(Z1, Z2: TComplex): Boolean;
begin
  Result := (Z1.Real <> Z2.Real) or (Z1.Imag <> Z2.Imag);
end;

function TComplex.Phase: Double; { -Pi~Pi之间 }
begin
  Result := Arctan2(Imag, Real);
  Exit;
  Try
    If Real <> 0 Then
      Result := Arctan2(Imag, Real)
    Else
      Result := PI / 2.;
    if Real <= 0 Then
    Begin
      If Imag > 0 Then
        Result := PI + Result;
      If Imag <= 0 Then
        Result := Result - PI;
    End;
  Except
    Raise;
  end;
end;

function TComplex.PhaseDeg: Double;
begin
  Result := Self.Phase * 180 / System.PI;
  if Real = 0 then
  begin
    if Imag = 0 then
    begin
      Result := -1800;
    end;
  end;
end;

function TComplex.PhaseDegFirst: Double;
var
  a, b: Double;
  azim, strike: Double;
begin
  azim := Self.PhaseDeg;
  strike := azim;
  if strike <= -90 then
  begin
    strike := strike + 180;
  end;
  if strike > 90 then
  begin
    strike := strike - 180;
  end;
  a := System.Math.Tan(strike * System.PI / 180);
  b := abs(System.Math.Tan(azim * System.PI / 180));
  if abs(b - a) > 0.00001 then
  begin
    strike := -strike;
  end;
  Result := strike;
end;

function TComplex.Sin: TComplex;
Var
  Z1, Z2: TComplex;
  Eip, Ein: TComplex;
Begin
  Eip := TComplex.Create(0, 1);
  Ein := TComplex.Create(0, -1);
  Z1 := Eip * Self;
  Z2 := Ein * Self;
  Result := Z1.Exp - Z2.Exp;
  With Result Do
  Begin
    Real := Real / 2;
    Imag := Imag / 2;
  end;
End;

function TComplex.Sinh: TComplex;
begin
  Result := (Self.Exp - (-Self).Exp) / 2;
end;

function TComplex.Sqr: TComplex;
begin
  Result.Real := System.Sqr(Self.Real) - System.Sqr(Self.Imag);
  Result.Imag := 2 * Self.Real * Self.Imag;
end;

function TComplex.Sqrt: TComplex;
Var
  Mdl, Phs: Double;
Begin
  Mdl := System.Sqrt(Self.Module);
  Phs := Self.Phase / 2.0;
  If System.Cos(Phs) < 0 then
  Begin
    If Phs > PI then
      Phs := Phs - PI
    else
      Phs := Phs + PI;
  End;
  With Result Do
  Begin
    Real := Mdl * System.Cos(Phs);
    Imag := Mdl * System.Sin(Phs);
  end;
End;

class Operator TComplex.Subtract(R: Double; z: TComplex): TComplex;
begin
  Result.Real := R - z.Real;
  Result.Imag := z.Imag;
end;

function TComplex.Supplementary: TComplex;
begin
  Result.Real := -Self.Real;
  Result.Imag := -Self.Imag;
end;

procedure TComplex.SupplementarySelf;
begin
  Self.Real := -Self.Real;
end;

function TComplex.Tan: TComplex;
begin
  Result := Self.Sin / Self.Cos;
end;

function TComplex.Tanh: TComplex;
begin
  if Self.Real > 100 then
  begin
    Result := CMPLX_1;
  end
  else if Self.Real < -100 then
  begin
    Result := -CMPLX_1;
  end
  else
  begin
    Result := (Self.Exp - (-Self).Exp) / (Self.Exp + (-Self).Exp);
  end;
end;

function TComplex.ToString: String;
begin
  Result := '(' + Real.ToString + ',' + Imag.ToString + ')';
end;

function TComplex.ToXML: String;
begin
  Result := Real.ToString + ',' + Imag.ToString;
end;

class Operator TComplex.Subtract(Z1, Z2: TComplex): TComplex;
begin
  Result.Real := Z1.Real - Z2.Real;
  Result.Imag := Z1.Imag - Z2.Imag;
end;

class Operator TComplex.Subtract(z: TComplex; R: Double): TComplex;
begin
  Result.Real := z.Real - R;
  Result.Imag := z.Imag;
end;

{ Integer1DHelper }

procedure Integer1DHelper.Append(app: Integer);
var
  ct: Integer;
begin
  ct := Self.Count;
  SetLength(Self, ct + 1);
  Self[ct] := app;
end;

procedure Integer1DHelper.Clear;
begin
  SetLength(Self, 0);
end;

function Integer1DHelper.Count: Integer;
begin
  Result := Length(Self);
end;

{ Double1DHelper }

procedure Double1DHelper.SaveTo(const fileName: String);
var
  strs: TStringList;
  I, Count: Integer;
begin
  strs := TStringList.Create;
  Count := Length(Self);
  for I := 0 to Count - 1 do
  begin
    strs.Add(Self[I].ToString);
  end;
  strs.SaveToFile(fileName);
end;

{ TBytesHelper }

procedure TBytesHelper.Append(bts: TBytes);
var
  applen, len: Integer;
begin
  len := Length(Self);
  applen := Length(bts);
  SetLength(Self, len + applen);
  Move(bts[0], Self[len], applen);
end;

function TBytesHelper.Count: Int64;
begin
  Result := Length(Self);
end;

end.
