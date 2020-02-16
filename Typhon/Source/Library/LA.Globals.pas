unit LA.Globals;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

const
  EPSILON = 1E-8;

function Is_zero(x: double): boolean;
function Is_equal(a, b: double): boolean;

implementation

function Is_zero(x: double): boolean;
begin
  Result := Abs(x) < EPSILON;
end;

function Is_equal(a, b: double): boolean;
begin
  Result := Abs(a - b) < EPSILON;
end;

end.
