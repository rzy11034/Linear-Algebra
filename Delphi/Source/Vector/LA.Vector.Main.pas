unit LA.Vector.Main;

interface

uses
  SysUtils,
  LA.Vector;

procedure Main;

implementation

procedure Main;
var
  vec1, vec2, zero: TVector;
begin
  vec1 := TVector.Create([5, 2]);
  WriteLn(vec1.ToString);
  WriteLn(vec1.Length);
  WriteLn(Format('vec[0] = %s, vec[1] = %s', [vec1[0].ToString, vec1[1].ToString]));

  vec2 := TVector.Create([3, 1]);

  WriteLn(Format('%s + %s = %s', [vec1.ToString, vec2.ToString, (vec1 + vec2).ToString]));
  WriteLn(Format('%s - %s = %s', [vec1.ToString, vec2.ToString, (vec1 - vec2).ToString]));

  WriteLn(Format('%S * %s = %s', [(3).ToString, vec1.ToString, (3 * vec1).ToString]));
  WriteLn(Format('%s * %s = %s', [vec1.ToString, (3).ToString, (vec1 * 3).ToString]));

  WriteLn(Format('+%s = %s', [vec1.ToString, (+vec1).ToString]));
  WriteLn(Format('-%s = %s', [vec1.ToString, (-vec1).ToString]));

  zero := TVector.zero(2);
  WriteLn(zero.ToString);
  WriteLn(Format('%s + %s = %s', [vec1.ToString, zero.ToString, (vec1 + zero).ToString]));

  WriteLn(Format('norm(%s) = %s', [vec1.ToString, vec1.Norm.ToString]));
  WriteLn(Format('norm(%s) = %s', [vec2.ToString, vec2.Norm.ToString]));
  WriteLn(Format('norm(%s) = %s', [zero.ToString, zero.Norm.ToString]));

  WriteLn(Format('normalize(%s) = %s', [vec1.ToString, vec1.Normalize.ToString]));
  WriteLn(vec1.Normalize.Norm.ToString);

  WriteLn(Format('normalize(%s) = %s', [vec2.ToString, vec2.Normalize.ToString]));
  WriteLn(vec2.Normalize.Norm.ToString);

  try
    zero.Normalize
  except
    WriteLn('Cannot normalize zero vector ', zero.ToString);    
  end;

  WriteLn(Format('%s * %s = %s', [vec1.ToString, vec2.ToString, (vec1 * vec2).ToString]));
  WriteLn(vec1.Dot(vec2).ToString);
end;

end.
