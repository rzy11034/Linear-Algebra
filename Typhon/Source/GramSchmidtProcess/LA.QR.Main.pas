unit LA.QR.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LA.Matrix,
  LA.GramSchmidtProcess;

procedure Main;

implementation

procedure Main;
var
  a1, a2: TMatrix;
  qr1, qr2: TQR;
begin
  a1 := TMatrix.Create([[1, 1, 2], [1, 1, 0], [1, 0, 0]]);
  qr1 := QR(a1);
  WriteLn(qr1.Q^.ToString, #10);
  WriteLn(qr1.R^.ToString, #10);
  WriteLn(qr1.Q^.Dot(qr1.R^).ToString, #10);

  a2 := TMatrix.Create([[2, -1, -1], [2, 0, 2], [2, -1, 3]]);
  qr2 := QR(a2);
  WriteLn(qr2.Q^.ToString, #10);
  WriteLn(qr2.R^.ToString, #10);
  WriteLn(qr2.Q^.Dot(qr2.R^).ToString, #10);
end;

end.
