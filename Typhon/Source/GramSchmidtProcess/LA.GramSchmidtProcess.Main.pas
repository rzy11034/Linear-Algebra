unit LA.GramSchmidtProcess.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LA.Vector,
  LA.Matrix,
  LA.GramSchmidtProcess;

procedure Main;

implementation

procedure Main;
var
  basis1, basis2, basis3, basis4: TVectorArr;
  res1, res2, res3, res4: TVecList;
  i: integer;
begin
  basis1 := [TVector.Create([2, 1]), TVector.Create([1, 1])];
  res1 := GramSchmidtProcess(basis1);
  for i := 0 to res1.GetSize - 1 do
    WriteLn(res1[i].ToString, ' ');
  WriteLn;

  for i := 0 to res1.GetSize - 1 do
    res1[i] := res1[i] / res1[i].Norm;
  for i := 0 to res1.GetSize - 1 do
    WriteLn(res1[i].ToString, ' ');
  WriteLn;
  WriteLn(res1[0].Dot(res1[1]).ToString, #10);

  //----------------------------------------------------------

  basis2 := [TVector.Create([2, 3]), TVector.Create([4, 5])];
  res2 := GramSchmidtProcess(basis2);
  for i := 0 to res2.GetSize - 1 do
    WriteLn(res2[i].ToString, ' ');
  WriteLn;

  for i := 0 to res2.GetSize - 1 do
    res2[i] := res2[i] / res2[i].Norm;
  for i := 0 to res2.GetSize - 1 do
    WriteLn(res2[i].ToString, ' ');
  WriteLn;
  WriteLn(res2[0].Dot(res2[1]), #10);

  //----------------------------------------------------------

  basis3 := [TVector.Create([1, 0, 1]), TVector.Create([3, 1, 1]), TVector.Create([-1, -1, -1])];
  res3 := GramSchmidtProcess(basis3);
  for i := 0 to res3.GetSize - 1 do
    WriteLn(res3[i].ToString, ' ');
  WriteLn;

  for i := 0 to res3.GetSize - 1 do
    res3[i] := res3[i] / res3[i].Norm;
  for i := 0 to res3.GetSize - 1 do
    WriteLn(res3[i].ToString, ' ');
  WriteLn;
  WriteLn(res3[0].Dot(res3[1]));
  WriteLn(res3[0].Dot(res3[2]));
  WriteLn(res3[1].Dot(res3[2]));
  WriteLn;

  //------------------------------------------------------------

  basis4 := [TVector.Create([1, 1, 5, 2]), TVector.Create([-3, 3, 4, -2]),
    TVector.Create([-1, -2, 2, 5])];
  res4 := GramSchmidtProcess(basis4);
  for i := 0 to res4.GetSize - 1 do
    WriteLn(res4[i].ToString, ' ');
  WriteLn;

  for i := 0 to res4.GetSize - 1 do
    res4[i] := res4[i] / res4[i].Norm;
  for i := 0 to res4.GetSize - 1 do
    WriteLn(res4[i].ToString, ' ');
  WriteLn;
  WriteLn(res4[0].Dot(res4[1]));
  WriteLn(res4[0].Dot(res4[2]));
  WriteLn(res4[1].Dot(res4[2]));
  WriteLn;
end;

end.
