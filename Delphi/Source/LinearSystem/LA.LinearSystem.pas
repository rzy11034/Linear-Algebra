unit LA.LinearSystem;

interface

uses
  System.SysUtils,
  LA.Matrix,
  LA.Vector;

type
  TLinearSystem = class(TObject)
  type
    TVectorArr = TArray<TVector>;

  private
    __row, __col: integer;

    procedure __forward;
    procedure __backward;
    function __max_row(index: integer): integer;
    procedure __swapRow(var a, b: TVector);

  public
    Ab: TVectorArr;

    constructor Create(mtx: TMatrix; vec: TVector);
    destructor Destroy; override;

    /// <summary> 高斯约旦消元法 </summary>
    procedure Gauss_Jordan_Elimination;

    function ToString: string; override;
  end;

implementation

{ TLinearSystem }

constructor TLinearSystem.Create(mtx: TMatrix; vec: TVector);
var
  tmpList: TVector.TLists;
  tmpVecArr: TVectorArr;
  i: integer;
begin
  if mtx.Row_num <> vec.Len then
    raise Exception.Create('row number of Matrix must be equal to the length of Vector');

  __row := mtx.Row_num;
  __col := mtx.Col_num;

  //TODO : no this restriction
  //if __row <> __col then
  //
  SetLength(tmpVecArr, __row);
  for i := 0 to __row - 1 do
  begin
    tmpList := Copy(mtx.Get_Row_vector(i).UnderlyingList);
    Insert([vec[i]], tmpList, Length(tmpList));
    tmpVecArr[i] := TVector.Create(tmpList);
  end;

  Ab := tmpVecArr;
end;

destructor TLinearSystem.Destroy;
begin
  inherited Destroy;
end;

procedure TLinearSystem.Gauss_Jordan_Elimination;
begin
  __forward;
  __backward;
end;

function TLinearSystem.ToString: string;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create;
  try
    for i := 0 to __row - 1 do
    begin
      for j := 0 to __col - 1 do
      begin
        sb.Append(Ab[i][j]).Append(#9);
      end;

      sb.Append(' |'#9).Append(Ab[i][__col]).Append(#10);
    end;

    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

procedure TLinearSystem.__backward;
var
  i, j: integer;
begin
  for i := __row - 1 downto 0 do
    for j := i - 1 downto 0 do
      Ab[j] := Ab[j] - Ab[j][i] * Ab[i];
end;

procedure TLinearSystem.__forward;
var
  i, j, max_row: integer;
begin
  for i := 0 to __row - 1 do
  begin
    // Ab[i][i]为主元
    max_row := __max_row(i);
    __swapRow(Ab[i], Ab[max_row]);

    // 将主元归为一
    Ab[i] := Ab[i] / Ab[i][i]; // TODO: self.Ab[i][i] == 0?
    for j := i + 1 to __row - 1 do
      Ab[j] := Ab[j] - Ab[j][i] * Ab[i];
  end;
end;

function TLinearSystem.__max_row(index: integer): integer;
var
  best: double;
  ret, i: integer;
begin
  best := Ab[index][index];
  ret := index;

  for i := index + 1 to __row - 1 do
  begin
    if Ab[i][index] > best then
    begin
      best := Ab[i][index];
      ret := i;
    end;
  end;

  Result := ret;
end;

procedure TLinearSystem.__swapRow(var a, b: TVector);
var
  tmp: TVector;
begin
  tmp := a;
  a := b;
  b := tmp;
end;

end.               
