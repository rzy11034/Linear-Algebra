unit LA.Vector;

interface

uses
  System.SysUtils;

type
  TLists = TArray<double>;

  TVector = record
  private
    class var __data: TLists;

  public
    class procedure Create(list: TLists); static;

    // <summary> 返回向量长度（有多少个元素） </summary>
    class function Length: integer; static;
    /// <summary> 取向量的第index个元素 </summary>
    class function GetItem(index: integer): double; static;
    class function ToString: string; static;
    class property Item[index: integer]: double read GetItem; default;
  end;

procedure Main;

implementation

procedure Main;
var
  vec: TVector;
begin
  vec.Create([5.1, 3.1]);
  WriteLn(vec.ToString);
  WriteLn(vec.Length);
  WriteLn(Format('vec[0] = %s, vec[1] = %s', [vec[0], vec[1]]));
end;

{ TVector }

class procedure TVector.Create(list: TLists);
begin
  __data := list;
end;

class function TVector.GetItem(index: integer): double;
begin
  Result := __data[index];
end;

class function TVector.Length: integer;
begin
  Result := System.Length(__data);
end;

class function TVector.ToString: string;
var
  sb: TStringBuilder;
  i: integer;
begin

  sb := TStringBuilder.Create;
  try
    sb.Append('(');

    for i := 0 to high(__data) do
    begin
      sb.Append(__data[i]);

      if i = high(__data) then
        sb.Append(')')
      else
        sb.Append(', ');
    end;

    Result := sb.ToString;
  finally
    FreeAndNil(sb);
  end;
end;

end.                      
