unit LA.Utils;

interface

uses
  System.SysUtils;

type
  TLAUtils = class
  public
    class procedure DrawLine;
  end;

resourcestring
  END_OF_PROGRAM_EN = 'Press any key to continue...';
  END_OF_PROGRAM_CH = '按任意键继续...';

implementation

{ TLAUtils }

class procedure TLAUtils.DrawLine;
var
  i: integer;
begin
  for i := 0 to 70 do
  begin
    write('-');
  end;
  Writeln;
end;

end.
