unit translit;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils;

function Translit(s: string): string;

implementation

function Translit(s: string): string;
const
  rus: string = 'àáâãäå¸æçèéêëìíîïğñòóôõö÷øùüûúışÿÀÁÂÃÄÅ¨ÆÇÈÉÊËÌÍÎÏĞÑÒÓÔÕÖ×ØÙÜÛÚİŞß (),';
  lat: array[1..70] of string = ('a', 'b', 'v', 'g', 'd', 'e', 'yo', 'zh', 'z', 'i', 'y', 'k',
'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'f', 'kh', 'ts', 'ch', 'sh', 'shch',
'', 'y', '', 'e', 'yu', 'ya', 'A', 'B', 'V', 'G', 'D', 'E', 'Yo', 'Zh', 'Z',
'I', 'Y', 'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U', 'F', 'Kh', 'Ts', 'Ch',
'Sh', 'Shch', '', 'Y', '', 'E', 'Yu', 'Ya', '-', '', '', '');
var
  p, i, l: integer;
begin
  Result := '';
  l := Length(s);
  for i := 1 to l do
  begin
    p := Pos(s[i], rus);
    if p<1 then Result := Result + s[i] else Result := Result + lat[p];
  end;
end;

end.

