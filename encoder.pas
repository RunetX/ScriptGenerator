unit encoder;

{$mode objfpc}{$H+}

interface

uses
  Windows;

implementation

function MyEncoder(strUebergabe: string) : string;
begin
    // Ab Lazarus 1.2.0 muss der folgende Absatz eingefügt werden:
    strUebergabe := Utf8ToAnsi(strUebergabe);

    // Bis Lazarus 1.0.14 genügte dieser Teil:
    strUebergabe := strUebergabe + #0;
    CharToOEM(PChar(strUebergabe), @strUebergabe[1]);
    Delete(strUebergabe, Length(strUebergabe), 1);
    Result := strUebergabe;
end;

end.

