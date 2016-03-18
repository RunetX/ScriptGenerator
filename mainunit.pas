unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Spin, ComCtrls, ExtCtrls, Buttons, INIFiles, Windows, translit,
  aboutunit, lconvencoding, ShellApi, lazutf8;

type
    TFileVersionInfo = record
       Major : Cardinal;
       Minor : Cardinal;
       Revision : Cardinal;
       Build : Cardinal;
    end;

type

  { TMainForm }

  TMainForm = class(TForm)
    AboutMenuItem: TMenuItem;
    ArcNumSpinEdit: TSpinEdit;
    BasesListView: TListView;
    CheckBDBitBtn: TBitBtn;
    Choosev8iMenuItem: TMenuItem;
    Create1CDirsBtn: TButton;
    ExitMenuItem: TMenuItem;
    FileMenuItem: TMenuItem;
    GenerateBtn: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    MainMenu: TMainMenu;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PasswordEdit: TEdit;
    Path7zipEdit: TEdit;
    PathCloudEdit: TEdit;
    SaveDialog1: TSaveDialog;
    ScanBaseTimer: TTimer;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SelectDriveCmbx: TComboBox;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    UsePassCB: TCheckBox;
    v8iFileEdit: TEdit;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure BasesListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure BasesListViewEnter(Sender: TObject);
    procedure BasesListViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BasesListViewClick(Sender: TObject);
    procedure CheckBDBitBtnClick(Sender: TObject);
    procedure Choosev8iMenuItemClick(Sender: TObject);
    procedure Create1CDirsBtnClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GenerateBtnClick(Sender: TObject);
    procedure Path7zipEditClick(Sender: TObject);
    procedure PathCloudEditClick(Sender: TObject);
    procedure ScanBaseTimerTimer(Sender: TObject);
    procedure SelectDriveCmbxChange(Sender: TObject);
    procedure v8iFileEditChange(Sender: TObject);
    function CrDir(Path: String):integer;
    function GetFileVersion(const AFileName:string):TFileVersionInfo;
  private
    { private declarations }
    FListItem: TListItem;
    procedure CheckBases;
    procedure LoadBLFile;
    procedure OpenV8I;
    procedure FillDrives;
    procedure SelectedListItemStateSave;
    procedure SelectedListItemStateRestore;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

function RunAsAdmin(const Handle: Hwnd; const Path, Params: string): Boolean;
var
  sei: TShellExecuteInfoA;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd := Handle;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PAnsiChar(Path);
  sei.lpParameters := PAnsiChar(Params);
  sei.nShow := SW_SHOWNORMAL;
  Result := ShellExecuteExA(@sei);
end;

function TMainForm.GetFileVersion(const AFileName:string):TFileVersionInfo;
  var
    { useful only as long as we don't need to touch different stack pages }
    buf : array[0..3071] of byte;
    bufp : pointer;
    fn : string;
    valsize,
    size : DWORD;
    h : DWORD;
    valrec : PVSFixedFileInfo;
  begin
    //result:=Default(TFileVersionInfo);
    fn:=AFileName;
    UniqueString(fn);
    size:=GetFileVersionInfoSizeA(pchar(fn),@h);
    if size>sizeof(buf) then
      begin
        getmem(bufp,size);
        try
          if GetFileVersionInfoA(pchar(fn),h,size,bufp) then
            if VerQueryValue(bufp,'\',valrec,valsize) then
              with valrec^ do
                Result.Major:= HiWord(valrec^.dwFileVersionMS);
                Result.Minor:=LoWord(valrec^.dwFileVersionMS);
                Result.Revision:=HiWord(valrec^.dwFileVersionLS);
                Result.Build:=LoWord(valrec^.dwFileVersionLS);
        finally
          freemem(bufp);
        end;
      end
    else
      begin
        if GetFileVersionInfoA(pchar(fn),h,size,@buf) then
          if VerQueryValue(@buf,'\',valrec,valsize) then

              Result.Major:= HiWord(valrec^.dwFileVersionMS);
              Result.Minor:=LoWord(valrec^.dwFileVersionMS);
              Result.Revision:=HiWord(valrec^.dwFileVersionLS);
              Result.Build:=LoWord(valrec^.dwFileVersionLS);
      end;
  end;

function TMainForm.CrDir(Path: String):integer;
begin
  if not(DirectoryExists(Path)) then
    if not CreateDir(Path) then
      begin
        ShowMessage('Не удалось создать директорию ' + Path);
        Result := 1;
      end
    else
      Result := 0
  else
    ShowMessage('Директория ' + Path + ' уже создана');
end;

procedure TMainForm.FillDrives;
var
    Drive: Char;
    DriveLetter: string;
    OldMode: Word;
begin
    OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try

    // Search all drive letters
    for Drive := 'A' to 'Z' do
    begin
      DriveLetter := Drive + ':\';

      case GetDriveType(PChar(DriveLetter)) of
       DRIVE_FIXED:     SelectDriveCmbx.Items.Add(DriveLetter);
      end;
    end;

  finally
    // Restores previous Windows error mode.
    SetErrorMode(OldMode);
  end;
end;

procedure TMainForm.SelectDriveCmbxChange(Sender: TObject);
begin
  // Изменилось выбранное значение ComboBox
  // Проверяем, отлично ли оно от пустого
  // Присваиваем значение пути соответствующему Edit
  if SelectDriveCmbx.ItemIndex>-1 then
     begin
       PathCloudEdit.Text := SelectDriveCmbx.Text + '1C\Backups\';
     end;
end;

procedure TMainForm.CheckBases;
var
 i: Integer;
 BasePath: String;
begin
    if BasesListView.Items.Count>0 then
        begin
            for i:=0 to BasesListView.Items.Count-1 do
                begin
                    BasePath := BasesListView.Items.Item[i].SubItems[0];
                    BasePath := Copy(BasePath, 7, Length(BasePath)-8);

                    BasesListView.Items.Item[i].Checked :=
                    not(FileExistsUTF8(BasePath + '\1Cv8tmp.1CD'));
                end;
        end;
end;

procedure TMainForm.OpenV8I;
begin
    OpenDialog1.Filter:= 'Файл списка информационных баз|ibases.v8i';
    if OpenDialog1.Execute then
        v8iFileEdit.Text := OpenDialog1.FileName;
end;

procedure TMainForm.LoadBLFile;
var
 IniF: TINIFile;
 FileStream: TFileStream;
 FileName: String;
 MarkHolder: Cardinal;
 IniSections: TStringList;
 IniSection: String;
 Reply: Integer;
begin
  // Чтение списка информационных баз
  // и вывод их в ListView
  IniSections := TStringList.Create;

  if(FileExistsUTF8(v8iFileEdit.Text))then
  begin
    try
      FileName := UTF8ToSys(v8iFileEdit.Text);
      // Финт ушами для корректного чтения файлов UTF-8 with BOM
      FileStream := TFileStream.Create(FileName, fmOpenReadWrite);
      // Здесь необходимо добавить корректную обработку ошибки
      // при чтении пустого файла
      MarkHolder := FileStream.ReadDWord;
      if (MarkHolder and $00BFBBEF) = $00BFBBEF then
        FileStream.Position := 3
      else
        FileStream.Position := 0;

      IniF := TIniFile.Create(FileStream);
      IniF.ReadSections(IniSections);

      BasesListView.Clear;

      for IniSection in IniSections do
        begin
          with BasesListView.Items.Add do
          begin
            Caption := IniSection;
            SubItems.Add(IniF.ReadString(IniSection, 'Connect', ''));
          end;
        end;
      BasesListView.Items.Item[0].Selected := true;
      BasesListView.Items.Item[0].Focused  := true;
      StatusBar1.SimpleText:= BasesListView.Items[0].SubItems[0];
    finally
      IniF.Free;
      FileStream.Free;
    end;
  end else
        begin
                Reply := Application.MessageBox('Желаете выбрать файл вручную?',
                    'Файл списка информационных баз не найден...', MB_ICONQUESTION + MB_YESNO);
                if Reply = IDYES then
                    OpenV8I;
        end;
end;

procedure TMainForm.v8iFileEditChange(Sender: TObject);
begin
    LoadBLFile;
end;

procedure TMainForm.GenerateBtnClick(Sender: TObject);
Var
 Strings: TStrings;
 TransStr, BasePath, ArcPath, BackupPath, TransBaseName: String;
 StrCompRes, Reply, BoxStyle: Integer;
begin
  StrCompRes := CompareStr(PathCloudEdit.Text, 'Путь к каталогу с архивами');
  if StrCompRes<>0 then
  begin
    TransStr             := BasesListView.Selected.Caption;
    TransBaseName        := AnsiToUtf8(translit.Translit(Utf8ToAnsi(TransStr)));
    SaveDialog1.FileName := UTF8ToSys('1C.Backup-' + TransBaseName + '.bat');

    if SaveDialog1.Execute then
      begin
          Strings := TStringList.Create;

          BasePath := BasesListView.Selected.SubItems[0];
          BasePath := Copy(BasePath, 7, Length(BasePath)-8);

          ArcPath   := PathCloudEdit.Text + TransBaseName;

          if not(DirectoryExists(UTF8ToSys(ArcPath))) then
                if not CreateDir(UTF8ToSys(ArcPath)) then
                    ShowMessage('Не получилось создать директорию базы в Backups!');

          With Strings do
              begin
                Add('set sHour=%TIME:~0,2%');
                Add('set sMinute=%TIME:~3,2%');
                Add('set s7ZPath="'     + Path7zipEdit.Text     + '"');
                Add('set sCloudPath="'  + ArcPath  + '"');
                Add('set sDBPath="'     + BasePath              + '\1Cv8.1CD"');
                if UsePassCB.Checked then
                    Add('set sDBPass="'     + PasswordEdit.Text     + '"');
                Add('set /a iCount = '  + ArcNumSpinEdit.ValueToStr(ArcNumSpinEdit.Value));
                Add('');
                Add('for /f "skip=%iCount% usebackq delims=" %%i in (');
                Add('	`dir /b /a:-d /o:-d /t:w %sCloudPath%`');
                Add(') do del /f /q %sCloudPath%\%%~i');
                Add('');
                if UsePassCB.Checked then
                    Add('%s7ZPath% a %sCloudPath%\baza[%date%-%sHour: =0%%sMinute%].7z %sDBPath% -p%sDBPass%')
                else
                    Add('%s7ZPath% a %sCloudPath%\baza[%date%-%sHour: =0%%sMinute%].7z %sDBPath%');
              end;
           Try
             Strings.Text := UTF8ToCP866(Strings.Text);
             BackupPath := UTF8ToSys(SaveDialog1.FileName);

             if FileExists(BackupPath) then
               begin
                BoxStyle := MB_ICONQUESTION + MB_YESNO;
                Reply := Application.MessageBox('Файл с таким именем уже существует. Перезаписать?',
                    'Перезаписать файл?', BoxStyle);
                if Reply = IDYES then
                    Strings.SaveToFile(BackupPath);
               end
             else
                Strings.SaveToFile(BackupPath);


          Finally
              Strings.Free;
          End;
      end;
  end else ShowMessage('Не задан каталог с архивами!');
end;

procedure TMainForm.Path7zipEditClick(Sender: TObject);
begin
  OpenDialog1.Filter:= 'Исполняемый файл архиватора 7zip|7z.exe';
  if OpenDialog1.Execute then
      begin
        Path7zipEdit.Text := OpenDialog1.Filename;
      end;
end;

procedure TMainForm.PathCloudEditClick(Sender: TObject);
var
   PathLen: Integer;
begin
   if SelectDirectoryDialog1.Execute then
      begin
        PathLen := Length(SelectDirectoryDialog1.FileName);
        if PathLen>3 then
            PathCloudEdit.Text := SelectDirectoryDialog1.FileName
        else
            begin
               PathCloudEdit.Text := Copy(SelectDirectoryDialog1.FileName, 0,(PathLen-1));
            end;
      end;
end;

procedure TMainForm.ScanBaseTimerTimer(Sender: TObject);
begin
    CheckBases;
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Choosev8iMenuItemClick(Sender: TObject);
begin
  OpenV8I;
end;

procedure TMainForm.Create1CDirsBtnClick(Sender: TObject);
var
  DriveLetter: String;
  ErrorCounter: Integer;
begin

  ErrorCounter := 0;

  if SelectDriveCmbx.ItemIndex>-1 then
     begin
        DriveLetter := SelectDriveCmbx.Text;
        ErrorCounter := ErrorCounter + CrDir(DriveLetter + '1C');
        ErrorCounter := ErrorCounter + CrDir(DriveLetter + '1C\Addons');
        ErrorCounter := ErrorCounter + CrDir(DriveLetter + '1C\Backups');
        ErrorCounter := ErrorCounter + CrDir(DriveLetter + '1C\Bases');
        ErrorCounter := ErrorCounter + CrDir(DriveLetter + '1C\Updates');
        ErrorCounter := ErrorCounter + CrDir(DriveLetter + '1C\Updates\Cfgs');
        ErrorCounter := ErrorCounter + CrDir(DriveLetter + '1C\Updates\Plts');

        if ErrorCounter=0 then
          ShowMessage('Директории 1С созданы.');
     end
  else
    ShowMessage('Выберите диск для создания директорий 1С!');
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  AboutForm.Show;
end;

procedure TMainForm.BasesListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if (Change=ctState) then
     SelectedListItemStateSave;
end;

procedure TMainForm.BasesListViewEnter(Sender: TObject);
begin
  if BasesListView.Selected = nil then
  begin
    FListItem :=BasesListView.Items[0]; // Initialization
    SelectedListItemStateRestore;
  end;
end;

procedure TMainForm.BasesListViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if BasesListView.GetItemAt(X,Y) = nil then
    begin
      SelectedListItemStateRestore;
    end;
end;

procedure TMainForm.SelectedListItemStateRestore;
begin
  BasesListView.Selected := FListItem;
  BasesListView.Selected.Focused := True; // Always focused
end;

procedure TMainForm.SelectedListItemStateSave;
begin
  FListItem := BasesListView.Selected;
end;

procedure TMainForm.BasesListViewClick(Sender: TObject);
begin
 if Assigned(BasesListView.Selected) then
   StatusBar1.SimpleText := BasesListView.Items[BasesListView.Selected.Index].SubItems[0]
end;

procedure TMainForm.CheckBDBitBtnClick(Sender: TObject);
var
  PFPath:     String;
  FilePath:   String;
  ComLine:    String;
  ComLineSys: String;
  FileVer:TFileVersionInfo;
begin
  ShowMessage('Сделайте копию перед проверкой!');

  ComLine := '1 ';
  ComLine := ComLine + '"' +
             Copy(StatusBar1.SimpleText, 7, Length(StatusBar1.SimpleText)-8)
             + '\1Cv8.1CD" ';

  PFPath   := GetEnvironmentVariableUTF8('PROGRAMFILES');
  FilePath := PFPath + '\1cv8\common\1cestart.exe';

  if FileExists(FilePath) then
    begin
      FileVer  := GetFileVersion(FilePath);
      ComLine  := ComLine + IntToStr(FileVer.Major)+'.'+IntToStr(FileVer.Minor)+'.'+
                        IntToStr(FileVer.Revision)+'.'+IntToStr(FileVer.Build);
      ComLineSys := UTF8ToCP1251(ComLine);
      if FileExists('runchdbfl.exe') then
        if SysUtils.Win32MajorVersion >= 6 then
            RunAsAdmin(MainForm.Handle, 'runchdbfl.exe', ComLineSys)
        else
            ShellExecute(0,nil, PChar('runchdbfl.exe'),PChar(ComLineSys),nil,1)
      else
        ShowMessage('Не найден стартер chdbfl.exe!');
    end
  else
    ShowMessage('Файл 1cestart.exe не найден!');
end;

function GetWin(Comand: string): string;
var
  buff: array [0 .. $FF] of char;
begin
  ExpandEnvironmentStrings(PChar(Comand), buff, SizeOf(buff));
  Result := buff;
end;

procedure TMainForm.FormShow(Sender: TObject);
Var
 FilePath: String;
 PartPath: String;

 AppDataPath: String;
begin

  AppDataPath := GetEnvironmentVariableUTF8('APPDATA');
  //GetWin('%AppData%');
  v8iFileEdit.Text := AppDataPath + '\1C\1CEStart\ibases.v8i';

  // Проверка установленного архиватора 7zip
  PartPath := GetEnvironmentVariableUTF8('PROGRAMFILES');
  FilePath := PartPath + '\7-Zip\7z.exe';
  if(FileExistsUTF8(FilePath))then
      Path7zipEdit.Text := FilePath
  else
    begin
      PartPath := Copy(PartPath, 0,(length(PartPath)-6));
      FilePath := PartPath + '\7-Zip\7z.exe';
      if(FileExistsUTF8(FilePath))then
         Path7zipEdit.Text := FilePath
    end;

  // Установка пути сохранения скрипта по умолчанию
  SaveDialog1.InitialDir := FilePath;

  // Проверка запущенных баз
  CheckBases;

  // Заполнение списка системных дисков
  FillDrives;
end;

end.

