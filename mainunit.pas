unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, Spin, ComCtrls, ExtCtrls, INIFiles, Windows, translit, aboutunit, lconvencoding;

type

  { TMainForm }

  TMainForm = class(TForm)
    UsePassCB: TCheckBox;
    Label6: TLabel;
    AboutMenuItem: TMenuItem;
    v8iFileEdit: TEdit;
    Label5: TLabel;
    BasesListView: TListView;
    FileMenuItem: TMenuItem;
    Choosev8iMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    PasswordEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    PathCloudEdit: TEdit;
    Path7zipEdit: TEdit;
    GenerateBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu: TMainMenu;
    SaveDialog1: TSaveDialog;
    ArcNumSpinEdit: TSpinEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    ScanBaseTimer: TTimer;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure Choosev8iMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GenerateBtnClick(Sender: TObject);
    procedure Path7zipEditClick(Sender: TObject);
    procedure PathCloudEditChange(Sender: TObject);
    procedure PathCloudEditClick(Sender: TObject);
    procedure ScanBaseTimerTimer(Sender: TObject);
    procedure CheckBases;
    procedure LoadBLFile;
    procedure OpenV8I;
    procedure v8iFileEditChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

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
 TransStr, BasePath, YaDPath, ArcPath, BackupPath, TransBaseName: String;
 StrCompRes, Reply, BoxStyle: Integer;
begin
  StrCompRes := CompareStr(PathCloudEdit.Text, 'Кликните здесь для выбора директории Яндекс.Диска!');
  if StrCompRes<>0 then
  begin
    TransStr             := BasesListView.Selected.Caption;
    TransBaseName        := AnsiToUtf8(translit.Translit(Utf8ToAnsi(TransStr)));
    SaveDialog1.FileName := UTF8ToSys('1Cbackup-' + TransBaseName + '.bat');

    if SaveDialog1.Execute then
      begin
          Strings := TStringList.Create;

          BasePath := BasesListView.Selected.SubItems[0];
          BasePath := Copy(BasePath, 7, Length(BasePath)-8);

          YaDPath   := PathCloudEdit.Text;
          ArcPath   := YaDPath + '\1Cbackup\' + TransBaseName;

          if not(DirectoryExists(UTF8ToSys(ArcPath))) then
                if not CreateDir(UTF8ToSys(ArcPath)) then
                    ShowMessage('Не получилось создать директорию базы в 1Cbackup!');

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
  end else ShowMessage('Не задана директория синхронизации с облаком!');
end;

procedure TMainForm.Path7zipEditClick(Sender: TObject);
begin
  OpenDialog1.Filter:= 'Исполняемый файл архиватора 7zip|7z.exe';
  if OpenDialog1.Execute then
      begin
        Path7zipEdit.Text := OpenDialog1.Filename;
      end;
end;

procedure TMainForm.PathCloudEditChange(Sender: TObject);
var
 Reply: Integer;
 CloudDirPath: String;
begin

  CloudDirPath := UTF8ToSys(PathCloudEdit.Text + '\1Cbackup');

  if not(DirectoryExists(CloudDirPath)) then
    begin
        begin
            Reply := Application.MessageBox('Отсутствует директория 1Cbackup в корне Яндекс.Диска! Создать?',
                    'Директория 1Cbackup не обнаружена', MB_ICONQUESTION + MB_YESNO);
                if Reply = IDYES then
                    if not CreateDir(CloudDirPath) then
                        ShowMessage('Не получилось :(');
        end
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
               ShowMessage('Длина пути менее 3 символов');
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

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  AboutForm.Show;
end;

procedure TMainForm.FormShow(Sender: TObject);
Var
 FilePath: String;
 PartPath: String;
begin

  v8iFileEdit.Text := CP866ToUTF8(SysUtils.GetEnvironmentVariable('APPDATA')) + '\1C\1CEStart\ibases.v8i';;

  // Проверка установленного архиватора 7zip
  PartPath := CP866ToUTF8(SysUtils.GetEnvironmentVariable('PROGRAMFILES'));
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

  FilePath := CP866ToUTF8(SysUtils.GetEnvironmentVariable('USERPROFILE')) + '\YandexDisk';

  // Установка каталога Яндекс.Диска по умолчанию
  if DirectoryExistsUTF8(FilePath) then
  begin
    PathCloudEdit.Text := FilePath;
  end;

  // Установка пути сохранения скрипта по умолчанию
  SaveDialog1.InitialDir := FilePath;

  // Проверка запущенных баз
  CheckBases;
end;

end.

