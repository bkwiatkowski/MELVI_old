unit parameter;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, stypes;

type

  { TFmParameter }

  TFmParameter = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnProcessVar: TButton;
    PnTop: TPanel;
    SgParameter: TStringGrid;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnProcessVarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SgParameterDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure SgParameterSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    { private declarations }
    fOrigPar: paramarray;
  public
    { public declarations }
    procedure ShowParameters;
    procedure UpdateProcessWindow;
  end;

var
  FmParameter: TFmParameter;

implementation

uses frontend, data, fileio, equations;
{$R *.lfm}

{ TFmParameter }

procedure TFmParameter.FormCreate(Sender: TObject);
var
  i:integer;
begin
  SgParameter.RowCount:= ModelDef.numparam + 1;
  SgParameter.AutoSizeColumns;
  SgParameter.cells[0,0]:='Parameter Symbol';
  SgParameter.cells[1,0]:='Parameter Name';
  SgParameter.cells[2,0]:='Parameter Value';
  SgParameter.cells[3,0]:='Parameter Units';
  for i := 1 to ModelDef.numparam do
   begin
     SgParameter.cells[0,i] := par[i].symbol;
     SgParameter.cells[1,i] := par[i].name;
     SgParameter.cells[3,i] := par[i].units;
   end;
  SgParameter.row := 1;
  SgParameter.col := 2;
end;

procedure TFmParameter.SgParameterDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  S:string;
begin
 if aCol <> 2 then SgParameter.Canvas.Brush.Color:= clScrollBar;
 SgParameter.Canvas.FillRect(aRect);
 S := SgParameter.Cells[aCol, aRow];
 SgParameter.Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, S);
end;

procedure TFmParameter.SgParameterSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
 if aCol = 2 then
  begin
   CanSelect := True;
   SgParameter.Editor := SgParameter.EditorbyStyle(cbsAuto)
  end
 else
   SgParameter.Editor := SgParameter.EditorbyStyle(cbsNone)
end;

procedure TFmParameter.BtnOKClick(Sender: TObject);
var
  i:integer;
begin
  for i:=1 to ModelDef.numparam do par[i].value := strtofloat(SgParameter.Cells[2,i]);
  BtnProcessVar.Caption := '&Show Processes';
  if DataForm.visible then DataForm.BtnCancelClick(FmParameter);
end;

procedure TFmParameter.BtnProcessVarClick(Sender: TObject);
begin
  if not DataForm.Visible then
    begin
     DataForm.ShowProcess;
     BtnProcessVar.Caption := '&Update Processes';
    end;
  UpdateProcessWindow;
end;

procedure TFmParameter.BtnCancelClick(Sender: TObject);
begin
  par := fOrigPar;
  BtnProcessVar.Caption := '&Show Processes';
  if DataForm.visible then DataForm.BtnCancelClick(FmParameter);
end;

procedure TFmParameter.ShowParameters;
var
  i:integer;
begin
  if paramfilename = '' then
    FmParameter.Caption := 'Edit Parameters - File: None Specified'
  else
    FmParameter.Caption := 'Edit Parameters - File: ' + paramfilename;
  if (paramfilename = '') or (driverfilename = '') then
   begin
     BtnProcessVar.Enabled:=False;
   end
  else
   begin
     BtnProcessVar.Enabled:=True;
   end;
  fOrigPar := par;
  SgParameter.AutoSizeColumns;
  SgParameter.Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,
            goColSizing,goThumbTracking,goEditing];
  for i := 1 to ModelDef.numparam do SgParameter.cells[2,i] := floattostr(par[i].value);
end;

procedure TFmParameter.UpdateProcessWindow;
var
  i:integer;
begin
  for i:=1 to ModelDef.numparam do par[i].value := strtofloat(SgParameter.Cells[2,i]);
  try
   // Open driverfile, read first row of drivers, and then close the driver file.
    openDriverFile(driverfilename,fileio.driverlist);
    last_time := 0;
    next_time := 0;
    GetCurrentDrivers(Caltime,drive);
    processes(Caltime,caltime,drive,par,stat,proc,false); // Calculate processes
  except
    on EfileError do
      MessageDlg('Error reading driver file. Process values were not calculated.', mtWarning, [mbOK], 0);
    on E: Exception do
      MessageDlg('Invalid Parameter. Process values were not calculated.', mtWarning, [mbOK], 0);
  end;
  DataForm.UpdateProcess;
end;

end.

