{ A form to view and edit data. Currently there are two types of data that can
  be shown in this form, state variables and process variables.
  The state variables form allows the user to view and edit the state variables.
  The process variable form is used for viewing only. The user can not edit the
  process variables directly. To change a process value it is necessary to
  modify the parameters in the parameter form. The process values shown in this
  form are automatically updated when the process values in the parameter form
  are updated.
 }
unit data;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids,stypes, StdCtrls, ExtCtrls, PrintersDlgs, math;

type

  { TDataForm }

  TDataForm = class(TForm)
    PnlTop: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    DlgPrint: TPrintDialog;
    BtPrint: TButton;
    StringGrid1: TStringGrid;
    procedure BtnOKClick(Sender: TObject);  dynamic;
    procedure BtPrintClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ShowStates;
    procedure SaveStates;
    procedure ShowProcess;
    procedure StringGrid1CheckboxToggled(sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure UpdateProcess;
    procedure ClearGrid;
    procedure BtnCancelClick(Sender: TObject); dynamic;
    procedure StringGrid1SelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);  dynamic;
    procedure StringGrid1DrawCell(Sender: TObject; Col, Row: Integer;
      Rect: TRect; State: TGridDrawState); dynamic;
  private
    { Private declarations }
    ftempState:Statearray;
  public
    { Public declarations }
  end;

var
  DataForm : TDataForm;
  DataShowing : TDataType;

implementation

uses fileio, frontend;

{$R *.lfm}


// Set up the form
procedure TDataForm.FormCreate(Sender: TObject);
begin
  StringGrid1.RowCount := ModelDef.numstate;
  StringGrid1.FocusColor := clRed;
  StringGrid1.cells[0,0]:='HoldConstant?';
  StringGrid1.cells[1,0]:='Reset?';
  StringGrid1.cells[2,0]:='Variable Symbol';
  StringGrid1.cells[3,0]:='Variable Name';
  StringGrid1.cells[4,0]:='Variable Value';
  StringGrid1.cells[5,0]:='Variable Units';
end;

procedure TDataForm.ShowStates;
var
  i:integer;
begin
 Ftempstate := Stat;
 Caption := 'State Variables';  // Form Caption
 DataShowing := dtstate;
 StringGrid1.rowcount := ModelDef.numstate+1;
 StringGrid1.Col := 1;
 StringGrid1.Row := 1;
 StringGrid1.Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,
           goColSizing,goThumbTracking,goEditing];
 StringGrid1.ColWidths[0] := 150;
 StringGrid1.ColWidths[1] := 150;
 for i:= 1 to ModelDef.numstate do  // Write state variables, names, values and
   begin                            // units to grid
     StringGrid1.cells[2,i] := stat[i].symbol;
     StringGrid1.cells[3,i] := stat[i].name;
     StringGrid1.Cells[5,i] := stat[i].units;
     StringGrid1.cells[4,i] := floattostr(stat[i].value);
     if stat[i].holdconstant then StringGrid1.Cells[0,i] := 'True'
     else StringGrid1.Cells[0,i] := 'False';
     if stat[i].Reset then StringGrid1.Cells[1,i] := 'True'
     else StringGrid1.Cells[1,i] := 'False';
   end;
 DataForm.Width := min(round(0.9*Screen.Width), Stringgrid1.colcount*Stringgrid1.defaultcolwidth+10);
 DataForm.Height := min(round(0.8*Screen.Height), StringGrid1.rowcount*Stringgrid1.defaultrowheight+ PnlTop.Height +10);
 BtnOK.Visible := True;
 BtnOK.Enabled := True;
 BtnCancel.Caption := '&Cancel';
 BtnCancel.Left := 480;
 DataForm.ActiveControl := StringGrid1;
 ShowModal;   // Show the form
end;


// Set up the form to show process variables then show the data
procedure tDataForm.ShowProcess;
begin
 ftempState := stat; // Necessary so that initial state variable values are saved.
 Caption := 'Process Variables';    // Form Caption
 DataShowing := dtProcess;
 StringGrid1.rowcount:=ModelDef.numprocess+1;
 StringGrid1.Col := 1;
 StringGrid1.Row := 1;
 StringGrid1.ColWidths[0] := 0;
 StringGrid1.ColWidths[1] := 0;
 StringGrid1.Options := [goFixedVertLine,goFixedHorzLine,goVertLine,goHorzLine,
           goColSizing,goThumbTracking];
 UpdateProcess;
 DataForm.Width := min(round(0.9*Screen.Width), (Stringgrid1.colcount-2)*Stringgrid1.defaultcolwidth+10);
 DataForm.Height := min(round(0.8*Screen.Height), StringGrid1.rowcount*Stringgrid1.defaultrowheight+ PnlTop.Height +10);
 BtnOK.Visible := False;
 BtnOK.Enabled := False;
 BtnCancel.Caption := '&Close';
 BtnCancel.Left:=20;
 Show;
end;

procedure TDataForm.StringGrid1CheckboxToggled(sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
begin
  if aCol = 0 then
    if astate = cbchecked then stat[aRow].holdconstant := true
    else stat[aRow].holdconstant := false;
  if acol = 1 then
    if astate = cbchecked then stat[aRow].reset := true
    else stat[aRow].reset := false;
end;

// Form has been modified, user wants to keep the changes
procedure TDataForm.BtnOKClick(Sender: TObject);         // FIX what if processes are showing
begin
if DataShowing = dtState{ and StringGrid1Modified }then
 begin
  SaveStates;
  NeedToSavePar := True;   // State variables have been changed, parameter file
                           // needs to be saved
  RunComplete := False;
 end;
 ClearGrid;
end;

// Clear the grid when the form closes. When the form is showing process
// variables also close the form manually since it doesn't do it automatically.
procedure TDataForm.BtnCancelClick(Sender: TObject);
begin
 if datashowing = dtstate then stat := ftempState;  // Set states back to original values
 ClearGrid;
 if DataForm.Visible then DataForm.Close;
end;

// Update global array of state variables with the changes made to the string grid
procedure tDataForm.SaveStates;
var
 i:integer;
begin
 for i:=1 to ModelDef.numstate do
   begin   // Copy value in grid to the global state array
     stat[i].value := strtofloat(StringGrid1.Cells[4,i]);
     // Holdconstant and reset values are saved as soon as they are changed so don't need to do them here
   end;
end;

{ The procedure updates the process variable values shown in the Dataform.}
procedure tDataForm.UpdateProcess;
var
 i:integer;
begin
 for i:= 1 to ModelDef.numprocess do  // Write process variables, names, values and
   begin                            // units to grid
     StringGrid1.cells[3,i] := proc[i].name;
     StringGrid1.Cells[5,i] := proc[i].units;
     StringGrid1.Cells[2,i] := proc[i].symbol;
     StringGrid1.cells[4,i] := floattostr(proc[i].value);
   end;
end;

// Clear the StringGrid so that new data can be written to it
procedure tDataForm.ClearGrid;
var
 i,j : integer;
begin    // Set grid cells to empty strings
 for i := 0 to StringGrid1.RowCount - 1 do
   for j := 2 to StringGrid1.ColCount - 1 do        // Don't do columns 0 and 1 because their the check boxes
    StringGrid1.Cells[j,i] := '';
 StringGrid1.RowCount := 3;   // Reset row count
end;

// Print out the state variables.
{ This procedure is currently not enabled because it will only print the visible
  state variables. Needs to be fixed to print them all. }
procedure TDataForm.BtPrintClick(Sender: TObject);
begin
with DlgPrint do
   if execute then
     // Print  StringGrid1.PaintTo();
   ;
end;

procedure TDataForm.StringGrid1SelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
 if DataShowing = dtstate then
   if (Col = 4) or (Col = 0) or (Col = 1) then
     StringGrid1.Editor := StringGrid1.EditorbyStyle(cbsAuto)
   else
     StringGrid1.Editor := StringGrid1.EditorbyStyle(cbsNone)
 else
   StringGrid1.Editor := StringGrid1.EditorbyStyle(cbsNone);
end;

procedure TDataForm.StringGrid1DrawCell(Sender: TObject; Col,
  Row: Integer; Rect: TRect; State: TGridDrawState);
var
 s:string;
begin
 if DataShowing = dtstate then
   begin
     if Col <> 4 then
       StringGrid1.Canvas.Brush.Color:= clScrollBar;
   end
 else
       StringGrid1.Canvas.Brush.Color:= clScrollBar;

 StringGrid1.Canvas.FillRect(Rect);
 S := StringGrid1.Cells[Col, Row];
 StringGrid1.Canvas.TextOut(Rect.Left + 2, Rect.Top + 2, S);
end;

end.
