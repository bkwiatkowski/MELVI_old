unit Display;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, TAGraph, TASeries,
  TASources, TATools, TACustomSource, TALegendPanel, TATransformations, stypes;

const
  Maxseries = 10;
  FirstRow = 3;

type
  plotarray=array[1..MaxSeries] of string;

type

  { TFmDisplayOutput }

  TFmDisplayOutput = class(TForm)
    BtnClearSeries: TButton;
    BtnCloseDisplay: TButton;
    BtnUpdateChart: TButton;
    BtnRun: TButton;
    ChartToolset1: TChartToolset;
    ChartToolset1PanClickTool1: TPanClickTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomClickTool1: TZoomClickTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ChBAxisAutoScale: TAutoScaleAxisTransform;
    ChBAxisLogarithm: TLogarithmAxisTransform;
    ChBAxisTransforms: TChartAxisTransformations;
    CbxParameters: TCheckBox;
    CbxStates: TCheckBox;
    ChLAxisAutoScale: TAutoScaleAxisTransform;
    ChLAxisLogarithm: TLogarithmAxisTransform;
    ChLAxisTransforms: TChartAxisTransformations;
    ChOutput: TChart;
    ChOutputLineSeries1: TLineSeries;
    ChOutputLineSeries10: TLineSeries;
    ChOutputLineSeries2: TLineSeries;
    ChOutputLineSeries3: TLineSeries;
    ChOutputLineSeries4: TLineSeries;
    ChOutputLineSeries5: TLineSeries;
    ChOutputLineSeries6: TLineSeries;
    ChOutputLineSeries7: TLineSeries;
    ChOutputLineSeries8: TLineSeries;
    ChOutputLineSeries9: TLineSeries;
    LblEdPar1: TLabeledEdit;
    LblEdPar2: TLabeledEdit;
    LblEdPar3: TLabeledEdit;
    LblEdPar4: TLabeledEdit;
    LblDirections: TLabel;
    LbxSeriesSelect: TListBox;
    ListChartSource1: TListChartSource;
    ListChartSource10: TListChartSource;
    ListChartSource2: TListChartSource;
    ListChartSource3: TListChartSource;
    ListChartSource4: TListChartSource;
    ListChartSource5: TListChartSource;
    ListChartSource6: TListChartSource;
    ListChartSource7: TListChartSource;
    ListChartSource8: TListChartSource;
    ListChartSource9: TListChartSource;
    MIYScale: TMenuItem;
    MIYSelect: TMenuItem;
    MIXScale: TMenuItem;
    MIXSelect: TMenuItem;
    MIShowTable: TMenuItem;
    MIShowChart: TMenuItem;
    MIShow: TMenuItem;
    MIYaxis: TMenuItem;
    MIUpdate: TMenuItem;
    MIPrintChart: TMenuItem;
    MIXaxis: TMenuItem;
    MILoadFile: TMenuItem;
    MIClose: TMenuItem;
    MISaveOutput: TMenuItem;
    MIChart: TMenuItem;
    MmDisplay: TMainMenu;
    MIWindow: TMenuItem;
    PnlParameters: TPanel;
    PnlRerun: TPanel;
    PnlChartButtons: TPanel;
    PnlTop: TPanel;
    PnlTopRight: TPanel;
    PnlBottom: TPanel;
    PnlBottomLeft: TPanel;
    PnlBottomRight: TPanel;
    RgChartValues: TRadioGroup;
    SgModelOutput: TStringGrid;
    sLblPar1: TBoundLabel;
    sLblPar2: TBoundLabel;
    SLblPar3: TBoundLabel;
    SplBottomLR: TSplitter;
    SplTopBottom: TSplitter;
    procedure BtnClearSeriesClick(Sender: TObject);
    procedure BtnCloseDisplayClick(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure BtnUpdateChartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LblEdParKeyPress(Sender: TObject; var Key: char);
    procedure MILoadFileClick(Sender: TObject);
    procedure MISaveOutputClick(Sender: TObject);
    procedure MIScaleClick(Sender: TObject);
    procedure PnlTopRightClick(Sender: TObject);
    procedure RgChartValuesClick(Sender: TObject);
    procedure UpdateChart;
    procedure lblParClick(sender: Tobject);
    procedure LblEdParExit(sender: Tobject);
    procedure refreshLblEdPar;
    procedure cbxParamsClick(Sender: TObject);
    procedure cbxStatesClick(Sender: TObject);
    procedure MIShowChange(Sender: TObject);
    procedure MISelectAxisClick(Sender: TObject);
  private
    { private declarations }
    FFilename:String;
    FCurrentAxis:TAxis;
    FxAxis:string;
    FDisplayStep:double;
    FDisplayStyle:TDisplayStyle;
    FInitialView:Boolean;
    FNumberofSeries:integer;    // Current number of series selected
    FSeriestoPlot:plotarray;    // The list of series to be plotted
    procedure AddSeriestoPlot(ListBox: Tlistbox);
    procedure RemoveSeriestoPlot(seriesname:string);
    function GetColumnNumber(seriesname:string):integer;
 public
    { public declarations }
    FirstWrite:Boolean;
    CurrentRow: integer;
    DisplayFilename:string;
    AutoShowChart:Boolean;
    DisplayStyle:TDisplayStyle;
    property Filename:String read FFilename write FFilename;
    property CurrentAxis:TAxis read FCurrentAxis write FCurrentAxis;
    property displaystep:double read FDisplayStep write FDisplayStep;
    property xAxis:string read FxAxis write FxAxis;
    property NumberofSeries:integer read FNumberofSeries write FNumberofSeries;
    procedure FillListBox(ListBox:TListBox);
    procedure ClearSeriestoPlot(Listbox:TListBox);
    procedure WriteOutputfromMem;
    procedure WritePurgeOutputfromMem;
    procedure ClearGrid;
    procedure StoreResults(ctime:double; var darray:drivearray; var sarray:statearray;
         var parray:processarray);

 end;

var
  FmDisplayOutput: TFmDisplayOutput;

implementation

uses frontend, ParamList, calculate, ReloadDlg, trouble, fileio, SeriesForm, ScaleDlg, options;
{$R *.lfm}

{ TFmDisplayOutput }

procedure TFmDisplayOutput.FormCreate(Sender: TObject);
var
  i: integer;
begin
 FInitialView := True;
 FirstWrite := True;
 chOutput.Title.Text.text := modeldef.modelname; // Chart title

 SgModelOutput.RowCount := FirstRow;
 SgModelOutput.colcount := modeldef.numdrive + modeldef.numprocess + 1 +ModelDef.numstate;                //***********HERE
 // don't need numstate in the above count because the derivatives are no
 // longer in the table so the count of the derivatives in numprocess takes
 // care of numstate.
 for i := 0 to SgModelOutput.colcount - 1 do SgModelOutput.cells[i,0] := inttostr(i);
 SgModelOutput.cells[0,1] := 'Time';             // Column name
 SgModelOutput.cells[0,2] := ModelDef.timeunit;   // Column units
 for i := 1 to modeldef.numdrive do         // Add drivers to stringgrid
   begin
    SgModelOutput.cells[i,1] := drive[i].name;     // Column name
    SgModelOutput.cells[i,2] := drive[i].units;    // Column units
   end;
 for i := 1 to modeldef.numstate do         // Add state variables to grid
   begin
    SgModelOutput.cells[modeldef.numdrive + i,1] := stat[i].name;   // Column name
    SgModelOutput.cells[modeldef.numdrive + i,2] := stat[i].units;  // Column units
   end;
 for i := ModelDef.numstate + 1 to modeldef.numprocess do       // Add process variables to grid
   begin
    SgModelOutput.cells[modeldef.numdrive + i,1] := proc[i].name;  // Column name
    SgModelOutput.cells[modeldef.numdrive + i,2] := proc[i].units; // Column units
   end;
 for i := 1 to ModelDef.numstate do       // Add derivatives to grid                //************HERE
   begin
    SgModelOutput.cells[modeldef.numdrive+ modeldef.numprocess + i,1] := proc[i].name;  // Column name
    SgModelOutput.cells[modeldef.numdrive+ modeldef.numprocess + i,2] := proc[i].units; // Column units
   end;

 FillListBox(LbxSeriesSelect);
 FxAxis := 'Time';
// FDisplayStep := FmOptions.RunOptions.Time_step;
{ autoshowchart := false;       }
 CurrentRow := FirstRow;
 DisplayFilename := 'Memory';
 DisplayStyle:=dsChart;
 FmDisplayOutput.ActiveControl:=Lbxseriesselect;

 {$ifdef Darwin}
    LblEdPar1.EditLabel.ShowAccelChar:=False;
    LblEdPar1.EditLabel.Caption:= 'Parameter 1';
    LblEdPar2.EditLabel.ShowAccelChar:=False;
    LblEdPar2.EditLabel.Caption := 'Parameter 2';
    LblEdPar3.EditLabel.ShowAccelChar:=False;
    LblEdPar3.EditLabel.Caption := 'Parameter 3';
    LblEdPar4.EditLabel.ShowAccelChar:=False;
    LblEdPar4.EditLabel.Caption := 'Parameter 4';
 {$endif}
end;

procedure TFmDisplayOutput.FormResize(Sender: TObject);
var
  numcol:double;
begin
 if fInitialView then
  begin
   FmDisplayOutput.Height:=round(0.8*screen.Height);
   FmDisplayOutput.Width := round(0.8*screen.width);
   FmDisplayOutput.Top := 0;
   fInitialView := False;
  end;
 if DisplayStyle = dsChart then
  begin
   PnlBottom.Constraints.MinHeight:=250;
   PnlBottom.Height:=2*(FmDisplayOutput.Height div 5);
   numcol := LbxSeriesSelect.ClientWidth/(8*stringlength);  // assumes 8 units per character
   if numcol < 1 then numcol := 1;
   LbxSeriesSelect.Columns:=round(numcol);
  end
 else     // Showing the table and not the chart
  begin
  // Shrink bottom panel and move the close buttom so it's visible
   PnlBottom.Constraints.MinHeight:=0;
   PnlBottom.Height:=BtnCloseDisplay.Height*3;
   BtnCloseDisplay.Top := (PnlBottom.Height - BtnCloseDisplay.Height) div 2 - 1;
  end;
end;

procedure TFmDisplayOutput.FormShow(Sender: TObject);
begin
 FmDisplayOutput.Caption := 'Model Output - File: ' + DisplayFilename;
// MessageDlg('number selected = ' + inttostr(LbxSeriesSelect.SelCount), mtInformation, [mbOK], 0);
 if DisplayStyle = dsChart then
  begin
   SgModelOutput.Visible:=False;
   PnlTop.Show;
   PnlBottomLeft.Show;
   BtnUpdateChart.Enabled := True;
   BtnUpdateChart.Visible:=True;
   BtnClearSeries.Enabled:=True;
   BtnClearSeries.Visible:=True;
   RgChartValues.Visible := True;
   RgChartValues.Enabled := True;
  end
 else   // Showing Data table not chart
  begin
   SgModelOutput.Visible:=True;
   PnlTop.Hide;
   PnlBottomLeft.Hide;
   BtnUpdateChart.Enabled := False;
   BtnUpdateChart.Visible:=False;
   BtnClearSeries.Enabled:=False;
   BtnClearSeries.Visible:=False;
   RgChartValues.Visible := False;
   RgChartValues.Enabled := False;
  end;
 FmDisplayOutput.FormResize(FmShellMain);
 cbxParameters.Checked := DlgReload.cbParams.Checked;
 cbxStates.Checked := DlgReload.cbState.Checked;
end;


procedure TFmDisplayOutput.LblEdParKeyPress(Sender: TObject; var Key: char);
begin
   if (Key = Chr(13)) then LblEdParExit(Sender);
end;

procedure TFmDisplayOutput.MILoadFileClick(Sender: TObject);
var
  ttime:double;
  tempdrive:drivearray;
  tempstate:statearray;
  tempprocess:processarray;
begin
 tempdrive := drive;
 tempstate := stat;
 tempprocess := proc;
 ClearGrid;
 if FmShellMain.DlgOpenOutput.execute then
  if FmShellMain.DlgOpenOutput.filename <> '' then
   begin
    DisplayFilename := FmShellMain.DlgOpenOutput.filename;
    try
     OpenOutputFile(FmShellMain.DlgOpenOutput.FileName, ModelDef.numdrive, drive, ModelDef.numstate,
       stat, ModelDef.numprocess, proc, flread);
     while not eof(outfile) do
      begin
       ReadOutputFile(ttime, ModelDef.numdrive, tempdrive, ModelDef.numstate, tempstate,
        ModelDef.numprocess, tempprocess);
       StoreResults(ttime, tempdrive, tempstate, tempprocess);
      end;
    finally
     CloseOutputFile;
    end;
   end;
end;

procedure TFmDisplayOutput.MISaveOutputClick(Sender: TObject);
begin
 if DisplayFilename = 'Memory' then
  begin
    FmShellMain.ChooseParamFile(FmDisplayOutput);
    WriteOutputfromMem;
  end
 else
  MessageDlg('Data already saved in file ' + DisplayFilename, mtInformation, [mbOK], 0 );
end;

procedure TFmDisplayOutput.MIScaleClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Name = 'MIXScale' then
   begin
    CurrentAxis := axBottom;
   end
  else
   begin
    CurrentAxis := axLeft;
   end;
  DlgScale.ShowModal;
end;

procedure TFmDisplayOutput.PnlTopRightClick(Sender: TObject);
begin

end;

procedure TFmDisplayOutput.RgChartValuesClick(Sender: TObject);
begin

end;

procedure TFmDisplayOutput.MISelectAxisClick(Sender: TObject);
begin
 if (Sender as TMenuItem).Name = 'MIXSelect' then
    CurrentAxis := axBottom
 else
    CurrentAxis := axLeft;
 FmSeries.ShowModal;
end;

procedure TFmDisplayOutput.MIShowChange(Sender: TObject);
begin
 if (Sender as TMenuItem).Name = 'MIShowTable' then
   DisplayStyle := dsTable
 else
   DisplayStyle := dsChart;
 FmDisplayOutput.FormShow(Sender);
end;

procedure TFmDisplayOutput.BtnCloseDisplayClick(Sender: TObject);
begin
  FmDisplayOutput.Close;
  DlgReload.cbParams.Checked := cbxParameters.Checked;
  DlgReload.cbState.Checked := cbxStates.Checked;
end;

procedure TFmDisplayOutput.BtnRunClick(Sender: TObject);
begin
 if MessageDlg('Rerun model using current run options?', mtConfirmation, [mbYes,mbNo], 0) = mrYes then
  begin
   SgModelOutput.BeginUpdate;
   dlgReload.okBtn.click;
   FmShellMain.btnRun.click;
   if FmTrouble.visible then FmTrouble.BringToFront;
   SgModelOutput.EndUpdate(True);
  end;
end;

procedure TFmDisplayOutput.BtnClearSeriesClick(Sender: TObject);
begin
  ClearSeriestoPlot(LbxSeriesSelect);
end;

procedure TFmDisplayOutput.BtnUpdateChartClick(Sender: TObject);
begin
 UpdateChart;
 refreshLblEdPar;
end;

procedure TFmDisplayOutput.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
 BtnCloseDisplayClick(sender);
end;

procedure TFmDisplayOutput.UpdateChart;
var
 i,j,xcolumn,ycolumn:integer;
 CurrentChartSource:^TListChartSource;
begin
try
 New(CurrentChartSource);
 // Inactivate previous series
 for i := 0 to MaxSeries - 1 do Choutput.series[i].active := false;

 // Put title on Bottom Axis
 xcolumn := GetColumnNumber(xAxis);
 ChOutput.BottomAxis.Title.Caption := SgModelOutput.cells[xcolumn,1];

 // Clear the chart of previous plots
// for i:=0 to ChOutput.SeriesCount-1 do ChOutput.Series[i].Clear;
// If ChOutput.SeriesCount > 0 then ChOutput.ClearSeries;
 ChOutputLineSeries1.Clear;
 ChOutputLineSeries2.Clear;
 ChOutputLineSeries3.Clear;
 ChOutputLineSeries4.Clear;
 ChOutputLineSeries5.Clear;
 ChOutputLineSeries6.Clear;
 ChOutputLineSeries7.Clear;
 ChOutputLineSeries8.Clear;
 ChOutputLineSeries9.Clear;
 ChOutputLineSeries10.Clear;

 // Store series to plot in plotarray
 AddSeriestoPlot(LbxSeriesSelect);

 // Activate the number of series to plot.
 for i := 0 to FNumberofSeries - 1 do Choutput.series[i].active := true;

 // Check for divide by zero if relative change is selected
 if (RgChartValues.ItemIndex=1) then
  for j := 0 to FNumberofSeries - 1 do
   begin
     ycolumn := GetColumnNumber(FSeriestoPlot[j+1]);
     if (RgChartValues.ItemIndex=1) and (strtofloat(SgModelOutput.Cells[ycolumn,FirstRow]) = 0) then  // The if itemindex=1 needs to be here so that the code doesn't give the error for every series on the chart. It only needs to be corrected once.
     begin
       MessageDlg('Unable to display relative values because y0 is zero. Displaying actual values instead.', mtInformation, [mbOK], 0 );
       RgChartValues.ItemIndex:=0;
     end;
   end;

 // Add each series to the datasource.
 for j := 0 to FNumberofSeries - 1 do
   begin
    ycolumn := GetColumnNumber(FSeriestoPlot[j+1]);
    case j of
     0: begin
         CurrentChartSource^:=ListChartSource1;
         ChOutputLineSeries1.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     1: begin
         CurrentChartSource^:=ListChartSource2;
         ChOutputLineSeries2.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     2: begin
         CurrentChartSource^:=ListChartSource3;
         ChOutputLineSeries3.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     3: begin
         CurrentChartSource^:=ListChartSource4;
         ChOutputLineSeries4.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     4: begin
         CurrentChartSource^:=ListChartSource5;
         ChOutputLineSeries5.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     5: begin
         CurrentChartSource^:=ListChartSource6;
         ChOutputLineSeries6.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     6: begin
         CurrentChartSource^:=ListChartSource7;
         ChOutputLineSeries7.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     7: begin
         CurrentChartSource^:=ListChartSource8;
         ChOutputLineSeries8.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     8: begin
         CurrentChartSource^:=ListChartSource9;
         ChOutputLineSeries9.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     9: begin
         CurrentChartSource^:=ListChartSource10;
         ChOutputLineSeries10.Title := SgModelOutput.Cells[ycolumn,1];
        end;
    end;
    for i := FirstRow to SgModelOutput.RowCount - 1 do
     begin
      case RgChartValues.ItemIndex of
         0: CurrentChartSource.add(strtofloat(SgModelOutput.Cells[xcolumn,i]),
              strtofloat(SgModelOutput.Cells[ycolumn,i]));
         1: CurrentChartSource.add(strtofloat(SgModelOutput.Cells[xcolumn,i]),
              strtofloat(SgModelOutput.Cells[ycolumn,i])/strtofloat(SgModelOutput.Cells[ycolumn,FirstRow]));
         2: CurrentChartSource.add(strtofloat(SgModelOutput.Cells[xcolumn,i]),
              strtofloat(SgModelOutput.Cells[ycolumn,i])-strtofloat(SgModelOutput.cells[ycolumn,FirstRow]));
      end;
     end;
   end;
 refreshLblEdPar;
finally
 Dispose(CurrentChartSource);
end;
end;

procedure TFmDisplayOutput.FillListBox(ListBox:TListBox);
var
 i:integer;
begin
 ListBox.Items.Clear;
 ListBox.Items.Add('Time');
 for i := 1 to ModelDef.numdrive do   // Add driver names to listbox
  ListBox.Items.Add(drive[i].name);
 for i := 1 to ModelDef.numstate do   // Add state variable names to listbox
  ListBox.Items.Add(stat[i].name);
 for i := ModelDef.numstate + 1 to ModelDef.numprocess do // Add process variable names to listbox
  ListBox.Items.Add(proc[i].name);
end;

// Adds a series to the FSeriestoPlot array
procedure TFmDisplayOutput.AddSeriestoPlot(ListBox: Tlistbox);
var
 index:integer;
begin
  FNumberofSeries := 0;
  for index := 0 to Listbox.Items.Count - 1 do
    begin
     if Listbox.Selected[index] then
      begin
       if FNumberofSeries = MaxSeries then
          MessageDlg('Cannot display series. Maximum is 10 series.', mtError, [mbOK], 0)
       else
         begin
          FNumberofSeries := FNumberofSeries + 1;
          FSeriestoPlot[FNumberofSeries] := ListBox.Items[index];
         end;
      end;
    end;
end;

// Removes a series from the FSeriestoPlot array
procedure TFmDisplayOutput.RemoveSeriestoPlot(seriesname:string);
var
 temparray:plotarray;
 i,index:integer;
begin
  index := 1;
  temparray := FSeriestoPlot;
  for i := 1 to FNumberofSeries do
   begin
     if temparray[i] = seriesname then
       index := i;
   end;
  for i := index to FNumberofSeries - 1 do
   begin
     FSeriestoPlot[i] := temparray[i + 1];
   end;
  FNumberofSeries := FNumberofSeries - 1;
end;

// Clears the selections in a TListbox
procedure TFmDisplayOutput.ClearSeriestoPlot(Listbox:TListBox);
var
  i:integer;
begin
  for i := 0 to Listbox.Items.Count - 1 do
    begin
     if Listbox.Selected[i] then
      begin
       RemoveSeriestoPlot(Listbox.Items[i]);
       Listbox.Selected[i] := False;
      end;
    end;
end;

// Clear old data from the StringGrid to prevent data overlap when new data is added.
procedure TFmDisplayOutput.ClearGrid;
var
 i,j:integer;
begin
 for i := FirstRow to SgModelOutput.RowCount - 1 do
   for j := 0 to SgModelOutput.ColCount - 1 do
    SgModelOutput.Cells[j,i] := ''; // Set all cells to empty strings
 SgModelOutput.RowCount := FirstRow; // Decrease size of grid
 CurrentRow := FirstRow;
end;

// Get the stringgrid column number for the variable in seriesname
function TFmDisplayOutput.GetColumnNumber(seriesname:string):integer;
var
  j,num:integer;
begin
  num := -1;
  for j := 0 to SgModelOutput.Colcount - 1 do
   begin
    if SgModelOutput.Cells[j,1] = seriesname then num := j;
   end;
  GetColumnNumber := num;
end;

procedure TFmDisplayOutput.lblParClick(sender: Tobject);
var
 tempName : string;
begin
// Getting the number of the label, i.e. 1, 2, 3, or 4 so fmParamList knows which label/edit to modify
 tempName := (sender as tcontrol).name;
 delete(tempName, 1, 8);
 fmParamList.whichEdparselected := strToInt(tempName);
 fmParamList.showmodal;
end;

{ Updates the parameter value shown in the maskedit. }
procedure TFmDisplayOutput.refreshLblEdPar;
var
 thisLblEdit : tLabeledEdit;
 i : integer;
 parIndex : integer;
 temp: string;
begin
 with PnlParameters do
  begin
   for i := 0 to controlCount-1 do //look at all the LabeledEdits
    begin
     if controls[i] is TLabeledEdit then
      begin
       thisLblEdit := controls[i] as TLabeledEdit;
       temp := thisLblEdit.EditLabel.Caption;
       if pos('Parameter', temp) = 0 then
          begin
           parIndex := fmCalculate.getArrayIndex(vtParameter, thisLblEdit.EditLabel.Caption);
           thisLblEdit.Text := floatToStr(par[parIndex].value);
          end;
      end;
    end;
  end;
end;

{ Stores the new values of the parameters in the global array, par. }
procedure TFmDisplayOutput.LblEdParExit(Sender: TObject);
var
 thisLblEd : TlabeledEdit;
 parIndex : integer;
begin
 thisLblEd := sender as TlabeledEdit;
 if thisLblEd.modified then
   begin
    parIndex := fmCalculate.getArrayIndex(vtParameter, thisLblEd.editlabel.Caption);
    try
      par[parIndex].value := strToFloat(thisLblEd.text);
    except
      messageDlg('Please choose a number', mtWarning, [mbOK], 0);
      FmDisplayOutput.ActiveControl := thisLblEd;
    end;
    refreshLblEdPar;
   end;
end;

procedure TFmDisplayOutput.cbxParamsClick(Sender: TObject);
begin
 DlgReload.CbParams.Checked:=cbxParameters.Checked;
end;

procedure TFmDisplayOutput.cbxStatesClick(Sender: TObject);
begin
 DlgReload.cbState.Checked:=cbxStates.Checked;
end;

procedure TFmDisplayOutput.WriteOutputfromMem;
var
  i, j:integer;
  tempstring: string;
  outfile: textfile;
begin
  assignfile(outfile, outfilename);
  if (FmOptions.RunOptions.AppendOutputFile) and (not FirstWrite) then
   append(outfile)
  else
   rewrite(outfile);    // Create a new file
  try
    for j := 1 to SgModelOutput.RowCount - 1 do    // Start at 1 to skip column numbers
      begin
        tempstring := SgModelOutput.Cells[0, j];
        for i := 1 to SgModelOutput.ColCount - 1 do tempstring := tempstring + ', ' + SgModelOutput.Cells[i, j];
        writeln(outfile, tempstring);
      end;
  finally
    closefile(outfile);
  end;
end;

procedure TFmDisplayOutput.WritePurgeOutputfromMem;
var
  i, j:integer;
  tempstring: string;
  outfile: textfile;
begin
  assignfile(outfile, outfilename);
  try
  if FirstWrite then
   begin
    rewrite(outfile);    // Create a new file
      for j := 1 to SgModelOutput.RowCount - 1 do    // Start at 1 to skip column numbers
        begin
          tempstring := SgModelOutput.Cells[0, j];
          for i := 1 to SgModelOutput.ColCount - 1 do tempstring := tempstring + ', ' + SgModelOutput.Cells[i, j];
          writeln(outfile, tempstring);
        end;
    FirstWrite := False;
   end
  else
   begin
    append(outfile);    // Open existing file for writing
      for j := 3 to SgModelOutput.RowCount - 1 do    // Start at 3 to skip column numbers and variable names and units
        begin
          tempstring := SgModelOutput.Cells[0, j];
          for i := 1 to SgModelOutput.ColCount - 1 do tempstring := tempstring + ', ' + SgModelOutput.Cells[i, j];
          writeln(outfile, tempstring);
        end;
   end;
  ClearGrid;
  finally
    closefile(outfile);
  end;
end;

procedure TFmDisplayOutput.StoreResults(ctime:double; var darray:drivearray; var sarray:statearray;
         var parray:processarray);
var
 j: integer;
begin
 SgModelOutput.rowcount := SgModelOutput.rowcount + 1;
 SgModelOutput.Cells[0,currentrow] := format('%g',[ctime]); // Write time
 for j:= 1 to ModelDef.numdrive    // Write drivers
  do SgModelOutput.Cells[j,currentrow] := format('%.20g',[darray[j].value]);

 for j:= 1 to ModelDef.numstate    // Write state variables
  do SgModelOutput.Cells[ModelDef.numdrive + j,currentrow]
             := format('%.20g',[sarray[j].value]);
 for j:= ModelDef.numstate + 1 to ModelDef.numprocess  // Write process variables
   do SgModelOutput.Cells[ModelDef.numdrive + j, currentrow]
             := format('%.20g',[parray[j].value]);
 for j:= 1 to ModelDef.numstate do         // Derivatives                                          //****************HERE
   SgModelOutput.Cells[ModelDef.numdrive + ModelDef.numprocess + j, currentrow]
            := format('%.20g',[parray[j].value]);
 currentrow := currentrow + 1;
 if SgModelOutput.RowCount>=100000 then
  begin
   WritePurgeOutputFromMem;
   FmShellMain.LargeOutput := True;
  end;
end;

end.


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, TAGraph, TASeries,
  TASources, TATools, TACustomSource, TALegendPanel, TATransformations, stypes;

const
  Maxseries = 10;
  FirstRow = 3;

type
  plotarray=array[1..MaxSeries] of string;

type

  { TFmDisplayOutput }

  TFmDisplayOutput = class(TForm)
    BtnClearSeries: TButton;
    BtnCloseDisplay: TButton;
    BtnUpdateChart: TButton;
    BtnRun: TButton;
    ChartToolset1: TChartToolset;
    ChartToolset1PanClickTool1: TPanClickTool;
    ChartToolset1PanDragTool1: TPanDragTool;
    ChartToolset1ZoomClickTool1: TZoomClickTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ChBAxisAutoScale: TAutoScaleAxisTransform;
    ChBAxisLogarithm: TLogarithmAxisTransform;
    ChBAxisTransforms: TChartAxisTransformations;
    CbxParameters: TCheckBox;
    CbxStates: TCheckBox;
    ChLAxisAutoScale: TAutoScaleAxisTransform;
    ChLAxisLogarithm: TLogarithmAxisTransform;
    ChLAxisTransforms: TChartAxisTransformations;
    ChOutput: TChart;
    ChOutputLineSeries1: TLineSeries;
    ChOutputLineSeries10: TLineSeries;
    ChOutputLineSeries2: TLineSeries;
    ChOutputLineSeries3: TLineSeries;
    ChOutputLineSeries4: TLineSeries;
    ChOutputLineSeries5: TLineSeries;
    ChOutputLineSeries6: TLineSeries;
    ChOutputLineSeries7: TLineSeries;
    ChOutputLineSeries8: TLineSeries;
    ChOutputLineSeries9: TLineSeries;
    LblEdPar1: TLabeledEdit;
    LblEdPar2: TLabeledEdit;
    LblEdPar3: TLabeledEdit;
    LblEdPar4: TLabeledEdit;
    LblDirections: TLabel;
    LbxSeriesSelect: TListBox;
    ListChartSource1: TListChartSource;
    ListChartSource10: TListChartSource;
    ListChartSource2: TListChartSource;
    ListChartSource3: TListChartSource;
    ListChartSource4: TListChartSource;
    ListChartSource5: TListChartSource;
    ListChartSource6: TListChartSource;
    ListChartSource7: TListChartSource;
    ListChartSource8: TListChartSource;
    ListChartSource9: TListChartSource;
    MIYScale: TMenuItem;
    MIYSelect: TMenuItem;
    MIXScale: TMenuItem;
    MIXSelect: TMenuItem;
    MIShowTable: TMenuItem;
    MIShowChart: TMenuItem;
    MIShow: TMenuItem;
    MIYaxis: TMenuItem;
    MIUpdate: TMenuItem;
    MIPrintChart: TMenuItem;
    MIXaxis: TMenuItem;
    MILoadFile: TMenuItem;
    MIClose: TMenuItem;
    MISaveOutput: TMenuItem;
    MIChart: TMenuItem;
    MmDisplay: TMainMenu;
    MIWindow: TMenuItem;
    PnlParameters: TPanel;
    PnlRerun: TPanel;
    PnlChartButtons: TPanel;
    PnlTop: TPanel;
    PnlTopRight: TPanel;
    PnlBottom: TPanel;
    PnlBottomLeft: TPanel;
    PnlBottomRight: TPanel;
    RgChartValues: TRadioGroup;
    SgModelOutput: TStringGrid;
    sLblPar1: TBoundLabel;
    sLblPar2: TBoundLabel;
    SLblPar3: TBoundLabel;
    SplBottomLR: TSplitter;
    SplTopBottom: TSplitter;
    procedure BtnClearSeriesClick(Sender: TObject);
    procedure BtnCloseDisplayClick(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure BtnUpdateChartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LblEdParKeyPress(Sender: TObject; var Key: char);
    procedure MILoadFileClick(Sender: TObject);
    procedure MISaveOutputClick(Sender: TObject);
    procedure MIScaleClick(Sender: TObject);
    procedure PnlTopRightClick(Sender: TObject);
    procedure RgChartValuesClick(Sender: TObject);
    procedure UpdateChart;
    procedure lblParClick(sender: Tobject);
    procedure LblEdParExit(sender: Tobject);
    procedure refreshLblEdPar;
    procedure cbxParamsClick(Sender: TObject);
    procedure cbxStatesClick(Sender: TObject);
    procedure MIShowChange(Sender: TObject);
    procedure MISelectAxisClick(Sender: TObject);
  private
    { private declarations }
    FFilename:String;
    FCurrentAxis:TAxis;
    FxAxis:string;
    FDisplayStep:double;
    FDisplayStyle:TDisplayStyle;
    FInitialView:Boolean;
    FNumberofSeries:integer;    // Current number of series selected
    FSeriestoPlot:plotarray;    // The list of series to be plotted
    procedure AddSeriestoPlot(ListBox: Tlistbox);
    procedure RemoveSeriestoPlot(seriesname:string);
    function GetColumnNumber(seriesname:string):integer;
 public
    { public declarations }
    FirstWrite:Boolean;
    CurrentRow: integer;
    DisplayFilename:string;
    AutoShowChart:Boolean;
    property Filename:String read FFilename write FFilename;
    property CurrentAxis:TAxis read FCurrentAxis write FCurrentAxis;
    property displaystep:double read FDisplayStep write FDisplayStep;
    property xAxis:string read FxAxis write FxAxis;
    property DisplayStyle:TDisplayStyle read FDisplayStyle write FDisplayStyle;
    property NumberofSeries:integer read FNumberofSeries write FNumberofSeries;
    procedure FillListBox(ListBox:TListBox);
    procedure ClearSeriestoPlot(Listbox:TListBox);
    procedure WriteOutputfromMem;
    procedure WritePurgeOutputfromMem;
    procedure ClearGrid;
    procedure StoreResults(ctime:double; var darray:drivearray; var sarray:statearray;
         var parray:processarray);

 end;

var
  FmDisplayOutput: TFmDisplayOutput;

implementation

uses frontend, ParamList, calculate, ReloadDlg, trouble, fileio, SeriesForm, ScaleDlg, options;
{$R *.lfm}

{ TFmDisplayOutput }

procedure TFmDisplayOutput.FormCreate(Sender: TObject);
var
  i: integer;
begin
 FInitialView := True;
 FirstWrite := True;
 chOutput.Title.Text.text := modeldef.modelname; // Chart title

 SgModelOutput.RowCount := FirstRow;
 SgModelOutput.colcount := modeldef.numdrive + modeldef.numprocess + 1 + modeldef.numstate;        // ***********HERE************
 // don't need numstate in the above count because the derivatives are no
 // longer in the table so the count of the derivatives in numprocess takes
 // care of numstate.
 for i := 0 to SgModelOutput.colcount - 1 do SgModelOutput.cells[i,0] := inttostr(i);
 SgModelOutput.cells[0,1] := 'Time';             // Column name
 SgModelOutput.cells[0,2] := ModelDef.timeunit;   // Column units
 for i := 1 to modeldef.numdrive do         // Add drivers to stringgrid
   begin
    SgModelOutput.cells[i,1] := drive[i].name;     // Column name
    SgModelOutput.cells[i,2] := drive[i].units;    // Column units
   end;
 for i := 1 to modeldef.numstate do         // Add state variables to grid
   begin
    SgModelOutput.cells[modeldef.numdrive + i,1] := stat[i].name;   // Column name
    SgModelOutput.cells[modeldef.numdrive + i,2] := stat[i].units;  // Column units
   end;
 for i := ModelDef.numstate + 1 to modeldef.numprocess do       // Add process variables to grid
   begin
    SgModelOutput.cells[modeldef.numdrive + i,1] := proc[i].name;  // Column name
    SgModelOutput.cells[modeldef.numdrive + i,2] := proc[i].units; // Column units
   end;
 for i := 1 to ModelDef.numstate do       // Add derivatives to grid
   begin
    SgModelOutput.cells[modeldef.numdrive + modeldef.numprocess+ i,1] := proc[i].name;  // Column name
    SgModelOutput.cells[modeldef.numdrive + modeldef.numprocess+i,2] := proc[i].units; // Column units
   end;

 FillListBox(LbxSeriesSelect);
 FxAxis := 'Time';
// FDisplayStep := FmOptions.RunOptions.Time_step;
{ autoshowchart := false;       }
 CurrentRow := FirstRow;
 DisplayFilename := 'Memory';
 DisplayStyle:=dsChart;
 FmDisplayOutput.ActiveControl:=Lbxseriesselect;

 {$ifdef Darwin}
    LblEdPar1.EditLabel.ShowAccelChar:=False;
    LblEdPar1.EditLabel.Caption:= 'Parameter 1';
    LblEdPar2.EditLabel.ShowAccelChar:=False;
    LblEdPar2.EditLabel.Caption := 'Parameter 2';
    LblEdPar3.EditLabel.ShowAccelChar:=False;
    LblEdPar3.EditLabel.Caption := 'Parameter 3';
    LblEdPar4.EditLabel.ShowAccelChar:=False;
    LblEdPar4.EditLabel.Caption := 'Parameter 4';
 {$endif}
end;

procedure TFmDisplayOutput.FormResize(Sender: TObject);
var
  numcol:double;
begin
 if fInitialView then
  begin
   FmDisplayOutput.Height:=round(0.8*screen.Height);
   FmDisplayOutput.Width := round(0.8*screen.width);
   FmDisplayOutput.Top := 0;
   fInitialView := False;
  end;
 if DisplayStyle = dsChart then
  begin
   PnlBottom.Constraints.MinHeight:=250;
   PnlBottom.Height:=2*(FmDisplayOutput.Height div 5);
   numcol := LbxSeriesSelect.ClientWidth/(8*stringlength);  // assumes 8 units per character
   if numcol < 1 then numcol := 1;
   LbxSeriesSelect.Columns:=round(numcol);
  end
 else     // Showing the table and not the chart
  begin
  // Shrink bottom panel and move the close buttom so it's visible
   PnlBottom.Constraints.MinHeight:=0;
   PnlBottom.Height:=BtnCloseDisplay.Height*3;
   BtnCloseDisplay.Top := (PnlBottom.Height - BtnCloseDisplay.Height) div 2 - 1;
  end;
end;

procedure TFmDisplayOutput.FormShow(Sender: TObject);
begin
 FmDisplayOutput.Caption := 'Model Output - File: ' + DisplayFilename;
// MessageDlg('number selected = ' + inttostr(LbxSeriesSelect.SelCount), mtInformation, [mbOK], 0);
 if DisplayStyle = dsChart then
  begin
   SgModelOutput.Visible:=False;
   PnlTop.Show;
   PnlBottomLeft.Show;
   BtnUpdateChart.Enabled := True;
   BtnUpdateChart.Visible:=True;
   BtnClearSeries.Enabled:=True;
   BtnClearSeries.Visible:=True;
   RgChartValues.Visible := True;
   RgChartValues.Enabled := True;
  end
 else   // Showing Data table not chart
  begin
   SgModelOutput.Visible:=True;
   PnlTop.Hide;
   PnlBottomLeft.Hide;
   BtnUpdateChart.Enabled := False;
   BtnUpdateChart.Visible:=False;
   BtnClearSeries.Enabled:=False;
   BtnClearSeries.Visible:=False;
   RgChartValues.Visible := False;
   RgChartValues.Enabled := False;
  end;
 FmDisplayOutput.FormResize(FmShellMain);
 cbxParameters.Checked := DlgReload.cbParams.Checked;
 cbxStates.Checked := DlgReload.cbState.Checked;
end;


procedure TFmDisplayOutput.LblEdParKeyPress(Sender: TObject; var Key: char);
begin
   if (Key = Chr(13)) then LblEdParExit(Sender);
end;

procedure TFmDisplayOutput.MILoadFileClick(Sender: TObject);
var
  ttime:double;
  tempdrive:drivearray;
  tempstate:statearray;
  tempprocess:processarray;
begin
 tempdrive := drive;
 tempstate := stat;
 tempprocess := proc;
 ClearGrid;
 if FmShellMain.DlgOpenOutput.execute then
  if FmShellMain.DlgOpenOutput.filename <> '' then
   begin
    DisplayFilename := FmShellMain.DlgOpenOutput.filename;
    try
     OpenOutputFile(FmShellMain.DlgOpenOutput.FileName, ModelDef.numdrive, drive, ModelDef.numstate,
       stat, ModelDef.numprocess, proc, flread);
     while not eof(outfile) do
      begin
       ReadOutputFile(ttime, ModelDef.numdrive, tempdrive, ModelDef.numstate, tempstate,
        ModelDef.numprocess, tempprocess);
       StoreResults(ttime, tempdrive, tempstate, tempprocess);
      end;
    finally
     CloseOutputFile;
    end;
   end;
end;

procedure TFmDisplayOutput.MISaveOutputClick(Sender: TObject);
begin
 if DisplayFilename = 'Memory' then
  begin
    FmShellMain.ChooseParamFile(FmDisplayOutput);
    WriteOutputfromMem;
  end
 else
  MessageDlg('Data already saved in file ' + DisplayFilename, mtInformation, [mbOK], 0 );
end;

procedure TFmDisplayOutput.MIScaleClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Name = 'MIXScale' then
   begin
    CurrentAxis := axBottom;
   end
  else
   begin
    CurrentAxis := axLeft;
   end;
  DlgScale.ShowModal;
end;

procedure TFmDisplayOutput.PnlTopRightClick(Sender: TObject);
begin

end;

procedure TFmDisplayOutput.RgChartValuesClick(Sender: TObject);
begin

end;

procedure TFmDisplayOutput.MISelectAxisClick(Sender: TObject);
begin
 if (Sender as TMenuItem).Name = 'MIXSelect' then
    CurrentAxis := axBottom
 else
    CurrentAxis := axLeft;
 FmSeries.ShowModal;
end;

procedure TFmDisplayOutput.MIShowChange(Sender: TObject);
begin
 if (Sender as TMenuItem).Name = 'MIShowTable' then
   DisplayStyle := dsTable
 else
   DisplayStyle := dsChart;
 FmDisplayOutput.FormShow(Sender);
end;

procedure TFmDisplayOutput.BtnCloseDisplayClick(Sender: TObject);
begin
  FmDisplayOutput.Close;
  DlgReload.cbParams.Checked := cbxParameters.Checked;
  DlgReload.cbState.Checked := cbxStates.Checked;
end;

procedure TFmDisplayOutput.BtnRunClick(Sender: TObject);
begin
 if MessageDlg('Rerun model using current run options?', mtConfirmation, [mbYes,mbNo], 0) = mrYes then
  begin
   SgModelOutput.BeginUpdate;
   dlgReload.okBtn.click;
   FmShellMain.btnRun.click;
   if FmTrouble.visible then FmTrouble.BringToFront;
   SgModelOutput.EndUpdate(True);
  end;
end;

procedure TFmDisplayOutput.BtnClearSeriesClick(Sender: TObject);
begin
  ClearSeriestoPlot(LbxSeriesSelect);
end;

procedure TFmDisplayOutput.BtnUpdateChartClick(Sender: TObject);
begin
 UpdateChart;
 refreshLblEdPar;
end;

procedure TFmDisplayOutput.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
 BtnCloseDisplayClick(sender);
end;

procedure TFmDisplayOutput.UpdateChart;
var
 i,j,xcolumn,ycolumn:integer;
 CurrentChartSource:^TListChartSource;
begin
try
 New(CurrentChartSource);
 // Inactivate previous series
 for i := 0 to MaxSeries - 1 do Choutput.series[i].active := false;

 // Put title on Bottom Axis
 xcolumn := GetColumnNumber(xAxis);
 ChOutput.BottomAxis.Title.Caption := SgModelOutput.cells[xcolumn,1];

 // Clear the chart of previous plots
// for i:=0 to ChOutput.SeriesCount-1 do ChOutput.Series[i].Clear;
// If ChOutput.SeriesCount > 0 then ChOutput.ClearSeries;
 ChOutputLineSeries1.Clear;
 ChOutputLineSeries2.Clear;
 ChOutputLineSeries3.Clear;
 ChOutputLineSeries4.Clear;
 ChOutputLineSeries5.Clear;
 ChOutputLineSeries6.Clear;
 ChOutputLineSeries7.Clear;
 ChOutputLineSeries8.Clear;
 ChOutputLineSeries9.Clear;
 ChOutputLineSeries10.Clear;

 // Store series to plot in plotarray
 AddSeriestoPlot(LbxSeriesSelect);

 // Activate the number of series to plot.
 for i := 0 to FNumberofSeries - 1 do Choutput.series[i].active := true;

 // Check for divide by zero if relative change is selected
 if (RgChartValues.ItemIndex=1) then
  for j := 0 to FNumberofSeries - 1 do
   begin
     ycolumn := GetColumnNumber(FSeriestoPlot[j+1]);
     if (RgChartValues.ItemIndex=1) and (strtofloat(SgModelOutput.Cells[ycolumn,FirstRow]) = 0) then  // The if itemindex=1 needs to be here so that the code doesn't give the error for every series on the chart. It only needs to be corrected once.
     begin
       MessageDlg('Unable to display relative values because y0 is zero. Displaying actual values instead.', mtInformation, [mbOK], 0 );
       RgChartValues.ItemIndex:=0;
     end;
   end;

 // Add each series to the datasource.
 for j := 0 to FNumberofSeries - 1 do
   begin
    ycolumn := GetColumnNumber(FSeriestoPlot[j+1]);
    case j of
     0: begin
         CurrentChartSource^:=ListChartSource1;
         ChOutputLineSeries1.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     1: begin
         CurrentChartSource^:=ListChartSource2;
         ChOutputLineSeries2.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     2: begin
         CurrentChartSource^:=ListChartSource3;
         ChOutputLineSeries3.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     3: begin
         CurrentChartSource^:=ListChartSource4;
         ChOutputLineSeries4.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     4: begin
         CurrentChartSource^:=ListChartSource5;
         ChOutputLineSeries5.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     5: begin
         CurrentChartSource^:=ListChartSource6;
         ChOutputLineSeries6.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     6: begin
         CurrentChartSource^:=ListChartSource7;
         ChOutputLineSeries7.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     7: begin
         CurrentChartSource^:=ListChartSource8;
         ChOutputLineSeries8.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     8: begin
         CurrentChartSource^:=ListChartSource9;
         ChOutputLineSeries9.Title := SgModelOutput.Cells[ycolumn,1];
        end;
     9: begin
         CurrentChartSource^:=ListChartSource10;
         ChOutputLineSeries10.Title := SgModelOutput.Cells[ycolumn,1];
        end;
    end;
    for i := FirstRow to SgModelOutput.RowCount - 1 do
     begin
      case RgChartValues.ItemIndex of
         0: CurrentChartSource.add(strtofloat(SgModelOutput.Cells[xcolumn,i]),
              strtofloat(SgModelOutput.Cells[ycolumn,i]));
         1: CurrentChartSource.add(strtofloat(SgModelOutput.Cells[xcolumn,i]),
              strtofloat(SgModelOutput.Cells[ycolumn,i])/strtofloat(SgModelOutput.Cells[ycolumn,FirstRow]));
         2: CurrentChartSource.add(strtofloat(SgModelOutput.Cells[xcolumn,i]),
              strtofloat(SgModelOutput.Cells[ycolumn,i])-strtofloat(SgModelOutput.cells[ycolumn,FirstRow]));
      end;
     end;
   end;
 refreshLblEdPar;
finally
 Dispose(CurrentChartSource);
end;
end;

procedure TFmDisplayOutput.FillListBox(ListBox:TListBox);
var
 i:integer;
begin
 ListBox.Items.Clear;
 ListBox.Items.Add('Time');
 for i := 1 to ModelDef.numdrive do   // Add driver names to listbox
  ListBox.Items.Add(drive[i].name);
 for i := 1 to ModelDef.numstate do   // Add state variable names to listbox
  ListBox.Items.Add(stat[i].name);
 for i := ModelDef.numstate + 1 to ModelDef.numprocess do // Add process variable names to listbox
  ListBox.Items.Add(proc[i].name);
end;

// Adds a series to the FSeriestoPlot array
procedure TFmDisplayOutput.AddSeriestoPlot(ListBox: Tlistbox);
var
 index:integer;
begin
  FNumberofSeries := 0;
  for index := 0 to Listbox.Items.Count - 1 do
    begin
     if Listbox.Selected[index] then
      begin
       if FNumberofSeries = MaxSeries then
          MessageDlg('Cannot display series. Maximum is 10 series.', mtError, [mbOK], 0)
       else
         begin
          FNumberofSeries := FNumberofSeries + 1;
          FSeriestoPlot[FNumberofSeries] := ListBox.Items[index];
         end;
      end;
    end;
end;

// Removes a series from the FSeriestoPlot array
procedure TFmDisplayOutput.RemoveSeriestoPlot(seriesname:string);
var
 temparray:plotarray;
 i,index:integer;
begin
  index := 1;
  temparray := FSeriestoPlot;
  for i := 1 to FNumberofSeries do
   begin
     if temparray[i] = seriesname then
       index := i;
   end;
  for i := index to FNumberofSeries - 1 do
   begin
     FSeriestoPlot[i] := temparray[i + 1];
   end;
  FNumberofSeries := FNumberofSeries - 1;
end;

// Clears the selections in a TListbox
procedure TFmDisplayOutput.ClearSeriestoPlot(Listbox:TListBox);
var
  i:integer;
begin
  for i := 0 to Listbox.Items.Count - 1 do
    begin
     if Listbox.Selected[i] then
      begin
       RemoveSeriestoPlot(Listbox.Items[i]);
       Listbox.Selected[i] := False;
      end;
    end;
end;

// Clear old data from the StringGrid to prevent data overlap when new data is added.
procedure TFmDisplayOutput.ClearGrid;
var
 i,j:integer;
begin
 for i := FirstRow to SgModelOutput.RowCount - 1 do
   for j := 0 to SgModelOutput.ColCount - 1 do
    SgModelOutput.Cells[j,i] := ''; // Set all cells to empty strings
 SgModelOutput.RowCount := FirstRow; // Decrease size of grid
 CurrentRow := FirstRow;
end;

// Get the stringgrid column number for the variable in seriesname
function TFmDisplayOutput.GetColumnNumber(seriesname:string):integer;
var
  j,num:integer;
begin
  num := -1;
  for j := 0 to SgModelOutput.Colcount - 1 do
   begin
    if SgModelOutput.Cells[j,1] = seriesname then num := j;
   end;
  GetColumnNumber := num;
end;

procedure TFmDisplayOutput.lblParClick(sender: Tobject);
var
 tempName : string;
begin
// Getting the number of the label, i.e. 1, 2, 3, or 4 so fmParamList knows which label/edit to modify
 tempName := (sender as tcontrol).name;
 delete(tempName, 1, 8);
 fmParamList.whichEdparselected := strToInt(tempName);
 fmParamList.showmodal;
end;

{ Updates the parameter value shown in the maskedit. }
procedure TFmDisplayOutput.refreshLblEdPar;
var
 thisLblEdit : tLabeledEdit;
 i : integer;
 parIndex : integer;
 temp: string;
begin
 with PnlParameters do
  begin
   for i := 0 to controlCount-1 do //look at all the LabeledEdits
    begin
     if controls[i] is TLabeledEdit then
      begin
       thisLblEdit := controls[i] as TLabeledEdit;
       temp := thisLblEdit.EditLabel.Caption;
       if pos('Parameter', temp) = 0 then
          begin
           parIndex := fmCalculate.getArrayIndex(vtParameter, thisLblEdit.EditLabel.Caption);
           thisLblEdit.Text := floatToStr(par[parIndex].value);
          end;
      end;
    end;
  end;
end;

{ Stores the new values of the parameters in the global array, par. }
procedure TFmDisplayOutput.LblEdParExit(Sender: TObject);
var
 thisLblEd : TlabeledEdit;
 parIndex : integer;
begin
 thisLblEd := sender as TlabeledEdit;
 if thisLblEd.modified then
   begin
    parIndex := fmCalculate.getArrayIndex(vtParameter, thisLblEd.editlabel.Caption);
    try
      par[parIndex].value := strToFloat(thisLblEd.text);
    except
      messageDlg('Please choose a number', mtWarning, [mbOK], 0);
      FmDisplayOutput.ActiveControl := thisLblEd;
    end;
    refreshLblEdPar;
   end;
end;

procedure TFmDisplayOutput.cbxParamsClick(Sender: TObject);
begin
 DlgReload.CbParams.Checked:=cbxParameters.Checked;
end;

procedure TFmDisplayOutput.cbxStatesClick(Sender: TObject);
begin
 DlgReload.cbState.Checked:=cbxStates.Checked;
end;

procedure TFmDisplayOutput.WriteOutputfromMem;
var
  i, j:integer;
  tempstring: string;
  outfile: textfile;
begin
  assignfile(outfile, outfilename);
  if (FmOptions.RunOptions.AppendOutputFile) and (not FirstWrite) then
   append(outfile)
  else
   rewrite(outfile);    // Create a new file
  try
    for j := 1 to SgModelOutput.RowCount - 1 do    // Start at 1 to skip column numbers
      begin
        tempstring := SgModelOutput.Cells[0, j];
        for i := 1 to SgModelOutput.ColCount - 1 do tempstring := tempstring + ', ' + SgModelOutput.Cells[i, j];
        writeln(outfile, tempstring);
      end;
  finally
    closefile(outfile);
  end;
end;

procedure TFmDisplayOutput.WritePurgeOutputfromMem;
var
  i, j:integer;
  tempstring: string;
  outfile: textfile;
begin
  assignfile(outfile, outfilename);
  try
  if FirstWrite then
   begin
    rewrite(outfile);    // Create a new file
      for j := 1 to SgModelOutput.RowCount - 1 do    // Start at 1 to skip column numbers
        begin
          tempstring := SgModelOutput.Cells[0, j];
          for i := 1 to SgModelOutput.ColCount - 1 do tempstring := tempstring + ', ' + SgModelOutput.Cells[i, j];
          writeln(outfile, tempstring);
        end;
    FirstWrite := False;
   end
  else
   begin
    append(outfile);    // Open existing file for writing
      for j := 3 to SgModelOutput.RowCount - 1 do    // Start at 3 to skip column numbers and variable names and units
        begin
          tempstring := SgModelOutput.Cells[0, j];
          for i := 1 to SgModelOutput.ColCount - 1 do tempstring := tempstring + ', ' + SgModelOutput.Cells[i, j];
          writeln(outfile, tempstring);
        end;
   end;
  ClearGrid;
  finally
    closefile(outfile);
  end;
end;

procedure TFmDisplayOutput.StoreResults(ctime:double; var darray:drivearray; var sarray:statearray;
         var parray:processarray);
var
 j: integer;
begin
 SgModelOutput.rowcount := SgModelOutput.rowcount + 1;
 SgModelOutput.Cells[0,currentrow] := format('%g',[ctime]); // Write time
 for j:= 1 to ModelDef.numdrive    // Write drivers
  do SgModelOutput.Cells[j,currentrow] := format('%.8g',[darray[j].value]);

 for j:= 1 to ModelDef.numstate    // Write state variables
  do SgModelOutput.Cells[ModelDef.numdrive + j,currentrow]
             := format('%.8g',[sarray[j].value]);
 for j:= ModelDef.numstate + 1 to ModelDef.numprocess  // Write process variables
  do SgModelOutput.Cells[ModelDef.numdrive + j, currentrow]
             := format('%.8g',[parray[j].value]);
 currentrow := currentrow + 1;
 if SgModelOutput.RowCount>=100000 then
  begin
   WritePurgeOutputFromMem;
   FmShellMain.LargeOutput := True;
  end;
end;

end.

