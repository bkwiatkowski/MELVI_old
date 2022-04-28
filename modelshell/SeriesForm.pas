{ This form is a dialog which contains a listbox. The listbox is used to hold
  the list of all variables in the model. The user chooses items from the
  to be shown in the chart on the Display form. }
unit SeriesForm;

interface

uses SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs, stypes;

type
  TFmSeries = class(TForm)
    Panel1: TPanel;
    LbxChooseSeries: TListBox;
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    BtnClearSelection: TButton;
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure BtnClearSelectionClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FmSeries: TFmSeries;

implementation

{$R *.lfm}

uses display;

{ Depending on whether the user is choosing items for the x or y axis disable or
  enable multiple selections accordingly. }
procedure TFmSeries.FormShow(Sender: TObject);
var
 i, seriesindex:integer;
 seriesname:string;
begin
 FmDisplayOutput.FillListBox(FmSeries.LbxChooseSeries);
 if FmDisplayOutput.CurrentAxis = axBottom then   // Choosing Independent Axis
  begin
   // Set the form caption including instructions
   FmSeries.Caption := 'Independent Axis - Choose one series.';
   // Can only choose one series for independent axis, disable multiselect
   LbxChooseSeries.MultiSelect := False;
   BtnClearSelection.Enabled := False;
   BtnClearSelection.Visible := False;
   seriesname := FmDisplayOutput.xAxis;
   seriesindex := LbxChooseSeries.Items.Indexof(seriesname);
   LbxChooseSeries.ItemIndex := seriesindex;
  end
 else // Dependent Axis
  begin
   // Set the form caption including instructions
   FmSeries.Caption := 'Dependent Axis - Choose a maximum of 10 series.';
   // Can choose up to 10 series for the dependent axis, enable multiselect
   LbxChooseSeries.MultiSelect := True;
   BtnClearSelection.Enabled := True;
   BtnClearSelection.Visible := True;
   for i := 0 to LbxChooseSeries.Items.Count - 1 do
    LbxChooseSeries.Selected[i] := FmDisplayOutput.LbxSeriesSelect.Selected[i];
  end;
end;

{}
procedure TFmSeries.OKBtnClick(Sender: TObject);
var
 i:integer;
begin
 if FmDisplayOutput.CurrentAxis = axBottom then   // Set the Independent Axis
  begin
   // Loop over the items in the list
   for i := 0 to LbxChooseSeries.Items.Count - 1 do
    if LbxChooseSeries.Selected[i] then
       FmDisplayOutput.xAxis := LbxChooseSeries.Items[i];
  end
 else  // Set the Dependent Axes. CurrentAxis = axLeft
  begin
   for i := 0 to LbxChooseSeries.Items.Count - 1 do
       FmDisplayOutput.LbxSeriesSelect.Selected[i]:=LbxChooseSeries.Selected[i];
  end;
end;

procedure TFmSeries.BtnClearSelectionClick(Sender: TObject);
begin
 FmDisplayOutput.ClearSeriestoPlot(FmSeries.LbxChooseSeries);;
end;

procedure TFmSeries.FormResize(Sender: TObject);
var
 numcol:integer;
begin
  numcol := round(LbxChooseSeries.ClientWidth/(8*stringlength));  // assumes 8 units per character
  if numcol < 1 then numcol := 1;
  LbxChooseSeries.Columns:=round(numcol);
end;

end.
