{ A modal dialog box to modify the axis scales on the chart in the display.pas
  file. The dialog box contains edit boxes for the axis minimum, maximum, and
  increment; a radio group box to choose linear or logarithmic axis; and a
  checkbox to enable/disable autoscaling. Any changes made to the edit boxes
  automatically disable autoscaling. }
unit ScaleDlg;

interface

uses SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, stypes;

type
  TDlgScale = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    LblMin: TLabel;
    LblMax: TLabel;
    EdMinValue: TEdit;
    EdMaxValue: TEdit;
    LblScale: TLabel;
    RGAxisType: TRadioGroup;
    LblIncrem: TLabel;
    EdIncrem: TEdit;
    CBAuto: TCheckBox;
    procedure OKBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EditExit(Sender:TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BottomMinValue, BottomMaxValue, BottomIncrem:double;
    LeftMinValue, LeftMaxValue, LeftIncrem:double;
  end;

var
  DlgScale: TDlgScale;

implementation

uses display, dialogs;  { TODO 2 -obk : remove dialogs from uses clause once chart is working }

{$R *.lfm}

{ Before showing the form set the Dialog box title and default values for all
  objects. These values are dependent on which axis is the current axis in the
  chart. }
procedure TDlgScale.FormShow(Sender: TObject);
begin
  if FmDisplayOutput.CurrentAxis = axBottom then    // Independent Axis
    begin
      DlgScale.Caption := 'Independent Axis';
      if FmDisplayOutput.ChBAxisLogarithm.Enabled then
        RGAxisType.ItemIndex := 1     // Check logarithmic radio button
      else
        RGAxisType.ItemIndex := 0;    // Check linear radio button
      CBAuto.Checked := not FmDisplayOutput.ChOutput.Extent.UseXMin;
      EdMinValue.text := floattostr(FmDisplayOutput.ChOutput.Extent.XMin);
      EdMaxValue.text := floattostr(FmDisplayOutput.ChOutput.Extent.XMax);
    end
  else  // CurrentAxis = axLeft     Dependent Axis
    begin
      DlgScale.Caption := 'Dependent Axis' ;
      if FmDisplayOutput.ChLAxisLogarithm.Enabled then
        RGAxisType.ItemIndex := 1    // Check logarithmic radio button
      else
        RGAxisType.ItemIndex := 0;   // Check linear radio button
      CBAuto.Checked := not FmDisplayOutput.ChOutput.Extent.UseYMin;
      EdMinValue.text := floattostr(FmDisplayOutput.ChOutput.Extent.YMin);
      EdMaxValue.text := floattostr(FmDisplayOutput.ChOutput.Extent.YMax);
    end;
end;

// Implement any changes made in this dialog
procedure TDlgScale.OKBtnClick(Sender: TObject);
begin
  { Changing the axis type won't work until the bug in TaChart that causes an access
     violation when setting the axis for the lineseries is fixed.  Sept 22, 2011 BK}
// Set the axis type
 if RgAxisType.itemindex = 0 then   // Linear axis
  if FmDisplayOutput.CurrentAxis = axBottom then
    FmDisplayOutput.ChBAxisLogarithm.Enabled:=false
  else
    FmDisplayOutput.ChLAxisLogarithm.Enabled:=false
 else                   // Logarithmic axis
  if FmDisplayOutput.CurrentAxis = axBottom then
    FmDisplayOutput.ChBAxisLogarithm.Enabled:=true
  else
    FmDisplayOutput.ChLAxisLogarithm.Enabled:=true;

//Set the minimum and maximum axis values
 if CbAuto.checked then    // Autoscale
  begin
   if FmDisplayOutput.CurrentAxis=axBottom then
    begin
     FmDisplayOutput.ChOutput.Extent.UseXMin := False;
     FmDisplayOutput.ChOutput.Extent.UseXMax := False;
    end
   else   // Left Axis
    begin
     FmDisplayOutput.ChOutput.Extent.UseYMin := False;
     FmDisplayOutput.ChOutput.Extent.UseYMax := False;
    end;
  end
 else        // Manual axis scale
  begin
   if FmDisplayOutput.CurrentAxis=axBottom then
    begin
     FmDisplayOutput.ChOutput.Extent.UseXMin := True;
     FmDisplayOutput.ChOutput.Extent.XMin := strtofloat(EdMinValue.Text);
     FmDisplayOutput.ChOutput.Extent.UseXMax := True;
     FmDisplayOutput.ChOutput.Extent.XMax := strtofloat(EdMaxValue.Text);
    end
   else  // Left Axis
    begin
     FmDisplayOutput.ChOutput.Extent.UseYMin := True;
     FmDisplayOutput.ChOutput.Extent.YMin := strtofloat(EdMinValue.Text);
     FmDisplayOutput.ChOutput.Extent.UseYMax := True;
     FmDisplayOutput.ChOutput.Extent.YMax := strtofloat(EdMaxValue.Text);
    end;
  end
end;

procedure TDlgScale.EditExit(Sender:TObject);
begin
 if (Sender as TEdit).modified = true then
    CBAuto.Checked := false;
end;

end.
