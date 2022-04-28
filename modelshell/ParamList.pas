unit ParamList;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TFmParamList = class(TForm)
    lbParamSymbols: TListBox;
    btnSelect: TButton;
    btnCancel: TButton;
    lbParamNames: TListBox;
    rbParameterSymbol: TRadioButton;
    rbParameterName: TRadioButton;
    procedure btnSelectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbParameterSymbolClick(Sender: TObject);
    procedure rbParameterNameClick(Sender: TObject);
    procedure lbParamSymbolsClick(Sender: TObject);
    procedure lbParamNamesClick(Sender: TObject);
  private
    { Private declarations }
  public
    whichEdparselected: integer;
    { Public declarations }
  end;

var
  FmParamList: TFmParamList;

implementation
uses frontend, Display, calculate, stypes;
{$R *.lfm}

procedure TFmParamList.FormCreate(Sender: TObject);
var
 i : integer;
begin
 // fill the boxes with names and symbols
 for i := 1 to modelDef.numparam do begin
  lbParamSymbols.Items.Add(par[i].symbol);
  lbParamNames.Items.Add(par[i].name);
 end;
 // select the first in the list
 lbParamSymbols.ItemIndex := 0;
 lbParamNames.ItemIndex := 0;

 // make sure the symbols box is on top
 lbParamSymbols.visible := true;
 lbParamNames.visible := false;
 LbparamSymbols.Align:=alTop;
 LbParamNames.Align:=alNone;
end;

procedure TFmParamList.btnSelectClick(Sender: TObject);
var
 tempString : string;
 tempIndex : integer;
 ParAlreadySelected:Boolean;
begin

ParAlreadySelected:=False;
tempString := lbParamSymbols.items[lbParamSymbols.itemIndex];
tempIndex := fmCalculate.getArrayIndex(vtParameter,tempString);

// Check if the selected parameter is already displayed.
if (FmDisplayOutput.lblEdPar1.EditLabel.caption = tempString) or
   (FmDisplayOutput.lblEdPar2.EditLabel.caption = tempString) or
   (FmDisplayOutput.lblEdPar3.EditLabel.caption = tempString) or
   (FmDisplayOutput.lblEdPar4.EditLabel.caption = tempString) then
 begin
  messageDlg('That parameter is already displayed', mtWarning, [mbOK], 0);
  ParAlreadySelected:=True;
 end
else
 begin
 // which of the four labels in the display form was selected
  case whichEdparselected of
   1: begin
       FmDisplayOutput.lblEdPar1.EditLabel.caption := tempString;
       FmDisplayOutput.LblEdPar1.text := floatToStr(par[tempIndex].value);
       FmDisplayOutput.LblEdPar1.Enabled := true;
      end;
   2: begin
       FmDisplayOutput.lblEdPar2.EditLabel.caption := tempString;
       FmDisplayOutput.lblEdPar2.text := floatToStr(par[tempIndex].value);
       FmDisplayOutput.lblEdPar2.Enabled := true;   // fix necessary?
      end;
   3: begin
       FmDisplayOutput.lblEdPar3.EditLabel.caption := tempString;
       FmDisplayOutput.lblEdPar3.text := floatToStr(par[tempIndex].value);
       FmDisplayOutput.lblEdPar3.Enabled := true;
      end;
   4: begin
       FmDisplayOutput.lblEdPar4.EditLabel.caption := tempString;
       FmDisplayOutput.lblEdPar4.text := floatToStr(par[tempIndex].value);
       FmDisplayOutput.lblEdPar4.Enabled := true;
      end;
   else
    begin
     messageDlg('There`s a problem with the selection of a parameter',
      mtInformation, [mbOK], 0);
    end;
  end;
 end;
if not ParAlreadySelected then FmParamList.Close;
end;

procedure TFmParamList.rbParameterSymbolClick(Sender: TObject);
begin
 // make sure the symbols box is on top
 lbParamSymbols.visible := true;
 LbParamSymbols.Align:=alTop;
 lbParamNames.visible := false;
 LbParamNames.Align:=alNone;
end;

procedure TFmParamList.rbParameterNameClick(Sender: TObject);
begin
 // make sure the names box is on top
 fmParamList.lbParamSymbols.visible := false;
 LbParamSymbols.Align:=alNone;
 fmParamList.lbParamNames.visible := true;
 LbParamNames.Align:=alTop;
end;

procedure TFmParamList.lbParamSymbolsClick(Sender: TObject);
begin
 // to always have the same name and symbol selected
 lbParamNames.itemIndex := lbParamSymbols.itemIndex;
end;

procedure TFmParamList.lbParamNamesClick(Sender: TObject);
begin
 // to always have the same name and symbol selected
 lbParamSymbols.itemIndex := lbParamNames.itemIndex;
end;

end.
