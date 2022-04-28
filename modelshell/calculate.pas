unit calculate;

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MaskEdit, ExtCtrls, stypes;

type
  TFmCalculate = class(TForm)
    PnTop: TPanel;
    ScrollBox1: TScrollBox;
    BtnShowallProc: TButton;
    BtnCancel: TButton;
    RgLabelType: TRadioGroup;
    BtnOK: TButton;
    GbStatus: TGroupBox;
    ShStatus: TShape;
    BtnSaveParamFile: TButton;
    procedure FormSetup(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateForm(Sender: TObject; CalSteadyState:Boolean);
    function GetArrayIndex(VarType:TVarType; symbol: string):Integer;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FmCalculate: TFmCalculate;

implementation

uses frontend, equations, fileio, data;

{$R *.lfm}

{ This procedure changes the original generic names of objects on the form to
  names that match the real life names of the variables. To do this the
  procedure loops over all components in the form looking for labels and
  maskedits. For each label or maskedit it determines whether it refers to
  a state variable, driver variable, process variable, or parameter. Then it
  searches the appropriate array (stat, drive, proc, or par respectively) to
  find the array index, the appropriate name and units, and, if a parameter
  file and driver file were chosen, the current values. }
procedure TFmCalculate.FormSetup(Sender: TObject);
begin
end;  // end FormSetup procedure

{ This procedure updates all the values on the form before showing it. }
procedure TFmCalculate.FormShow(Sender: TObject);
begin  // FormShow procedure

end; // FormShow procedure

{ Update the values and the Status button on the form. }
procedure TFmCalculate.UpdateForm(Sender: TObject; CalSteadyState: Boolean);
begin
end;

{ Function to search the global arrays for a particular variable. Returns the
  array index of the specified variable. Returns a zero if the variable was not
  found. }
function TFmCalculate.GetArrayIndex(VarType:TVarType; symbol: string):Integer;
var
 i, number:integer;
begin
 number := 0;
 case VarType of
   vtParameter:
    begin
     for i := 1 to ModelDef.numparam do
      begin
        if symbol = par[i].symbol then number := i;
      end;
    end;
   vtDriver:
    begin
     for i := 1 to ModelDef.numdrive do
      begin
        if symbol = drive[i].symbol then number := i;
      end;
    end;
   vtState:
    begin
     for i := 1 to ModelDef.numstate do
      begin
        if symbol = stat[i].symbol then number := i;
      end;
    end;
   vtProcess:
    begin
     for i := ModelDef.numstate to ModelDef.numprocess do
      begin
        if symbol = proc[i].symbol then number := i;
      end;
    end;
 end;
 
 if number <> 0 then
   GetArrayIndex := number
 else
   raise Exception.Create('Could not find symbol, ' + symbol + ' in array.');
end;

end.

