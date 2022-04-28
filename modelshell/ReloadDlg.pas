{ A modal dialog box to select what to read from the parameter file; state
  variables, parameters or both. }
unit ReloadDlg;

interface

uses SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs;

type
  TDlgReload = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    MoDirections: TMemo;
    CbParams: TCheckBox;
    CbState: TCheckBox;
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DlgReload: TDlgReload;

implementation

uses stypes, fileio, frontend;

{$R *.lfm}

// Reload the selected items from the parameter file
procedure TDlgReload.OKBtnClick(Sender: TObject);
var
   temppar: paramarray;
   tempstat: statearray;
begin
  tempstat := stat;       // to get names and units into the temporary array
  temppar := par;         // to get names and units into the temporary array
 // Read data from  the parameter file and store in the temporary arrays
  ReadParamFile(paramfilename, modeldef.numparam, temppar, modeldef.numstate,
                 tempstat, FmShellMain.currentresid);
  // Copy values read from the file into the global variables only if checkbox checked
  if CbParams.Checked and CbState.Checked then
   begin
    stat := tempstat;
    par := temppar;
   end
  else if CbParams.Checked then
     par := temppar
  else if CbState.Checked then
     stat := tempstat;
end;

end.
