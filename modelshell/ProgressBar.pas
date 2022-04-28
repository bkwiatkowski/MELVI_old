unit ProgressBar;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls;
//type Tapp = (mainmodel, fuzzycal);
type Tcprocedure = procedure of object;
type
  TFmProgress = class(TForm)
    PbRunStatus: TProgressBar;
    LblBegin: TLabel;
    LblEnd: TLabel;
    BtnCancel: TButton;
    procedure BtnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
//    apprunning: Tapp;
    CancelProcedure: Tcprocedure;
  end;

var
  FmProgress: TFmProgress;

implementation

uses frontend;

{$R *.lfm}

procedure TFmProgress.BtnCancelClick(Sender: TObject);
begin
 if messageDlg('Do you want to abort this run?', mtConfirmation,
   [mbYes, mbNo], 0) = mrYes
 then
  begin
//   raise EUserCancel.Create('User canceled run. ');
{   if apprunning = mainmodel then
     FmShellMain.CancelRun
   else if apprunning = fuzzycal then
     FmCalMain.CancelRun;   }
    CancelProcedure;
  end;
end;

procedure TFmProgress.FormShow(Sender: TObject);
begin
  PbRunStatus.Position := 0;
end;

procedure TFmProgress.FormCreate(Sender: TObject);
begin
 CancelProcedure := FmShellMain.CancelRun;
end;

end.
