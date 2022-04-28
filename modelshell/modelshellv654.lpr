program modelshellv654;

{$MODE Delphi}
    
uses
  Forms, Interfaces,
  frontend in 'frontend.pas' {FmShellMain},
  aboutbox in 'aboutbox.pas' {FmAbout},
//  bkPageControl in 'bkPageControl.pas',         { TODO 2 -obk : add pagecontrol back in }
  trouble in 'trouble.pas' {FmTrouble},
  data in 'data.pas' {DataForm},
  display in 'display.pas' {FmDisplayOutput},
  equations in 'equations.pas',
  fileio in 'fileio.pas',
  integrator in 'integrator.pas',
  note in 'note.pas' {FmNote},
  Options in 'Options.pas' {FmOptions},
  ParamList in 'ParamList.pas' {FmParamList},
  parameter in 'parameter.pas' {FmParameter},
  ProgressBar in 'ProgressBar.pas' {FmProgress},
  ReloadDlg in 'ReloadDlg.pas' {DlgReload},
  ScaleDlg in 'ScaleDlg.pas' {DlgScale},
  SeriesForm in 'SeriesForm.pas' {FmSeries},
  stypes in 'stypes.pas',
  calculate in 'calculate.pas' {FmCalculate};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Modelshell';
  Application.CreateForm(TFmShellMain, FmShellMain);
  Application.CreateForm(TFmOptions, FmOptions);
  Application.CreateForm(TFmDisplayOutput, FmDisplayOutput);
  Application.CreateForm(TDataForm, DataForm);
  Application.CreateForm(TFmAbout, FmAbout);
  Application.CreateForm(TFmTrouble, FmTrouble);
  Application.CreateForm(TFmNote, FmNote);
  Application.CreateForm(TFmParamList, FmParamList);
  Application.CreateForm(TFmParameter, FmParameter);
  Application.CreateForm(TFmProgress, FmProgress);
  Application.CreateForm(TDlgReload, DlgReload);
  Application.CreateForm(TDlgScale, DlgScale);
  Application.CreateForm(TFmSeries, FmSeries);
  Application.CreateForm(TFmCalculate, FmCalculate);
  Application.Run;
end.
