program modelbatch;

uses
  Forms, Interfaces,
  batchmain in 'batchmain.pas' {FmBatchMain},
  aboutbox in 'aboutbox.pas' {FmAbout},
  calculate in 'calculate.pas' {FmCalculate},
  data in 'data.pas' {DataForm},
  Display in 'Display.pas' {FmDisplayData},
  equations in 'equations.pas',
  fileio in 'fileio.pas',
  frontend in 'frontend.pas' {MainForm},
  integrator in 'integrator.pas',
  note in 'note.pas' {FmNote},
  ProgressBar in 'ProgressBar.pas' {FmProgress},
  ReloadDlg in 'ReloadDlg.pas' {DlgReload},
  ScaleDlg in 'Scaledlg.pas' {DlgScale},
  SeriesForm in 'SeriesForm.pas' {FmSeries},
  stypes in 'stypes.pas',
  trouble in 'Trouble.pas' {FmTrouble},
  ParamList in 'ParamList.pas' {FmParamList},
  Options in 'Options.pas' {FmOptions};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmBatchMain, FmBatchMain);
  Application.CreateForm(TFmAbout, FmAbout);
  Application.CreateForm(TFmShellMain, FmShellMain);
  Application.CreateForm(TFmParamList, FmParamList);
  Application.CreateForm(TFmDisplayOutput, FmDisplayOutput);
  Application.CreateForm(TFmOptions, FmOptions);
  FmShellMain.Visible := False;
  FmShellMain.Enabled := False;
  Application.Run;
end.
