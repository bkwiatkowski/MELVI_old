unit Options;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Menus, MaskEdit, stypes;

type

  { TFmOptions }

  TFmOptions = class(TForm)
    BtnCancel: TButton;
    BtnOK: TButton;
    BtnDefaults: TButton;
    CbxWriteEvery: TCheckBox;
    EdWriteEvery: TEdit;
    LblTimeUnit5: TLabel;
    LblTimeUnit6: TLabel;
    PcOptions: TPageControl;
    TsRunOptions: TTabSheet;
    TsOutputOptions: TTabSheet;
    RbNormalRun: TRadioButton;
    RbSpecialRun: TRadioButton;
    CbRepeatDrivers: TCheckBox;
    EdRepeatDriveTime: TEdit;
    LblTimeUnit2: TLabel;
    CbResetStates: TCheckBox;
    EdResetStateTime: TEdit;
    LblTimeUnit1: TLabel;
    BtnOpenStates: TButton;
    CbRuntoSS: TCheckBox;
    LblSSCriteria: TLabel;
    EdSSCriteria: TEdit;
    LblSSTime: TLabel;
    EdSSTime: TEdit;
    TsTimeSteps: TTabSheet;
    LblTimeStep: TLabel;
    METimeStep: TMaskEdit;
    LblDiscreteStep: TLabel;
    MEDiscreteStep: TMaskEdit;
    LblTimeUnit4: TLabel;
    Panel1: TPanel;
    CbHoldStatesConstant: TCheckBox;
    CbAppendOutput: TCheckBox;
    RbOutputIntervals: TRadioButton;
    EdOutputTimeStep: TEdit;
    LblTimeUnit3: TLabel;
    EdOutputOffset: TEdit;
    RbOutEndofRunOnly: TRadioButton;
    LblErrorMult: TLabel;
    MeErrorMult: TMaskEdit;
    LblErrorMult2: TLabel;
    RbOutputIntervalsAnnual: TRadioButton;
    EdOutputDayofYear: TEdit;
    LblYearlyCaution: TLabel;
    CbNoOutputFile: TCheckBox;
    procedure CbxWriteEveryClick(Sender: TObject);
    procedure EdWriteEveryExit(Sender: TObject);
    procedure EdWriteEveryKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure CbRepeatDriversClick(Sender: TObject);
    procedure CbResetStatesClick(Sender: TObject);
    procedure EdRepeatDriveTimeExit(Sender: TObject);
    procedure EdResetStateTimeExit(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateFmRunOptions(Sender: TObject);
    procedure RbNormalRunClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure RbOutputIntervalsClick(Sender: TObject);
    procedure CbAppendOutputClick(Sender: TObject);
    procedure CheckOutputStep(var newvalue:double);
    procedure CheckDiscreteStep(var newvalue:double);
    procedure CbRuntoSSClick(Sender: TObject);
    procedure EdOutputTimeStepExit(Sender: TObject);
    procedure CheckOutputOffset(newvalue:double);
    procedure EdOutputOffsetExit(Sender: TObject);
    procedure BtnDefaultsClick(Sender: TObject);
    procedure EdRepeatDriveTimeKeyPress(Sender: TObject; var Key: Char);
    procedure EdResetStateTimeKeyPress(Sender: TObject; var Key: Char);
    procedure EdOutputTimeStepKeyPress(Sender: TObject; var Key: Char);
    procedure EdOutputOffsetKeyPress(Sender: TObject; var Key: Char);
    procedure CbRuntoSSKeyPress(Sender: TObject; var Key: Char);
    procedure EdSSCriteriaExit(Sender: TObject);
    procedure EdSSCriteriaKeyPress(Sender: TObject; var Key: Char);
    procedure BtnOpenStatesClick(Sender: TObject);
    procedure EdSSTimeExit(Sender: TObject);
    procedure EdSSTimeKeyPress(Sender: TObject; var Key: Char);
    procedure METimeStepExit(Sender: TObject);
    procedure METimeStepKeyPress(Sender: TObject; var Key: Char);
    procedure MEDiscreteStepKeyPress(Sender: TObject; var Key: Char);
    procedure MEDiscreteStepExit(Sender: TObject);
    procedure CbHoldStatesConstantClick(Sender: TObject);
    procedure MeErrorMultExit(Sender: TObject);
    procedure MeErrorMultKeyPress(Sender: TObject; var Key: Char);
    procedure EdOutputDayofYearExit(Sender: TObject);
    procedure EdOutputDayofYearKeyPress(Sender: TObject; var Key: Char);
    procedure CbNoOutputFileClick(Sender: TObject);
  private
    { Private declarations }
    fIntialRunOptions: TRunOptions;
  public
    { Public declarations }
    DefaultRunOptions: TRunOptions;
    RunOptions: TRunOptions;
  end;

var
  FmOptions: TFmOptions;

implementation

{$R *.lfm}

uses frontend, data;

procedure TFmOptions.FormCreate(Sender: TObject);
begin
 LblTimeUnit1.Caption := ModelDef.timeunit + '.';
 LblTimeUnit2.Caption := ModelDef.timeunit + '.';
 LblTimeUnit3.Caption := ModelDef.timeunit + '(s), beginning at ' + ModelDef.timeunit;
 LblTimeUnit4.Caption := ModelDef.timeunit + '.';
 LblTimeUnit5.Caption := ModelDef.timeunit + '. Use with extreme caution.';
 LblTimeUnit6.Caption := ModelDef.timeunit + '.';
 with RunOptions do
  begin
    NormalRun := True;
    Time_step := strtofloat(FmOptions.METimeStep.text);
    DiscreteStep := Time_step;
    RepeatDrivers := False;
    RepeatDriveTime := 0;
    ResetStates := False;
    ResetStateTime := 0;
    RuntoSS := False;
    SSCriteria := 0;
    SSTime := 0;
    HoldStatesConstant := False;
    Outputstep := time_step;
    Outputoffset := 0;
    OutputEORonly := False;
    OutputAnnually := False;
    OutputAnnuallyDay := 0;
    AppendOutputFile := False;
    OutputFile := True;
    stepcounter := 1;
    outcounter := 0;
    WriteEvery := 0;
    ErrorMult := 1;
  end;
 DefaultRunOptions := RunOptions;
end;

procedure TFmOptions.CbxWriteEveryClick(Sender: TObject);
begin
  if CbxWriteEvery.Checked then EdWriteEvery.SetFocus;
end;

procedure TFmOptions.EdWriteEveryExit(Sender: TObject);
var
  newwritestep:double;
 begin
  try
 //  try
    if (EdWriteEvery.Text <> '') or (EdWriteEvery.Text <> '0')then
     begin
      RunOptions.NormalRun:=False;
      newwritestep := strtofloat(EdWriteEvery.Text);
      // Add code here to make the write step makes sense
      RunOptions.WriteEvery := newwritestep;
     end;
{   except on E: Exception do
    begin
     RunOptions.Outputstep := newoutstep;
     EdOutputTimeStep.Text := floattostr(newoutstep);
     MessageDlg('Invalid output time step. ' + E.message,
       mtwarning, [mbOK], 0);
    end;     }
  // end;
  finally
   UpdateFmRunOptions(sender);
  end;
 end;

procedure TFmOptions.EdWriteEveryKeyPress(Sender: TObject; var Key: char);
begin
   if (Key = Chr(13)) then EdWriteEveryExit(Sender);
end;

procedure TFmOptions.CbRepeatDriversClick(Sender: TObject);
begin
 RunOptions.RepeatDrivers := CbRepeatDrivers.Checked;
 UpdateFmRunOptions(Sender);
 if RunOptions.RepeatDrivers then EdRepeatDriveTime.SetFocus;
end;

procedure TFmOptions.CbResetStatesClick(Sender: TObject);
var
 i: integer;
begin
 RunOptions.ResetStates := CbResetStates.Checked;
 if not RunOptions.ResetStates then
   for i := 1 to ModelDef.numstate do stat[i].Reset := false;
 UpdateFmRunOptions(Sender);
 if RunOptions.ResetStates then EdResetStateTime.SetFocus;
end;

procedure TFmOptions.EdRepeatDriveTimeExit(Sender: TObject);
begin
 try
  if EdRepeatDriveTime.Text <> '0' then
   begin
    RunOptions.RepeatDriveTime := strtoint(EdRepeatDriveTime.Text);
    CbRepeatDrivers.Checked := True;
   end;
 except
   MessageDlg('Invalid driver repeat time. Run set to Normal run.', mtwarning,
      [mbOK], 0);
   RunOptions.NormalRun := True;
 end;
 UpdateFmRunOptions(Sender);
end;

procedure TFmOptions.EdResetStateTimeExit(Sender: TObject);
begin
 try
  if EdResetStateTime.Text <> '0' then
   begin
    RunOptions.ResetStateTime := strtofloat(EdResetStateTime.Text);
    CbResetStates.Checked := True;
   end;
 except
   MessageDlg('Invalid state variable reset time. Run set to Normal run.',
      mtwarning, [mbOK], 0);
   RunOptions.NormalRun := True;
 end;
 UpdateFmRunOptions(Sender);
end;

procedure TFmOptions.BtnCancelClick(Sender: TObject);
begin
 RunOptions := fIntialRunOptions;
 FmOptions.UpdateFmRunOptions(Sender);
 FmOptions.Close;
 FmShellMain.UpdateFmShellMain;
end;

procedure TFmOptions.FormShow(Sender: TObject);
begin
 if FmShellMain.lastitem.name = 'MITimeSteps' then
   PcOptions.ActivePage := TsTimeSteps
 else if FmShellMain.lastitem.name = 'MISpecialRun' then
   PcOptions.ActivePage := TsRunOptions
 else
   PcOptions.ActivePage := TsOutputOptions;
 UpdateFmRunOptions(Sender);
 fIntialRunOptions := RunOptions;
end;

procedure TFmOptions.UpdateFmRunOptions(Sender: TObject);
begin
 if RunOptions.NormalRun then
   begin
     RbNormalRun.Checked := True;
     CbRepeatDrivers.Checked := False;
     CbRepeatDrivers.Enabled := False;
     EdRepeatDriveTime.Text := floattostr(RunOptions.RepeatDriveTime);
     EdRepeatDriveTime.Enabled := False;
     CbResetStates.Checked := False;
     CbResetStates.Enabled := False;
     CbHoldStatesConstant.Checked := False;
     CbHoldStatesConstant.Enabled := False;
     BtnOpenStates.Enabled := False;
     EdResetStateTime.Enabled := False;
     EdResetStateTime.Text := floattostr(RunOptions.ResetStateTime);
     CbRuntoSS.Checked := False;
     CbRuntoSS.Enabled := False;
     LblSSCriteria.Enabled := False;
     EdSSCriteria.Enabled := False;
     EdSSCriteria.Text := floattostr(RunOptions.SSCriteria);
     LblSSTime.Enabled := False;
     EdSSTime.Enabled := False;
     EdSSTime.Text := floattostr(RunOptions.SSTime);
   end
 else // Special Run
   begin
     RbSpecialRun.Checked := True;
     CbRepeatDrivers.Enabled := True;
     EdRepeatDriveTime.Enabled := True;
     CbResetStates.Enabled := True;
     EdResetStateTime.Enabled := True;
     CbHoldStatesConstant.Enabled := True;
     BtnOpenStates.Enabled := True;
     CbRuntoSS.Enabled := True;
     LblSSCriteria.Enabled := True;
     EdSSCriteria.Enabled := True;
     EdSSTime.Enabled := True;
     LblSSTime.Enabled := True;

     if RunOptions.RepeatDrivers then
      begin
       CbRepeatDrivers.Checked := True;
       EdRepeatDriveTime.Text := floattostr(RunOptions.RepeatDriveTime);
      end
     else
      begin
       CbRepeatDrivers.Checked := False;
       EdRepeatDriveTime.Text := floattostr(DefaultRunOptions.RepeatDriveTime);
      end;

     CbResetStates.Checked := RunOptions.ResetStates;
     if RunOptions.ResetStates then
       EdResetStateTime.Text := floattostr(RunOptions.ResetStateTime)
     else
       EdResetStateTime.Text := floattostr(DefaultRunOptions.ResetStateTime);

     CbHoldStatesConstant.Checked := RunOptions.HoldStatesConstant;

     CbRuntoSS.Checked := RunOptions.RuntoSS;
     if RunOptions.RuntoSS then
      begin
       EdSSCriteria.Text := floattostr(100*RunOptions.SSCriteria);
       EdSSTime.Text := floattostr(RunOptions.SSTime);
      end
     else
      begin
       EdSSCriteria.Text := floattostr(100*DefaultRunOptions.SSCriteria);
       EdSSTime.Text := floattostr(DefaultRunOptions.SSTime);
      end;
   end;

 EdOutputOffset.Text := floattostr(RunOptions.Outputoffset);
 EdOutputTimeStep.Text := floattostr(RunOptions.Outputstep);
 EdOutputDayofYear.Text := floattostr(RunOptions.OutputAnnuallyDay);
 if RunOptions.OutputEORonly then
   begin
     RbOutEndofRunOnly.Checked := True;
     EdOutputTimeStep.Enabled := False;
     EdOutputOffset.Enabled := False;
     EdOutputDayofYear.Enabled := False;
     FmShellMain.MIOutOptions.Checked := True;
   end
 else if (not RunOptions.OutputAnnually) then   // Output intervals
   begin
     RbOutputIntervals.Checked := True;
     EdOutputTimeStep.Enabled := True;
     EdOutputOffset.Enabled := True;
     EdOutputDayofYear.Enabled := False;
     if (RunOptions.Outputstep = DefaultRunOptions.Outputstep) and
        (RunOptions.Outputoffset = DefaultRunOptions.Outputoffset) then
       FmShellMain.MIOutOptions.Checked := False
     else
       FmShellMain.MIOutOptions.Checked := True;
   end
 else                        // Output on specific calendar day
   begin
     RbOutputIntervalsAnnual.Checked := True;
     EdOutputDayofYear.Text := floattostr(RunOptions.OutputAnnuallyDay);
     EdOutputDayofYear.Enabled := True;
     EdOutputTimeStep.Enabled := False;
     EdOutputOffset.Enabled := False;
     FmShellMain.MIOutOptions.Checked := True;
   end;

 if RunOptions.AppendOutputFile then
   begin
     CbAppendOutput.Checked := True;
     FmShellMain.MIOutOptions.Checked := True;
   end
 else
   begin
     CbAppendOutput.Checked := False;
   end;

 if RunOptions.outputfile then
   begin
     CbNooutputfile.Checked := False;
     FmShellMain.MIOutOptions.Checked := True;
   end
 else
   begin
     CbNooutputfile.Checked := True;
   end;

 if RunOptions.WriteEvery <> 0 then
   begin
    CbxWriteEvery.Checked:=True;
    FmShellMain.MIOutOptions.Checked := True;
    EdWriteEvery.Text:= floattostr(RunOptions.WriteEvery);
   end
 else
   begin
    CbxWriteEvery.Checked:=False;
    EdWriteEvery.Text:='0';
   end;
// FIX Add code here to set Output options mI to unchecked if all the options have been turned off.

 METimeStep.text := floattostr(RunOptions.time_step);
 MEDiscreteStep.Text := floattostr(RunOptions.DiscreteStep);
 if (RunOptions.time_step = DefaultRunOptions.Time_step) and
    (RunOptions.DiscreteStep = DefaultRunOptions.DiscreteStep) then
       FmShellMain.MITimeSteps.Checked := False
 else
       FmShellMain.MITimeSteps.Checked := True;

 MeErrorMult.text := inttostr(RunOptions.ErrorMult);
end;

procedure TFmOptions.RbNormalRunClick(Sender: TObject);
begin
 RunOptions.NormalRun := RbNormalRun.Checked;
 UpdateFmRunOptions(Sender);
end;

procedure TFmOptions.BtnOKClick(Sender: TObject);
begin
 FmShellMain.UpdateFmShellMain;
end;

procedure TFmOptions.RbOutputIntervalsClick(Sender: TObject);
begin
 RunOptions.OutputEORonly := RbOutEndofRunOnly.Checked;
 RunOptions.OutputAnnually := RbOutputIntervalsAnnual.Checked;
 UpdateFmRunOptions(Sender);
 if RunOptions.OutputAnnually then EdOutputDayofYear.SetFocus
 else if (not RunOptions.OutputEORonly) then EdOutputTimeStep.SetFocus;
end;

procedure TFmOptions.CbNoOutputFileClick(Sender: TObject);
begin
 RunOptions.outputfile := not CbNoOutputFile.Checked;
 UpdateFmRunOptions(Sender);
end;

procedure TFmOptions.CbAppendOutputClick(Sender: TObject);
begin
 RunOptions.AppendOutputFile := CbAppendOutput.Checked;
 UpdateFmRunOptions(Sender);
end;

procedure TFmOptions.CbRuntoSSClick(Sender: TObject);
begin
 RunOptions.RuntoSS := CbRuntoSS.Checked;
 UpdateFmRunOptions(Sender);
end;

procedure TFmOptions.EdOutputTimeStepExit(Sender: TObject);
var
 newoutstep:double;
begin
 try
  try
   if (EdOutputTimeStep.Text <> ''){and (EdOutputTimeStep.Modified)}then
    begin
     newoutstep := strtofloat(EdOutputTimeStep.Text);
     CheckOutputStep(newoutstep);
     RunOptions.Outputstep := newoutstep;
    end;
  except on E: Exception do
   begin
    RunOptions.Outputstep := newoutstep;
    EdOutputTimeStep.Text := floattostr(newoutstep);
    MessageDlg('Invalid output time step. ' + E.message,
      mtwarning, [mbOK], 0);
   end;
  end;
 finally
  UpdateFmRunOptions(sender);
 end;
end;

procedure TFmOptions.CheckOutputStep(var newvalue:double);
begin
 if newvalue < Runoptions.time_step then
  begin
   newvalue := Runoptions.time_step;
   raise EStepTooSmall.Create('Output time step is less than integrator '
          + 'time step. Output time step increased to time step. ');
  end;

 if newvalue > time_stop - time_start then
  begin
   newvalue := time_stop - time_start;
   raise EStepTooLarge.Create('Output time step is greater than run '
     + 'length. Output time step decreased to run length. ');
  end;

 if abs(newvalue/RunOptions.Time_step - round(newvalue/RunOptions.Time_step)) > epsilon then
  begin
    newvalue := RunOptions.time_step;
    raise EStepNotMultiple.Create('Output time step must be a multiple '
           + 'of integrator time step. Output time step changed to current ' +
           'integrator time_step. ');
  end;
end;

procedure TFmOptions.CheckOutputOffset(newvalue:double);
begin
{ if newvalue < RunOptions.time_step then
   raise Exception.Create('Output offset is less than simulation time step. '); }
 if newvalue > time_stop then
   raise Exception.Create('Output offset is greater than simulation length. ');
end;

procedure TFmOptions.EdOutputOffsetExit(Sender: TObject);
var
 newoutoffset: double;
begin
 try
  try
   if (EdOutputOffset.Text <> ''){and (EdOutputOffset.Modified)}then
    begin
     newoutoffset := strtoint(EdOutputOffset.Text);
     CheckOutputOffset(newoutoffset);
     RunOptions.Outputoffset := newoutoffset;
    end;
  except on E: Exception do
   begin
    RunOptions.Outputoffset := RunOptions.Time_step;
    MessageDlg('Invalid output offset. ' + E.message +
      'Output offset reset to simulation time step. ', mtwarning, [mbOK], 0);
   end;
  end;
 finally
  UpdateFmRunOptions(sender);
 end;
end;

procedure TFmOptions.BtnDefaultsClick(Sender: TObject);
begin
 if MessageDlg('Reset ALL options (Run, Output and Time step options) to the ' +
        'default values?', mtwarning,
        [mbyes,mbno],0) = mryes then RunOptions := DefaultRunOptions;
 UpdateFmRunOptions(BtnDefaults);
end;

procedure TFmOptions.EdRepeatDriveTimeKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then EdRepeatDriveTimeExit(Sender);
end;

procedure TFmOptions.EdResetStateTimeKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = Chr(13)) then EdResetStateTimeExit(Sender);
end;

procedure TFmOptions.EdOutputTimeStepKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = Chr(13)) then EdOutputTimeStepExit(Sender);
end;

procedure TFmOptions.EdOutputOffsetKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = Chr(13)) then EdOutputOffsetExit(Sender);
end;

procedure TFmOptions.CbRuntoSSKeyPress(Sender: TObject; var Key: Char);
begin
  CbRuntoSSClick(Sender);
end;

procedure TFmOptions.EdSSCriteriaExit(Sender: TObject);
begin
 try
  if EdSSCriteria.Text <> '0' then
   begin
    RunOptions.SSCriteria := strtofloat(EdSSCriteria.Text)/100;
    CbRuntoSS.Checked := True;
   end;
 except
   MessageDlg('Invalid steady state criteria. Run set to Normal run.',
      mtwarning, [mbOK], 0);
   RunOptions.NormalRun := True;
 end;
 UpdateFmRunOptions(Sender);
 if RunOptions.RuntoSS then EdSSTime.SetFocus;
end;

procedure TFmOptions.EdSSCriteriaKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = Chr(13)) then EdSSCriteriaExit(Sender);
end;

procedure TFmOptions.BtnOpenStatesClick(Sender: TObject);
begin
 DataForm.ShowStates;  // Show the data form containing state variables
end;

procedure TFmOptions.EdSSTimeExit(Sender: TObject);
begin
 try
  if EdSSTime.Text <> '0' then
   begin
    RunOptions.SSTime := strtoint(EdSSTime.Text);
    CbRuntoSS.Checked := True;
   end;
 except
   MessageDlg('Invalid steady state time. Run set to Normal run.',
      mtwarning, [mbOK], 0);
   RunOptions.NormalRun := True;
 end;
 UpdateFmRunOptions(Sender);
end;

procedure TFmOptions.EdSSTimeKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = Chr(13)) then EdSSTimeExit(Sender);
end;

procedure TFmOptions.METimeStepExit(Sender: TObject);
var
 newtimestep: double;         // fix
begin          // remove newtimestep once fix MEtimestep.modified
 if (METimeStep.text <> '') then
  begin
   newtimestep := strtofloat(METimeStep.text);
   if newtimestep <> RunOptions.Time_step then
   try
    try
      RunOptions.Time_Step := newtimestep;
      ReadytoRun := False;
      RunComplete := False; // Previous runs are invalid.
      FmShellMain.UpdateFmShellMain;  // Update the form.
      CheckOutputStep(RunOptions.Outputstep);
    except
     on E: EStepError do
      begin
       RunOptions.Outputstep := RunOptions.time_step;
       FmOptions.EdOutputTimeStep.Text := floattostr(RunOptions.time_step);
       MessageDlg(E.message, mtwarning, [mbOK], 0);
      end;
    end;
   finally
    UpdateFmRunOptions(sender);
   end;
  end;
end;

procedure TFmOptions.METimeStepKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = Chr(13)) then MeTimeStepExit(Sender);
end;

procedure TFmOptions.MEDiscreteStepKeyPress(Sender: TObject;
  var Key: Char);
begin
 if (Key = Chr(13)) then MeTimeStepExit(Sender);
end;

procedure TFmOptions.MEDiscreteStepExit(Sender: TObject);
var
 newtimestep: double;
begin                    // fix
 if (MEDiscreteStep.text <> '') then
  begin
   newtimestep := strtofloat(MEDiscreteStep.text);
   if newtimestep <> RunOptions.DiscreteStep then
   try
    try
      RunOptions.DiscreteStep := newtimestep;
      ReadytoRun := False;
      RunComplete := False; // Previous runs are invalid.
      FmShellMain.UpdateFmShellMain;  // Update the form.
      CheckDiscreteStep(RunOptions.DiscreteStep);
    except
     on E: EStepError do
      begin
       RunOptions.DiscreteStep := RunOptions.time_step;
       FmOptions.MEDiscreteStep.Text := floattostr(RunOptions.time_step);
       MessageDlg(E.message, mtwarning, [mbOK], 0);
      end;
    end;
   finally
    UpdateFmRunOptions(sender);
   end;
  end;
end;

procedure TFmOptions.CheckDiscreteStep(var newvalue:double);
begin
 if newvalue < Runoptions.time_step then
  begin
   newvalue := Runoptions.time_step;
   raise EStepTooSmall.Create('Discrete time step is less than integrator '
          + 'time step. Discrete time step increased to time step. ');
  end;

 if newvalue > time_stop - time_start then
  begin
   newvalue := time_stop - time_start;
   raise EStepTooLarge.Create('Discrete time step is greater than run '
     + 'length. Discrete time step decreased to run length. ');
  end;

 if abs(newvalue/RunOptions.Time_step - round(newvalue/RunOptions.Time_step)) >
                                                                 epsilon then
  begin
    newvalue := RunOptions.time_step;
    raise EStepNotMultiple.Create('Discrete time step must be a multiple '
           + 'of integrator time step. Discrete time step changed to current ' +
           'integrator time step. ');
  end;
end;

procedure TFmOptions.CbHoldStatesConstantClick(Sender: TObject);
begin
 RunOptions.HoldStatesConstant := CbHoldStatesConstant.Checked;
 UpdateFmRunOptions(Sender);
end;

procedure TFmOptions.MeErrorMultExit(Sender: TObject);
begin         // fix - add error checking here
 RunOptions.ErrorMult := strtoint(MeErrorMult.Text);
end;

procedure TFmOptions.MeErrorMultKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = Chr(13)) then MeErrorMultExit(Sender);
end;

procedure TFmOptions.EdOutputDayofYearExit(Sender: TObject);
begin
 if EdOutputDayofYear.Text <> '' then
  begin
   RunOptions.OutputAnnuallyDay := strtofloat(EdOutputDayofYear.text);
  end;
 UpdateFmRunOptions(sender);
end;

procedure TFmOptions.EdOutputDayofYearKeyPress(Sender: TObject;
  var Key: Char);
begin
 if (Key = Chr(13)) then EdOutputDayofYearExit(Sender);
end;

end.


