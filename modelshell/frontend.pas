{ This file creates and controls the main window of the modeling application.
  This window is called FmShellMain in the code. Describe the use of ReadytoRun and
  RunComplete here.}
unit frontend;

{$MODE Delphi}

interface                      

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$ifdef Darwin}
    MacOSAll,
  {$endif}
  StdCtrls, Menus, stypes, ExtCtrls, MaskEdit, Printers, TAGraph, Lazfileutils;

const
  epsilon = 1e-8;
  fullstep = 1;
  DefaultUnixDirectory = '/user/share/CrEquations';
  BundleResourceDirectory = '/Contents/Resources/';

type

  { TFmShellMain }

  TFmShellMain = class(TForm)
    BtnCloseShell: TButton;
    MainMenu1: TMainMenu;
    MEStartTime: TMaskEdit;
    MIFiles: TMenuItem;
    MISaveParam: TMenuItem;
    DlgOpenParam: TOpenDialog;
    DlgSaveParam: TSaveDialog;
    MIEdit: TMenuItem;
    MIEditParam: TMenuItem;
    MIEditStates: TMenuItem;
    MIChooseDrv: TMenuItem;
    MIOutput: TMenuItem;
    DlgOpenDriver: TOpenDialog;
    MIChooseParam: TMenuItem;
    MICalculate: TMenuItem;
    MIView: TMenuItem;
    MIShowChart: TMenuItem;
    MIParamFile: TMenuItem;
    MIDriverFile: TMenuItem;
    MIEditDrv: TMenuItem;
    DlgSaveDriver: TSaveDialog;
    DlgSaveOutput: TSaveDialog;
    MISaveAsParam: TMenuItem;
    MIExit: TMenuItem;
    MIBar: TMenuItem;
    MIShowTable: TMenuItem;
    ParamLabel: TLabel;
    DriverLabel: TLabel;
    OutputLabel: TLabel;
    StartLabel: TLabel;
    StopLabel: TLabel;
    PresentLabel: TLabel;
    MICalcSS: TMenuItem;
    MIEditDrv2: TMenuItem;
    MEStopTime: TMaskEdit;
    ParamBox: TEdit;
    DriverBox: TEdit;
    OutputBox: TEdit;
    BtnRun: TButton;
    PresentBox: TEdit;
    BtnReload: TButton;
    MICalTime: TMenuItem;
    MIViewOutFile: TMenuItem;
    DlgOpenOutput: TOpenDialog;
    EdNumSpecies: TEdit;       // MEL specific
    LblWelcome: TLabel;        // MEL specific
    LblDirections: TLabel;
    Run1: TMenuItem;
    MISpecialRun: TMenuItem;
    Help1: TMenuItem;
    MIAbout: TMenuItem;
    MINormalRun: TMenuItem;
    N1: TMenuItem;
    MIAutoChart: TMenuItem;
    N2: TMenuItem;
    MIOutOptions: TMenuItem;
    N3: TMenuItem;
    MITimeSteps: TMenuItem;
    MISaveMemOutput: TMenuItem;
    MIChooseOutput: TMenuItem;     // MEL specific
    procedure BtnCloseShellClick(Sender: TObject);
    procedure ChooseParamFile(Sender: TObject);
    procedure ChooseDriver(Sender: TObject);
    procedure ChooseOutputFile(Sender: TObject);
    procedure BtnRunClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MISaveParamClick(Sender: TObject);
    procedure MISaveAsParamClick(Sender: TObject);
    procedure MIEditParamClick(Sender: TObject);
    procedure MIEditStatesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MeStartTimeExit(Sender: TObject);
    procedure MeStopTimeExit(Sender: TObject);
    procedure BtnReloadClick(Sender: TObject);
    procedure UpdateFmShellMain;
    procedure MIEditDrvClick(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MICalcSSClick(Sender: TObject);
    procedure DisplayOutput(Sender: TObject);
    procedure SetNumSpecies(anEditBox: TEdit);     // MEL specific
    procedure EdNumSpecieschange(Sender: TObject);    // MEL specific
    procedure EdNumSpeciesExit(Sender: TObject);      // MEL specific
    procedure MICalTimeClick(Sender: TObject);
    procedure MIViewOutFileClick(Sender: TObject);
    procedure MEStopTimeKeyPress(Sender: TObject; var Key: Char);
    procedure MEStartTimeKeyPress(Sender: TObject; var Key: Char);
    procedure MIOpenOptionsClick(Sender: TObject);
    procedure MINormalRunClick(Sender: TObject);
    procedure MIAutoChartClick(Sender: TObject);
    procedure ParamBoxKeyPress(Sender: TObject; var Key: Char);
    procedure DriverBoxKeyPress(Sender: TObject; var Key: Char);
    procedure OutputBoxKeyPress(Sender: TObject; var Key: Char);
    procedure EdNumSpeciesKeyPress(Sender: TObject; var Key: Char);
    procedure MISaveMemOutputClick(Sender: TObject);
    function GetResourcePath(): string;
  private
    { Private declarations }
    procedure ResetStatesNow(Sender: TObject);
  public
    { Public declarations }
    CurrentPath:string;
    RunningInteractive: Boolean;
    lastitem: TComponent;
    CurrentResid: Double;
    BeginningofRun: Boolean;
    LargeOutput: Boolean;
    initialstates:statearray;
    procedure ShowProgressBar;
    procedure CancelRun;
 end;

var
  FmShellMain: TFmShellMain;
  time,time_start,Time_stop,CalTime: double;
  ModelDef: TModelDef;
  stat: statearray;
  drive: drivearray;
  par: paramarray;
  proc: processarray;
  paramfilename,driverfilename,outfilename: string;
  NeedToSavePar: Boolean = False;
  ReadyToRun: Boolean = False;
  RunComplete: Boolean = False;
  counttt:integer;
//  NewParamFile: Boolean;
//  NewDriverFile: Boolean;
  stopRun: boolean = false;
//  NumSpProc,NumSpPar:integer;     // MEL specific
//  numspp:integer;                 // MEL specific
implementation

uses fileio, calculate, display, ReloadDlg, data, parameter,
     equations, integrator, note, aboutbox, trouble, ProgressBar, Options;

{$R *.lfm}

{ This procedure sets up the modeling application. }
procedure TFmShellMain.FormCreate(Sender: TObject);
var
 tempstring, tempstring2: string;
 ll:integer;
begin
// Determine current path for dialog box initial directories
  CurrentPath := GetResourcePath();

  {CurrentPath := ExtractFilePath(Application.EXEName);       }
  LazFileUtils.SetCurrentDirUTF8(CurrentPath);

// Set initial values for model timing
  Time_start := strtofloat(MEStartTime.text);
  Time_stop := strtofloat(MEStopTime.text);
  CalTime := time_start;
// Set initial value for the number of species
//  numspp := 1;          // MEL specific

{ Call the counts procedure which defines the model structure. This procedure
  sets the names and units of the state variables, driver variables, process
  variables, and parameters. It also sets the values of the ModelDef structure
  which defines the number of state variables, driver variables, process
  variables and parameters.
}
  counts;
  Application.Title := ModelDef.modelname;
  next_drive := drive; // Initialize names and units of next_drive & last_drive
  last_drive := drive;
  FmShellMain.Caption := ModelDef.modelname;  // Set the FmShellMain caption
  tempstring := Application.Title;
  tempstring := lowercase(tempstring);
  RunningInteractive := True;
  BeginningofRun := True;
  fileio.driverlist:=tstringlist.create;
  fileio.driverlist.StrictDelimiter:=true;
  fileio.driverlist.Delimiter:=',';
 // Initialize driver names and units
  tempstring := 'Time, ';
  tempstring2 := ModelDef.timeunit;
  for ll:=1 to ModelDef.numdrive do
    begin
      tempstring := tempstring + drive[ll].name;
      tempstring2 := tempstring2 + drive[ll].units;
    end;
  fileio.driverlist.Add(tempstring);
  fileio.driverlist.Add(tempstring2);
end;

{ This procedure enables the user to choose a parameter file using several
  different methods. The user can click Choose Parameter file from the menu,
  click on the label Parameter File, or type a filename directly in the box.
  If the user chooses using either the menu or the label, an open file dialog
  is displayed. }
procedure TFmShellMain.ChooseParamFile(Sender: TObject);
var
 oldparamfilename : string;
 replace: Word;
begin
 oldparamfilename := paramfilename;
 DlgOpenParam.InitialDir:=CurrentPath;

 // If the user typed directly in the box
 if Sender is TEdit then paramfilename := parambox.text  // Set paramfilename
 else if RunningInteractive then // The user used either the menu or clicked on the label
   begin  // Show the open file dialog
           // First set the dialog box default filename to the current paramfile
      DlgOpenParam.filename := paramfilename;
          // If the user chooses OK in the dialog then set the new paramfilename
      if DlgOpenParam.execute then paramfilename := DlgOpenParam.filename;
   end;

 if (paramfilename <> oldparamfilename) or not RunningInteractive then
  begin
   if paramfilename <> '' then  // Be sure something was entered for the filename
     try
      if not LazFileUtils.FileExistsUTF8(paramfilename)  then  // If the paramter file doesn't exist
        // Create a new parameter file using values in memory
       begin
        WriteParamFile(paramfilename, ModelDef.numparam, par, ModelDef.numstate,
                        stat, currentresid);
//        NewParamFile := True;
       end
      else // Parameter file does exist, so read the values into the global arrays.
       begin
        if (oldparamfilename <> '') and RunningInteractive then
         replace := MessageDlg('Replace current values with those in file, ' +
            paramfilename + ' ?', mtConfirmation, [mbYes,mbNo], 0)
        else
         replace := mryes;
        if replace = mryes then
         begin
           ReadParamFile(paramfilename,ModelDef.numparam,par,ModelDef.numstate,
                         stat, currentresid);
//           NewParamFile := False;
         end
        else
         begin
           paramfilename := oldparamfilename;
//           NewParamFile := False;  // fix necessary here and if so what should the value be???
         end;
       end;
     except
      paramfilename := oldparamfilename;
      raise;
     end;
   RunComplete := False;
   Readytorun := false;   // Can't run the model.
  end;

 CurrentPath := ExtractFilePath(paramfilename);
 LazFileUtils.SetCurrentDirUTF8(CurrentPath);
 UpdateFmShellMain;  // Update the form
end;

procedure TFmShellMain.BtnCloseShellClick(Sender: TObject);
begin
  MIExitClick(Sender);
end;

{ This procedure enables the user to choose a Driver file using several
  different methods. The user can click Choose Driver file from the menu,
  click on the label Driver File, or type a filename directly in the box.
  If the user chooses using either the menu or the label, an open file dialog
  is displayed. }
procedure TFmShellMain.ChooseDriver(Sender: TObject);
var
 olddriverfilename : string;
 i:integer;
begin
 DlgOpenDriver.InitialDir:=CurrentPath;

 olddriverfilename := driverfilename;
// If the user typed directly in the edit box
 if Sender is TEdit then driverfilename:=DriverBox.text  // Set the driverfile
 else  // User used the menu or clicked on the Driver label
  begin   // Show the open file dialog
    // First set the default filename to the current driverfile
   DlgOpenDriver.filename := driverfilename;
    // Show the dialog and if the user clicks OK, set the new driver filename
   if DlgOpenDriver.execute then driverfilename := DlgOpenDriver.filename;
  end;

 if driverfilename <> olddriverfilename then
  begin
   if driverfilename <> '' then    // Create a new empty file { TODO : When a new file is created the edit driver window should automatically pop up }
     if not LazFileUtils.FileExistsUTF8(driverfilename)  then
       fileio.WriteDriverFile(driverfilename, fileio.driverlist);
   ReadytoRun := False; // This forces the edit boxes on the FmShellMain to be update with the new filename
   RunComplete := False;
  end;

 CurrentPath := ExtractFilePath(paramfilename);
 LazFileUtils.SetCurrentDirUTF8(CurrentPath);
 UpdateFmShellMain;
end;

{ This procedure enables the user to choose an Output file using several
  different methods. The user can click Choose Output file from the menu,
  click on the label Output File, or type a filename directly in the box.
  If the user chooses using either the menu or the label, an open file dialog
  is displayed. }
procedure TFmShellMain.ChooseOutputFile(Sender: TObject);
var
 oldoutfilename : string;
begin     // If the user typed directly in the edit box
 DlgSaveOutput.InitialDir:=CurrentPath;

 oldoutfilename := outfilename;

 if Sender is TEdit then  Outfilename := OutputBox.text  // Set the output file
 else   // User used the menu or clicked on the Output label
  begin  // Show the Save file dialog
    // First set the default filename to the current output file.
   DlgSaveOutput.filename := outfilename;
   // Show the dialog and if the user chooses OK, set the outfilename to the new file.
   if DlgSaveOutput.execute then outfilename := DlgSaveOutput.filename;
  end;

{  if outfilename <> oldoutfilename then
  begin
  if outfilename <> '' then  // Make sure a value was entered for the filename
    begin
     if LazFileUtils.FileExistsUTF8(outfilename) then  // If the file already exists
      // Confirm that the user wants to overwrite the existing file.
       if MessageDlg('Overwrite existing output file, ' + outfilename + '?',
                  mtConfirmation, [mbYes,mbNo], 0) = mrNo then
        begin
          outfilename := '';  // Set the outfile to no value
        end;
    end;              
  end;                 }
 FmDisplayOutput.DisplayFilename:=outfilename;
 CurrentPath := ExtractFilePath(paramfilename);
 LazFileUtils.SetCurrentDirUTF8(CurrentPath);
 RunComplete := False;
 ReadytoRun := False;  // Can't run without an output file.
 UpdateFmShellMain; // Update the form.
end;

{ This procedure saves the current values of the state variables and the
  parameters to the current parameter file. }
procedure TFmShellMain.MISaveParamClick(Sender: TObject);
begin
 WriteParamFile(paramfilename, ModelDef.numparam, par, ModelDef.numstate, stat,
                currentresid);
 RunComplete := False;
// NewParamFile := False;
 UpdateFmShellMain;
 Beep;
end;

{ Procedure to save the state variables and parameters to a new parameter file. }
procedure TFmShellMain.MISaveAsParamClick(Sender: TObject);
begin  // Show the Save dialog
DlgSaveParam.InitialDir:=CurrentPath;

with DlgSaveParam do
 begin
  // Set the default filename in save dialog to the current parameter file.
  filename := paramfilename;
  if execute then   // If the user chooses OK in the save dialog.
   begin
    paramfilename:=filename; // Set the paramfilename to the new filename.
    // Save the state variables and parameters to the current file.
    WriteParamFile(paramfilename, ModelDef.numparam, par, ModelDef.numstate, 
                stat, currentresid);
    // Set Ready to Run to false so the the edit boxes on the form will be
    // updated when UpdateFmShellMain is called.
//    NewParamFile := False;
    ReadytoRun := False;
    RunComplete := False;
    UpdateFmShellMain;
   end;
 end;
end;

procedure TFmShellMain.MISaveMemOutputClick(Sender: TObject);
begin
  FmDisplayOutput.WriteOutputfromMem;
end;

{ Procedure to allow the user to edit the parameter values. This procedure calls
  the Parameters Form defined in the parameters.pas unit. While the user is editing
  the parameters the FmShellMain is disabled. }
procedure TFmShellMain.MIEditParamClick(Sender: TObject);
begin
{ Show the parameter form. If the user clicked OK on the parameter form,
  meaning they changed the parameter values, update this form. }
 CalTime := Time_start;
 FmParameter.ShowParameters;
 if FmParameter.showmodal = mrOK then
  begin
// Parameters have been changed so any previous model runs are invalid.
   RunComplete := False;
   UpdateFmShellMain;   // Update the form.
   currentresid := 999;
  end;
end;

{ Procedure to allow the user to edit the state variables. This procedure calls
  the Data Form defined in the data.pas unit. While the user is editing
  the state variables the FmShellMain is disabled. }
procedure TFmShellMain.MIEditStatesClick(Sender: TObject);
begin
 DataForm.ShowStates;  // Show the data form containing state variables
 UpdateFmShellMain;
end;

{ Procedure to allow the user to edit the driver variables. This procedure calls
  the Note Form defined in the note.pas unit. While the user is editing
  the drivers the FmShellMain is disabled. }
procedure TFmShellMain.MIEditDrvClick(Sender: TObject);
begin
{ Show the note form. If the user clicked OK on the note form,
  meaning they changed the driver values, update this form. }
   FmNote.Show;
// Drivers have been changed so any previous model runs are invalid.
   RunComplete := False;
// User may have changed driver files, so force the edit boxes and menu to be updated.
   ReadytoRun := False;
   UpdateFmShellMain;
end;

{ Procedure to process changes made to the start time mask edit box. The mask
  edit box only allows numeric input with up to 5 digits. Values entered must be
  integers. To change the allowed values modify the mask using the object
  inspector. }
procedure TFmShellMain.MeStartTimeExit(Sender: TObject);
begin
 if MEStartTime.Text <> '' then
  if (MEStartTime.Text <> floattostr(time_Start)) then
// if (MEStartTime.Text <> '') and (MEStartTime.Modified) then
  begin
   Time_Start := strtofloat(MEStartTime.text); // Set the start time of the model.
   ReadytoRun := False;  // Force checking of start and stop times in UpdateFmShellMain.
   RunComplete := False;  // Previous runs are invalid.
   UpdateFmShellMain;   
  end;
end;

{ Procedure to process changes made to the start time mask edit box. The mask
  edit box only allows numeric input with up to 5 digits. Values entered must be
  integers. To change the allowed values modify the mask using the object
  inspector. }
procedure TFmShellMain.MeStopTimeExit(Sender: TObject);
begin
 if MeStopTime.Text <> '' then
  if (MeStopTime.Text <> floattostr(time_Stop)) then
// if (MEStopTime.text <> '') and (MeStopTime.Modified) then
  begin
   Time_Stop := strtofloat(MEStopTime.text); // Set the stop time of the model.
   ReadytoRun := False; // Force checking of start and stop times in UpdateFmShellMain.
   RunComplete := False;  // Previous runs are invalid.
   UpdateFmShellMain;
  end;
end;

{ This procedure shows the output from the model run in table or chart format.
  It uses the form defined in display.pas. }
procedure TFmShellMain.DisplayOutput(Sender: TObject);
begin
 if (Sender as TMenuItem).Name = 'MIShowTable' then
  FmDisplayOutput.DisplayStyle := dsTable // Set the type of display.
 else
  FmDisplayOutput.DisplayStyle := dsChart;
 FmDisplayOutput.Enabled := True;
 if not FmDisplayOutput.Visible then
   FmDisplayOutput.ShowModal
 else
   FmDisplayOutput.WindowState:=wsNormal;
 // or is this what I want
//   FmDisplayOutput.BringToFront;
end;

{ Show an about box containing general information about the model. }
procedure TFmShellMain.MIAboutClick(Sender: TObject);
begin
 FmAbout.Caption := 'About ' + ModelDef.modelname;
 FmAbout.LblModel.Caption := ModelDef.modelname;
 FmAbout.LblVersion.Caption := ModelDef.versionnumber;
 FmAbout.MoContact.Lines.Add(ModelDef.contactperson);
 FmAbout.MoContact.Lines.Add(ModelDef.contactaddress1);
 FmAbout.MoContact.Lines.Add(ModelDef.contactaddress2);
 FmAbout.MoContact.Lines.Add(ModelDef.contactaddress3);
 FmAbout.ShowModal;
end;

{ This procedure presents a dialog which allows the user to reread the state
  variables and/or parameters from the parameter form. It uses the Reload
  dialog defined in the ReloadDlg.pas unit.}
procedure TFmShellMain.BtnReloadClick(Sender: TObject);
begin  // Show the Reload Dialog
if paramfilename <> '' then
 begin
  if DlgReload.ShowModal = mrOK then  // If the user clicked OK, values were reloaded
   begin
    RunComplete := False; // so previous runs are invalid.
    UpdateFmShellMain;
   end;
 end
else
  MessageDlg('Invalid parameter file. No values reloaded.',mtWarning,[mbOK],0);
end;

// This procedure is used to update the controls on the Main Form, i.e. enabling
// and disabling them when appropriate.
procedure TFmShellMain.UpdateFmShellMain;
begin
 if not ReadytoRun then
  begin
   ParamBox.text := paramfilename; // save it's name in the paramfilename variable
   if paramfilename <> '' then // if a parameter file has been chosen
    begin
     MISaveParam.Enabled := true;  // enable the save parameter file menu item
    end;
     // if a driver variable has been chosen, save it's name to driverfilename
   if driverfilename <> '' then DriverBox.text := driverfilename;
    // if an output file name has been chosen, save it's name to outfilename
   if outfilename <> '' then OutputBox.text := outfilename;
// If all files have been chosen then enable the run button and calculate menu.
   if (DriverBox.text <> '') and (ParamBox.text <> '') then
     begin
//       MICalculate.enabled := true; // Enable the calculate menu
       if (OutputBox.text <> '') and (Time_stop > Time_start) then
         begin
          ReadytoRun := true;
          BtnRun.enabled := true;
          FmOptions.RunOptions.stepcounter := FmOptions.DefaultRunOptions.stepcounter;
         end;
     end;
  end;
 if RunComplete then  // If a run is complete
  begin
   MISaveMemOutput.Enabled := true;
   last_time := 0;
   next_time := 0;
  end
 else
  begin // Run is not complete or previous run is now invalid
   MISaveMemOutput.Enabled := false;
   PresentBox.Text := floattostr(0); // Set the present time to 0
  end;
 if FmShellMain.ActiveControl is TEdit then
   if FmShellMain.ActiveControl.Name = 'ParamBox' then
    FmShellMain.ActiveControl := BtnCloseShell;
 if FmOptions.RunOptions.NormalRun then
   MINormalRun.Checked := True
 else
   MISpecialRun.Checked := True;
end;

// This procedure controls the actual running of a model scenario.
procedure TFmShellMain.BtnRunClick(Sender: TObject);
var
  j, outputfreq, stepsperfullstep, nok, nbad, idx, endstate:integer;
  tempstat:yValueArray;
  drivetime, nextstep, remainder, errorest, SStest, SSstep, temp:double;
  steadystate, ResetDriverFile: Boolean;
  previousstat, tempstate: statearray;
  basename,anewname,theext:string;
begin
 BeginningofRun := True;
 ResetDriverFile := False;
 LargeOutput := False;
 openDriverFile(driverfilename,fileio.driverlist);
 if RunningInteractive then
  begin
   ShowProgressBar;
   FmShellMain.Enabled := False;
  end;
 try try
  stepsperfullstep := round(fullstep/FmOptions.RunOptions.Time_step);
  initialstates := stat;
  last_time := 0;
  next_time := 0;
  nextstep := FmOptions.RunOptions.Time_step;
  time := time_start;
  drivetime := time_start;
  steadystate := false;
// Variables used to keep track of when to write to output file
  outputfreq := round(FmOptions.RunOptions.Outputstep/FmOptions.RunOptions.Time_step);
// Calculate and write out the initial state of system.
  GetCurrentDrivers(drivetime,drive);
  processes(time,drivetime,drive,par,stat,proc,false);
  FmDisplayOutput.FirstWrite := True;
  FmDisplayOutput.ClearGrid;
  if (not FmOptions.RunOptions.AppendOutputFile) and
    (not FmOptions.RunOptions.OutputEORonly) and
    (FmOptions.RunOptions.Outputoffset = FmOptions.DefaultRunOptions.Outputoffset) then
       FmDisplayOutput.StoreResults(time_start-fullstep, drive, stat, proc);
  randomize;      // MEL specific
  previousstat := stat;
  while (time < time_stop + fullstep) and (not steadystate) do
    begin
//      if time-10 < epsilon then nextstep := FmOptions.RunOptions.Time_step;
      for j:=1 to ModelDef.numstate do tempstat[j] := stat[j].value;
      RK4_5_Integrator(tempstat, modeldef.numstate, time, time+FmOptions.RunOptions.Time_step, drivetime,
           nextstep,drive, par);
      for j:=1 to ModelDef.numstate do stat[j].value := tempstat[j];
      temp := abs((time+FmOptions.RunOptions.Time_step)/FmOptions.RunOptions.DiscreteStep -       // time
                round((time+FmOptions.RunOptions.Time_step)/FmOptions.RunOptions.DiscreteStep));   // time
      BeginningofRun := False;
      if temp > epsilon then
         processes(time, drivetime, drive, par, stat, proc, false)
      else
         processes(time, drivetime, drive, par, stat, proc, true);
// Write output
      FmOptions.RunOptions.outcounter := FmOptions.RunOptions.outcounter + 1;
      if FmOptions.RunOptions.OutputEORonly then
       begin
         if time >= time_stop then FmDisplayOutput.StoreResults(time, drive, stat, proc);
       end
      else if FmOptions.RunOptions.OutputAnnually then
       begin
         if DayofYear = FmOptions.RunOptions.OutputAnnuallyDay then
            FmDisplayOutput.StoreResults(time, drive, stat, proc);
       end
      else
       begin        // All output options except OutputEOR and OutputAnnually
         if (time >= FmOptions.RunOptions.Outputoffset) then
           begin
             if (time = FmOptions.RunOptions.Outputoffset) or
                (FmOptions.RunOptions.outcounter = outputfreq) then
               begin
                 if (stepsperfullstep <> fullstep) and
                  (FmOptions.RunOptions.time_step <> FmOptions.RunOptions.Outputstep) then
                     FmDisplayOutput.StoreResults(time, drive, stat, proc)
                 else
                     FmDisplayOutput.StoreResults(time, drive, stat, proc);
                 FmOptions.RunOptions.outcounter := FmOptions.DefaultRunOptions.outcounter;
               end;
           end
         else      // Haven't reached the outputoffset yet
           begin
             FmOptions.RunOptions.outcounter := FmOptions.DefaultRunOptions.outcounter;
           end;
       end;
// Save output to file mid run
      if FmOptions.RunOptions.WriteEvery <> 0 then
        if frac(time/FmOptions.RunOptions.WriteEvery) < epsilon then
         FmDisplayOutput.WritePurgeOutputfromMem;
// Reset State variables option
      if FmOptions.RunOptions.ResetStates then
        begin
          remainder := time - int(time/FmOptions.RunOptions.ResetStateTime)*FmOptions.RunOptions.ResetStateTime;
          if remainder = 0 then FmShellMain.ResetStatesNow(BtnRun);
        end;
// Run to steady state option
      if FmOptions.RunOptions.RuntoSS then
        begin
          SSstep := (FmOptions.RunOptions.Time_step - FmOptions.RunOptions.SSTime*
                    (round(FmOptions.RunOptions.Time_step/FmOptions.RunOptions.SSTime)));
          SStest := (time - FmOptions.RunOptions.SSTime*
                    (round(time/FmOptions.RunOptions.SSTime)));
          if (SStest >= 0) and (SStest < SSstep) then
           begin
             steadystate := true;
             for j := 1 to ModelDef.numstate do
              begin
                   errorest := abs(previousstat[j].value -
                                                  stat[j].value)/stat[j].value;
                if errorest <= FmOptions.RunOptions.SSCriteria then
                  steadystate := steadystate and true
                else
                  steadystate := steadystate and false;
              end;
              previousstat := stat;
             // If outputEORonly then modify time_stop and steadystate so that
             // loop executes one more time and then outputs results.
             if steadystate and FmOptions.RunOptions.OutputEORonly then
              begin
                steadystate := false;
                time_stop := time + FmOptions.RunOptions.Time_step;
              end;
           end;
        end;
// Next time step
      time := time_start + FmOptions.RunOptions.stepcounter*FmOptions.RunOptions.Time_step;
      if (not FmOptions.RunOptions.RepeatDrivers) or (time <= FmOptions.RunOptions.RepeatDriveTime) then drivetime := time;
      FmOptions.RunOptions.stepcounter := FmOptions.RunOptions.stepcounter + 1;
// Repeat drivers option
      if ResetDriverFile then
         begin
           drivetime := time_start;
           last_time := 0;     next_time := 0;
           ResetDriverFile := False;
         end;
      if (FmOptions.RunOptions.RepeatDrivers) and (time >= FmOptions.RunOptions.RepeatDriveTime) then
         drivetime := time - int(time/FmOptions.RunOptions.RepeatDriveTime)*FmOptions.RunOptions.RepeatDriveTime;  // mod function for doubles
      if drivetime = 0 then
         begin
           drivetime := FmOptions.RunOptions.RepeatDriveTime;
           ResetDriverFile := True;
         end;
// Update progress bar
      if RunningInteractive then
           FmProgress.PbRunStatus.Position := (round(time - time_start)*100)
                           div round(time_stop - time_start);
// check for pending messages (for example, the cancel button)
      application.ProcessMessages;
      if stopRun then break;
    end;
 finally
  if FmOptions.RunOptions.OutputFile then
   if FmOptions.RunOptions.WriteEvery = 0 then FmDisplayOutput.WriteOutputfromMem;
  stopRun := false;
  RunComplete := True;
  FmOptions.RunOptions.stepcounter := FmOptions.DefaultRunOptions.stepcounter;
  FmOptions.RunOptions.outcounter := FmOptions.DefaultRunOptions.outcounter;
  ResetStatesNow(BtnRun);
  if RunningInteractive then
   begin
    UpdateFmShellMain;
    FmShellMain.Enabled := True;
    FmDisplayOutput.Enabled := True;
    PresentBox.Text := floattostrf(Time,ffgeneral,5,1);
    FmProgress.Hide;
    if (FmDisplayOutput.autoShowChart) and (not FmDisplayOutput.Visible) then
      DisplayOutput(MIShowChart);
    if LargeOutput then
     MessageDlg('Output was very large. To prevent a system crash, output was '
      + 'written to the output file and removed from memory during the run. ' +
      'Only the final portion of the run is in memory. You will need to ' +
      'use a different program to view the results.' ,mtWarning,[mbOK],0);
   end
  else
   begin
     // Save parameter file if running a calibration in batch mode
     if par[FmCalculate.GetArrayIndex(vtparameter,'calibrate')].value<>-999 then
      begin
     basename:=ExtractFileName(paramfilename);
     theext:=ExtractFileExt(paramfilename);
     delete(basename,pos(theext,basename),length(basename));
     anewname:=basename+'.cal'+theext;
     // Save the end of the calibration
     WriteParamFile(anewname, ModelDef.numparam, par, ModelDef.numstate,
                        stat, currentresid);
     // Create a new parameter file for steady state run.
     tempstate:=stat;
     endstate:=FmCalculate.GetArrayIndex(vtstate,'RCa');
     for idx := 1 to endstate do
       tempstate[idx].Reset:=false;
     anewname:=basename+'.final'+theext;
     par[FmCalculate.GetArrayIndex(vtparameter,'calibrate')].value:=-999;
     WriteParamFile(anewname, ModelDef.numparam, par, ModelDef.numstate,
                        tempstate, currentresid);
     // Create climate parameter files
     //CO2
     anewname:=basename+'.CO2'+theext;
     par[FmCalculate.GetArrayIndex(vtparameter,'FlagCa')].value:=1;
     WriteParamFile(anewname, ModelDef.numparam, par, ModelDef.numstate,
                        tempstate, currentresid);
     //T
     anewname:=basename+'.T'+theext;
     par[FmCalculate.GetArrayIndex(vtparameter,'FlagCa')].value:=0;
     par[FmCalculate.GetArrayIndex(vtparameter,'FlagT')].value:=1;
     WriteParamFile(anewname, ModelDef.numparam, par, ModelDef.numstate,
                        tempstate, currentresid);
     //Ppt
     anewname:=basename+'.Ppt'+theext;
     par[FmCalculate.GetArrayIndex(vtparameter,'FlagT')].value:=0;
     par[FmCalculate.GetArrayIndex(vtparameter,'FlagPpt')].value:=1;
     WriteParamFile(anewname, ModelDef.numparam, par, ModelDef.numstate,
                        tempstate, currentresid);
     //Call
     anewname:=basename+'.Call'+theext;
     par[FmCalculate.GetArrayIndex(vtparameter,'FlagCa')].value:=1;
     par[FmCalculate.GetArrayIndex(vtparameter,'FlagT')].value:=1;
     par[FmCalculate.GetArrayIndex(vtparameter,'FlagPpt')].value:=1;
     WriteParamFile(anewname, ModelDef.numparam, par, ModelDef.numstate,
                        tempstate, currentresid);

      end;
   end;
 end;
 except
  on EFileError do raise;
  on E: Exception do
   begin
   if RunningInteractive then
    begin
     FmTrouble.MmError.Lines.Clear;
     FmTrouble.MmError.Lines.Add(E.Message);
     FmTrouble.ShowModal;
    end
    else
     raise;
   end;
 end;
end;

procedure TFmShellMain.FormDestroy(Sender: TObject);
begin
  if Assigned(fileio.driverlist) then
    FreeAndNil(fileio.driverlist);
end;

procedure TFmShellMain.MIExitClick(Sender: TObject);
var
  answer:word;
begin
  if NeedToSavePar = True then
        begin
          answer := MessageDlg('Save parameter file before exiting?',
                 mtConfirmation, mbYesNoCancel, 0);
          if answer <> mrCancel then
           if answer = mrYes then
             begin
              DlgSaveParam.Filename := paramfilename;
              if DlgSaveParam.execute then
                WriteParamFile(paramfilename, ModelDef.numparam, par,
                                  ModelDef.numstate, stat, currentresid);
             end
           else
             close;
        end
      else close;
end;

procedure TFmShellMain.MICalTimeClick(Sender: TObject);
begin
// Ask the user for the time to calculate a steady state at.
  CalTime := strtofloat(InputBox('Steady State Time',
    'Enter the time from the driver file at which to calculate a steady state.',
    floattostr(CalTime)));
  RunComplete := False;
end;

procedure TFmShellMain.MICalcSSClick(Sender: TObject);
begin
 FmCalculate.showmodal;
end;

procedure TFmShellMain.SetNumSpecies(anEditBox: TEdit);     // MEL specific
begin
{ if anEditBox.text <> '' then // If a value has been entered.
  begin
    numspp:= strtoint(anEditBox.text); // Set the number of species
    if numspp<1 then numspp:=1;
    if numspp>Maxspecies then numspp:=Maxspecies;
    anEditBox.text := inttostr(numspp);
    counts;
    LblWelcome.Visible:=false;
    LblWelcome.Enabled:=false;
    LblDirections.Visible:=false;
    LblDirections.Enabled:=false;
    EdNumSpecies.Enabled:=false;

    MIFiles.Enabled:=true;
    MIEdit.Enabled:=true;
    MIView.Enabled:=true;
    ParamLabel.Visible:=true;
    Parambox.Visible:=true;
    DriverLabel.Visible:=true;
    DriverBox.Visible:=true;
    OutputLabel.Visible:=true;
    OutputBox.Visible:=true;
    StartLabel.Visible:=true;
    MeStartTime.Visible:=true;
    StopLabel.Visible:=true;
    MeStopTime.Visible:=true;
    StepLabel.Visible:=true;
    MeTimeStep.Visible:=true;
    BtnRun.Visible:=true;
    PresentLabel.Visible:=true;
    PresentBox.Visible:=true;
    BtnReload.Visible:=true;
    if RunningInteractive then
     begin
      Fmparameter.FormDestroy(EdNumSpecies);
      Fmparameter.FormCreate(EdNumSpecies);
      Fmdisplaydata.FormCreate(EdNumSpecies);
      DataForm.FormCreate(EdNumSpecies);
     end;
  end;          }
end;

procedure TFmShellMain.EdNumSpeciesChange(Sender: TObject);
begin
{ FmShellMain.SetNumSpecies(FmShellMain.EdNumSpecies);    }
end;

procedure TFmShellMain.EdNumSpeciesExit(Sender: TObject);
begin
{ if EdNumSpecies.text = '' then                FmShellMain.ActiveControl := EdNumSpecies;    }
end;                 // MEL specific

procedure TFmShellMain.MIViewOutFileClick(Sender: TObject);
begin
 FmDisplayOutput.MILoadFileClick(Sender)
end;

procedure TFmShellMain.MEStopTimeKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = Chr(13)) then MeStopTimeExit(Sender);
end;

procedure TFmShellMain.MEStartTimeKeyPress(Sender: TObject; var Key: Char);
begin
 if (Key = Chr(13)) then MeStartTimeExit(Sender);
end;

procedure TFmShellMain.MIOpenOptionsClick(Sender: TObject);
begin
 if (Sender as TComponent).name = 'MITimeSteps' then
   lastitem := MITimeSteps
 else if (Sender as TComponent).name = 'MISpecialRun' then
   lastitem := MISpecialRun
 else
   lastitem := MIOutOptions;
 UpdateFmShellMain;
 FmOptions.ShowModal;
end;

procedure TFmShellMain.MINormalRunClick(Sender: TObject);
begin
 FmOptions.RunOptions.NormalRun := True;
 FmOptions.UpdateFmRunOptions(MINormalRun);
 MINormalRun.Checked := True;
 RunComplete := False;
 Readytorun := False;
end;

procedure TFmShellMain.ResetStatesNow(Sender: TObject);
var
 i: integer;
begin
 for i := 1 to ModelDef.numstate do
   begin
     if stat[i].reset then stat[i].value := initialstates[i].value;
   end;
end;

procedure TFmShellMain.MIAutoChartClick(Sender: TObject);
begin
 if MIAutoChart.Checked then
   MIAutoChart.Checked := False
 else
   MIAutoChart.Checked := True;
 FmDisplayOutput.AutoShowChart:=MIAutoChart.Checked;
end;

procedure TFmShellMain.ParamBoxKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseParamFile(Sender);
end;

procedure TFmShellMain.DriverBoxKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseDriver(Sender);
end;

procedure TFmShellMain.OutputBoxKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseOutputFile(Sender);
end;

procedure TFmShellMain.EdNumSpeciesKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then EdNumSpeciesExit(Sender);
end;

procedure TFmShellMain.ShowProgressBar;
begin
  FmProgress.Caption := modeldef.modelname + ' run progress';
  FmProgress.LblBegin.Caption := floattostr(time_start);
  FmProgress.LblEnd.Caption := floattostr(time_stop);
  FmProgress.CancelProcedure := CancelRun;
//  if FmDisplayOutput.Visible then FmProgress.Parent:= FmDisplayOutput;
  FmProgress.Show;
end;

procedure TFmShellMain.CancelRun;
begin
  stopRun := True;
  FmDisplayOutput.autoShowChart := false;
end;

function TFmShellMain.GetResourcePath(): string;
{$ifdef Darwin}
var
 pathRef:CFURLRef;
 pathCFStr: CFStringRef;
 pathStr: shortstring;
{$endif}
begin
{$ifdef Unix}
{$ifdef Darwin}
  pathRef := CFBundleCopyBundleURL(CFBundleGetMainBundle());
  pathCFStr := CFURLCopyFileSystemPath(pathRef, kCFURLPOSIXPathStyle);
  CFStringGetPascalString(pathCFStr, @pathStr, 255, CFStringGetSystemEncoding());
  CFRelease(pathRef);
  CFRelease(pathCFStr);

  Result := pathStr + BundleResourceDirectory;
{$else}
  Result := DefaultUnixDirectory;
{$endif}
{$else} // Windows
  Result := ExtractFilePath(Application.exeName);
{$endif}
end;

end.
