unit batchmain;

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, fileutil, Lazfileutils, strutils,stypes;

const
  numlayers = 4;
  DefaultUnixDirectory = '/user/share/CrEquations';
  BundleResourceDirectory = '/Contents/Resources/';

type

  { TFmBatchMain }

  TFmBatchMain = class(TForm)
    LblBatchFile: TLabel;
    EdBatchFile: TEdit;
    MmDescription: TMemo;
    BtnRunBatch: TButton;
    BtnCancelBatch: TButton;
    DlgOpenBatch: TOpenDialog;
    EdNumSpecies: TEdit;
    LblNumSpecies: TLabel;
    procedure LblBatchFileClick(Sender: TObject);
    procedure ChooseBatchFile(Sender: TObject);
    procedure BtnRunBatchClick(Sender: TObject);
    procedure BtnCancelBatchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EdNumSpeciesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CreateNextDriver(ofilename, dfilename: string);
    procedure CreateNextDriver4L(ofilename, dfilename: string);
    function GetResourcePath(): string;
    procedure RunSpinup(pfile, dfile, ofile:string);
  private
    { Private declarations }
    fregrowopt:TRunOptions;
  public
    { Public declarations }
    BatchFilename: string;
    Defpath: string;

  end;

var
  FmBatchMain: TFmBatchMain;

implementation

{$R *.lfm}

uses frontend, fileio, Options, calculate, Display;

procedure TFmBatchMain.LblBatchFileClick(Sender: TObject);
begin
 ChooseBatchFile(Sender);
 if not (batchfilename = '') then
  FmBatchMain.ActiveControl := BtnRunBatch;
end;

procedure TFmBatchMain.ChooseBatchFile(Sender: TObject);
var
 ThisEdit: TEdit;
begin
 DlgOpenBatch.InitialDir:=DefPath;
// If the user typed directly in the edit box
 if Sender is TEdit then
  begin
   ThisEdit := Sender as TEdit;
   BatchFilename:=ThisEdit.text  // Set the batch file
  end
 else  // User clicked on the label
  begin   // Show the open file dialog
   ThisEdit := ((Sender as TLabel).Focuscontrol as TEdit);
    // First set the default filename to the current file
   DlgOpenBatch.filename := BatchFilename;
    // Show the dialog and if the user clicks OK, set the new filename
   if DlgOpenBatch.execute then
    begin
      BatchFilename := DlgOpenBatch.filename;
      ThisEdit.text := Batchfilename;
    end;
  end;

 if (BatchFilename <> '') then
   if ((sender as Tcomponent).name <> 'LblBatchFilec') or
        ((sender as Tcomponent).name <> 'EdBatchFilec') then
        BtnRunBatch.Enabled := True;
 DefPath := ExtractFilePath(batchfilename);
 LazFileUtils.SetCurrentDirUTF8(ExtractFilePath(batchfilename));
end;

procedure TFmBatchMain.BtnRunBatchClick(Sender: TObject);
var
 LogFilename, parname, lastsec:string;
 temp: double;
 RunCrashed:Boolean;
 dotpos,Caidx, Tidx, T35Idx, Pptidx: integer;
begin
 FmShellMain.RunningInteractive := False;
 BtnRunBatch.Enabled := False;
 BtnCancelBatch.Enabled := False;
 RunCrashed := False;
 LogFilename := ChangeFileExt(BatchFilename,'.log');
// ChangeExtension(LogFilename,'log');
 OpenLogFile(LogFilename, BatchFilename);
 OpenBatchFile(BatchFilename);
 try
  ReadBatchFile(paramfilename,driverfilename,outfilename,time_start,time_stop,
                   stat,FmOptions.RunOptions);
  if (Application.Title = 'sensitivity') and (paramfilename <> 'senstest.par')
    then raise Exception.Create('Invalid parameter file for sensitivity test.');
  repeat
   try
    ReadParamFile(paramfilename, ModelDef.numparam,par, ModelDef.numstate,stat,temp);

    // MEL Multisite Code, create subsequent parameter files and spin up efforts for regrow simulations
    parname:=ExtractFileNameOnly(paramfilename);
    dotpos:=rpos('.',parname);
    lastsec:=copy(parname,dotpos+1,5);
    if lastsec='SS' then
     begin
       // Create climate driver files
       //CO2
       Caidx:=FmCalculate.GetArrayIndex(vtParameter, 'FlagCa');
       Tidx:=FmCalculate.GetArrayIndex(vtParameter, 'FlagT');
       T35idx:=FmCalculate.GetArrayIndex(vtParameter,'FlagT35');
       Pptidx:=FmCalculate.GetArrayIndex(vtParameter, 'FlagPpt');

       par[Tidx].value:=0;
       par[T35idx].value:=0;
       par[Pptidx].value:=0;
       par[Caidx].value:=1;
       WriteParamFile(parname+'.Ca.par',ModelDef.numparam,par,ModelDef.numstate,stat,FmShellMain.Currentresid);

       //T
       par[Tidx].value:=1;
       par[T35idx].value:=1;
       par[Pptidx].value:=0;
       par[Caidx].value:=0;
       WriteParamFile(parname+'.T.par',ModelDef.numparam,par,ModelDef.numstate,stat,FmShellMain.Currentresid);

       //Ppti
       par[Pptidx].value:=1;
       par[Tidx].value:=0;
       par[T35idx].value:=0;
       par[Caidx].value:=0;
       WriteParamFile(parname+'.Ppti.par',ModelDef.numparam,par,ModelDef.numstate,stat,FmShellMain.Currentresid);

       //Pptd
       par[Pptidx].value:=-1;
       par[Tidx].value:=0;
       par[T35idx].value:=0;
       par[Caidx].value:=0;
       WriteParamFile(parname+'.Pptd.par',ModelDef.numparam,par,ModelDef.numstate,stat,FmShellMain.Currentresid);

       //PptiT
       par[Caidx].value:=0;
       par[Tidx].value:=1;
       par[T35idx].value:=1;
       par[Pptidx].value:=1;
       WriteParamFile(parname+'.PptiT.par',ModelDef.numparam,par,ModelDef.numstate,stat,FmShellMain.Currentresid);

       //PptdT
       par[Caidx].value:=0;
       par[Tidx].value:=1;
       par[T35idx].value:=1;
       par[Pptidx].value:=-1;
       WriteParamFile(parname+'.PptdT.par',ModelDef.numparam,par,ModelDef.numstate,stat,FmShellMain.Currentresid);

       //CaT
       par[Caidx].value:=1;
       par[Tidx].value:=1;
       par[T35idx].value:=1;
       par[Pptidx].value:=0;
       WriteParamFile(parname+'.CaT.par',ModelDef.numparam,par,ModelDef.numstate,stat,FmShellMain.Currentresid);

       //CaPpti
       par[Caidx].value:=1;
       par[Tidx].value:=0;
       par[T35idx].value:=0;
       par[Pptidx].value:=1;
       WriteParamFile(parname+'.CaPpti.par',ModelDef.numparam,par,ModelDef.numstate,stat,FmShellMain.Currentresid);

       //CaPptd
       par[Caidx].value:=1;
       par[Tidx].value:=0;
       par[T35idx].value:=0;
       par[Pptidx].value:=-1;
       WriteParamFile(parname+'.CaPptd.par',ModelDef.numparam,par,ModelDef.numstate,stat,FmShellMain.Currentresid);

       //Calli
       par[Caidx].value:=1;
       par[Tidx].value:=1;
       par[T35idx].value:=1;
       par[Pptidx].value:=1;
       WriteParamFile(parname+'.Calli.par',ModelDef.numparam,par,ModelDef.numstate,stat,FmShellMain.Currentresid);

       //Calld
       par[Caidx].value:=1;
       par[Tidx].value:=1;
       par[T35idx].value:=1;
       par[Pptidx].value:=-1;
       WriteParamFile(parname+'.Calld.par',ModelDef.numparam,par,ModelDef.numstate,stat,FmShellMain.Currentresid);
end;

    // Spin up efforts for regrow simulations and save par file for actual regrow run
    if AnsiContainsStr(lowercase(parname),'spinup regrow') and stat[1].Reset then
     begin
       RunSpinup(paramfilename,driverfilename,outfilename);
     end
    else
     begin
       FmShellMain.BtnRunClick(Sender);
     end;

    WriteLogFile(LogFilename,outfilename,'Run Complete - '+ DateTimeToStr(Now));
  // single layer version
 ///   CreateNextDriver(outfilename, driverfilename);
  // 4 layer version
 //   CreateNextDriver4L(outfilename, driverfilename);
   except
    on E: Exception do
     begin
      WriteLogFile(LogFilename, outfilename, E.Message + ' - '
        + DateTimeToStr(Now));
      if not (Application.Title = 'modelbatch') then RunCrashed := True;
     end;
   end;
  until (ReadBatchFile(paramfilename,driverfilename,outfilename,
              time_start,time_stop,stat,FmOptions.RunOptions) = False) or
        (RunCrashed);
  WriteLogFile(LogFilename,batchfilename,'Batch Job Complete.');
 finally
  CloseBatchFile;
  CloseLogFile;
  BtnRunBatch.Enabled := True;
  BtnCancelBatch.Enabled := True;
  FmBatchMain.ActiveControl := BtnRunBatch;
  if RunCrashed then Abort{raise Exception.Create('Error in batch program')};
 end;
end;

procedure TFmBatchMain.BtnCancelBatchClick(Sender: TObject);
begin
 FmBatchMain.Close;
end;

procedure TFmBatchMain.FormCreate(Sender: TObject);
begin
 SetCurrentDirUTF8(GetResourcePath());
 DefPath:=GetResourcePath();
 FmBatchMain.ActiveControl := EdBatchFile;
 with fregrowOpt do
  begin
   NormalRun:=false;
   Time_step:=1;
   DiscreteStep:=1;
   RepeatDrivers:=true;
   RepeatDriveTime:=365;
   ResetStates:=true;
   ResetStateTime:=365;
   RuntoSS:=false;
   SSCriteria:=0;
   SSTime:=0;
   HoldStatesConstant:=false;  // Used in fuzzy calibrator
   Outputstep:=365;      // The timestep specified by the user for output
   Outputoffset:=0;    // No output for time less than outputoffset
   OutputEORonly:=false;  // output only if time = stop_time + Time_step
   OutputAnnually:=false;
   OutputAnnuallyDay:=0;
   AppendOutputFile:=false;
   stepcounter := 1;
   outcounter := 0;
   WriteEvery := 0;
   ErrorMult := 1;
   OutputFile:=false;
  end;
end;

procedure TFmBatchMain.EdNumSpeciesChange(Sender: TObject);
begin
 FmShellMain.SetNumSpecies(FmBatchMain.EdNumSpecies);
end;

procedure TFmBatchMain.FormShow(Sender: TObject);
begin
 if not (pos('MEL',ModelDef.modelname) > 0) then
  begin
   LblNumSpecies.Visible := False;
   LblNumSpecies.Enabled := False;
   EdNumSpecies.Visible := False;
   EdNumSpecies.Enabled := False;
  end;
end;

procedure TFmBatchMain.CreateNextDriver(ofilename, dfilename: string);
var
 catchnum, sgrownum, colLnh4d, colLno3d, colLdocd, colLdond, numRnh4u, numRno3u: integer;
 numRdonu, numRdocu, numWr, numDrd, numDrc, num, j:integer;
 tempstring, tempstring2, filename, basedrivename: string;
 Wrfile, Drdfile, Drcfile, Drlfile, oldDrive, newDrive: textfile;
 tempdrive: drivearray;
 temptime: double;

function GetColumnNumber(name:string):integer;
var
 i,colnum:integer;
 tempstring:string;
begin
 i := 1;
 colnum := 0;
 repeat
   tempstring := FmDisplayOutput.SgModelOutput.Cells[i,1];
   tempstring := trim(tempstring);
   if tempstring = name then colnum := i;
   i := i + 1;
 until (colnum <> 0) or (i > FmDisplayOutput.SgModelOutput.Colcount - 1);
 result := colnum;
end;

begin
 // Set up names and units of drive array
 tempdrive := drive;

 // Get catchment number
 tempstring := trim(ofilename);
 num := pos('cell',tempstring);
 delete(tempstring,1,num+3);
 catchnum := strtoint(tempstring[1]) + 1;    // Add 1 because calculating drivers for NEXT catchment

 // Get base driver filename
 basedrivename := dfilename;
 num := pos('cell',basedrivename);
 delete(basedrivename,num+4,length(basedrivename));

 // Read in outputfile for this catchment, get column numbers for N fluxes to downslope catchment
 // Note that I've assumed that the output file has output at the same time points as the driver!!
 // FIX
 sgrownum := 3;  // 1 row for column numbers, 1 for names and 1 for units, 0 based
 colLnh4d := GetColumnNumber('N NH4 loss downslope');
 colLno3d := GetColumnNumber('N NO3 loss downslope');
 colLdocd := GetColumnNumber('C DOC loss downslope');
 colLdond := GetColumnNumber('N DON loss downslope');

 // Get array index of drivers that will be changed
 numWr := FmCalculate.GetArrayIndex(vtdriver, 'Wr');
 numDrd := FmCalculate.GetArrayIndex(vtdriver, 'Drd');
 numDrc := FmCalculate.GetArrayIndex(vtdriver, 'Drc');
 numRnh4u := FmCalculate.GetArrayIndex(vtdriver, 'Rnh4u');
 numRno3u := FmCalculate.GetArrayIndex(vtdriver, 'Rno3u');
 numRdocu := FmCalculate.GetArrayIndex(vtdriver, 'Rdocu');
 numRdonu := FmCalculate.GetArrayIndex(vtdriver, 'Rdonu');

 if (catchnum > 2) and (catchnum < 7) then
  try
 // Open Wr file
   filename := 'soil mois.xs' + inttostr(catchnum) + '1.dat';
   assignfile(Wrfile, filename);
   reset(Wrfile);

 // Open Drd file
   if catchnum < 5 then
    begin
     filename := 'lateral.xq' + inttostr(catchnum) + '1.dat';
     assignfile(Drdfile, filename);
     reset(Drdfile);
    end;

 // Open Drc file
   filename := 'lateral.xqc' + inttostr(catchnum) + '1.dat';
   assignfile(Drcfile, filename);
   reset(Drcfile);

 // Open Drl file, in this version the drainage is added to the lateral flow
   filename := 'drainage.xg' + inttostr(catchnum) + '1.dat';
   assignfile(Drlfile, filename);
   reset(Drlfile);

 // Open original driver file to get Temp, CO2, etc
   filename := dfilename;
   assignfile(OldDrive, filename);
   reset(OldDrive);

 // Create new driver file
   filename := basedrivename + inttostr(catchnum) + '.drr';
   assignfile(newDrive, filename);
   rewrite(newDrive);

 // Copy names and units from olddriver to the new driver
   readln(oldDrive, tempstring);
   writeln(NewDrive, tempstring);
   readln(oldDrive, tempstring);
   writeln(NewDrive, tempstring);

 // Advance all hydrology files to 1 Jan 1999
   for j := 1 to 1029 do
    begin
      if catchnum < 5 then readln(Drdfile, tempstring);
      readln(Drcfile, tempstring);
      readln(Drlfile, tempstring);
      readln(Wrfile, tempstring);
    end;

   while not eof(OldDrive) do
    begin
     read(oldDrive, temptime);   // Read drive time
     for j:=1 to ModelDef.numdrive do
       read(oldDrive, tempdrive[j].value);  // Read drivers
     readln(oldDrive);      // Advance to next line

    // Replace Wr, Drd and Drc with values for this catchment from hydrology model
     readln(Wrfile, tempstring);
     tempdrive[numWr].value := strtofloat(tempstring);
     if catchnum < 5 then readln(Drdfile, tempstring) else tempstring := '0';
     readln(Drlfile, tempstring2);
     tempdrive[numDrd].value := strtofloat(tempstring) + strtofloat(tempstring2);
     readln(Drcfile, tempstring);
     tempdrive[numDrc].value := strtofloat(tempstring);

     // Replace Rnh4u, Rno3u, Rdocu and Rdonu with values calculated for upslope catchment by MEL
     tempdrive[numRnh4u].value :=
         strtofloat(FmDisplayOutput.SgModelOutput.Cells[colLnh4d, sgrownum]);
     tempdrive[numRno3u].value :=
         strtofloat(FmDisplayOutput.SgModelOutput.Cells[colLno3d, sgrownum]);
     tempdrive[numRdocu].value :=
         strtofloat(FmDisplayOutput.SgModelOutput.Cells[colLdocd, sgrownum]);
     tempdrive[numRdonu].value :=
         strtofloat(FmDisplayOutput.SgModelOutput.Cells[colLdond, sgrownum]);
     sgrownum := sgrownum + 1;   // Increment row for next round

    // Write the drivers to the new driver file
     write(NewDrive,format('%-25.13e',[temptime]),' ');   // Write drive time
     for j := 1 to ModelDef.numdrive do
       write(NewDrive,format('%-25.13e',[tempdrive[j].value]),' '); // Write drivers
     writeln(NewDrive);   // Write return
    end;
  finally
   CloseFile(oldDrive);
   CloseFile(newDrive);
   if catchnum < 5 then CloseFile(DrdFile);
   CloseFile(DrcFile);
   CloseFile(DrlFile);
   CloseFile(WrFile);
  end;
end;

procedure TFmBatchMain.CreateNextDriver4L(ofilename, dfilename: string);
var
 catchnum, sgrownum, num, j, k:integer;
 colLnh4d, colLno3d, colLdocd, colLdond, numRnh4u, numRno3u, numRdonu, numRdocu,
 numWr, numDrd, numDrc, numDrl: array[1..numlayers] of integer;
 tempstring, filename, basedrivename: string;
 oldDrive, newDrive: textfile;
 Wrfile, Drdfile, Drcfile, Drlfile: array[1..numlayers] of textfile;
 tempdrive: drivearray;
 temptime: double;

function GetColumnNumber(name:string):integer;
var
 i,colnum:integer;
 tempstring:string;
begin
 i := 1;
 colnum := 0;
 repeat
   tempstring := FmDisplayOutput.SgModelOutput.Cells[i,1];
   tempstring := trim(tempstring);
   if tempstring = name then colnum := i;
   i := i + 1;
 until (colnum <> 0) or (i > FmDisplayOutput.SgModelOutput.Colcount - 1);
 result := colnum;
end;

begin
 // Set up names and units of drive array
 tempdrive := drive;

 // Get catchment number
 tempstring := trim(ofilename);
 num := pos('cell', tempstring);
 delete(tempstring, 1, num+3);
 catchnum := strtoint(tempstring[1]) + 2;    // Add 2 because calculating drivers for every other catchment
                                             // i.e. 1 flows to 3, 2 to 4, etc.
 // Get base driver filename
 basedrivename := dfilename;
 num := pos('cell', basedrivename);
 delete(basedrivename, num+4, length(basedrivename));

 // Read in outputfile for this catchment, get column numbers for N fluxes to downslope catchment
 // Note that I've assumed that the output file has output at the same time points as the driver!!
 // FIX
 sgrownum := 3;  // 1 row for column numbers, 1 for names and 1 for units, 0 based
 for j := 1 to numlayers do
  begin
   colLnh4d[j] := GetColumnNumber('*N NH4 loss downslope' + inttostr(j));
   colLno3d[j] := GetColumnNumber('*N NO3 loss downslope' + inttostr(j));
   colLdocd[j] := GetColumnNumber('*C DOC loss downslope' + inttostr(j));
   colLdond[j] := GetColumnNumber('*N DON loss downslope' + inttostr(j));

 // Get array index of drivers that will be changed
   numWr[j] := FmCalculate.GetArrayIndex(vtdriver, 'Wr' + '[' + inttostr(j) + ']');
   numDrd[j] := FmCalculate.GetArrayIndex(vtdriver, 'Drd' + '[' + inttostr(j) + ']');
   numDrc[j] := FmCalculate.GetArrayIndex(vtdriver, 'Drc' + '[' + inttostr(j) + ']');
   numDrl[j] := FmCalculate.GetArrayIndex(vtdriver, 'Drl' + '[' + inttostr(j) + ']');
   numRnh4u[j] := FmCalculate.GetArrayIndex(vtdriver, 'Rnh4u' + '[' + inttostr(j) + ']');
   numRno3u[j] := FmCalculate.GetArrayIndex(vtdriver, 'Rno3u' + '[' + inttostr(j) + ']');
   numRdocu[j] := FmCalculate.GetArrayIndex(vtdriver, 'Rdocu' + '[' + inttostr(j) + ']');
   numRdonu[j] := FmCalculate.GetArrayIndex(vtdriver, 'Rdonu' + '[' + inttostr(j) + ']');
  end;

 if (catchnum > 2) and (catchnum < 7) then
  try
   for j := 1 to numlayers do
    begin
 // Open Wr file
     filename := 'soil mois.xs' + inttostr(catchnum) + inttostr(j) + '.dat';
     assignfile(Wrfile[j], filename);
     reset(Wrfile[j]);

 // Open Drd file
     if catchnum < 5 then
      begin
       filename := 'lateral.xq' + inttostr(catchnum) + inttostr(j) + '.dat';
       assignfile(Drdfile[j], filename);
       reset(Drdfile[j]);
      end;

 // Open Drc file
     filename := 'lateral.xqc' + inttostr(catchnum) + inttostr(j) + '.dat';
     assignfile(Drcfile[j], filename);
     reset(Drcfile[j]);

 // Open Drl file
     if j <> numlayers then
      begin
       filename := 'drainage.xg' + inttostr(catchnum) + inttostr(j) + '.dat';
       assignfile(Drlfile[j], filename);
       reset(Drlfile[j]);
      end;
   end;

 // Open original driver file to get Temp, CO2, etc
   filename := dfilename;
   assignfile(OldDrive, filename);
   reset(OldDrive);

 // Create new driver file
   filename := basedrivename + inttostr(catchnum) + '.drr';
   assignfile(newDrive, filename);
   rewrite(newDrive);

 // Copy names and units from olddriver to the new driver
   readln(oldDrive, tempstring);
   writeln(NewDrive, tempstring);
   readln(oldDrive, tempstring);
   writeln(NewDrive, tempstring);

 // Advance all hydrology files to 1 Jan 1999
   for j := 1 to 1029 do
    for  k := 1 to numlayers do
     begin
      readln(Wrfile[k], tempstring);
      if catchnum < 5 then readln(Drdfile[k], tempstring);
      readln(Drcfile[k], tempstring);
      if k <> numlayers then readln(Drlfile[k], tempstring);
     end;

   while not eof(OldDrive) do
    begin
     read(oldDrive, temptime);   // Read drive time
     for j:=1 to ModelDef.numdrive do
       read(oldDrive, tempdrive[j].value);  // Read drivers
     readln(oldDrive);      // Advance to next line

    // Replace Wr, Drd, Drc and Drl with values for this catchment from hydrology model
     for j := 1 to numlayers do
      begin
       readln(Wrfile[j], tempstring);
       tempdrive[numWr[j]].value := strtofloat(tempstring);
       if catchnum < 5 then readln(Drdfile[j], tempstring) else tempstring := '0';
       tempdrive[numDrd[j]].value := strtofloat(tempstring);
       if j <> numlayers then readln(Drlfile[j], tempstring) else tempstring := '0';
       tempdrive[numDrl[j]].value := strtofloat(tempstring);
       readln(Drcfile[j], tempstring);
       tempdrive[numDrc[j]].value := strtofloat(tempstring);

     // Replace Rnh4u, Rno3u, Rdocu and Rdonu with values calculated for upslope catchment by MEL
       tempdrive[numRnh4u[j]].value :=
         strtofloat(FmDisplayOutput.SgModelOutput.Cells[colLnh4d[j], sgrownum]);
       tempdrive[numRno3u[j]].value :=
         strtofloat(FmDisplayOutput.SgModelOutput.Cells[colLno3d[j], sgrownum]);
       tempdrive[numRdocu[j]].value :=
         strtofloat(FmDisplayOutput.SgModelOutput.Cells[colLdocd[j], sgrownum]);
       tempdrive[numRdonu[j]].value :=
         strtofloat(FmDisplayOutput.SgModelOutput.Cells[colLdond[j], sgrownum]);
      end;
     sgrownum := sgrownum + 1;   // Increment row for next round

    // Write the drivers to the new driver file
     write(NewDrive,format('%-25.13e',[temptime]),' ');   // Write drive time
     for j := 1 to ModelDef.numdrive do
       write(NewDrive,format('%-25.13e',[tempdrive[j].value]),' '); // Write drivers
     writeln(NewDrive);   // Write return
    end;
  finally
   CloseFile(oldDrive);
   CloseFile(newDrive);
   for j := 1 to numlayers do
    begin
     CloseFile(WrFile[j]);
     if catchnum < 5 then CloseFile(DrdFile[j]);
     CloseFile(DrcFile[j]);
     if j <> numlayers then CloseFile(DrlFile[j]);
    end;
  end;
end;

function TFmBatchMain.GetResourcePath(): string;
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

procedure TFmBatchMain.RunSpinup(pfile, dfile, ofile:string);
var
 holdstart, holdstop:double;
 holdopt:TRunOptions;
begin
 holdstart:=time_start;
 holdstop:=time_stop;
 holdopt:=FmOptions.RunOptions;
 time_start:=1;
 time_stop:=7300;
 FmOptions.RunOptions:=fregrowOpt;

 FmShellMain.BtnRunClick(FmBatchMain);

 // Turn off resets
 stat[FmCalculate.GetArrayIndex(vtState, 'BC')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'BN')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'BP')].reset:=false;


 stat[FmCalculate.GetArrayIndex(vtState, 'DC')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'DN')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'DP')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'WC')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'WN')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'WP')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'SC')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'SN')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'SP')].reset:=false;

 stat[FmCalculate.GetArrayIndex(vtState, 'ENH4')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'ENO3')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'EPO4')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'Pa')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'Pno')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'Poccl')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'W')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'WSnow')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'fc')].reset:=false;
 stat[FmCalculate.GetArrayIndex(vtState, 'SQ')].reset:=false;

 paramfilename:=stringreplace(paramfilename,'spinup regrow','regrow',[rfIgnoreCase,rfReplaceAll]);

 WriteParamFile(paramfilename, ModelDef.numparam,par, ModelDef.numstate,stat,FmShellMain.Currentresid);

 // Set run options back to what was specified in the batch file
 time_start:=holdstart;
 time_stop:=holdstop;
 FmOptions.RunOptions:=holdOpt;
end;

end.
