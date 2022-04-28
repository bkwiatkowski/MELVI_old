{ Contains the code for all file input and output. All I/O should be done using
  these procedures. There is one exception. In the note.pas file the drivers are
  written to the driver file directly from the memo component. It is done that
  way because the memo component has a function to write to a file and using
  that function is easier then converting the text in the memo to the correct
  form for the writedriverfile function. }
unit fileio;

interface

uses sysutils, classes, Dialogs, stypes, frontend, Options, Lazfileutils;

type  TAction = (flRead, flWrite);
      EFileError = Class(Exception);

// Model I/O
function  ReadParamFile(var filename:string; const npar:integer;
           var parray:paramarray; const nstat:integer;
           var sarray:statearray; var Tresid:double):Boolean;
procedure WriteParamFile(filename:string; const npar:integer;
          var parray:paramarray; const nstat:integer; var sarray:statearray;
          var Tresid:double);
procedure GetCurrentDrivers(CurrTime:double; var CurrDrivers:DriveArray);
function openDriverFile(filename:string; dlist:Tstringlist):Boolean;
function  WriteDriverFile(filename:string; dlist:Tstringlist):Boolean;
procedure OpenOutputFile(filename:string; const ndrive:integer; const darray:drivearray;
           const numstat:integer; var sarray:statearray; const numproc:integer;
           var parray:processarray; Action:TAction);
function  ReadOutputFile(var time:double; const numdrive:integer; var darray:
          drivearray; const numstat:integer; var sarray:statearray; const
          numproc:integer; var parray:processarray):Boolean;
function WriteOutputFile(time:double; const numstat:integer;
          var sarray:statearray; const numproc:integer;
          var parray:processarray):Boolean;
procedure CloseOutputFile;

// Batch I/O
procedure OpenBatchFile(filename:string);
function  ReadBatchFile(var paramname, drivername, outputname:string; var begintime,
             endtime:double; var tstat:statearray; var opt:TRunOptions):Boolean;
procedure CloseBatchFile;
procedure OpenLogFile(filename, batchfile:string);
function  WriteLogFile(filename:string; outputname:string;
                        StatusMessage:string):Boolean;
procedure CloseLogFile;

// Sensitivity I/O
procedure OpenListFile(filename:string);
function  ReadListFile(var paramfilename, driverfilename, paramname:string;
                        var newparamvalue:double):Boolean;
procedure CloseListFile;
procedure OpenMeasFile(filename:string);
function  ReadMeasFile(var thisline:string): Boolean;
procedure CloseMeasFile;
procedure OpenSensOutFile(filename:string);
function  WriteSensOutFile(var paramfilename, paramname:string;
             var newparamvalue:double; TestStatus:string; ResidCL, ResidCW,
             ResidCR, ResidNL, ResidNW, ResidNR, ResidCT, ResidNT:
             double):Boolean;
procedure CloseSensOutFile;

// Modelshell I/O
procedure  ReadModelDef(filename:string; var tempmodeldef:Tmodeldef;
     var tstat:statearray; var tpar:paramarray; var tproc:processarray;
     var tdrive:drivearray);

// Ensemble Kalman Filter I/O
procedure OpenTextFile(filename:string; var filehandle:textfile);
function  ReadTextFile(var filehandle: textfile; var aline:string):Boolean;
function CheckforOpenFiles(filename: string): Boolean;
function WriteKalmanOutFile(filename:string; var outdata:TKstate; saveuncorrupted:Boolean):Boolean;


// General procedures
procedure ChangeExtension(var filename:string; NewExt:string);
procedure RemoveSpaces(var somestring:string);
function ParCount(processnum:integer) : integer;

var
 last_time,next_time:double;
 last_drive,next_drive: drivearray;
 driverfile:textfile;
 driverlist:Tstringlist;
 currdline:integer;
 timeFound:boolean;
 outfile, outfile2:textfile;
 batchfile:textfile;
 logfile:textfile;
 listfile:textfile;
 sensOutfile:textfile;
 calOutfile:textfile;
 measfile:textfile;
 ParamFileVersion:string;

implementation

// Parameter File I/O

{ The parameter file is a space delimited ASCII file containing the parameter
  values and the initial values of the state variables. Each line of the file
  contains one parameter or state variable. The first item on the line is the
  variable name (a string up to stringlength characters), the second item is the numerical
  value of the item and the final item on the line is the units of the parameter
  or state variable (also a string up to stringlength characters long). }

{ This function reads the entire parameter file if it exists or creates a new
   empty file. }
function ReadParamFile(var filename:string; const npar:integer;
           var parray:paramarray; const nstat:integer;
           var sarray:statearray; var Tresid:double):Boolean;
var
  i,charnum:integer;
  dum, dum2, dum3, dum4, tempvalue:string[stringlength+1];
  tempstring1, tempstring2 :string;
  paramfile:textfile;
  StatesRead, ParamsRead: Boolean;
begin
  ReadParamFile := False;
  assignfile(paramfile,filename);
  NeedToSavePar := False;
  reset(paramfile);
  readln(paramfile, ParamFileVersion);
  SetLength(tempstring1, stringlength + 2);
  strlcopy(PChar(tempstring1), PChar(ParamFileVersion), stringlength + 1);
 { Delete any spaces in the model name read from the parameter file and any
   spaces in a copy of the modelname field of modeldef. This is to prevent
   the invalid parameter file version error due to differences in spacing. }
  tempstring1 := trim(tempstring1);
  charnum := pos(' ',tempstring1);
  while charnum <> 0 do
    begin
      delete(tempstring1,charnum,1);
      charnum := pos(' ',tempstring1);
    end;
 // And now delete the spaces from the modelname.
  tempstring2 := ModelDef.modelname;
  charnum := pos(' ',tempstring2);
  while charnum <> 0 do
    begin
      delete(tempstring2,charnum,1);
      charnum := pos(' ',tempstring2);
    end;

 // Compare the 2 strings containing the modelname.
  if StrIComp(PChar(tempstring1),PChar(tempstring2)) <> 0 then
    begin
      reset(paramfile);
      ParamFileVersion := 'Unknown'; // Model names don't match
    end
  else  // Model names do match, now extract the version number and compare
    begin
      delete(ParamFileVersion,1,stringlength + 1);
      ParamFileVersion := trim(ParamFileVersion);
      charnum := pos(' ',ParamFileVersion);
      if charnum <> 0 then
        delete(ParamFileVersion,charnum,Length(ParamFileVersion)-charnum+1);
    end;

 try
  if ParamFileVersion <> ModelDef.versionnumber then
    begin
      raise EFileError.create('The parameter file selected is not compatible with this model. '
                   + 'No parameters read.');
    end
  else
{ Model name and version match the parameter file so read in the state variables
  and parameters. }
    begin
     i := 0;
     StatesRead:=false;
     ParamsRead:=False;
     repeat
       i := i + 1;
       readln(paramfile,dum3,tempvalue,dum4,dum,dum2);
       if (i <= ModelDef.numstate) and (not StatesRead) then
         begin
          sarray[i].value := strtofloat(tempvalue);
          dum := trim(dum);
          dum2 := trim(dum2);
          if (dum[1] = 'T') or (dum[1] = 't') then
            sarray[i].holdconstant := True
          else
            sarray[i].holdconstant := False;
          if (dum2[1] = 'T') or (dum2[1] = 't') then
            sarray[i].reset := True
          else
            sarray[i].reset := False;
          if i = ModelDef.numstate then
           begin
            StatesRead := True;
            i := 0;
           end;
         end;
       if StatesRead and (i>0) then
         begin
          parray[i].value := strtofloat(tempvalue);
          if i = ModelDef.numparam then ParamsRead := True;
         end;
       if StatesRead and ParamsRead then
         begin
          readln(paramfile,dum,Tresid);
          if Tresid = 0 then Tresid := 999;
         end;
     until eof(paramfile);
     ReadParamFile := true;
     if i <> ModelDef.numparam then
       raise Exception.Create('Model and parameter file have a different number of parameters.');
    end;
 finally
  closefile(paramfile);
 end;
end;  // End readparamfile

{ Writes the entire parameter file, overwriting any existing file. Frontend
  does the checking to be sure you want to overwrite an existing file. See
  DlgSaveParam. }
procedure WriteParamFile(filename:string; const npar:integer;
           var parray:paramarray; const nstat:integer;
           var sarray:statearray; var Tresid:double);
var
  i:integer;
  paramfile:textfile;
  tstring1, tstring2: string;
begin
  assignfile(paramfile,filename);
  rewrite(paramfile);
  // Write out the model name and version.
  writeln(paramfile, format('%-25.25s',[ModelDef.modelname]), ' ',
                     format('%-25.25s',[ModelDef.VersionNumber]), ' ',
                     format('%-77s',['Units                     Holdconstant              Reset']));
  // Write state variable name, value, units, holdconstant and reset
  for i:= 1 to nstat do
   begin
    if sarray[i].holdconstant then
     tstring1 := 'True'
    else
     tstring1 := 'False';
    if sarray[i].reset then
     tstring2 := 'True'
    else
     tstring2 := 'False';
    writeln(paramfile,format('%-25.25s',[sarray[i].name]),' ',
                      format('%-25.25e',[sarray[i].value]),' ',
                             format('%-25.25s',[sarray[i].units]), ' ',
                          //   sarray[i].holdconstant:-25, ' ', sarray[i].reset:-25);
                             format('%-25.25s',[tstring1{sarray[i].holdconstant}]), ' ',
                             format('%-25.25s',[tstring2{sarray[i].reset}]));
   end;
  // Write parameter name, value and units
  for i:= 1 to npar do
    writeln(paramfile,format('%-25.25s',[parray[i].name]),' ',
                      format('%-25e',[parray[i].value]),' ',
                             format('%-25.25s',[parray[i].units]));
  writeln(paramfile,format('%-25.25s',['Total Residual ']),' ',format('%-25g',[Tresid]));
  closefile(paramfile);
  NeedToSavePar := false;
end;  // end of writeparamfile


// Driver File I/O

{ The driver file is a comma delimited ASCII file containing the driving
  variables. The first line of the file contains the names of the drivers,
  the second line contains the units, and subsequent lines contain the driver data.
  Each line of the driver file contains simulation time as the first value,
  followed by a value for each driver variable. A driver file must contain at
  least one line of driver data. Lines after the first data line are optional.
  If there are multiple driver data lines, the driver variables are held constant
  at the last value read until the next the simulation time equals the time value
  in the next driver data line. Negative time values are a flag that creates a
  linear ramp from drivers specified on the line before the negative time to the
  drivers specified on the line with the negative time. The function
  GetCurrentDrivers calculates any ramping if necessary.

  All access to the drivers is done using GetCurrentDrivers.
}

{ This procedure gets the values for the drivers at the time specified in
  CurrTime. If the next_time read in from the driver file is negative, this
  procedure calculates a linear ramp between the values in last_drive and
  next_drive. }
procedure GetCurrentDrivers(CurrTime:double; var CurrDrivers:DriveArray);
var
 tfrac: double;
 ll:integer;
 tempstring:tstringlist;
 anint: integer;
begin
 tempstring:=Tstringlist.create;
 try
 tempstring.Delimiter:=',';
 tempstring.StrictDelimiter:=true;

// No previous values have been read. Read two lines from drivers.
 if (last_time = 0) and (next_time = 0) then
  begin
   currdline := 2;
   tempstring.DelimitedText:=driverlist[currdline];
   last_time:=strtoint(tempstring[0]);
   for ll:=1 to ModelDef.numdrive do last_drive[ll].value:=strtofloat(tempstring[ll]);
   CurrDrivers:=last_drive;
   next_time:=last_time;
   next_drive:=last_drive;
  end;

// Advance to next line of drivers
 while (CurrTime>=abs(next_time)) and (currdline<=driverlist.count-1) do
   begin
    last_time := next_time;
    last_drive := next_drive;
    CurrDrivers:=next_drive;

    currdline:=currdline+1;
    tempstring.Clear;
    if currdline<driverlist.count then
     begin
      tempstring.DelimitedText:=driverlist[currdline];
      next_time:=strtoint(tempstring[0]);
      for ll:=1 to ModelDef.numdrive do next_drive[ll].value:=strtofloat(tempstring[ll]);
     end;
   end;
 finally
  if assigned(tempstring) then FreeandNil(tempstring);
 end;

{ If next_time is less than zero calculate the drivers using a linear ramp
  between last_time and next_time. }
 if (next_time < 0) and (abs(next_time)<>abs(last_time)) then
  begin
   tfrac := (CurrTime-abs(last_time))/(abs(next_time)-abs(last_time));
   for ll := 1 to ModelDef.numdrive do
     CurrDrivers[ll].value := last_drive[ll].value + tfrac*(next_drive[ll].value -
                                       last_drive[ll].value);
  end;
end;    // getcurrentdrivers

// Opens and read entire driver file into stringlist
function openDriverFile(filename:string; dlist:Tstringlist):Boolean;
var
 idx:integer;
begin
 if not LazFileUtils.FileExistsUTF8(filename)  then
  begin
   MessageDlg('Cannot read driver file. File does not exist.', mtError,[mbOK],0);
   openDriverFile:=false;
  end
 else
  begin
   try
     dlist.LoadFromFile(driverfilename);
     // Remove any empty lines
     for idx := dlist.count - 1 downto 0 do
       begin
         if Trim(dlist[idx]) = '' then
         dlist.Delete(idx);
       end;
     openDriverFile:=true;
   except
     raise Exception.Create('Unable to read driver file.');
   end;
  end;
end;     // openDriverFile

// Writes entire driver file.
function WriteDriverFile(filename:string; dlist:Tstringlist):Boolean;
begin
 try
  dlist.SaveToFile(filename);
  WriteDriverFile:=true;
 except
  WriteDriverFile:=false;
  raise;
 end;
end;   // writedriverfile

// Output File I/O

{ The output file is a space delimited ASCII file containing values for the
  state variables and processes at each time step of the model run. The first
  item on a line is the time followed by the state variables and then the
  process variables. The first two lines of the file are header lines. The first
  line contains the names of the variables and the second line contains the
  units.

  The output file must be opened and closed using OpenOutputFile and
  CloseOutputFile. Be sure to call CloseOutputFile when you are done.}

// Opens the output file for reading or writing depending on the value of Action
procedure OpenOutputFile(filename:string; const ndrive:integer; const darray:drivearray;
           const numstat:integer; var sarray:statearray; const numproc:integer;
           var parray:processarray; Action:TAction);
var
  j:integer;
  temp:string;
begin
 assignfile(outfile,filename);
 if Action = flread then  // Open output file for reading
   begin
     reset(outfile);
     readln(outfile,temp); // Read variable names and throw them away
     readln(outfile,temp); // Read variable units and throw them away
   end
 else  // Open output file to write
   begin
    if FmOptions.RunOptions.AppendOutputFile then
     begin
      append(outfile);
     end
    else
     begin
      rewrite(outfile);    // Create a new output file
      temp := 'Time';
      for j := 1 to ModelDef.numdrive do temp := temp + ', ' + drive[j].name;
      for j := 1 to numstat do temp := temp + ', ' + sarray[j].name;
      for j := ModelDef.numstate + 1 to numproc do temp := temp + ', ' + parray[j].name;
      writeln(outfile, temp);
      temp := ModelDef.timeunit;
      for j := 1 to ModelDef.numdrive do temp := temp + ', ' + drive[j].units;                             //   Units
      for j := 1 to numstat do temp := temp + ', ' + sarray[j].units;
      for j := ModelDef.numstate + 1 to numproc do temp := temp + ', ' + parray[j].units;
      writeln(outfile, temp);
     end;
   end;                      
end;  // openoutputfile

// Reads one line of the output file
function ReadOutputFile(var time:double; const numdrive:integer; var darray:
         drivearray; const numstat:integer; var sarray:statearray; const
         numproc:integer;var parray:processarray):Boolean;
var
 j, num:integer;
 tempstring: string;
begin
  ReadOutputFile := False;
  if (not eof(outfile)) then
    begin
     readln(outfile, tempstring);
     num := pos(',', tempstring);
     time := strtofloat(copy(tempstring, 1, num-1));
     for j:= 1 to ModelDef.numdrive do
      begin
        delete(tempstring, 1, num);
        num := pos(',', tempstring);
        darray[j].value := strtofloat(copy(tempstring, 1, num-1));
      end;
     for j := 1 to numstat do
      begin
        delete(tempstring, 1, num);
        num := pos(',', tempstring);
        sarray[j].value := strtofloat(copy(tempstring, 1, num-1));
      end;
     for j := ModelDef.numstate + 1 to numproc do
      begin
        delete(tempstring, 1, num);
        num := pos(',', tempstring);
        if num <> 0 then
          parray[j].value := strtofloat(copy(tempstring, 1, num-1))
        else
          parray[j].value := strtofloat(tempstring);
      end;
     ReadOutputFile := True;
    end;
end;  // readoutputfile

// Writes one line to the output file
function WriteOutputFile(time:double; const numstat:integer;
         var sarray:statearray; const numproc:integer;
         var parray:processarray):Boolean;
var
  j:integer;
begin
   write(outfile, floattostr(time));  // Write time
   for j := 1 to ModelDef.numdrive
      do write(outfile, ', ' + floattostr(drive[j].value));
   for j := 1 to numstat     // State variables
      do write(outfile, ', ' + floattostr(sarray[j].value));
   for j := ModelDef.numstate + 1 to numproc     // Process variables
      do write(outfile, ', ' + floattostr(parray[j].value));
   writeln(outfile);  // Write return
   WriteOutputFile := True;
end;  // writeoutputfile

// Closes the output file
procedure CloseOutputFile;
begin
   closefile(outfile);
end;

// Batch File I/O

{ The batch file is a comma delimited ASCII file containing all the information
  necessary to do a model run. Each line represents one run of the model and
  contains the parameter file, driver file, and output file names, the start
  time, the stop time and the time step. The first is an example.

  The batch file must be opened and closed using OpenBatchFile and
  CloseBatchFile. Be sure to call CloseBatchFile when you are done.}
  
{ Opens the batch file, reads in the first line and discards it. }
procedure OpenBatchFile(filename:string);
var
tempstring:string;
begin
 assignfile(batchfile,filename);
 reset(batchfile);
 readln(batchfile,tempstring);    // Read names and throw away
end;

{ Reads a line from the batch file. Returns true if the read was successful and
  returns false otherwise. }
function  ReadBatchFile(var paramname, drivername, outputname:string; var begintime,
             endtime:double; var tstat:statearray; var opt:TRunOptions):Boolean;
var
 tempstring1, tempstring2: string;
 numbegin, numend:integer;
begin
 if not eof(batchfile) then // Check for end of file
  begin
   readln(batchfile,tempstring1);   // Read line
   numbegin := 1;
   numend := pos(',',tempstring1);                  // param filename
   paramname := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(paramname,pos(',',paramname),1);
   paramname := trim(paramname);

   numend := pos(',',tempstring1);                  // driver filename
   drivername := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(drivername,pos(',',drivername),1);
   drivername := trim(drivername);

   numend := pos(',',tempstring1);                  // output filename
   outputname := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(outputname,pos(',',outputname),1);
   outputname := trim(outputname);

   numend := pos(',',tempstring1);                  // Start time
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(tempstring2,pos(',',tempstring2),1);
   begintime := strtofloat(tempstring2);

   numend := pos(',',tempstring1);                  // Stop time
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(tempstring2,pos(',',tempstring2),1);
   endtime := strtofloat(tempstring2);

   numend := pos(',',tempstring1);                 // Time step
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(tempstring2,pos(',',tempstring2),1);
   opt.Time_step := strtofloat(tempstring2);

   numend := pos(',',tempstring1);                // Discrete step
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(tempstring2,pos(',',tempstring2),1);
   opt.Discretestep := strtofloat(tempstring2);

   numend := pos(',',tempstring1);               // Normal Run?
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   tempstring2 := trim(tempstring2);
   if (tempstring2[1] = 't') or (tempstring2[1] = 'T') then
     Opt.NormalRun := True
   else
     Opt.NormalRun := False;

   numend := pos(',',tempstring1);               // Repeat Drivers?
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   tempstring2 := trim(tempstring2);
   if (tempstring2[1] = 't') or (tempstring2[1] = 'T') then
     Opt.RepeatDrivers := True
   else
     Opt.RepeatDrivers := False;

   numend := pos(',',tempstring1);               // Repeat drive time
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(tempstring2,pos(',',tempstring2),1);
   Opt.RepeatDriveTime := strtofloat(tempstring2);

   numend := pos(',',tempstring1);               // Reset States?
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   tempstring2 := trim(tempstring2);
   if (tempstring2[1] = 't') or (tempstring2[1] = 'T') then
     Opt.ResetStates := True
   else
     Opt.ResetStates := False;

   numend := pos(',',tempstring1);               // Reset State Time
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(tempstring2,pos(',',tempstring2),1);
   Opt.ResetStateTime := strtofloat(tempstring2);

   numend := pos(',',tempstring1);              // Run to SS
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   tempstring2 := trim(tempstring2);
   if (tempstring2[1] = 't') or (tempstring2[1] = 'T') then
     Opt.RuntoSS := True
   else
     Opt.RuntoSS := False;

   numend := pos(',',tempstring1);             // SS Criteria
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(tempstring2,pos(',',tempstring2),1);
   Opt.SSCriteria := strtofloat(tempstring2)/100;

   numend := pos(',',tempstring1);             // SS Time
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(tempstring2,pos(',',tempstring2),1);
   Opt.SSTime := strtofloat(tempstring2);

   numend := pos(',',tempstring1);             // Hold States Constant
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   tempstring2 := trim(tempstring2);
   if (tempstring2[1] = 't') or (tempstring2[1] = 'T') then
     Opt.HoldStatesConstant := True
   else
     Opt.HoldStatesConstant := False;

   numend := pos(',',tempstring1);             // Output Step
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(tempstring2,pos(',',tempstring2),1);
   Opt.OutputStep := strtofloat(tempstring2);

   numend := pos(',',tempstring1);             // Output Offset
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(tempstring2,pos(',',tempstring2),1);
   Opt.OutputOffset := strtofloat(tempstring2);

   numend := pos(',',tempstring1);             //Output EOR only
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   tempstring2 := trim(tempstring2);
   if (tempstring2[1] = 't') or (tempstring2[1] = 'T') then
     Opt.OutputEORonly := True
   else
     Opt.OutputEORonly := False;

   numend := pos(',',tempstring1);            // Output Annually
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   tempstring2 := trim(tempstring2);
   if (tempstring2[1] = 't') or (tempstring2[1] = 'T') then
     Opt.OutputAnnually := True
   else
     Opt.OutputAnnually := False;

   numend := pos(',',tempstring1);             // Day of Year for annual output
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(tempstring2,pos(',',tempstring2),1);
   Opt.OutputAnnuallyDay := strtofloat(tempstring2);

   numend := pos(',',tempstring1);            // Append Output File
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   tempstring2 := trim(tempstring2);
   if (tempstring2[1] = 't') or (tempstring2[1] = 'T') then
     Opt.AppendOutputFile := True
   else
     Opt.AppendOutputFile := False;

   numend := pos(',',tempstring1);            // No Output File
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   tempstring2 := trim(tempstring2);
   if (tempstring2[1] = 't') or (tempstring2[1] = 'T') then
     Opt.OutputFile := True
   else
     Opt.OutputFile := False;

   Opt.ErrorMult := strtoint(tempstring1);    // Error Mult

   ReadBatchFile := True;
  end
 else
   ReadBatchFile := False;
end;

{ Closes the batch file. }
procedure CloseBatchFile;
begin
 closefile(batchfile);
end;

// Log File I/O

{ The log file is an ASCII file summarizing the batch job defined in the
  batch file. It contains the date and time, the batch file
  name, and for each run: the output file name and a message with the status
  of the run. }
  
{ Opens the Batch log file and writes out initial information. }
procedure OpenLogFile(filename, batchfile:string);
begin
 assignfile(logfile,filename);
 rewrite(logfile);
 writeln(logfile, 'Output log for Modelshell Batch Utility Version ' + ModelDef.versionnumber);
 writeln(logfile, 'Generated: ' + DateTimeToStr(Now));
 writeln(logfile, 'Output from batch file: ' + batchfile);
end;

{ Write a line to the log file containing the output file name and a status
  message. }
function  WriteLogFile(filename:string; outputname:string;
                        StatusMessage:string):Boolean;
begin
 writeln(logfile, outputname + ': ' + StatusMessage);
 WriteLogFile := True;
end;

procedure CloseLogFile;
begin
 closefile(logfile);
end;

// List File I/O

{ The list file is a comma delimited ASCII file containing information
  necessary for running a sensitivity analysis. Each line represents one
  sensitivity test and contains the parameter file name, the driver file name
  the parameter to vary, the new value for the parameter. The first line is
  an example.

  The list file must be opened and closed using OpenListFile and
  CloseListFile. Be sure to call CloseListFile when you are done.}
  
{ Opens the list file, reads in the first line and discards it. }
procedure OpenListFile(filename:string);
var
tempstring:string;
begin
 assignfile(listfile,filename);
 reset(listfile);
 readln(listfile,tempstring);    // Read names and throw away
end;

{ Reads a line from the list file. Returns true if the read was successful and
  returns false otherwise. }
function  ReadListFile(var paramfilename, driverfilename, paramname:string;
                        var newparamvalue:double):Boolean;
var
 tempstring1,tempstring2: string;
 numbegin,numend: integer;
begin
 if not eof(listfile) then // Check for end of file
  begin
   readln(listfile, tempstring1);   // Read line
   removespaces(tempstring1);
   numbegin := 1;
   numend := pos(',',tempstring1);
   paramfilename := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(paramfilename,pos(',',paramfilename),1);

   numend := pos(',',tempstring1);
   driverfilename := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(driverfilename,pos(',',driverfilename),1);

   numend := pos(',',tempstring1);
   paramname := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(paramname,pos(',',paramname),1);

   numend :=  length(tempstring1);
   tempstring2 := copy(tempstring1,numbegin,numend-numbegin+1);
   delete(tempstring1,numbegin,numend);
   delete(tempstring2,pos(',',tempstring2),1);
   newparamvalue := strtofloat(tempstring2);

   ReadListFile := True;
  end
 else
   ReadListFile := False;
end;

{ Closes the list file. }
procedure CloseListFile;
begin
 closefile(listfile);
end;

// Measured data file I/O

{ The measured data file is a comma delimited ASCII file containing the
  measured data to be compared with the model. Each line represents one
  comparison and contains the treatment code, the simulation year,
  the measured leaf C, wood C, root C, leaf N, wood N, and root N. Note that
  the treatment code is used to identify the output file to read the data from.
  Therefore, the treatment code must appear in the output filename and it must
  appear in only one output filename. The year is the simulation year that
  corresponds to the measured data on this line.

  The measured data file must be opened and closed using OpenMeasFile
  and CloseMeasFile. Be sure to call CloseMeasFile when you are done.}

{ This procedure opens the Measured data file for reading, reads in the first
  line containing the headings and discards it.}
procedure OpenMeasFile(filename:string);
var
 tempstring: string;
begin
 assignfile(measfile,filename);
 reset(measfile);
 readln(measfile,tempstring);    // Read names and throw away
end;

{ Reads one line of the measured data file. Parsing of the line read in is done
  in a different procedure to allow easy changing of the measured data. }
function  ReadMeasFile(var thisline:string): Boolean;
begin
 if not eof(measfile) then
  begin
    readln(measfile,thisline);
    ReadMeasFile := true;
  end
 else
    ReadMeasFile := false;
end;

{ Closes the measured data file. }
procedure CloseMeasFile;
begin
 CloseFile(measfile);
end;


// Comma File I/O

{ Procedure to open and read one line of a comma delimited ASCII file. The first
  line of the file is assumed to contain column headings and is read into the
  variables headings.

  The comma data file must be opened and closed using OpenCommaFile
  and CloseFile. Be sure to call CloseFile when you are done.}

{ This procedure opens a comma delimited data file for reading, reads in the first
  line containing the headings and discards it.}
procedure OpenTextFile(filename:string; var filehandle:textfile);
begin
 assignfile(filehandle,filename);
 reset(filehandle);
end;

{ Reads one line of a comma delimited data file. Parsing of the line read in is
  done by the calling routine. }
function  ReadTextFile(var filehandle: textfile; var aline:string):Boolean;
begin
 if not eof(filehandle) then
  begin
    readln(filehandle,aline);
    ReadTextFile := true;
  end
 else
    ReadTextFile := false;
end;

function CheckforOpenFiles(filename: string): Boolean;
var
 outhandle, outhandlex, outhandlen, outhandleq, outhandley: textfile;
 answer: Word;
 outfile, outfilex, outfilen, outfileq, outfiley: boolean;
begin
 outfile := False;
 outfilex := False;
 outfilen := False;
 outfileq := False;
 outfiley := False;
 try
  try
   if LazFileUtils.FileExistsUTF8(filename)  then
   begin
    outfile := True;
    assignfile(outhandle,filename);
    rewrite(outhandle);
   end;
   if LazFileUtils.FileExistsUTF8(filename+'max')  then
   begin
    outfilex := True;
    assignfile(outhandlex,filename+'max');
    rewrite(outhandlex);
   end;
   if LazFileUtils.FileExistsUTF8(filename+'min')  then
   begin
    outfilen := True;
    assignfile(outhandlen,filename+'min');
    rewrite(outhandlen);
   end;
   if LazFileUtils.FileExistsUTF8(filename+'q')  then
   begin
    outfileq := True;
    assignfile(outhandleq,filename+'q');
    rewrite(outhandleq);
   end;
   if LazFileUtils.FileExistsUTF8(filename+'ymean')  then
   begin
    outfiley := True;
    assignfile(outhandley,filename+'ymean');
    rewrite(outhandley);
   end;
   CheckforOpenFiles := True;
  finally
   if outfile then CloseFile(outhandle);
   if outfilex then CloseFile(outhandlex);
   if outfilen then CloseFile(outhandlen);
   if outfileq then CloseFile(outhandleq);
   if outfiley then CloseFile(outhandley);
  end;
 except
  answer := MessageDlg('Output files are in use. Close all output files and THEN click OK.'
           + ' Click Cancel to abort the run.', mtWarning, [mbOK,mbCancel], 0);
  CheckforOpenFiles := False;
  if answer = 2 then raise;
 end;
end;

function WriteKalmanOutFile(filename:string; var outdata:TKstate; saveuncorrupted:Boolean):Boolean;
var
 outhandle, outhandlex, outhandlen, outhandleq, outhandley,
 outhandleunmean, outhandleuncorx, outhandleuncorn: textfile;
 i, j: integer;
begin
 WriteKalmanOutFile := false;
 assignfile(outhandle,filename);   
 assignfile(outhandlex,filename+'max');
 assignfile(outhandlen,filename+'min');
 assignfile(outhandleq,filename+'q'); 
 assignfile(outhandley,filename+'Ymean');
 rewrite(outhandle);
 rewrite(outhandlex);
 rewrite(outhandlen);
 rewrite(outhandleq);  
 rewrite(outhandley);
 if saveuncorrupted then
  begin
   assignfile(outhandleunmean,filename+'uncor');
   assignfile(outhandleuncorx,filename+'uncormax');
   assignfile(outhandleuncorn,filename+'uncormin');
   rewrite(outhandleunmean);
   rewrite(outhandleuncorx);
   rewrite(outhandleuncorn);
  end;

 try
// write time
   write(outhandle, 'Time');
   write(outhandlex, 'Time');
   write(outhandlen, 'Time');
   write(outhandleq, 'Time');
   write(outhandley, 'Time');
// Write Variable Names
   for i := 0 to outdata.NumTotKalman - 1 do
      begin
         write(outhandle, ', ', outdata.names[i]);
         write(outhandlex, ', ', outdata.names[i]);
         write(outhandlen, ', ', outdata.names[i]);
         write(outhandleq, ', ', outdata.names[i]);
         write(outhandley, ', ', outdata.names[i]);
      end;
   writeln(outhandle); 
   writeln(outhandlex);
   writeln(outhandlen);
   writeln(outhandleq); 
   writeln(outhandley);

//write time units
   write(outhandle, ModelDef.timeunit);
   write(outhandlex, ModelDef.timeunit);
   write(outhandlen, ModelDef.timeunit);
   write(outhandleq, ModelDef.timeunit);
   write(outhandley, ModelDef.timeunit);
// Write Variable Units
   for i := 0 to outdata.NumTotKalman - 1 do
      begin
         write(outhandle, ', ', outdata.units[i]);
         write(outhandlex, ', ', outdata.units[i]);
         write(outhandlen, ', ', outdata.units[i]);
         write(outhandleq, ', ', outdata.units[i]);
         write(outhandley, ', ', outdata.units[i]);
      end;
   writeln(outhandle); 
   writeln(outhandlex);
   writeln(outhandlen);
   writeln(outhandleq); 
   writeln(outhandley);

// Write Data
   for j := 0 to outdata.NumObservations - 1 do
    begin
     write(outhandle, outdata.time[j]);
     write(outhandlex, outdata.time[j]);
     write(outhandlen, outdata.time[j]);
     write(outhandleq, outdata.time[j]);
     write(outhandley, outdata.time[j]);
     for i := 0 to outdata.NumTotKalman - 1 do
        begin
           write(outhandle, ', ', outdata.xmean[j,i]);
           write(outhandlex, ', ', outdata.xmax[j,i]);
           write(outhandlen, ', ', outdata.xmin[j,i]);
           write(outhandleq, ', ', outdata.Q[j,i]);
           write(outhandley, ', ', outdata.Ymean[j,i]);
        end;
     writeln(outhandle); 
     writeln(outhandlex);
     writeln(outhandlen);
     writeln(outhandleq);  
     writeln(outhandley);
    end;

   if saveuncorrupted then
    begin
     // column titles
      write(outhandleunmean, 'Time');
      write(outhandleuncorx, 'Time');
      write(outhandleuncorn, 'Time');
      for I := 0 to outdata.numtotkalman - 1 do
        begin
         write(outhandleunmean, ', ', outdata.names[i]);
         write(outhandleuncorx, ', ', outdata.names[i]);
         write(outhandleuncorn, ', ', outdata.names[i]);
        end;
      writeln(outhandleunmean);
      writeln(outhandleuncorx);
      writeln(outhandleuncorn);

     // column units
      write(outhandleunmean, ModelDef.timeunit);
      write(outhandleuncorx, ModelDef.timeunit);
      write(outhandleuncorn, ModelDef.timeunit);
      for i := 0 to outdata.NumTotKalman - 1 do
        begin
         write(outhandleunmean, ', ', outdata.units[i]);
         write(outhandleuncorx, ', ', outdata.units[i]);
         write(outhandleuncorn, ', ', outdata.units[i]);
        end;
      writeln(outhandleunmean);
      writeln(outhandleuncorx);
      writeln(outhandleuncorn);

     // output data
     for j := 0 to outdata.NumObservations - 1 do
      begin
       write(outhandleunmean, outdata.time[j]);
       write(outhandleuncorx, outdata.time[j]);
       write(outhandleuncorn, outdata.time[j]);
       for i := 0 to outdata.NumTotKalman - 1 do
        begin
           write(outhandleunmean, ', ', outdata.UnCorXmean[j,i]);
           write(outhandleuncorx, ', ', outdata.UncorXmax[j,i]);
           write(outhandleuncorn, ', ', outdata.UncorXmin[j,i]);
        end;
       writeln(outhandleunmean);
       writeln(outhandleuncorx);
       writeln(outhandleuncorn);
      end;
    end;
   
   WriteKalmanOutFile := true;
 finally
   CloseFile(outhandle); 
   CloseFile(outhandlex);
   CloseFile(outhandlen);
   CloseFile(outhandleq);  
   CloseFile(outhandley);
   if saveuncorrupted then
    begin
      CloseFile(outhandleunmean);
      CloseFile(outhandleuncorx);
      CloseFile(outhandleuncorn);
    end;
 end;

end;

// Sensitivity Output File I/O

{ The sensitivity output file is a comma delimited ASCII file containing the
  results of the sensitivity analysis. Each line represents one sensitivity
  test and contains the parameter file name, the parameter to vary, the new
  value for the parameter and residuals for leaf, wood and root carbon and
  nitrogen.

  The sensitivity output file must be opened and closed using OpenSensOutFile
  and CloseSensOutFile. Be sure to call CloseSensOutFile when you are done.}

{ Opens the sensitivity output file and writes out column headings. }
procedure OpenSensOutFile(filename:string);
var
tempstring:string;
begin
 tempstring := 'Parameter File, Parameter, New Value, Test Status, ResidCL,' +
     ' ResidCW, ResidCR, ResidNL, ResidNW, ResidNR, ResidCT, ResidNT';
 assignfile(sensoutfile,filename);
 rewrite(sensoutfile);
 writeln(sensoutfile,tempstring);
end;

{ Write a line to the sens output file. }
function  WriteSensOutFile(var paramfilename, paramname:string;
             var newparamvalue:double; TestStatus:string; ResidCL, ResidCW,
             ResidCR, ResidNL, ResidNW, ResidNR, ResidCT, ResidNT:
             double):Boolean;
var
 tempstring: string;
begin
 tempstring := paramfilename + ',' + paramname + ',' + floattostr(newparamvalue)
    + ',' + TestStatus + ',' + floattostr(ResidCL) + ',' + floattostr(ResidCW) +
     ',' + floattostr(ResidCR) + ',' + floattostr(ResidNL) + ',' +
    floattostr(ResidNW) + ',' + floattostr(ResidNR) + ',' + floattostr(ResidCT)
    + ',' + floattostr(ResidNT);
 writeln(sensoutfile,tempstring);
 WriteSensOutFile := True;
end;

{ Closes the Sensitivity output file. }
procedure CloseSensOutFile;
begin
 closefile(SensOutFile);
end;

procedure ChangeExtension(var filename:string; NewExt:string);
var
 num:integer;
begin
 num := pos('.',filename);
 delete(filename,num,length(filename)-num+1);
 filename := filename + '.' + NewExt;
end;

procedure RemoveSpaces(var somestring:string);
var
 num: integer;
begin
 num := Pos(' ', somestring);
 while num > 0 do
   begin
    delete(somestring,num,1);
    num := Pos(' ', somestring);
   end;
end;

{ Reads in the model definition file to create a new model. }
procedure  ReadModelDef(filename:string; var tempmodeldef:Tmodeldef;
     var tstat:statearray; var tpar:paramarray; var tproc:processarray;
     var tdrive:drivearray);
var
 modelfile:textfile;
 i,j,npar:integer;
 tempstring, tname, tunits, tsymbol, tnpar:string;

procedure SetString(var tfile:textfile; var tstring:string);
var
 num:integer;
begin
 readln(tfile,tstring);
 num := pos('=',tstring);
 delete(tstring,1,num);
end;

procedure SetNUS(var tfile:textfile; var tstring, tname, tunits,
                                            tsymbol:string);
var
 num:integer;
begin
 readln(tfile,tstring);
 num := pos(',',tstring);
 tname := copy(tstring,1,num-1);
 delete(tstring,1,num);
 num := pos(',',tstring);
 tunits := copy(tstring,1,num-1);
 delete(tstring,1,num);
 num := pos(',',tstring);
 if num = 0 then
  begin
   num := length(tstring);
   tsymbol := copy(tstring,1,num);
  end
 else
  begin
   tsymbol := copy(tstring,1,num-1);
   delete(tstring,1,num);
  end;
end;

procedure SetPD(var tstring, tnpar:string);
var
 num:integer;
begin
 num := pos(',',tstring);
 tnpar := copy(tstring,1,num-1);
 delete(tstring,1,num);
// num := length(tstring);
end;

begin
try try
 assignfile(modelfile,filename);
 reset(modelfile);
 readln(modelfile,tempstring);  // Throw away the first line of the file
// ModelDef
 SetString(modelfile,tempstring);
 tempmodeldef.modelname := tempstring;
 SetString(modelfile,tempstring);
 tempmodeldef.versionnumber := tempstring;
 SetString(modelfile,tempstring);
 tempmodeldef.timeunit := tempstring;
 readln(modelfile, tempstring);    // Throw away blank line

// States
 SetString(modelfile,tempstring);
 tempmodeldef.numstate := strtoint(tempstring);
 readln(modelfile,tempstring);  // Throw away comment line
 for i := 1 to tempmodeldef.numstate do
  begin
   SetNUS(modelfile, tempstring, tname, tunits, tsymbol);
   tstat[i].name := tname;
   tstat[i].units := tunits;
   tstat[i].symbol := tsymbol;
  end;
 readln(modelfile, tempstring);    // Throw away blank line

// Processes
 SetString(modelfile,tempstring);
 tempmodeldef.numprocess := tempModelDef.numstate + strtoint(tempstring);
 readln(modelfile,tempstring);  // Throw away comment line
 for i := tempmodeldef.numstate + 1 to tempmodeldef.numprocess do
  begin
   SetNUS(modelfile, tempstring, tname, tunits, tsymbol);
   tproc[i].name := tname;
   tproc[i].units := tunits;
   tproc[i].symbol := tsymbol;
   SetPD(tempstring, tnpar);
   tproc[i].parameters := strtoint(tnpar);
   if tproc[i].parameters <> 0 then
   begin
    npar := ParCount(i);
    for j := 1 to tproc[i].parameters do
     begin
      SetNUS(modelfile, tempstring, tname, tunits, tsymbol);
      tpar[npar + j].name := tname;
      tpar[npar + j].units := tunits;
      tpar[npar + j].symbol := tsymbol;
     end;
   end;
  end;
 readln(modelfile, tempstring);    // Throw away blank line

// Drivers
 SetString(modelfile,tempstring);
 tempmodeldef.numdrive := strtoint(tempstring);
 readln(modelfile,tempstring);  // Throw away comment line
 for i := 1 to tempmodeldef.numstate do
  begin
   SetNUS(modelfile, tempstring, tname, tunits, tsymbol);
   tdrive[i].name := tname;
   tdrive[i].units := tunits;
   tdrive[i].symbol := tsymbol;
  end;

 { Set the names, units, and symbols of the processes and the parameters. The
  maximum length for the name, units and symbol is stringlength (currently 25)
  characters.

  The first numstate processes are the derivatives of the state variables. Set
  the names and units accordingly.}
 for i:= 1 to tempmodeldef.numstate do tproc[i].name:='d'+tstat[i].name+'dt';
 for i:= 1 to tempmodeldef.numstate do tproc[i].units := tstat[i].units + 't-1';
 for i:= 1 to tempmodeldef.numstate do tproc[i].symbol := 'd' + tstat[i].symbol + 'dt';

{ Sum up the total number of parameters in the model and store it in the
  tempmodeldef structure. }
 tempmodeldef.numparam := 0;
 for i := 1 to tempmodeldef.NumProcess do
  tempmodeldef.numparam := tempmodeldef.numparam + tproc[i].parameters;

finally
 CloseFile(modelfile);
end;
except
 raise EFileError.Create('Error reading Model Definition file. Application Terminated');
end;
end;

{This function counts the parameters in all processes less than processnum.}
function ParCount(processnum:integer) : integer;
var
 NumberofParams, counter : integer;
begin
  NumberofParams := 0;
  for counter := ModelDef.numstate + 1 to processnum - 1 do
         NumberofParams := NumberofParams + proc[counter].parameters;
  ParCount := NumberofParams;
end; // end of parcount function

end.
