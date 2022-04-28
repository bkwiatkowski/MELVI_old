unit Crmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  {$ifdef Darwin}
    MacOSAll,
  {$endif}
  ExtCtrls, ComCtrls, stypes;

const
  maxArrays = 150;
  DefaultUnixDirectory = '/user/share/CrEquations';
  BundleResourceDirectory = '/Contents/Resources/';
  maxParperArray = 20;

type
  TPart = (stBeginning, stEnd);
  TVarInfo = record
    vType: TVarType;
    IsArray: Boolean;
    Name: string;
    Units: string;
    Symbol: string;
    numParam: integer;
    numRow: integer;
    numCol: integer;
  end;

  TMatrixInfo = record
    VarName: string;
    numRow: integer;
    Numcol: integer;
    NumUniqueParam: integer;
  end;
  TArrayList = array[1..MaxArrays] of Tmatrixinfo;

  { TFmMain }

  TFmMain = class(TForm)
    BtnCreateCode: TButton;
    BtnClose: TButton;
    HeaderControl1: THeaderControl;
    LblEdFilename: TLabeledEdit;
    MmInstruct: TMemo;
    DlgOpenModelDef: TOpenDialog;
    DlgOpenEqPas: TOpenDialog;
    StatusBar1: TStatusBar;
    procedure ChooseDefFile(Sender: TObject);
    procedure BtnCreatecodeClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function IsDigit(astring:string; index:integer): Boolean;
  private
    { private declarations }
    fModelPath:string;
    fResourcePath:string;
    FequationsFileName:string;
    ftempmodeldef:Tmodeldef;
    ftempstate:statearray;
    ftempprocess:processarray;
    ftempdrive:drivearray;
    ftemppar:paramarray;
    fnumOtherDouble, numOtherInt, numBoolean:integer;
    ftempotherDouble, tempotherInt:paramarray;
    ftempBoolean: paramarray;
    fModelDefFilename:String;
    farrays: TArrayList;
    fTotalNumArrays: integer;
    fvarlist:Tstringlist;
    fModelDef:Tstringlist;
    fEquations:Tstringlist;
    procedure DefModel;
    procedure DefStates;
    procedure DefProcess;
    procedure DefDrive;
    procedure DefOtherVar;
    procedure DefBooleanVar;
    procedure GetFunctions;
    procedure CreateProcessesProc;
    function LineSearch(AstringList: Tstringlist; Sstring:string):integer;
    function DivideString(fromString, divider:string; WhichPart:TPart):string;
    function ParseString(acsvstring: string; vtype: TVarType): TVarInfo;
    //    procedure ParseString(var tstring, tname, tunits, tsymbol: string;
//               var tnpar:integer; var tptype:processtype; var tnrow, tncol: integer);
    function ParCount(processnum:integer):integer;
    function GetArrayNum(varname:string): integer;
    function CheckDerivatives(fline: integer): integer;
    function RemoveTrailingComma(lnum: integer): integer;
    procedure RemoveFinalDouble;
    function RemoveArrayIndex(tstring: string): string;
    function GetResourcePath(): string;
  public
    { public declarations }
  end; 

var
  FmMain: TFmMain;

implementation

{$R *.lfm}

{ TFmMain }

{ Select the model description file }
procedure TFmMain.ChooseDefFile(Sender: TObject);
begin
 // If the user typed directly in the box
 if Sender is TEdit then fModelDefFilename := LblEdFilename.Caption  // Set filename
 else // The user clicked either the menu or the label
   begin  // Show the open file dialog
           // First set the dialog box default filename to the current file
      DlgOpenModelDef.filename := fModelDefFilename;
          // If the user chooses OK in the dialog then set the new paramfilename
      if DlgOpenModelDef.execute then fModelDefFilename := DlgOpenModelDef.filename;
   end;
 LblEdFilename.Caption := fModelDefFilename;
 fModelPath := ExtractFilePath(fModelDefFilename);
 if fModelDefFilename <> '' then BtnCreateCode.Enabled := True;
end;

procedure TFmMain.BtnCreatecodeClick(Sender: TObject);
var
  fs: TFileStream;
begin
 fvarlist.Clear;
 fModelDef:=Tstringlist.Create;
 fEquations:=Tstringlist.Create;
 try
 if fModelDefFilename <> '' then
  begin
   try
    fs := nil;
    BtnCreateCode.Enabled := False;
    BtnCreateCode.Caption := 'Running'; { TODO 3 -oBK -icicing : Why does the button caption not change on the Mac? }
    if not FileExists(fEquationsFilename) then
     repeat
      MessageDlg('A required file, equationsblank.pas, is not in the expected ' +
        'location. Please locate the file using the dialog box.', mtError, [mbOK],0);
      if DlgOpenEqPas.Execute then fEquationsFilename := DlgOpenEqPas.FileName;
     until FileExists(fEquationsFilename);

// Load the equations.pas file using the TStringList.LoadfromFile procedure because it's a plain text file
// and using the LoadRichText procedure of RichMemo only reads the comments at the beginning of the file.
//    MmEquations.Clear;
//    MmEquations.Lines.LoadFromFile(fEquationsFilename);
    fEquations.LoadFromFile(fEquationsFilename);

//    MmModelDef.Clear;
//    fModelDef.LoadFromFile(fModelDefFilename);
    fModelDef.LoadFromFile(fModelDefFilename);

    fTotalNumArrays := 1;
    DefModel;
    DefStates;
    DefProcess;
    DefDrive;
    DefOtherVar;
    CreateProcessesProc;
//    MmEquations.Lines.SavetoFile(fModelPath + 'equations.pas');
    fEquations.SaveToFile(fModelPath + 'equations.pas');
    BtnCreateCode.Enabled := True;
    BtnCreateCode.Caption := '&Create code';
    MessageDlg('Your model is ready to compile. Using Windows Explorer, find '
       + 'and double click on the file modelshell6543.lpi, your model will open '
       + 'inside Lazarus. Click on the green triangle in the menu bar to run '
       + 'your model.', mtInformation, [mbOK], 0)
   except
    raise;
{    on EStringListError do
      if E.Message = 'String list does not allow duplicates' then
         MessageDlg('Duplicate variable names. Translation failed. Ensure that state, process and driver  ' +
         'variable names differ in the first 25 characters and try again. ', mtError, [mbOK], 0);
      if E.Message = 'Index out of bounds' then
         Messag  }
   end;
  end
 else
  MessageDlg('Invalid Model Definition File. Please reenter',
              mtWarning, [mbOK], 0);
 finally
  if assigned(fModelDef) then FreeandNil(fModelDef);
  if assigned(fEquations) then FreeandNil(fEquations);
 end;
end;

{ Determines the model name, version, and time unit based on the file read into
  the ReModelDef memo component. The Model name, version and time unit are
  copied to the  RMmEquations memo and to the tempModelDef record. }
procedure TFmMain.DefModel;
var
 tempstring:string;
 fromLineNum, toLineNum:integer;
begin
 fromLineNum := LineSearch(fModelDef,'Model Name');
 ftempmodeldef.modelname :=DivideString(fModelDef[fromLineNum],'=',stEnd);
 if length(ftempmodeldef.modelname) > stringlength then
    raise Exception.Create('Model name is too long. Decrease model name to 25 characters or less and rerun Create Equations.');

 tempstring := 'ModelDef.modelname := ''' + ftempmodeldef.modelname + ''';';
 toLineNum := LineSearch(fEquations,'ModelDef.modelname :=');
 fEquations[toLineNum] := tempstring;

 fromLineNum := LineSearch(fModelDef,'Model Version');
 ftempmodeldef.versionnumber :=
                          DivideString(fModelDef[fromLineNum],'=',stEnd);
 tempstring := 'ModelDef.versionnumber := ''' + ftempmodeldef.versionnumber
                  + ''';';
 toLineNum := LineSearch(fEquations,'ModelDef.versionnumber :=');
 fEquations[toLineNum] := tempstring;

 fromLineNum := LineSearch(fModelDef,'Model Time Unit');
 ftempmodeldef.timeunit := DivideString(fModelDef[fromLineNum],'=',stEnd);
 tempstring := 'ModelDef.timeunit := ''' + ftempmodeldef.timeunit + ''';';
 toLineNum := LineSearch(fEquations,'ModelDef.timeunit :=');
 fEquations[toLineNum] := tempstring;

 fromLineNum := LineSearch(fModelDef,'Model Contact');
 ftempmodeldef.contactperson :=DivideString(fModelDef[fromLineNum],'=',stEnd);
 tempstring := 'ModelDef.contactperson := ''' + ftempmodeldef.contactperson + ''';';
 toLineNum := LineSearch(fEquations,'ModelDef.contactperson :=');
 fEquations[toLineNum] := tempstring;

 fromLineNum := LineSearch(fModelDef,'Model Contact Address Line 1');
 ftempmodeldef.contactaddress1 :=DivideString(fModelDef[fromLineNum],'=',stEnd);
 tempstring := 'ModelDef.contactaddress1 := ''' + ftempmodeldef.contactaddress1 + ''';';
 toLineNum := LineSearch(fEquations,'ModelDef.contactaddress1 :=');
 fEquations[toLineNum] := tempstring;

 fromLineNum := LineSearch(fModelDef,'Model Contact Address Line 2');
 ftempmodeldef.contactaddress2 :=DivideString(fModelDef[fromLineNum],'=',stEnd);
 tempstring := 'ModelDef.contactaddress2 := ''' + ftempmodeldef.contactaddress2 + ''';';
 toLineNum := LineSearch(fEquations,'ModelDef.contactaddress2 :=');
 fEquations[toLineNum] := tempstring;

 fromLineNum := LineSearch(fModelDef,'Model Contact Address Line 3');
 ftempmodeldef.contactaddress3 :=DivideString(fModelDef[fromLineNum],'=',stEnd);
 tempstring := 'ModelDef.contactaddress3 := ''' + ftempmodeldef.contactaddress3 + ''';';
 toLineNum := LineSearch(fEquations,'ModelDef.contactaddress3 :=');
 fEquations[toLineNum] := tempstring;
end;

{ Defines the state variables of the model based on the file read into the
  fModelDef memo.  }
procedure TFmMain.DefStates;
var
 i, j, m, fromline, toline:integer;
 tempstring: string;
 avar: tvarinfo;
// tname, tunits, tsymbol: Tshortstring;
begin
// Define the state variables
 fromline := LineSearch(fModelDef, 'States');
 // Advance 1 lines in the memo to get to the line above the state variable listing
 fromline := fromline + 2;
 tempstring := fModelDef[fromline];
 repeat
   tempstring := fModelDef[fromline];
   tempstring := trim(tempstring);
   fromline := fromline + 1;
 until tempstring <> '';
 i := 1;
 while tempstring <> 'End' do
   begin
     if i > maxstate then
       raise Exception.Create('Too many state variables. Increase maxstate in stypes.');
       avar:=ParseString(tempstring,vtstate);
  //   ParseString(tempstring, tsname, tsunits, tssymbol, tnpar, tptype, tnrow, tncol);
     fvarlist.Add(avar.name);
     if avar.name[1] <> '*' then       // Not an array variable
       begin
         ftempstate[i].name := avar.Name;
         ftempstate[i].units := avar.Units;
         ftempstate[i].symbol := avar.Symbol;
         i := i + 1;
       end
     else                         // An array or matrix variable
       begin
          if fTotalNumArrays > MaxArrays then
            raise Exception.Create('Too many arrays. Increase MaxArrays.');
          if avar.numCol = 0 then      // An array
            begin
              for j := 1 to avar.numRow do
               begin
                ftempstate[i + j - 1].name := avar.Name + inttostr(j);
                ftempstate[i + j - 1].units := avar.Units;
                ftempstate[i + j - 1].symbol := avar.Symbol + '[' + inttostr(j) + ']';
               end;
              i := i + avar.numRow;
            end
          else                   // A matrix
            begin
             for j := 1 to avar.numRow do
              for m := 1 to avar.numCol do
                begin
                 ftempstate[i + (j-1)*avar.numCol + m - 1].name := avar.Name + inttostr(j) + '-' + inttostr(m);
                 ftempstate[i + (j-1)*avar.numCol + m - 1].units := avar.Units;
                 ftempstate[i + (j-1)*avar.numCol + m - 1].symbol := avar.Symbol + '[' + inttostr(j) + '-' + inttostr(m) + ']';
                end;
             i := i + avar.numRow*avar.numCol;
            end;
         farrays[fTotalNumArrays].VarName := avar.Name; // Used in createprocessesproc procedure
         farrays[fTotalNumArrays].numRow := avar.numRow;
         farrays[fTotalNumArrays].Numcol := avar.numCol;
         fTotalNumArrays := fTotalNumArrays + 1;
        end;
     repeat
      tempstring := fModelDef[fromline];
      tempstring := trim(tempstring);
      fromline := fromline + 1;
     until tempstring <> '';
   end;
 ftempmodeldef.numstate := i - 1;     // Need ftempmodeldef for the equations

// Update the fEquations memo
 toline := LineSearch(fEquations,'ModelDef.numstate :=');
 tempstring := 'ModelDef.numstate := ' + inttostr(ftempmodeldef.numstate) + ';';
 fEquations[toline] := tempstring;

 toline := LineSearch(fEquations,'with stat[1] do');
 for i := 1 to 4 do fEquations.Delete(toline); // Delete the example code

 i := 1;
 repeat
   fEquations.Insert(toline, ' ');
   fEquations.Insert(toline + 1,'with stat[' + inttostr(i) + '] do');
   fEquations.Insert(toline + 2, ' begin');
   tempstring := '    name:=''' + ftempstate[i].name + ''';  '
                     + 'units:=''' + ftempstate[i].units + ''';  '
                     + 'symbol:=''' + ftempstate[i].symbol + ''';';
   fEquations.Insert(toline + 3, tempstring);
   fEquations.Insert(toline + 4, ' end;');
   toline := toline + 5;
   i := i + 1;
 until i = ftempmodeldef.numstate + 1;
end;

{ Defines the processes and parameters of the model based on the file read into
  the fModelDef memo. }
procedure TFmMain.DefProcess;
var
 iproc, j, k, m, mm, num, fromline, toline, numprevpar, arraynum,
     ipcol, iprow, ipar:integer;
 tempstring, tempstring2, statename: string;
 avar, apar: Tvarinfo;
 holdprocess: Tprocessvariable;
 holdparam: array[1..maxParperArray] of Tparamvariable;
begin
 { The first numstate processes are the derivatives of the state variables.
   These processes aren't used in this procedure but are used in the
   CreateProcessesProc. }
 iproc := 1;
 while iproc <= ftempmodeldef.numstate do
 //for iproc:= 1 to ftempmodeldef.numstate do
  begin
   if ftempstate[iproc].name[1] <> '*' then
    begin
      ftempprocess[iproc].name := 'd' + ftempstate[iproc].name + 'dt';
      ftempprocess[iproc].symbol := 'd' + ftempstate[iproc].symbol + 'dt';
      ftempprocess[iproc].units := ftempstate[iproc].units + ' t-1';
      iproc:=iproc+1;
    end
   else    // An array or matrix
    begin
      arraynum := GetArrayNum(ftempstate[iproc].name);
      statename := farrays[arraynum].VarName;
      delete(statename,1,1); // Remove *
      if farrays[arraynum].numcol = 0 then        // An array
       begin
        for j := 1 to farrays[arraynum].numrow do
         begin
          ftempprocess[iproc + j - 1].name := '*d' + statename + 'dt' + inttostr(j);
          tempstring := ftempstate[iproc + j -1].symbol;
          num := pos('[', tempstring);
          tempstring2 := copy(tempstring, num, length(tempstring) - num + 1);
          delete(tempstring, num, length(tempstring) - num + 1);
          ftempprocess[iproc + j - 1].symbol := 'd' + tempstring + 'dt' + tempstring2;
          ftempprocess[iproc + j -1].units := ftempstate[iproc].units + ' t-1';
         end;
        iproc := iproc + farrays[arraynum].numrow;
       end
      else                                      // A matrix
       begin
        for j := 1 to farrays[arraynum].numrow do
         for k := 1 to farrays[arraynum].numcol do
          begin
           ftempprocess[iproc+(j-1)*farrays[arraynum].Numcol+k-1].name := '*d' + statename + 'dt' + inttostr(j) + '-' + inttostr(k);
           tempstring := ftempstate[iproc+(j-1)*farrays[arraynum].Numcol+k-1].symbol;
           num := pos('[', tempstring);
           tempstring2 := copy(tempstring, num, length(tempstring) - num + 1);
           delete(tempstring, num, length(tempstring) - num + 1);
           ftempprocess[iproc+(j-1)*farrays[arraynum].Numcol+k-1].symbol := 'd' + tempstring + 'dt' + tempstring2;
           ftempprocess[iproc+(j-1)*farrays[arraynum].Numcol+k-1].units := ftempstate[iproc].units + ' t-1';
          end;
        iproc := iproc + farrays[arraynum].numrow*farrays[arraynum].numcol;
       end;
     end;
  end;

 // Define the processes
 fromline := LineSearch(fModelDef, 'Process');
 // Advance 3 lines in the memo to get from numprocess to the line above the variable listing.
 fromline := fromline + 3;
 repeat
   tempstring := fModelDef[fromline];
   tempstring := trim(tempstring);
   fromline := fromline + 1;
 until tempstring <> '';
 iproc := ftempmodeldef.numstate + 1;      // first process
 while tempstring <> 'End' do
 begin
  if iproc > maxprocess then
     raise Exception.Create('Too many processes. Increase maxprocess in stypes.');
  avar:=ParseString(tempstring,vtprocess);
  //ParseString(tempstring, tprocname, tprocunits, tprocsymbol, tnpar, tptype, tnrow, tncol);
  fvarlist.Add(avar.Name);
  if avar.numRow = 0  then        // Not an array or matrix variable
    begin
      ftempprocess[iproc].name := avar.Name;
      ftempprocess[iproc].units := avar.Units;
      ftempprocess[iproc].symbol := avar.Symbol;
      ftempprocess[iproc].parameters := avar.numParam;
      numprevpar := ParCount(iproc);
      if numprevpar + avar.numParam > maxparam then
          raise Exception.Create('Too many parameters. Increase maxparam in stypes.');
      for j := 1 to avar.numParam do
        begin
          repeat
            tempstring := fModelDef[fromline];
            tempstring := trim(tempstring);
            fromline := fromline + 1;
          until tempstring <> '';
          apar:=ParseString(tempstring,vtParameter);
    //      ParseString(tempstring, tparname, tparunits, tparsymbol, num, tptype, num, num);
          ftemppar[numprevpar + j].name := apar.Name;
          ftemppar[numprevpar + j].units := apar.Units;
          ftemppar[numprevpar + j].symbol := apar.Symbol;
        end;
      iproc := iproc + 1;
    end
  else                          // An array or matrix variable
    begin
     if avar.numCol = 0 then       // An array
      begin
      for k := 1 to avar.numRow do
       begin
        if k = 1 then
         begin
          ftempprocess[iproc + k - 1].name := avar.Name;
          ftempprocess[iproc + k - 1].units := avar.Units;
          ftempprocess[iproc + k - 1].symbol := avar.Symbol;
          ftempprocess[iproc + k - 1].parameters := 0;  // Actual number of parameters will be calculated in param loop
          holdprocess := ftempprocess[iproc + k - 1];
          ftempprocess[iproc + k - 1].name := avar.Name + inttostr(k);
          ftempprocess[iproc + k - 1].symbol := avar.Symbol + '[' + inttostr(k) + ']';
          numprevpar := ParCount(iproc + k - 1);
          farrays[fTotalNumArrays].VarName := avar.Name; // Used in createprocessesproc procedure
          farrays[fTotalNumArrays].numRow := avar.numRow;
          farrays[fTotalNumArrays].numCol := avar.numCol; // To make sure it isn't undefined.
          farrays[fTotalNumArrays].NumUniqueParam := avar.numParam;
          fTotalNumArrays := fTotalNumArrays + 1;
          if avar.numParam > maxParperArray then  // To change this value, change the variable declaration for holdparam
            raise exception.Create('Process, ' +
              ftempprocess[iproc + k - 1].name + ', has too many parameters.');
          if numprevpar + avar.numParam*avar.numRow > maxparam then       // Fix to account for matrices
                raise Exception.Create('Too many parameters. Increase maxparam in stypes.');
          for ipar := 1 to avar.numParam do
            begin
              repeat
                tempstring := fModelDef[fromline];
                tempstring := trim(tempstring);
                fromline := fromline + 1;
              until tempstring <> '';
              apar:=ParseString(tempstring,vtParameter);
//              ParseString(tempstring, tparname, tparunits, tparsymbol, num, tptype, num, num);
              holdparam[ipar].name:=apar.Name;
              holdparam[ipar].units:=apar.Units;
              holdparam[ipar].symbol:=apar.Symbol;
              for iprow:=1 to avar.numRow do
                begin
                  ftemppar[numprevpar+(ipar-1)*avar.numRow+iprow].name :=
                      holdparam[ipar].name + inttostr(iprow);
                  ftemppar[numprevpar+(ipar-1)*avar.numRow+iprow].units :=
                      holdparam[ipar].units;
                  ftemppar[numprevpar+(ipar-1)*avar.numRow+iprow].symbol :=
                      holdparam[ipar].symbol + '[' + inttostr(iprow) + ']';
                end;
              ftempprocess[iproc].parameters := ftempprocess[iproc].parameters + avar.numRow;
            end;
         end
        else // Not the first process of an array so use saved info instead of reading from file
         begin
          ftempprocess[iproc + k - 1] := holdprocess;
          ftempprocess[iproc + k - 1].name := holdprocess.name + inttostr(k);
          ftempprocess[iproc + k - 1].symbol := holdprocess.symbol + '[' + inttostr(k) + ']';
          ftempprocess[iproc + k - 1].parameters := 0;
         end;
       end;
       iproc := iproc + avar.numRow;
       if fTotalNumArrays > MaxArrays then
          raise Exception.Create('Too many arrays. Increase MaxArrays.');
      end
     else      // A matrix
      begin
       for mm := 1 to avar.numCol do
       for k := 1 to avar.numRow do
        begin
        if (k = 1) and (mm = 1) then    // First element of process matrix
         begin
          ftempprocess[iproc+(k-1)*avar.numCol+mm-1].name := avar.Name;
          ftempprocess[iproc+(k-1)*avar.numCol+mm-1].units := avar.Units;
          ftempprocess[iproc+(k-1)*avar.numCol+mm-1].symbol := avar.Symbol;
          ftempprocess[iproc+(k-1)*avar.numCol+mm-1].parameters := avar.numRow;  // Necessary for holdprocess and so that numprevpar gets calculated correctly
          holdprocess := ftempprocess[iproc+(k-1)*avar.numCol+mm-1];
          ftempprocess[iproc+(k-1)*avar.numCol+mm-1].name := avar.Name + inttostr(k) + '-' + inttostr(mm);
          ftempprocess[iproc+(k-1)*avar.numCol+mm-1].symbol := avar.Symbol + '[' + inttostr(k) + '-' + inttostr(mm) + ']';
          numprevpar := ParCount(iproc+(k-1)*avar.numCol+mm-1);
          farrays[fTotalNumArrays].VarName := avar.Name; // Used in createprocessesproc procedure
          farrays[fTotalNumArrays].numRow := avar.numRow;
          farrays[fTotalNumArrays].NumCol := avar.numCol;
          farrays[fTotalNumArrays].NumUniqueParam := avar.numParam;
          fTotalNumArrays := fTotalNumArrays + 1;    // Fix, why mm and not 1???  Changed to 1 because that makes more sense
          if avar.numParam > maxParperArray then  // To change this value, change the variable declaration for holdparam
            raise exception.Create('Process, ' +
              ftempprocess[iproc+(k-1)*avar.numCol+mm-1].name + ', has too many parameters.');
          if numprevpar + avar.numParam*avar.numRow*avar.numCol > maxparam then
                raise Exception.Create('Too many parameters. Increase maxparam in stypes.');
          ftempprocess[iproc].parameters := 0;
          for ipar := 1 to avar.numParam do
            begin
              repeat
                tempstring := fModelDef[fromline];
                tempstring := trim(tempstring);
                fromline := fromline + 1;
              until tempstring <> '';
              apar:=ParseString(tempstring,vtParameter);
 //             ParseString(tempstring, tparname, tparunits, tparsymbol, num, tptype, num, num);
              holdparam[ipar].name:=apar.Name;
              holdparam[ipar].units:=apar.Units;
              holdparam[ipar].symbol:=apar.Symbol;
              for iprow:=1 to avar.numRow do
               for ipcol:=1 to avar.numCol do
                begin
                  ftemppar[numprevpar+(ipar-1)*avar.numCol*avar.numRow+(iprow-1)*avar.numCol+ipcol].name :=
                      holdparam[ipar].name + inttostr(iprow) + '-' + inttostr(ipcol);
                  ftemppar[numprevpar+(ipar-1)*avar.numCol*avar.numRow+(iprow-1)*avar.numCol+ipcol].units :=
                      holdparam[ipar].units;
                  ftemppar[numprevpar+(ipar-1)*avar.numCol*avar.numRow+(iprow-1)*avar.numCol+ipcol].symbol :=
                      holdparam[ipar].symbol + '[' + inttostr(iprow) + '-' + inttostr(ipcol) + ']';
                end;
              ftempprocess[iproc].parameters := ftempprocess[iproc].parameters + avar.numRow*avar.numCol;
            end;
         end
        else // Not the first element of a matrix so use saved info instead of reading from file
         begin
          ftempprocess[iproc+(k-1)*avar.numCol+mm-1] := holdprocess;
          ftempprocess[iproc+(k-1)*avar.numCol+mm-1].name := holdprocess.name + inttostr(k) + '-' + inttostr(mm);
          ftempprocess[iproc+(k-1)*avar.numCol+mm-1].symbol := holdprocess.symbol + '[' + inttostr(k) + '-' +inttostr(mm)+ ']';
          ftempprocess[iproc+(k-1)*avar.numCol+mm-1].parameters := 0;
        end;
        end;
       iproc := iproc + avar.numRow*avar.numCol;
       if fTotalNumArrays > MaxArrays then
          raise Exception.Create('Too many arrays. Increase MaxArrays.');
      end;
    end;
  repeat
    tempstring := fModelDef[fromline];
    tempstring := trim(tempstring);
    fromline := fromline + 1;
  until tempstring <> '';
 end;
 ftempmodeldef.numprocess := iproc - 1;
 ftempmodeldef.numparam := ParCount(ftempmodeldef.numprocess);

// Update the fEquations memo
 toline := LineSearch(fEquations,'ModelDef.numprocess :=');
 tempstring := 'ModelDef.numprocess := ModelDef.numstate + ' +
           inttostr(ftempmodeldef.numprocess-ftempmodeldef.numstate) + ';';
 fEquations[toline] := tempstring;

 toline := LineSearch(fEquations,'CurrentProc := ModelDef.numstate');
 for j := 1 to 15 do fEquations.Delete(toline); // Delete example code.
 iproc := 1;
 repeat
   fEquations.Insert(toline, ' ');
   fEquations.Insert(toline + 1, 'CurrentProc := ModelDef.numstate + '
                                                   + inttostr(iproc) + ';');
   fEquations.Insert(toline + 2, 'With proc[CurrentProc] do');
   fEquations.Insert(toline + 3, '   begin');
   fEquations.Insert(toline + 4, '      name       := ''' +
               ftempprocess[ftempmodeldef.numstate + iproc].name + ''';');
   fEquations.Insert(toline + 5, '      units       := ''' +
               ftempprocess[ftempmodeldef.numstate + iproc].units + ''';');
   fEquations.Insert(toline + 6, '      symbol       := ''' +
               ftempprocess[ftempmodeldef.numstate + iproc].symbol + ''';');
   fEquations.Insert(toline + 7, '      parameters       := ' +
           inttostr(ftempprocess[ftempmodeldef.numstate + iproc].parameters) + ';');
   case ftempprocess[ftempmodeldef.numstate + iproc].ptype of
     ptGroup1: tempstring := 'ptGroup1';
     ptGroup2: tempstring := 'ptGroup2';
     ptGroup3: tempstring := 'ptGroup3';
     ptGroup4: tempstring := 'ptGroup4';
     ptGroup5: tempstring := 'ptGroup5';
   else
     tempstring := 'ptGroup1';
   end;
   fEquations.Insert(toline + 8, '      ptype       := ' + tempstring
     + ';');
   fEquations.Insert(toline + 9, '   end;');
   numprevpar := ParCount(ftempmodeldef.numstate + iproc);
   toline := toline + 10;
   if ftempprocess[ftempmodeldef.numstate + iproc].parameters > 0 then
     begin
      fEquations.Insert(toline, 'npar:=ParCount(CurrentProc);');
      for m := 1 to ftempprocess[ftempmodeldef.numstate + iproc].parameters do
       begin
         fEquations.Insert(toline + 1, 'with par[npar + ' +
                                                   inttostr(m) + '] do');
         fEquations.Insert(toline + 2, ' begin');
         tempstring := '    name:=''' + ftemppar[numprevpar + m].name + ''';  '
                         + 'units:=''' + ftemppar[numprevpar + m].units + ''';  '
                         + 'symbol:=''' + ftemppar[numprevpar + m].symbol + ''';';
         fEquations.Insert(toline + 3, tempstring);
         fEquations.Insert(toline + 4, ' end;');
         toline := toline + 4;
       end;
      toline := toline + 1;
     end;
   iproc := iproc + 1;
 until iproc = ftempmodeldef.numprocess - ftempmodeldef.numstate + 1;
end;

{ Defines the driver variables of the model based on the file read into
  the fModelDef memo. }
procedure TFmMain.DefDrive;
var
 i, j, m, fromline, toline:integer;
 tempstring: string;
 avar: TvarInfo;
begin
// Define the driver variables
 fromline := LineSearch(fModelDef, 'Drive');
 // Advance 2 lines in the memo to get to the line above the driver listing
 fromline := fromline + 2;
 repeat
   tempstring := fModelDef[fromline];
   tempstring := trim(tempstring);
   fromline := fromline + 1;
 until tempstring <> '';
 i := 1;
 while tempstring <> 'End' do
  begin
   if i > maxdrive then
      raise Exception.Create('Too many drivers. Increase maxdrive in stypes.');
   avar:=ParseString(tempstring,vtDriver);
  //   ParseString(tempstring, tname, tunits, tsymbol, tnpar, tptype, tnrow, tncol);
   fvarlist.Add(avar.Name);
   if avar.Name[1] <> '*' then     // Not an array variable
     begin
       ftempdrive[i].name := avar.Name;
       ftempdrive[i].units := avar.Units;
       ftempdrive[i].symbol := avar.Symbol;
       i := i + 1;
     end
   else                   // An array or matrix variable
     begin
      if avar.numCol=0 then        // Array
       begin
        for j := 1 to avar.numRow do
          begin
           ftempdrive[i + j - 1].name := avar.Name + inttostr(j);
           ftempdrive[i + j - 1].units := avar.Units;
           ftempdrive[i + j - 1].symbol := avar.Symbol + '[' + inttostr(j) + ']';
          end;
        farrays[fTotalNumArrays].VarName := avar.Name; // Used in createprocessesproc procedure
        farrays[fTotalNumArrays].numRow := avar.numRow;
        fTotalNumArrays := fTotalNumArrays + 1;
        if fTotalNumArrays > MaxArrays then
            raise Exception.Create('Too many arrays. Increase MaxArrays.');
        i := i + avar.numRow;
       end
      else                  // Matrix
       begin
        for j := 1 to avar.numRow do
         for m := 1 to avar.numCol do
          begin
           ftempdrive[i + (j-1)*avar.numCol + m - 1].name := avar.Name + inttostr(j) + '-' + inttostr(m);
           ftempdrive[i + (j-1)*avar.numCol + m  - 1].units := avar.Units;
           ftempdrive[i + (j-1)*avar.numCol + m  - 1].symbol := avar.Symbol + '[' + inttostr(j) + '-' + inttostr(m) + ']';
          end;
        farrays[fTotalNumArrays].VarName := avar.Name; // Used in createprocessesproc procedure
        farrays[fTotalNumArrays].numRow := avar.numRow;
        farrays[fTotalNumArrays].Numcol:= avar.numCol;
        fTotalNumArrays := fTotalNumArrays + 1;
        if fTotalNumArrays > MaxArrays then
            raise Exception.Create('Too many arrays. Increase MaxArrays.');
        i := i + avar.numRow*avar.numCol;
       end;
     end;
   repeat
     tempstring := fModelDef[fromline];
     tempstring := trim(tempstring);
     fromline := fromline + 1;
   until tempstring <> '';
  end;
 ftempmodeldef.numdrive := i - 1;     // Need ftempmodeldef for the equations

// Update the fEquations memo
 toline := LineSearch(fEquations,'ModelDef.numdrive :=');
 tempstring := 'ModelDef.numdrive := ' + inttostr(ftempmodeldef.numdrive) + ';';
 fEquations[toline] := tempstring;

 toline := LineSearch(fEquations,'with drive[1] do');
 for i := 1 to 4 do fEquations.Delete(toline); // Delete the example code

 for i := 1 to ftempmodeldef.numdrive do
   begin
     fEquations.Insert(toline, ' ');
     fEquations.Insert(toline + 1, 'with drive[' + inttostr(i) + '] do');
     fEquations.Insert(toline + 2, ' begin');
     tempstring := '    name:=''' + ftempdrive[i].name + ''';  '
                       + 'units:=''' + ftempdrive[i].units + ''';  '
                       + 'symbol:=''' + ftempdrive[i].symbol + ''';';
     fEquations.Insert(toline + 3, tempstring);
     fEquations.Insert(toline + 4, ' end;');
     toline := toline + 5;
   end;
end;

{ Defines the other variables (non output variables) of the model based on the file read into
  the fModelDef memo. }
procedure TFmMain.DefOtherVar;
var
 i, j, fromline:integer;
 tempstring: string;
 avar: TvarInfo;
begin
 fromline := LineSearch(fModelDef, 'Other Double Variables');
 // Advance 2 lines in the memo to get to the variable list
 fromline := fromline + 2;
 tempstring := fModelDef[fromline];
 repeat
   tempstring := fModelDef[fromline];
   tempstring := trim(tempstring);
   fromline := fromline + 1;
 until (tempstring <> '') or (fromline > fModelDef.Count);
 if fromline > fModelDef.Count then
   raise Exception.Create('Error - String "Other Double Variables" not found.');
 i := 1;
 while tempstring <> 'End' do
  begin
   avar:=ParseString(tempstring,vtOther);
   //   ParseString(tempstring, tname, tunits, tsymbol, tnpar, tptype, tnrow, tncol);
   if avar.Name[1] <> '*' then     // Not an array variable
     begin
       ftempotherDouble[i].name := avar.Name;
       ftempotherDouble[i].units := avar.Units;
       ftempotherDouble[i].symbol := avar.Symbol;
       i := i + 1;
     end
   else                   // An array variable
     begin
       for j := 1 to avar.numRow do
         begin
          ftempotherDouble[i + j - 1].name := avar.Name + inttostr(j);
          ftempotherDouble[i + j - 1].units := avar.Units;
          ftempotherDouble[i + j - 1].symbol := avar.Symbol;
         end;
       farrays[fTotalNumArrays].VarName := avar.Name; // Used in createprocessesproc procedure
       farrays[fTotalNumArrays].numRow := avar.numRow;
       fTotalNumArrays := fTotalNumArrays + 1;
       if fTotalNumArrays > MaxArrays then
           raise Exception.Create('Too many arrays. Increase MaxArrays.');
       i := i + avar.numRow;
     end;
   repeat
    tempstring := fModelDef[fromline];
    tempstring := trim(tempstring);
    fromline := fromline + 1;
   until tempstring <> '';
  end;
 fnumOtherDouble := i - 1;

 fromline := LineSearch(fModelDef, 'Other Integer Variables');
 // Advance 2 lines in the memo to get to the variable list
 fromline := fromline + 2;
 tempstring := fModelDef[fromline];
 repeat
   tempstring := fModelDef[fromline];
   tempstring := trim(tempstring);
   fromline := fromline + 1;
 until (tempstring <> '') or (fromline > fModelDef.Count);
 if fromline > fModelDef.Count then
   raise Exception.Create('Error - String "Other Integer Variables" not found.');
 i := 1;
 while tempstring <> 'End' do
  begin
   avar:=ParseString(tempstring,vtOther);
//   ParseString(tempstring, tname, tunits, tsymbol, tnpar, tptype, tnrow, tncol);
   if avar.Name[1] <> '*' then     // Not an array variable
     begin
       tempotherInt[i].name := avar.Name;
       tempotherInt[i].units := avar.Units;
       tempotherInt[i].symbol := avar.Symbol;
       i := i + 1;
     end
   else                   // An array variable
     begin
       for j := 1 to avar.numRow do
         begin
          tempotherInt[i + j - 1].name := avar.Name + inttostr(j);
          tempotherInt[i + j - 1].units := avar.Units;
          tempotherInt[i + j - 1].symbol := avar.Symbol;
         end;
       farrays[fTotalNumArrays].VarName := avar.Name; // Used in createprocessesproc procedure
       farrays[fTotalNumArrays].numRow := avar.numRow;
       fTotalNumArrays := fTotalNumArrays + 1;
       if fTotalNumArrays > MaxArrays then
           raise Exception.Create('Too many arrays. Increase MaxArrays.');
       i := i + avar.numRow;
     end;
   repeat
    tempstring := fModelDef[fromline];
    tempstring := trim(tempstring);
    fromline := fromline + 1;
   until tempstring <> '';
  end;
 numotherInt := i - 1;
end;

{ This procedure creates the processes procedure in equations.pas. First it
  defines the local variable names using the symbol stored in the arrays. Then
  the value in the global arrays is copied into the local variables. And then
  the model equations are copied from the fModelDef memo. Finally the values
  of the processes are copied from the local variables to the global variables.
    }
procedure TFmMain.CreateProcessesProc;
var
 toline, fromline1, fromline2, i, j, numprevpar, numemptylines, arraynum,
   linecount, lastline, num, posrandom, numtotpar, ipar:integer;
 tempstring, tempstring2, astring:string;
 WithinCalcDiscrete: Boolean;
begin
// Define the local variable names

 // State variables and their derivatives.
  toline := LineSearch(fEquations,'{States}');
 // fEquations.ma
  fEquations.Delete(toline + 1);   // Delete the example.
  i := 1;
  linecount := 1;
  lastline := toline + linecount;
  repeat
    if ftempstate[i].name[1] <> '*' then     // Not an array variable
      begin
        fEquations.Insert(toline + linecount, ftempstate[i].symbol + ', '
                           + ftempprocess[i].symbol + ', ');
        i := i + 1;
        lastline := toline + linecount;
      end
    else    // An array variable
      begin    // Remove comma and set var type to double before adding array.
        if (i <> 1) and (ftempstate[i-1].name[1] <> '*') then
          begin
            RemoveTrailingComma(lastline);
            fEquations[lastline] := fEquations[lastline] + ' :double;';
          end;
        // Add declaration for array or matrix variable
        arraynum := GetArrayNum(ftempstate[i].name);
        tempstring := RemovearrayIndex(ftempstate[i].symbol);
        tempstring2 := RemoveArrayIndex(ftempprocess[i].symbol);
        if farrays[arraynum].numcol = 0 then           // Array
         begin
          fEquations.Insert(toline + linecount, tempstring + ', '
              + tempstring2 + ': array[1..' +
              inttostr(farrays[arraynum].numRow) + '] of double;');
          i := i + farrays[arraynum].numRow;
          lastline := toline + linecount;
         end
        else                                          // Matrix
         begin
          fEquations.Insert(toline + linecount, tempstring + ', '
              + tempstring2 + ': array[1..' + inttostr(farrays[arraynum].numRow)
              + ',1..' + inttostr(farrays[arraynum].numcol) + '] of double;');
          i := i + farrays[arraynum].numRow*farrays[arraynum].numcol;
          lastline := toline + linecount;
         end;
     end;
    linecount := linecount + 1;
  until i = ftempmodeldef.numstate + 1;

 // Processes and parameters.
  toline := LineSearch(fEquations,'{processes and associated parameters}');
  fEquations.Delete(toline + 1);  // Delete the example.
  linecount := 1;
  i := 1;
  repeat
    if ftempprocess[ftempmodeldef.numstate + i].name[1] <> '*' then   // Not an array variable
      begin
        tempstring := ftempprocess[ftempmodeldef.numstate + i].symbol + ', ';
        numprevpar := ParCount(ftempmodeldef.numstate + i);
        for j := 1 to ftempprocess[ftempmodeldef.numstate + i].parameters do
         begin
           tempstring := tempstring + ftemppar[numprevpar + j].symbol + ', ';
         end;
        fEquations.Insert(toline + linecount, tempstring);
        i := i + 1;
        lastline := toline + linecount;
      end
    else         // An array variable or matrix
      begin
  // Remove comma and set var type to double before adding array if previous variable was not an array.
        if (i = 1) then
         begin
          RemoveTrailingComma(lastline);
          if (ftempstate[ftempmodeldef.numstate].name[1] <> '*') then
            fEquations[lastline] := fEquations[lastline] + ' :double;';
         end
        else if (ftempprocess[ftempmodeldef.numstate + i - 1].name[1] <> '*') then
         begin
          RemoveTrailingComma(lastline);
          fEquations[lastline] := fEquations[lastline] + ' :double;';
         end;
        // Add declaration for array variable
        arraynum := GetArrayNum(ftempprocess[ftempmodeldef.numstate + i].name);
        tempstring := RemoveArrayIndex(ftempprocess[ftempmodeldef.numstate + i].symbol) + ', ';
        numprevpar := ParCount(ftempmodeldef.numstate + i);
        j := 1;
        if farrays[arraynum].Numcol = 0 then
          numtotpar := farrays[arraynum].NumUniqueParam*farrays[arraynum].numRow
        else
          numtotpar := farrays[arraynum].NumUniqueParam*farrays[arraynum].numRow*farrays[arraynum].Numcol;
        while j <= numtotpar do
         begin
          tempstring := tempstring + RemoveArrayIndex(ftemppar[numprevpar+j].symbol) + ', ';
          if farrays[arraynum].numcol = 0 then
             j := j + farrays[arraynum].numrow
          else
             j := j + farrays[arraynum].numrow*farrays[arraynum].numcol;
         end;
        tempstring := trim(tempstring);
        j := length(tempstring);                 // fix, use removetrailingcomma?     won't work
        if tempstring[j] = ',' then delete(tempstring,j,1);
        if farrays[arraynum].numcol = 0 then   // An array
         begin
          tempstring := tempstring + ': array[1..' +
            inttostr(farrays[arraynum].numRow) + '] of double;';
          i := i + farrays[arraynum].numRow;
         end
        else               // A matrix
         begin
          tempstring := tempstring + ': array[1..' +
            inttostr(farrays[arraynum].numrow) + ',1..' +
            inttostr(farrays[arraynum].numcol) + '] of double;';
          i := i + farrays[arraynum].numrow*farrays[arraynum].numcol;
         end;
        fEquations.Insert(toline + linecount, tempstring);
        lastline := toline + linecount;
      end;
    linecount := linecount + 1;
  until i = ftempmodeldef.numprocess - ftempmodeldef.numstate + 1;

   // Remove the comma after the last variable if there are no other variables.
  if (ftempmodeldef.numdrive = 0) and (fnumOtherDouble = 0) then
      RemoveTrailingComma(lastline);

 // Driver variables.
  toline := LineSearch(fEquations,'{drivers}');
  fEquations.Delete(toline + 1);   // Delete the example.
  linecount := 1;
  i := 1;
  while i < ftempmodeldef.numdrive + 1 do
   begin
    if ftempdrive[i].name[1] <> '*' then   // Not an array variable
      begin
        fEquations.Insert(toline + linecount, ftempdrive[i].symbol + ', ');
        i := i + 1;
        lastline := toline + linecount;
      end
    else       // An array or matrix variable
      begin
   // Remove comma and set var type to double before adding array if previous variable was not an array.
        if (i = 1) and (ftempprocess[ftempmodeldef.numprocess].name[1] <> '*') then
         begin
          RemoveTrailingComma(lastline);
          fEquations[lastline] := fEquations[lastline] + ' :double;';
         end
        else if (i > 1) and (ftempdrive[i-1].name[1] <> '*') then
         begin
          RemoveTrailingComma(lastline);
          fEquations[lastline] := fEquations[lastline] + ' :double;';
         end;
        // Add declaration for array or matrix variable
        arraynum := GetArrayNum(ftempdrive[i].name);
        tempstring := ftempdrive[i].symbol;
        num := pos('[', tempstring);
        delete(tempstring, num, length(tempstring) - num + 1);
        if farrays[arraynum].Numcol=0 then
         begin           // Array
          fEquations.Insert(toline + linecount,
             tempstring + ': array[1..' +
             inttostr(farrays[arraynum].numRow) + '] of double;');
          i := i + farrays[arraynum].numRow;
         end
        else
         begin          // Marix
          fEquations.Insert(toline + linecount,
             tempstring + ': array[1..' + inttostr(farrays[arraynum].numRow) +
               ',1..' + inttostr(farrays[arraynum].numcol) + '] of double;');
          i := i + farrays[arraynum].numRow*farrays[arraynum].numcol;
         end;
        lastline := toline + linecount;
      end;
    linecount := linecount + 1;
   end;

// Other variables Doubles
  toline := LineSearch(fEquations,'{Other double}');
  fEquations.Delete(toline + 1);   // Delete the example.
  linecount := 1;
  i := 1;
  if fnumOtherDouble = 0 then      // Clean up because there are no other variables
   begin
    if ftempdrive[ftempmodeldef.numdrive].name[1] <> '*' then
      RemoveTrailingComma(lastline)
    else
      RemoveFinalDouble;
   end
  else     // There are other variables
   begin
    repeat
     if ftempotherDouble[i].name[1] <> '*' then   // Not an array variable
      begin
        fEquations.Insert(toline + linecount, ftempotherDouble[i].symbol + ', ');
        i := i + 1;
        lastline := toline + linecount;
        if i = fnumOtherDouble + 1 then RemoveTrailingComma(lastline);
      end
     else       // An array variable
      begin    // Remove comma and set var type to double before adding array if necessary
        if i = 1 then
         begin
          if (ftempmodeldef.numdrive > 0) and (ftempdrive[ftempmodeldef.numdrive].name[1] <> '*') then
           begin
            RemoveTrailingComma(lastline);
            fEquations[lastline] := fEquations[lastline] + ' :double;';
           end;
          if (ftempmodeldef.numdrive = 0) and (ftempprocess[ftempmodeldef.numprocess].name[1] <> '*') then
           begin
            RemoveTrailingComma(lastline);
            fEquations[lastline] := fEquations[lastline] + ' :double;';
           end;
         end
        else
         begin
          if ftempotherDouble[i-1].name[1] <> '*' then
           begin
            RemoveTrailingComma(lastline);
            fEquations[lastline] := fEquations[lastline] + ' :double;';
           end;
         end;
        // Add declaration for array variable
        arraynum := GetArrayNum(ftempotherDouble[i].name);
        fEquations.Insert(toline + linecount,
           ftempotherDouble[i].symbol + ': array[1..' +
           inttostr(farrays[arraynum].numRow) + '] of double;');
        i := i + farrays[arraynum].numRow;
        lastline := toline + linecount;
        if (i = fnumOtherDouble + 1) then RemoveFinalDouble;
      end;
     linecount := linecount + 1;
    until i = fnumOtherDouble + 1;
   end;

// Other variables integer
  toline := LineSearch(fEquations,'{Other integers}');
  linecount := 1;
  i := 1;
  if numOtherInt <> 0 then // There are other variables
  begin
   repeat
    if tempotherInt[i].name[1] <> '*' then   // Not an array variable
      begin
        fEquations.Insert(toline + linecount, tempotherInt[i].symbol + ', ');
        i := i + 1;
        lastline := toline + linecount;
        if i = numOtherInt + 1 then
         begin
          RemoveTrailingComma(lastline);
          fEquations[lastline] := fEquations[lastline] + ': integer;';
         end;
      end
    else       // An array variable
      begin
        if i>1 then   // Clean up comma and add :integer if previous other int existed
          if tempOtherInt[i-1].name[1] <> '*' then
           begin
            RemoveTrailingComma(lastline);
            fEquations[lastline] := fEquations[lastline] + ': integer;';
           end;
        // Add declaration for array variable
        arraynum := GetArrayNum(tempotherInt[i].name);
        fEquations.Insert(toline + linecount,
           tempotherInt[i].symbol + ': array[1..' +
           inttostr(farrays[arraynum].numRow) + '] of integer;');
        i := i + farrays[arraynum].numRow;
        lastline := toline + linecount;
      end;
    linecount := linecount + 1;
   until i = numOtherInt + 1;
  end;

// Boolean variables
  if numBoolean <> 0 then
   begin
    toline := LineSearch(fEquations,'{ Boolean Variables }');
    linecount := 1;
    i := 1;
    repeat
     if ftempBoolean[i].name[1] <> '*' then   // Not an array variable
      begin
        fEquations.Insert(toline + linecount, ftempBoolean[i].symbol + ', ');
        i := i + 1;
        lastline := toline + linecount;
        if i = numBoolean + 1 then
         begin
           RemoveTrailingComma(lastline);
           fEquations[lastline] := fEquations[lastline] + ': Boolean;';
         end;
      end
     else       // An array variable
      begin
        if (i>1) and (ftempBoolean[i-1].name[1] <> '*') then
          begin
           RemoveTrailingComma(lastline);
           fEquations[lastline] := fEquations[lastline] + ' :Boolean;';
          end;
        // Add declaration for array variable
        arraynum := GetArrayNum(ftempBoolean[i].name);
        fEquations.Insert(toline + linecount,
           ftempBoolean[i].symbol + ': array[1..' +
           inttostr(farrays[arraynum].numRow) + '] of Boolean;');
        i := i + farrays[arraynum].numRow;
        lastline := toline + linecount;
        if (i = numBoolean + 1) then RemoveFinalDouble;
      end;
     linecount := linecount + 1;
    until i = numBoolean + 1;
   end;

  // Copy any user defined functions into the processes procedure
  GetFunctions;

// Copy values from the global farrays to the local variables.
 // Drivers
  toline := LineSearch(fEquations,'{ Copy the drivers from the global array,');
  fEquations.Delete(toline + 1);   // Delete the example.
  linecount := 1;
  i := 1;
  while i < ftempmodeldef.numdrive + 1 do
   begin
    if ftempdrive[i].name[1] <> '*' then  // Not an array
      begin
        fEquations.Insert(toline + linecount, ftempdrive[i].symbol +
           ' := ' + 'tdrive[' + inttostr(i) + '].value;');
        i := i + 1;
      end
    else        // An array or matrix variable
      begin
        arraynum := GetArrayNum(ftempdrive[i].name);
        tempstring2 := ftempdrive[i].symbol;
        num := pos('[', tempstring2);
        delete(tempstring2, num, length(tempstring2) - num + 1);
        if farrays[arraynum].numcol = 0 then
         begin              // Array
          tempstring := 'for jj := 1 to ' + inttostr(farrays[arraynum].numRow)
            + ' do ' + tempstring2 + '[jj] := tdrive[' + inttostr(i) +
            ' + jj - 1].value;';
          fEquations.Insert(toline + linecount, tempstring);
          i := i + farrays[arraynum].numRow;
         end
        else               // Matrix
         begin
          tempstring := 'for jj := 1 to ' + inttostr(farrays[arraynum].numRow)
            + ' do for kk := 1 to ' + inttostr(farrays[arraynum].numcol) + ' do '
            + tempstring2 + '[jj,kk] := tdrive[' + inttostr(i) +
            ' + (jj-1)*' + inttostr(farrays[arraynum].numcol) + ' + kk - 1].value;';
          fEquations.Insert(toline + linecount, tempstring);
          i := i + farrays[arraynum].numRow*farrays[arraynum].numcol;
         end;
      end;
    linecount := linecount + 1;
   end;

 // State Variables
  toline := LineSearch(fEquations, '{ Copy the state variables from the global array');
  fEquations.Delete(toline + 1);   // Delete the example.
  linecount := 1;
  i := 1;
  repeat
    if ftempstate[i].name[1] <> '*' then  // Not an array
      begin
        fEquations.Insert(toline + linecount, ftempstate[i].symbol +
           ' := ' + 'tstat[' + inttostr(i) + '].value;');
        i := i + 1;
      end
    else        // An array or matrix variable
      begin
        arraynum := GetArrayNum(ftempstate[i].name);
        if farrays[arraynum].Numcol = 0 then           // Array
         begin
          tempstring := 'for jj := 1 to ' + inttostr(farrays[arraynum].numRow)
            + ' do ' +  RemoveArrayIndex(ftempstate[i].symbol)
            + '[jj] := tstat[' + inttostr(i-1) +
            ' + jj].value;';
          fEquations.Insert(toline + linecount, tempstring);
          i := i + farrays[arraynum].numRow;
         end
        else
         begin                           // Matrix
          tempstring := 'for jj := 1 to ' + inttostr(farrays[arraynum].numRow)
            + ' do for kk := 1 to ' + inttostr(farrays[arraynum].numcol)
            + ' do ' +  RemoveArrayIndex(ftempstate[i].symbol)
            + '[jj,kk] := tstat[' + inttostr(i) +
            ' + (jj-1)*' + inttostr(farrays[arraynum].numcol) + '+kk-1].value;';
          fEquations.Insert(toline + linecount, tempstring);
          i := i + farrays[arraynum].numRow*farrays[arraynum].numcol;
         end;
      end;
    linecount := linecount + 1;
  until i = ftempmodeldef.numstate + 1;

 // Parameters
  toline := LineSearch(fEquations, '  copy the value of the first parameter');
  toline := toline + 2;
  for i := 1 to 2 do fEquations.Delete(toline);  // Delete the example.
  i := ftempmodeldef.numstate + 1;
  repeat
    if ftempprocess[i].parameters > 0 then
     begin
     fEquations.Insert(toline,
         'npar:=ParCount(ModelDef.numstate + ' +
         inttostr(i-ftempmodeldef.numstate) + ');');
     toline := toline + 1;
     if ftempprocess[i].name[1] <> '*' then  // Not an array variable
      begin
        numprevpar := ParCount(i);
        for j := 1 to ftempprocess[i].parameters do
          begin
              begin
                fEquations.Insert(toline,
                   ftemppar[numprevpar + j].symbol + ' := par[npar + ' +
                   inttostr(j) + '].value;');
                toline := toline + 1;
              end;
          end;
        fEquations.Insert(toline + 1, ' ');
        toline := toline + 1;
        i := i + 1;
      end
     else       // An array or matrix variable
      begin
       numprevpar := ParCount(i);
       arraynum := GetArrayNum(ftempprocess[i].name);
       if farrays[arraynum].numcol = 0 then      // An array
        begin
         ipar := 0;
         repeat
          ipar:=ipar+1;
          tempstring := 'for jj := 1 to ' +
             inttostr(farrays[arraynum].numRow) + ' do ' +
             RemoveArrayIndex(ftemppar[numprevpar + farrays[arraynum].numRow*(ipar-1) + 1].symbol) +
             '[jj] := par[npar + ' +
             inttostr(farrays[arraynum].numRow) + '*' + inttostr(ipar-1) + ' + jj].value;';
          fEquations.Insert(toline, tempstring);
          toline := toline + 1;
         until ipar >= farrays[arraynum].NumUniqueParam;
         fEquations.Insert(toline + 1, ' ');
         toline := toline + 1;
         i := i + farrays[arraynum].numRow;
        end
       else   // A matrix
        begin
         ipar := 0;
         repeat
          ipar:=ipar+1;
          tempstring := 'for jj := 1 to ' + inttostr(farrays[arraynum].numRow) +
             ' do for kk := 1 to ' + inttostr(farrays[arraynum].numCol) + ' do ' +
             RemoveArrayIndex(ftemppar[numprevpar + farrays[arraynum].numcol*farrays[arraynum].numRow*(ipar-1) + 1].symbol) +
             '[jj, kk] := par[npar + ' + inttostr(farrays[arraynum].Numcol) + '*' +
             inttostr(farrays[arraynum].numRow) + '*' + inttostr(ipar-1) + ' + (jj-1)*' +
             inttostr(farrays[arraynum].numcol) + ' + kk].value;';
          fEquations.Insert(toline, tempstring);
          toline := toline + 1;
         until ipar >= farrays[arraynum].NumUniqueParam;
         fEquations.Insert(toline + 1, ' ');
         toline := toline + 1;
         i := i + farrays[arraynum].numRow*farrays[arraynum].Numcol;
        end;
      end;
     end
    else
     i := i + 1;
  until i = ftempmodeldef.numprocess + 1;

// Set all processes to -999 to catch undefined processes
  for i:=1 to ftempmodeldef.numprocess do
   begin
    tempstring := ftempprocess[i].symbol;
    tempstring := StringReplace(tempstring, '-', ',',[]);   // Converts dashes in matrix notation to comma.
    tempstring := tempstring + ' := -999;'; // This needs to be done after the replace so that the minus doesn't get replaced with a comma.
    fEquations.Insert(toline, tempstring);
    toline := toline+1;
   end;

 // Enter the equations.
  toline := LineSearch(fEquations,
                     '{ Enter the equations to calculate the processes here,');
  toline := toline + 2;
  fEquations.Delete(toline); // Delete the example equation.
  fromline1 := LineSearch(fModelDef, 'Equations');
  fromline2 := LineSearch(fModelDef, 'End Equations');

// Check for use of random function
  i := fromline1+1;
  WithinCalcDiscrete := False;
  repeat
    astring := fModelDef[i];
    if WithinCalcDiscrete = False then
      if pos('CalculateDiscrete', astring) <> 0 then WithinCalcDiscrete := True;
    posrandom := pos('random', astring);
    if (posrandom <> 0) and (not WithinCalcDiscrete) then
      raise Exception.Create('Random function can only be used within the CalculateDiscrete section.');
    i := i + 1;
  until i = fromline2;

  numemptylines := 0;
  i := 1;
  repeat
    tempstring := fModelDef[fromline1 + numemptylines + i];
    if tempstring <> 'End Equations' then
        fEquations.Insert(toline + i,
              fModelDef[fromline1 + numemptylines + i]);
    i := i + 1;
  until i + numemptylines >= fromline2 - fromline1;

// Equations to calculate the derivatives
  toline := LineSearch(fEquations, 'if tstat[1].HoldConstant');
  fromline1 := LineSearch(fModelDef, 'Derivatives');

  CheckDerivatives(fromline1);
  // Delete the example code
  for i := 0 to 3 do fEquations.Delete(toline);

  // Write the derivatives
  numemptylines := 0;
  linecount := 1;
  i := 1;
  repeat
    repeat
      tempstring := fModelDef[fromline1 + numemptylines + linecount];
      tempstring := trim(tempstring);
      linecount := linecount + 1;
    until tempstring <> '';
    if ftempstate[i].name[1] <> '*' then  // Not an array variable
      begin
        fEquations.Insert(toline, 'if (tstat[' + inttostr(i) +
           '].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then');
        fEquations.Insert(toline + 1, ' ' + ftempprocess[i].symbol +
          ' := 0');
        fEquations.Insert(toline + 2, 'else');
        fEquations.Insert(toline + 3, ' ' +
           fModelDef[fromline1 + numemptylines + linecount - 1]);
        fEquations.Insert(toline + 4, ' ');
        toline := toline + 5;
        i := i + 1;
      end
    else        // An array or matrix variable
      begin
        arraynum := GetArrayNum(ftempstate[i].name);
        tempstring2 := ftempprocess[i].symbol;
        num := pos('[', tempstring2);
        delete(tempstring2, num, length(tempstring2) - num + 1);
        if farrays[arraynum].numcol = 0 then                // Array
         begin
          fEquations.Insert(toline, 'for jj := 1 to ' +
             inttostr(farrays[arraynum].numRow) + ' do');
          fEquations.Insert(toline + 1, 'begin');
          fEquations.Insert(toline + 2, ' if (tstat[' + inttostr(i - 1) +
             ' + jj].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then');
          fEquations.Insert(toline + 3, '  ' + tempstring2 +
             '[jj] := 0');
          fEquations.Insert(toline + 4, ' else');
          fEquations.Insert(toline + 5, '  ' +
             fModelDef[fromline1 + numemptylines + linecount - 1]);
          fEquations.Insert(toline + 6, 'end;');
          fEquations.Insert(toline + 7, ' ');
          toline := toline + 8;
          i := i + farrays[arraynum].numRow;
         end
        else                                                // Matrix
         begin
          fEquations.Insert(toline, 'for jj := 1 to ' +
             inttostr(farrays[arraynum].numRow) + ' do for kk:=1 to ' +
             inttostr(farrays[arraynum].numcol) + ' do');
          fEquations.Insert(toline + 1, 'begin');
          fEquations.Insert(toline + 2, ' if (tstat[' + inttostr(i) +
             ' + (jj-1)*' + inttostr(farrays[arraynum].Numcol) +
             '+kk-1].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then');
          fEquations.Insert(toline + 3, '  ' + tempstring2 +
             '[jj,kk] := 0');
          fEquations.Insert(toline + 4, ' else');
          fEquations.Insert(toline + 5, '  ' +
             fModelDef[fromline1 + numemptylines + linecount - 1]);
          fEquations.Insert(toline + 6, 'end;');
          fEquations.Insert(toline + 7, ' ');
          toline := toline + 8;
          i := i + farrays[arraynum].numRow*farrays[arraynum].numcol;

         end;
      end;
  until i = ftempmodeldef.numstate + 1;

// Copy the new values from the local variables back into the global farrays.
 // State variables
  toline := LineSearch(fEquations, 'tstat[1].value := ');
  fEquations.Delete(toline);  // Delete the example.

  i := 1;
  linecount := 1;
  repeat
   if ftempstate[i].name[1] <> '*' then    // Not an array variable
    begin
      fEquations.Insert(toline + linecount - 1, 'tstat[' + inttostr(i) +
         '].value := ' + ftempstate[i].symbol + ';');
      i := i + 1;
    end
   else             // An array or matrix variable
    begin
      arraynum := GetArrayNum(ftempstate[i].name);
      tempstring2 := ftempstate[i].symbol;
      num := pos('[', tempstring2);
      delete(tempstring2, num, length(tempstring2) - num + 1);
      if farrays[arraynum].numcol=0 then          // Array
       begin
        fEquations.Insert(toline + linecount - 1, 'for jj := 1 to ' +
          inttostr(farrays[arraynum].numRow) + ' do tstat[' + inttostr(i-1) +
          ' + jj].value := ' + tempstring2 + '[jj];');
        i := i + farrays[arraynum].numRow;
       end
      else                                        // Matrix
       begin
        fEquations.Insert(toline + linecount - 1, 'for jj := 1 to ' +
          inttostr(farrays[arraynum].numRow) + ' do for kk := 1 to ' +
          inttostr(farrays[arraynum].numcol) + ' do tstat[' + inttostr(i) +
          ' + (jj-1)*' + inttostr(farrays[arraynum].Numcol) + '+kk-1].value := ' + tempstring2 + '[jj,kk];');
        i := i + farrays[arraynum].numRow*farrays[arraynum].Numcol;
       end;
    end;
   linecount := linecount + 1;
  until i = ftempmodeldef.numstate + 1;

 // State variable derivatives
  toline := LineSearch(fEquations, 'tproc[1].value := ');
  fEquations.Delete(toline);  // Delete the example.

  i := 1;
  linecount := 1;
  repeat
   if ftempstate[i].name[1] <> '*' then    // Not an array variable
    begin
      fEquations.Insert(toline + linecount - 1, 'tproc[' + inttostr(i) +
         '].value := ' + ftempprocess[i].symbol + ';');
      i := i + 1;
    end
   else             // An array or matrix variable
    begin
      arraynum := GetArrayNum(ftempstate[i].name);
      tempstring2 := ftempprocess[i].symbol;
      num := pos('[', tempstring2);
      delete(tempstring2, num, length(tempstring2) - num + 1);
      if farrays[arraynum].numcol=0 then          // Array
       begin
        fEquations.Insert(toline + linecount - 1, 'for jj := 1 to ' +
          inttostr(farrays[arraynum].numRow) + ' do tproc[' + inttostr(i-1) +
          ' + jj].value := ' + tempstring2 + '[jj];');
        i := i + farrays[arraynum].numRow;
       end
      else                                        // Matrix
       begin
        fEquations.Insert(toline + linecount - 1, 'for jj := 1 to ' +
          inttostr(farrays[arraynum].numRow) + ' do for kk := 1 to ' +
          inttostr(farrays[arraynum].numcol) + ' do tproc[' + inttostr(i) +
          ' + (jj-1)*' + inttostr(farrays[arraynum].Numcol) + '+kk-1].value := ' + tempstring2 + '[jj,kk];');
        i := i + farrays[arraynum].numRow*farrays[arraynum].Numcol;
       end;
    end;
   linecount := linecount + 1;
  until i = ftempmodeldef.numstate + 1;

 // Processes
  toline := LineSearch(fEquations, 'tproc[ModelDef.numstate + 1].value :=');
  fEquations.Delete(toline);  // Delete the example.

  i := 1;
  linecount := 1;
  repeat
   if ftempprocess[ftempmodeldef.numstate + i].name[1] <> '*' then    // Not an array variable
    begin
      fEquations.Insert(toline + linecount - 1, 'tproc[ModelDef.numstate + ' +
                inttostr(i) + '].value := ' +
                ftempprocess[ftempmodeldef.numstate + i].symbol + ';');
      i := i + 1;
    end
   else
    begin
      arraynum := GetArrayNum(ftempprocess[ftempmodeldef.numstate + i].name);
      if farrays[arraynum].numcol = 0 then   // An array
        begin
         fEquations.Insert(toline + linecount - 1, 'for jj := 1 to ' +
          inttostr(farrays[arraynum].numRow) + ' do tproc[ModelDef.numstate + ' +
          inttostr(i - 1) + ' + jj].value := ' +
         RemoveArrayIndex(ftempprocess[ftempmodeldef.numstate + i].symbol) + '[jj];');
         i := i + farrays[arraynum].numRow;
        end
      else   // A matrix not an array
        begin
         fEquations.Insert(toline + linecount - 1, 'for jj := 1 to ' +
          inttostr(farrays[arraynum].numrow{numcol}) + ' do  for kk := 1 to ' +
          inttostr(farrays[arraynum].numcol{numrow}) + ' do  tproc[ModelDef.numstate + ' +
          inttostr(i - 1) + ' + (jj{kk}-1)*' + inttostr(farrays[arraynum].numcol) + ' + kk{jj}].value := ' +
         RemoveArrayIndex(ftempprocess[ftempmodeldef.numstate + i].symbol) + '[jj,kk];');
         i := i + farrays[arraynum].numRow*farrays[arraynum].numcol;
        end;
    end;
   linecount := linecount + 1;
  until i = ftempmodeldef.numprocess - ftempmodeldef.numstate + 1;
end;


{ Searches tempmemo for the string Sstring and returns the line number
  containing Sstring. }
function TFmMain.Linesearch(AstringList: Tstringlist; Sstring:string):integer;
var
 i,temp:integer;
begin
 i := 0;
 temp := 0;
 while (i < AstringList.count - 1) and (temp = 0) do
  begin
    temp := pos(Sstring,AstringList[i]);
    i := i + 1;
  end;
 linesearch := i - 1;
end;

{ Divides the string, fromString, into two parts at the string, divider. Returns
  either the beginning or the end of the string based on the value of WhichPart.
  The string divider is not included in either part. }
function TFmMain.DivideString(fromString, divider:string;
                             WhichPart:TPart):string;
var
 whereDivide, slength: integer;
begin
 whereDivide := pos(divider, fromString);
 slength := length(fromString);
 if WhichPart = stBeginning then
   begin
     delete(fromString,whereDivide,slength-whereDivide+1);
     fromString := trim(fromString);
   end
 else
   begin
     delete(fromString,1,whereDivide);
     fromString := trim(fromString);
   end;
 DivideString := fromString;
end;

{ Separates tstring into the name, units, symbol, and npar using a comma as delimiter. }
function TFmMain.ParseString(acsvstring: string; vtype: TVarType): TVarInfo;
var
 tempstring: Tstringlist;
 newmVar: TVarInfo;
begin
newmVar.vType:=vtype;
tempstring:=Tstringlist.create;
try
 tempstring.Delimiter:=',';
 tempstring.StrictDelimiter:=true;
 tempstring.DelimitedText:=acsvstring;

 newmVar.Name:=trim(tempstring[0]);
 newmVar.Units:=trim(tempstring[1]);
 newmVar.Symbol:=trim(tempstring[2]);

// Check for unflagged array/matrix variables
 if (vtype<>vtprocess) and (tempstring.Count>3) and (newmVar.Name[1]<>'*') then
    raise Exception.Create('Inconsistent variable description. Variable, '+ newmVar.Name +
               ', is listed as an array or matrix but does not begin with *')
 else if (tempstring.count>4) and (newmVar.Name[1]<>'*') then
    raise Exception.Create('Inconsistent variable description. Variable, '+ newmVar.Name +
             ', is listed as an array or matrix but does not begin with *');

// Array variable?
 if newmVar.Name[1]='*' then
    newmVar.IsArray:=true
 else
    newmVar.IsArray:=false;

 if vtype<>vtprocess then  // Not a process variable
  begin
    if not newmVar.IsArray then
     begin
       newmVar.numParam:=0;
       newmVar.numRow:=0;
       newmVar.numCol:=0;
     end
    else   // An array
     begin
       newmVar.numParam:=0;
       if tempstring.Count<4 then   // Missing array specification
          raise Exception.Create('Missing number of array elements for variable '+ newmVar.Name + '.')
       else
        begin
         newmVar.numRow:=strtoint(tempstring[3]);
         if tempstring.Count=5 then
           newmVar.numCol:=strtoint(tempstring[4])
         else
           newmVar.numCol:=0;
        end;
     end;
  end
 else      // A process
  begin
   newmVar.numParam:=strtoint(tempstring[3]);
   if tempstring.Count>4 then // An array or matrix
    begin
     newmVar.numRow:=strtoint(tempstring[4]);
     if tempstring.Count=6 then
       newmVar.numCol:=strtoint(tempstring[5])
     else
       newmVar.numCol:=0;
    end
   else
    begin
     newmVar.numRow:=0;
     newmVar.numCol:=0;
    end;
  end;

 ParseString:=newmVar;
finally
 if assigned(tempstring) then FreeandNil(tempstring);
end;
end;

{procedure TFmMain.ParseString(var acsvstring, vname, vunits, vsymbol: string;
                          var npar:integer;  var nrow, ncol: integer);
var
 num, num2:integer;
 tstring2:string;
begin
 // Initialize variables that don't always get assigned so that that they are always defined on return
 tnpar := 0;
 tptype := ptGroup1;
 tnrow := 0;
 tncol := 0;

 num := pos(',',tstring);          // Name
 tname := DivideString(tstring, ',', stBeginning);
 delete(tstring, 1, num);

 num := pos(',',tstring);          // Units
 tunits := DivideString(tstring, ',', stBeginning);
 delete(tstring, 1, num);

 num := pos(',', tstring);         // Symbol
 tsymbol := DivideString(tstring, ',', stBeginning);

 if num <> 0 then               // An array  or matrix variable, a process, or both.
  begin
   delete(tstring, 1, num);

  // Check for the word 'group' to determine if this is a process or not
   num2 := pos('group',lowercase(tstring));
   if num2 = 0 then      // Not a process but an array or matrix variable
    begin
      num := pos(',',tstring);
      if num = 0 then
       begin
        tnrow := strtoint(DivideString(tstring, ',', stBeginning));
       end
      else
       begin
        tnrow := strtoint(DivideString(tstring, ',', stBeginning));
        delete(tstring, 1, num);
        tncol := strtoint(DivideString(tstring, ',', stBeginning));
       end;
    end
   else
    begin
      num := pos(',',tstring);
      tnpar := strtoint(DivideString(tstring, ',', stBeginning));
      delete(tstring, 1, num);

      tstring2 := DivideString(tstring, ',', stBeginning);
	  num2 := length('Group');
	  delete(tstring2, 1, num2);
      try
       num2 := strtoint(tstring2);
       case num2 of
          1: tptype := ptGroup1;
          2: tptype := ptGroup2;
          3: tptype := ptGroup3;
          4: tptype := ptGroup4;
          5: tptype := ptGroup5;
       else
          tptype := ptGroup1;
       end;
      except on E: Exception do
        tptype := ptGroup1;
      end;
      num := pos(',',tstring);
      if num = 0 then delete(tstring,pos('group',lowercase(tstring)),8)
      else delete(tstring,1,num);

      num := pos(',', tstring);
      if (num <> 0) then         // A process array or matrix
        begin
          tnrow := strtoint(DivideString(tstring, ',', stBeginning));
          delete(tstring, 1, num);

          num := length(trim(tstring));
          if num <> 0 then tncol := strtoint(tstring);
        end
      else if (length(trim(tstring)) <> 0)  then
        begin
          tnrow := strtoint(tstring);
        end;
     end;
  end;
end;       }

{This function counts the parameters in all processes less than processnum.}
function TFmMain.ParCount(processnum:integer):integer;
var
 NumberofParams, counter : integer;
begin
  NumberofParams := 0;
  for counter := ftempmodeldef.numstate + 1 to processnum - 1 do
    begin
         NumberofParams := NumberofParams + ftempprocess[counter].parameters;
    end;
  ParCount := NumberofParams;
end; // end of parcount function

procedure TFmMain.BtnCloseClick(Sender: TObject);
begin
  FmMain.Close;
end;

procedure TFmMain.FormCreate(Sender: TObject);
begin
 fResourcePath := GetResourcePath();
 fvarlist := Tstringlist.Create;
 fvarlist.Sorted:=True;
 fvarlist.Duplicates:=dupError;
 // Set the default model path to the folder CrEquations is in.
 {$ifdef Darwin}
 fModelPath:=fResourcePath;
 delete(fModelPath,pos(BundleResourceDirectory,fResourcePath),length(BundleResourceDirectory));
 {$else} // Windows, Unix
 fModelPath:=fResourcePath;
 {$endif}
 DlgOpenModelDef.InitialDir := fModelPath;
 fEquationsFilename := fResourcePath + 'equationsblank.pas';
 DlgOpenEqPas.InitialDir:= fResourcePath;
end;

procedure TFmMain.FormDestroy(Sender: TObject);
begin
 fvarlist.Free;
end;

function TFmMain.GetArrayNum(varname:string): integer;
var
 i, number:integer;
begin
 number := pos('-',varname);
 if number <> 0 then
  begin
   if isdigit(varname,number-2) then delete(varname,number-2,length(varname))
   else delete(varname,number-1,length(varname));
  end
 else
  begin
   number := length(varname);
   delete(varname,number,1);
  end;

 number := 0;
 for i := 1 to fTotalNumArrays do
    begin
      if varname = farrays[i].VarName then number := i;
    end;
 if number = 0 then
   begin
     delete(varname,length(varname),1);
     for i := 1 to fTotalNumArrays do
       if varname = farrays[i].varname then number := i;
   end;

 if number <> 0 then
   GetArrayNum := number
 else
   raise Exception.Create('Could not find variable, "' + varname + '" in arrays array.');
end;

procedure TFmMain.GetFunctions;
var
 fromline, toline, numsourcelines, i: integer;
begin
 fromline := LineSearch(fModelDef, 'Functions');
 toline := LineSearch(fModelDef, 'End Functions');
 numsourcelines := toline - fromline;

 toline := LineSearch(fEquations,'{ Functions or procedures }');
 if numsourcelines > 3 then
   begin
     i := 1;
     repeat
       fEquations.Insert(toline + i, fModelDef[fromline + i]);
       i := i + 1;
     until i = numsourcelines;
   end;
end;

function TFmMain.CheckDerivatives(fline: integer): integer;
var
 i, j, arraynum: integer;
 tempstring, tempstring2: string;
begin
 i := 1;
 fline := fline + 1;
 repeat
  repeat
   tempstring := fModelDef[fline];
   fline := fline + 1;
  until tempstring <> '';
  j := pos(':',tempstring);
  delete(tempstring, j, length(tempstring)-j+1);
  tempstring := trim(tempstring);
  if ftempprocess[i].name[1] = '*' then         // An array or matrix
    begin
     arraynum := GetArrayNum(ftempstate[i].name);
     j := pos('[',tempstring);
     delete(tempstring, j, length(tempstring)-j+1);

     tempstring2 := ftempprocess[i].symbol;
     j := pos('[',tempstring2);
     delete(tempstring2, j, length(tempstring2)-j+1);

     if lowercase(tempstring2) <> lowercase(tempstring) then
       raise Exception.Create('Derivatives out of order. Model translation failed.');
     if farrays[arraynum].Numcol = 0 then         // Array
      begin
       i := i + farrays[arraynum].numRow;
      end
     else                                       // Matrix
      begin
       i := i + farrays[arraynum].numRow*farrays[arraynum].Numcol;
      end;
    end
  else
    begin
     if lowercase(ftempprocess[i].symbol) <>
      lowercase(tempstring) then
     raise Exception.Create('Derivatives out of order. Model translation failed.');
     i := i + 1;
    end;
 until i = ftempmodeldef.numstate + 1;
 CheckDerivatives := 1;
end;

function TFmMain.RemoveTrailingComma(lnum: integer): integer;
var
 tempstring: string;
 ll: integer;
begin
 RemoveTrailingComma := 0;
 tempstring := fEquations[lnum];
 tempstring := trim(tempstring);
 ll := length(tempstring);
 if tempstring[ll] = ',' then delete(tempstring, ll, 1);
 fEquations[lnum] := tempstring;
 RemoveTrailingComma := 1;
end;

procedure TFmMain.RemoveFinalDouble;
var
 ll: integer;
begin
 ll := LineSearch(fEquations,':double; {Final double}');
 fEquations[ll] := '';
end;

function TFmMain.RemoveArrayIndex(tstring: string): string;
var
 i: integer;
begin
 i := pos('[', tstring);
 delete(tstring, i, length(tstring) - 1);
 RemoveArrayIndex := tstring;
end;

function TFmMain.GetResourcePath(): string;
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

function TFmMain.IsDigit(astring:string; index:integer): Boolean;
begin
 result := False;
 case astring[index] of
  '0'..'9': result := True;
 end;

end;


procedure TFmMain.DefBooleanVar;
var
 i, j, fromline:integer;
 avar: TvarInfo;
 tempstring: string;
begin
 fromline := LineSearch(fModelDef, 'Boolean Variables');
 // Advance 2 lines in the memo to get to the variable list
 fromline := fromline + 2;
 tempstring := fModelDef[fromline];
 repeat
   tempstring := fModelDef[fromline];
   tempstring := trim(tempstring);
   fromline := fromline + 1;
 until (tempstring <> '') or (fromline > fModelDef.Count);
 i := 1;
 while tempstring <> 'End' do
  begin
   avar:=ParseString(tempstring,vtBoolean);
 //  ParseString(tempstring, tname, tunits, tsymbol, tnpar, tptype, tnrow, tncol);
   if avar.Name[1] <> '*' then     // Not an array variable
     begin
       ftempBoolean[i].name := avar.Name;
       ftempBoolean[i].units := avar.Units;
       ftempBoolean[i].symbol := avar.Symbol;
       i := i + 1;
     end
   else                   // An array variable
     begin
       for j := 1 to avar.numRow do
         begin
          ftempBoolean[i + j - 1].name := avar.Name + inttostr(j);
          ftempBoolean[i + j - 1].units := avar.Units;
          ftempBoolean[i + j - 1].symbol := avar.Symbol;
         end;
       farrays[fTotalNumArrays].VarName := avar.Name; // Used in createprocessesproc procedure
       farrays[fTotalNumArrays].numRow := avar.numRow;
       fTotalNumArrays := fTotalNumArrays + 1;
       if fTotalNumArrays > MaxArrays then
           raise Exception.Create('Too many arrays. Increase MaxArrays.');
       i := i + avar.numRow;
     end;
   repeat
    tempstring := fModelDef[fromline];
    tempstring := trim(tempstring);
    fromline := fromline + 1;
   until tempstring <> '';
  end;
 numBoolean := i - 1;
end;

end.

