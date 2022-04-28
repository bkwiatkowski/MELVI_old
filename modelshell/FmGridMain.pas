unit FmGridMain;

interface

uses Windows, Classes, Graphics, Forms, Controls, Menus,
  Dialogs, StdCtrls, Buttons, ExtCtrls, ComCtrls, ImgList, StdActns,
  ActnList, ToolWin, stypes, Grids, BaseGrid, AdvGrid, AdvObj;

type
  PCellProp = ^TCellProp;
  TCellProp = record
    RowPos: integer;
    ColPos: integer;
    elevation: double;
    Runposition: integer;
    VegClass: integer;
    SoilClass: integer;
    ParamFile: string;
    DriverFile: string;
    OuputFile: string;
    RDocl: array[1..4] of Tstringlist;
    RDonl: array[1..4] of Tstringlist;
    Rnh4l: array[1..4] of Tstringlist;
    Rno3l: array[1..4] of Tstringlist;
  end;
  TVegFiles = record
    ParamFileName: string;
    DriverFileName: string;
  end;
  FVegClassInfo = record
    VegType: integer;
    Filenames: TVegFiles;
    RunOptions: TRunOptions;
  end;
  TFmGridMEL = class(TForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ActionList1: TActionList;
    FileNew1: TAction;
    FileOpen1: TAction;
    FileSave1: TAction;
    FileSaveAs1: TAction;
    FileSend1: TAction;
    FileExit1: TAction;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    HelpAbout1: TAction;
    StatusBar: TStatusBar;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    FileNewItem: TMenuItem;
    FileOpenItem: TMenuItem;
    FileSaveItem: TMenuItem;
    FileSaveAsItem: TMenuItem;
    N1: TMenuItem;
    FileSendItem: TMenuItem;
    N2: TMenuItem;
    FileExitItem: TMenuItem;
    Edit1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    Help1: TMenuItem;
    HelpAboutItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    PopupMenu1: TPopupMenu;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    LedRows: TLabeledEdit;
    LedColumns: TLabeledEdit;
    LedNoData: TLabeledEdit;
    LedVeg1ParamFile: TLabeledEdit;
    LedSoilClassFile: TLabeledEdit;
    LedVegClassFile: TLabeledEdit;
    LedSoilPropFile: TLabeledEdit;
    BtnRun: TButton;
    Button2: TButton;
    LedDEMFile: TLabeledEdit;
    LedVeg2ParamFile: TLabeledEdit;
    LedCellOrderFile: TLabeledEdit;
    LedDriver1File: TLabeledEdit;
    LedDriver2File: TLabeledEdit;
    LedRunOptionsFile: TLabeledEdit;
    AsgCellDriver: TAdvStringGrid;
    AsgGToutput: TAdvStringGrid;
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FileSave1Execute(Sender: TObject);
    procedure FileSaveAs1Execute(Sender: TObject);
    procedure FileSend1Execute(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChooseDEMFile(Sender: TObject);
    procedure ChooseVegClassFile(Sender: TObject);
    procedure ChooseVeg1ParamFile(Sender: TObject);
    procedure ChooseVeg2ParamFile(Sender: TObject);
    procedure ChooseSoilClassFile(Sender: TObject);
    procedure ChooseSoilPropFile(Sender: TObject);
    procedure ChooseCellOrderFile(Sender: TObject);
    procedure ChooseDriver1File(Sender: TObject);
    procedure ChooseDriver2File(Sender: TObject);
    procedure ChooseRunOptionsFile(Sender: TObject);
    procedure UpdateForm(Sender: TObject);
    procedure LedDriver2FileKeyPress(Sender: TObject; var Key: Char);
    procedure LedDriver1FileKeyPress(Sender: TObject; var Key: Char);
    procedure LedCellOrderFileKeyPress(Sender: TObject; var Key: Char);
    procedure LedVeg1ParamFileKeyPress(Sender: TObject; var Key: Char);
    procedure LedVeg2ParamFileKeyPress(Sender: TObject; var Key: Char);
    procedure LedDEMFileKeyPress(Sender: TObject; var Key: Char);
    procedure LedVegClassFileKeyPress(Sender: TObject; var Key: Char);
    procedure LedSoilClassFileKeyPress(Sender: TObject; var Key: Char);
    procedure LedSoilPropFileKeyPress(Sender: TObject; var Key: Char);
    procedure BtnRunClick(Sender: TObject);
    procedure LedRunOptionsFileKeyPress(Sender: TObject; var Key: Char);
    procedure FormDestroy(Sender: TObject);
    procedure ReadCellOrderFile;
    procedure ReadRunOptionsFile;
    function FindCellInList(RowNum, Colnum: integer): PCellProp;
    procedure Button2Click(Sender: TObject);
  private
    FFileName, FDEMFilename, FSoilClassFilename, FVegClassFilename,
    FSoilPropFilename, FVeg1ParamFilename, FVeg2ParamFilename, FCellOrderFilename,
    FDriver1Filename, FDriver2Filename, FRunOptionsFilename: String;
    FCellInfoList: TList;
    FVegInfo: array[0..1] of FVegClassInfo; // fix need to make this dynamic
    FTimeStart, FTimeStop: double;
    NumLayers, GTSoilLayers: integer;
  public
    { Public declarations }
    CurrentDirectory: String;
  end;

var
  FmGridMEL: TFmGridMEL;

implementation

uses
  SysUtils, Mapi, about, SHFolder, frontend, options, display;

{$R *.dfm}

resourcestring
  SUntitled  = 'Untitled';
  SOverwrite = 'OK to overwrite %s';
  SSendError = 'Error sending mail';

function DefaultSaveLocation: string;
var
  P: PChar;
begin
  {
    returns the location of 'My Documents' if it exists, otherwise it returns
    the current directory.
  }
  P := nil;
  try
    P := AllocMem(MAX_PATH);
    if SHGetFolderPath(0, CSIDL_PERSONAL, 0, 0, P) = S_OK then
      Result := P
    else
      Result := GetCurrentDir;
  finally
    FreeMem(P);
  end;
end;

procedure TFmGridMEL.FileNew1Execute(Sender: TObject);
begin
  SaveDialog.InitialDir := DefaultSaveLocation;
  FFileName := SUntitled;
//  RichEdit1.Lines.Clear;
//  RichEdit1.Modified := False;
end;

procedure TFmGridMEL.FileOpen1Execute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
//    RichEdit1.Lines.LoadFromFile(OpenDialog.FileName);
    FFileName := OpenDialog.FileName;
//    RichEdit1.SetFocus;
//    RichEdit1.Modified := False;
//    RichEdit1.ReadOnly := ofReadOnly in OpenDialog.Options;
  end;
end;

procedure TFmGridMEL.FileSave1Execute(Sender: TObject);
begin
  if (FFileName = SUntitled) or (FFileName = '') then
    FileSaveAs1Execute(Sender)
  else
  begin
//    RichEdit1.Lines.SaveToFile(FFileName);
//    RichEdit1.Modified := False;
  end;
end;

procedure TFmGridMEL.FileSaveAs1Execute(Sender: TObject);
begin
  with SaveDialog do
  begin
    FileName := FFileName;
    if Execute then
    begin
      if FileExists(FileName) then
        if MessageDlg(Format(SOverwrite, [FileName]),
          mtConfirmation, mbYesNoCancel, 0) <> idYes then Exit;
//      RichEdit1.Lines.SaveToFile(FileName);
      FFileName := FileName;
//      RichEdit1.Modified := False;
    end;
  end;
end;

procedure TFmGridMEL.FileSend1Execute(Sender: TObject);
var
  MapiMessage: TMapiMessage;
  MError: Cardinal;
begin
  with MapiMessage do
  begin
    ulReserved := 0;
    lpszSubject := nil;
//    lpszNoteText := PChar(RichEdit1.Lines.Text);
    lpszMessageType := nil; 
    lpszDateReceived := nil; 
    lpszConversationID := nil; 
    flFlags := 0;
    lpOriginator := nil; 
    nRecipCount := 0;
    lpRecips := nil;
    nFileCount := 0;
    lpFiles := nil; 
  end;

  MError := MapiSendMail(0, Application.Handle, MapiMessage,         
    MAPI_DIALOG or MAPI_LOGON_UI or MAPI_NEW_SESSION, 0);
  if MError <> 0 then MessageDlg(SSendError, mtError, [mbOK], 0);
end;

procedure TFmGridMEL.FormCreate(Sender: TObject);
begin
  FDEMFilename := 'NA';
  FVegClassFilename := 'NA';
  FVeg1ParamFilename := 'NA';
  FVeg2ParamFilename := 'NA';
  FSoilClassFilename := 'NA';
  FSoilPropFilename := 'NA';
  FDriver1Filename := 'NA';
  FDriver2Filename := 'NA';
  FCellOrderFilename := '';
  FRunOptionsFilename := '';
//  FmShellMain.Visible := False;
//  FmShellMain.RunningInteractive := false;

  CurrentDirectory := GetCurrentDir;
  NumLayers := 4;

  FCellInfoList := TList.Create;
  GTSoilLayers := 2;
end;

procedure TFmGridMEL.FormDestroy(Sender: TObject);
var
 i, k: integer;
 PACell: ^TCellProp;
begin
  for I := FCellInfoList.Count - 1 downto 0 do
   begin
   PACell := FCellInfoList[i];
    for k := 1 to Numlayers do
     begin
      PACell^.RDocl[k].Clear;
      PACell^.RDonl[k].Clear;
      PACell^.Rnh4l[k].Clear;
      PACell^.Rno3l[k].Clear;
      PACell^.RDocl[k].Free;
      PACell^.RDonl[k].Free;
      PACell^.Rnh4l[k].Free;
      PACell^.Rno3l[k].Free;
     end;
   end;
  dispose(PACell);
  FCellInfoLIst.Clear;
  FCellInfoList.Free;
end;

procedure TFmGridMEL.BtnRunClick(Sender: TObject);
var
 i, j, k: integer;
 PCurrCell: ^TCellProp;
 PCellH1, PCellH2, PCellH3, PCellH4, PCellH5, PCellH6, PCellH7, PCellH8: PCellProp;
 Wrcol, QwH1col, QwH2col, QwH3col, QwH4col, QwH5col, QwH6col, QwH7col, QwH8col, Qwd,
 GTWrcol, GTQwH1col, GTQwH2col, GTQwH3col, GTQwH4col, GTQwH5col, GTQwH6col,
 GTQwH7col, GTQwH8col, GTQwd, Lnh4lcol, Lno3lcol, Ldoclcol, Ldonlcol, Rnh4lcol,
 Rno3lcol, Rdoclcol, Rdonlcol: array[1..4] of integer;
 tempstring, tempstring2: string;

function GetColumnNumber(asgname, colname:string):integer;
var
 i,colnum:integer;
 tempstring:string;
begin
 i := 0;
 colnum := 0;
 if lowercase(asgname) = 'sgmodeloutput' then
  repeat
   tempstring := FmDisplayData.SgModelOutput.Cells[i,1];
   tempstring := trim(tempstring);
   if tempstring = colname then colnum := i;
   i := i + 1;
  until (colnum <> 0) or (i > FmDisplayData.SgModelOutput.Colcount - 1)
 else if lowercase(asgname) = 'asggtoutput' then
  repeat
   tempstring := AsgGToutput.Cells[i,0];
   tempstring := trim(tempstring);
   if tempstring = colname then colnum := i;
   i := i + 1;
  until (colnum <> 0) or (i > AsgGToutput.Colcount - 1)
 else {if lower(asgname) = 'asgcelldriver' then }
  repeat
   tempstring := AsgCellDriver.Cells[i,0];
   tempstring := trim(tempstring);
   if tempstring = colname then colnum := i;
   i := i + 1;
  until (colnum <> 0) or (i > AsgCellDriver.Colcount - 1);

 if colnum = 0 then result := -1 else result := colnum;
end;

begin
  BtnRun.Enabled := False;
  BtnRun.Caption := 'Running';
  ReadRunOptionsFile;  // This procedure MUST be called before ReadCellOrderFile so the param and driver files are defined.
  ReadCellOrderFile;
  try
    for I := 0 to FCellInfoList.Count - 1 do
     begin
      PCurrCell := FCellInfoList[i];
      AsgCellDriver.LoadFromCSV(CurrentDirectory + '\' + PCurrCell^.DriverFile);
      AsgGToutput.LoadFromCSV(CurrentDirectory + '\Cell_' + inttostr(PCurrCell^.RowPos)
         + '_' + inttostr(PCurrCell^.ColPos) + '.drr');
      // Add row to GT output to account for the units in driver file
      AsgGToutput.InsertRows(1,1);
      // Make sure the files are all the same length.
      if AsgCellDriver.RowCount <> Ftimestop - Ftimestart + 2 {names, units} then
        AsgCellDriver.RemoveRows(round(Ftimestop - Ftimestart + 2),
             AsgCellDriver.RowCount - round(Ftimestop - Ftimestart + 2));
      if AsgGToutput.RowCount <> Ftimestop - Ftimestart + 2 {names, units} then
        AsgGToutput.RemoveRows(round(Ftimestop - Ftimestart + 2),
             AsgGToutput.RowCount - round(Ftimestop - Ftimestart + 2));
   // Copy soil moisture, lateral flows, and drainage to driver file
      for k := 1 to 4 do
        begin      // Get source and destination column numbers for variables
          GTWrcol[k] := GetColumnNumber('AsgGToutput', 'sm' + inttostr(k) + '(mm)');
          GTQwH1col[k] := GetColumnNumber('AsgGToutput', 'Q' + inttostr(k) + '_1');
          GTQwH2col[k] := GetColumnNumber('AsgGToutput', 'Q' + inttostr(k) + '_2');
          GTQwH3col[k] := GetColumnNumber('AsgGToutput', 'Q' + inttostr(k) + '_3');
          GTQwH4col[k] := GetColumnNumber('AsgGToutput', 'Q' + inttostr(k) + '_4');
          GTQwH5col[k] := GetColumnNumber('AsgGToutput', 'Q' + inttostr(k) + '_5');
          GTQwH6col[k] := GetColumnNumber('AsgGToutput', 'Q' + inttostr(k) + '_6');
          GTQwH7col[k] := GetColumnNumber('AsgGToutput', 'Q' + inttostr(k) + '_7');
          GTQwH8col[k] := GetColumnNumber('AsgGToutput', 'Q' + inttostr(k) + '_8');
          GTQwd[k] := GetColumnNumber('AsgGToutput', 'G' + inttostr(k) + inttostr(k+1));

          Wrcol[k] := GetColumnNumber('AsgCellDriver', '*D: H2O content' + inttostr(k));
          QwH1col[k] := GetColumnNumber('AsgCellDriver', '*D: FlxH2O lateral H1-' + inttostr(k));
          QwH2col[k] := GetColumnNumber('AsgCellDriver', '*D: FlxH2O lateral H2-' + inttostr(k));
          QwH3col[k] := GetColumnNumber('AsgCellDriver', '*D: FlxH2O lateral H3-' + inttostr(k));
          QwH4col[k] := GetColumnNumber('AsgCellDriver', '*D: FlxH2O lateral H4-' + inttostr(k));
          QwH5col[k] := GetColumnNumber('AsgCellDriver', '*D: FlxH2O lateral H5-' + inttostr(k));
          QwH6col[k] := GetColumnNumber('AsgCellDriver', '*D: FlxH2O lateral H6-' + inttostr(k));
          QwH7col[k] := GetColumnNumber('AsgCellDriver', '*D: FlxH2O lateral H7-' + inttostr(k));
          QwH8col[k] := GetColumnNumber('AsgCellDriver', '*D: FlxH2O lateral H8-' + inttostr(k));
          Qwd[k] := GetColumnNumber('AsgCellDriver', '*D: FlxH2O drainage lay' + inttostr(k));

      // Copy column title and units to GT output so that when I copy the columns over the name and units are right
          if (Wrcol[k] <> -1) then
            if (GTWrcol[k] <> -1) then
             begin
              AsgGToutput.Cells[GTWrcol[k], 0] := AsgCellDriver.Cells[Wrcol[k], 0];
              AsgGToutput.Cells[GTWrcol[k], 1] := AsgCellDriver.Cells[Wrcol[k], 1];
              AsgCellDriver.Cols[Wrcol[k]] := AsgGToutput.Cols[GTWrcol[k]];
             end
            else if k <= GTSoilLayers then
             raise Exception.Create('GT and MEL disagree on number of soil layers, variable ' +
                'GTWrcol[' + inttostr(k) + ']' + '. Run aborted.');
          if (QwH1col[k] <> -1) then
            if GTQwH1col[k] <> -1 then
             begin
              AsgGToutput.Cells[GTQwH1col[k], 0] := AsgCellDriver.Cells[QwH1col[k], 0];
              AsgGToutput.Cells[GTQwH1col[k], 1] := AsgCellDriver.Cells[QwH1col[k], 1];
              AsgCellDriver.Cols[QwH1col[k]] := AsgGToutput.Cols[GTQwH1col[k]];
             end
            else if k <= GTSoilLayers then
             raise Exception.Create('GT and MEL disagree on number of soil layers, variable ' +
                'GTQwH1col[' + inttostr(k) + ']' + '. Run aborted.');

          if (QwH2col[k] <> -1) then
            if GTQwH2col[k] <> -1 then
             begin
              AsgGToutput.Cells[GTQwH2col[k], 0] := AsgCellDriver.Cells[QwH2col[k], 0];
              AsgGToutput.Cells[GTQwH2col[k], 1] := AsgCellDriver.Cells[QwH2col[k], 1];
              AsgCellDriver.Cols[QwH2col[k]] := AsgGToutput.Cols[GTQwH2col[k]];
             end
            else if k <= GTSoilLayers then
             raise Exception.Create('GT and MEL disagree on number of soil layers, variable ' +
                'GTQwH2col[' + inttostr(k) + ']' + '. Run aborted.');
          if (QwH3col[k] <> -1) then
            if GTQwH3col[k] <> -1 then
             begin
              AsgGToutput.Cells[GTQwH3col[k], 0] := AsgCellDriver.Cells[QwH3col[k], 0];
              AsgGToutput.Cells[GTQwH3col[k], 1] := AsgCellDriver.Cells[QwH3col[k], 1];
              AsgCellDriver.Cols[QwH3col[k]] := AsgGToutput.Cols[GTQwH3col[k]];
             end
            else if k <= GTSoilLayers then
             raise Exception.Create('GT and MEL disagree on number of soil layers, variable ' +
                'GTQwH3col[' + inttostr(k) + ']' + '. Run aborted.');
          if (QwH4col[k] <> -1) then
            if GTQwH4col[k] <> -1 then
             begin
              AsgGToutput.Cells[GTQwH4col[k], 0] := AsgCellDriver.Cells[QwH4col[k], 0];
              AsgGToutput.Cells[GTQwH4col[k], 1] := AsgCellDriver.Cells[QwH4col[k], 1];
              AsgCellDriver.Cols[QwH4col[k]] := AsgGToutput.Cols[GTQwH4col[k]];
             end
            else if k <= GTSoilLayers then
             raise Exception.Create('GT and MEL disagree on number of soil layers, variable ' +
                'GTQwH4col[' + inttostr(k) + ']' + '. Run aborted.');
          if (QwH5col[k] <> -1) then
            if GTQwH5col[k] <> -1 then
             begin
              AsgGToutput.Cells[GTQwH5col[k], 0] := AsgCellDriver.Cells[QwH5col[k], 0];
              AsgGToutput.Cells[GTQwH5col[k], 1] := AsgCellDriver.Cells[QwH5col[k], 1];
              AsgCellDriver.Cols[QwH5col[k]] := AsgGToutput.Cols[GTQwH5col[k]];
             end
            else if k <= GTSoilLayers then
             raise Exception.Create('GT and MEL disagree on number of soil layers, variable ' +
                'GTQwH5col[' + inttostr(k) + ']' + '. Run aborted.');
          if (QwH6col[k] <> -1) then
            if GTQwH6col[k] <> -1 then
             begin
              AsgGToutput.Cells[GTQwH6col[k], 0] := AsgCellDriver.Cells[QwH6col[k], 0];
              AsgGToutput.Cells[GTQwH6col[k], 1] := AsgCellDriver.Cells[QwH6col[k], 1];
              AsgCellDriver.Cols[QwH6col[k]] := AsgGToutput.Cols[GTQwH6col[k]];
             end
            else if k <= GTSoilLayers then
             raise Exception.Create('GT and MEL disagree on number of soil layers, variable ' +
                'GTQwH6col[' + inttostr(k) + ']' + '. Run aborted.');
          if (QwH7col[k] <> -1) then
            if GTQwH7col[k] <> -1 then
             begin
              AsgGToutput.Cells[GTQwH7col[k], 0] := AsgCellDriver.Cells[QwH7col[k], 0];
              AsgGToutput.Cells[GTQwH7col[k], 1] := AsgCellDriver.Cells[QwH7col[k], 1];
              AsgCellDriver.Cols[QwH7col[k]] := AsgGToutput.Cols[GTQwH7col[k]];
             end
            else if k <= GTSoilLayers then
             raise Exception.Create('GT and MEL disagree on number of soil layers, variable ' +
                'GTQwH7col[' + inttostr(k) + ']' + '. Run aborted.');
          if (QwH8col[k] <> -1) then
            if GTQwH8col[k] <> -1 then
             begin
              AsgGToutput.Cells[GTQwH8col[k], 0] := AsgCellDriver.Cells[QwH8col[k], 0];
              AsgGToutput.Cells[GTQwH8col[k], 1] := AsgCellDriver.Cells[QwH8col[k], 1];
              AsgCellDriver.Cols[QwH8col[k]] := AsgGToutput.Cols[GTQwH8col[k]];
             end
            else if k <= GTSoilLayers then
             raise Exception.Create('GT and MEL disagree on number of soil layers, variable ' +
                'GTQwH8col[' + inttostr(k) + ']' + '. Run aborted.');
          if (Qwd[k] <> -1) then
            if GTQwd[k] <> -1 then
             begin
              AsgGToutput.Cells[GTQwd[k], 0] := AsgCellDriver.Cells[Qwd[k], 0];
              AsgGToutput.Cells[GTQwd[k], 1] := AsgCellDriver.Cells[Qwd[k], 1];
              AsgCellDriver.Cols[Qwd[k]] := AsgGToutput.Cols[GTQwd[k]];
             end
            else if k <= GTSoilLayers - 1 then
             raise Exception.Create('GT and MEL disagree on number of soil layers, variable ' +
                'GTQwd[' + inttostr(k) + ']' + '. Run aborted.');

    // Copy lateral N and C inputs to driver file grid
          if PCurrCell^.Rnh4l[k].Count <> 0 then    // Assume if one input has values then they all have.
           begin
            Rnh4lcol[k] := GetColumnNumber('AsgCellDriver', '*D: NH4 lateral input' + inttostr(k));
            Rno3lcol[k] := GetColumnNumber('AsgCellDriver', '*D: NO3 lateral input' + inttostr(k));
            Rdonlcol[k] := GetColumnNumber('AsgCellDriver', '*D: DON lateral input' + inttostr(k));
            Rdoclcol[k] := GetColumnNumber('AsgCellDriver', '*D: DOC lateral input' + inttostr(k));
            for j := 1 to AsgCellDriver.RowCount - 1 do
             begin
              AsgCellDriver.Cells[Rnh4lcol[k], j] := PCurrCell^.Rnh4l[k].Strings[j];
              AsgCellDriver.Cells[Rno3lcol[k], j] := PCurrCell^.Rno3l[k].Strings[j];
              AsgCellDriver.Cells[Rdoclcol[k], j] := PCurrCell^.Rdocl[k].Strings[j];
              AsgCellDriver.Cells[Rdonlcol[k], j] := PCurrCell^.Rdonl[k].Strings[j];
             end;
           end;
        end;
      tempstring := PCurrCell^.DriverFile;
      tempstring2 := ExtractFileExt(tempstring);
      delete(tempstring, pos(tempstring2, tempstring), length(tempstring));
      PCurrCell^.DriverFile := tempstring + '_Cell' + inttostr(PCurrCell^.RowPos)
         + '_' + inttostr(PCurrCell^.ColPos) + '.drr';
      AsgCellDriver.SavetoCSV(CurrentDirectory + '\' + PCurrCell^.DriverFile);


   // Run MEL for this cell
      FmShellMain.RunningInteractive := False;  // This tells Shellmain not to show dialog boxes
      Time_start := FTimeStart;
      Time_stop := FTimeStop;
      Paramfilename := PCurrCell^.ParamFile;
      FmShellMain.ChooseParamFile(BtnRun);
      driverfilename := PCurrCell^.DriverFile;
      outfilename := PCurrCell^.OuputFile;
      if PCurrCell^.VegClass = FVegInfo[0].VegType then
         FmOptions.RunOptions := FVegInfo[0].RunOptions
      else if PCurrCell^.VegClass = FVegInfo[1].VegType then
         FmOptions.RunOptions := FVegInfo[1].RunOptions
      else
         raise Exception.Create('No Run options for vegetaton type ' +
            inttostr(PCurrCell^.VegClass) + '. Run aborted.');
      FmOptions.RunOptions := FmOptions.DefaultRunOptions;  // This line is to initialize options that aren't in the batch file yet.
      FmShellMain.BtnRunClick(BtnRun);
   // Get Run output
      FmDisplayData.Filename := outfilename;
      FmDisplayData.UpdateStringGrid;
// Save N flows to eight neighbors
   // Get Cell Locations for headings 1-8
      PCellH1 :=
         FindCellInList(PCurrCell^.RowPos, PCurrCell^.ColPos+1);
      PCellH2 :=
         FindCellInList(PCurrCell^.RowPos+1, PCurrCell^.ColPos+1);
      PCellH3 :=
         FindCellInList(PCurrCell^.RowPos+1, PCurrCell^.ColPos);
      PCellH4 :=
         FindCellInList(PCurrCell^.RowPos+1, PCurrCell^.ColPos-1);
      PCellH5 :=
         FindCellInList(PCurrCell^.RowPos, PCurrCell^.ColPos-1);
      PCellH6 :=
         FindCellInList(PCurrCell^.RowPos-1, PCurrCell^.ColPos-1);
      PCellH7 :=
         FindCellInList(PCurrCell^.RowPos-1, PCurrCell^.ColPos);
      PCellH8 :=
         FindCellInList(PCurrCell^.RowPos-1, PCurrCell^.ColPos+1);
    // Copy N flow to appropriate cellProp item
     // H1
      for k := 1 to 4 do
       begin
        if PCellH1 <> nil then
         begin
          Lnh4lcol[k] := GetColumnNumber('sgmodeloutput', '*N NH4 loss lateral H1-'
           + inttostr(k));
          Lno3lcol[k] := GetColumnNumber('sgmodeloutput', '*N NO3 loss lateral H1-'
           + inttostr(k));
          Ldoclcol[k] := GetColumnNumber('sgmodeloutput', '*C DOC loss lateral H1-'
           + inttostr(k));
          Ldonlcol[k] := GetColumnNumber('sgmodeloutput', '*N DON loss lateral H1-'
           + inttostr(k));
          if (PCellH1^.Rnh4l[k].Count = 0) and (Lnh4lcol[k] <> -1) then
           begin
            PCellH1^.Rnh4l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lnh4lcol[k]]);
            PCellH1^.Rnh4l[k].Delete(0);
           end
          else if (Lnh4lcol[k] <> -1) then
           for j := 2 to PCellH1^.Rnh4l[k].Count - 1 do
             PCellH1^.Rnh4l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH1^.Rnh4l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lnh4lcol[k],j+1]));
          if (PCellH1^.Rno3l[k].Count = 0) and (Lno3lcol[k] <> -1) then
           begin
            PCellH1^.Rno3l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lno3lcol[k]]);
            PCellH1^.Rno3l[k].Delete(0);
           end
          else if (Lno3lcol[k] <> -1) then
           for j := 2 to PCellH1^.Rno3l[k].Count - 1 do
             PCellH1^.Rno3l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH1^.Rno3l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lno3lcol[k],j+1]));
          if (PCellH1^.Rdocl[k].Count = 0) and (Ldoclcol[k] <> -1) then
           begin
            PCellH1^.Rdocl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldoclcol[k]]);
            PCellH1^.Rdocl[k].Delete(0);
           end
          else if (Ldoclcol[k] <> -1) then
           for j := 2 to PCellH1^.Rdocl[k].Count - 1 do
             PCellH1^.Rdocl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH1^.Rdocl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldoclcol[k],j+1]));
          if (PCellH1^.Rdonl[k].Count = 0) and (Ldonlcol[k] <> -1) then
           begin
            PCellH1^.Rdonl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldonlcol[k]]);
            PCellH1^.Rdonl[k].Delete(0);
           end
          else if (Ldonlcol[k] <> -1) then
           for j := 2 to PCellH1^.Rdonl[k].Count - 1 do
             PCellH1^.Rdonl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH1^.Rdonl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldonlcol[k],j+1]));
         end;

     // H2
        if PCellH2 <> nil then
         begin
          Lnh4lcol[k] := GetColumnNumber('sgmodeloutput', '*N NH4 loss lateral H2-'
           + inttostr(k));
          Lno3lcol[k] := GetColumnNumber('sgmodeloutput', '*N NO3 loss lateral H2-'
           + inttostr(k));
          Ldoclcol[k] := GetColumnNumber('sgmodeloutput', '*C DOC loss lateral H2-'
           + inttostr(k));
          Ldonlcol[k] := GetColumnNumber('sgmodeloutput', '*N DON loss lateral H2-'
           + inttostr(k));
          if (PCellH2^.Rnh4l[k].Count = 0) and (Lnh4lcol[k] <> -1) then
           begin
            PCellH2^.Rnh4l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lnh4lcol[k]]);
            PCellH2^.Rnh4l[k].Delete(0);
           end
          else if (Lnh4lcol[k] <> -1) then
           for j := 2 to PCellH2^.Rnh4l[k].Count - 1 do
             PCellH2^.Rnh4l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH2^.Rnh4l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lnh4lcol[k],j+1]));
          if (PCellH2^.Rno3l[k].Count = 0) and (Lno3lcol[k] <> -1) then
           begin
            PCellH2^.Rno3l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lno3lcol[k]]);
            PCellH2^.Rno3l[k].Delete(0);
           end
          else if (Lno3lcol[k] <> -1) then
           for j := 2 to PCellH2^.Rno3l[k].Count - 1 do
             PCellH2^.Rno3l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH2^.Rno3l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lno3lcol[k],j+1]));
          if (PCellH2^.Rdocl[k].Count = 0) and (Ldoclcol[k] <> -1) then
           begin
            PCellH2^.Rdocl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldoclcol[k]]);
            PCellH2^.Rdocl[k].Delete(0);
           end
          else if (Ldoclcol[k] <> -1) then
           for j := 2 to PCellH2^.Rdocl[k].Count - 1 do
             PCellH2^.Rdocl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH2^.Rdocl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldoclcol[k],j+1]));
          if (PCellH2^.Rdonl[k].Count = 0) and (Ldonlcol[k] <> -1) then
           begin
            PCellH2^.Rdonl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldonlcol[k]]);
            PCellH2^.Rdonl[k].Delete(0);
           end
          else if (Ldonlcol[k] <> -1) then
           for j := 2 to PCellH2^.Rdonl[k].Count - 1 do
             PCellH2^.Rdonl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH2^.Rdonl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldonlcol[k],j+1]));
         end;

     // H3
        if PCellH3 <> nil then
         begin
          Lnh4lcol[k] := GetColumnNumber('sgmodeloutput', '*N NH4 loss lateral H3-'
           + inttostr(k));
          Lno3lcol[k] := GetColumnNumber('sgmodeloutput', '*N NO3 loss lateral H3-'
           + inttostr(k));
          Ldoclcol[k] := GetColumnNumber('sgmodeloutput', '*C DOC loss lateral H3-'
           + inttostr(k));
          Ldonlcol[k] := GetColumnNumber('sgmodeloutput', '*N DON loss lateral H3-'
           + inttostr(k));
          if (PCellH3^.Rnh4l[k].Count = 0) and (Lnh4lcol[k] <> -1) then
           begin
            PCellH3^.Rnh4l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lnh4lcol[k]]);
            PCellH3^.Rnh4l[k].Delete(0);
           end
          else if (Lnh4lcol[k] <> -1) then
           for j := 2 to PCellH3^.Rnh4l[k].Count - 1 do
             PCellH3^.Rnh4l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH3^.Rnh4l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lnh4lcol[k],j+1]));
          if (PCellH3^.Rno3l[k].Count = 0) and (Lno3lcol[k] <> -1) then
           begin
            PCellH3^.Rno3l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lno3lcol[k]]);
            PCellH3^.Rno3l[k].Delete(0);
           end
          else if (Lno3lcol[k] <> -1) then
           for j := 2 to PCellH3^.Rno3l[k].Count - 1 do
             PCellH3^.Rno3l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH3^.Rno3l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lno3lcol[k],j+1]));
          if (PCellH3^.Rdocl[k].Count = 0) and (Ldoclcol[k] <> -1) then
           begin
            PCellH3^.Rdocl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldoclcol[k]]);
            PCellH3^.Rdocl[k].Delete(0);
           end
          else if (Ldoclcol[k] <> -1) then
           for j := 2 to PCellH3^.Rdocl[k].Count - 1 do
             PCellH3^.Rdocl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH3^.Rdocl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldoclcol[k],j+1]));
          if (PCellH3^.Rdonl[k].Count = 0) and (Ldonlcol[k] <> -1) then
           begin
            PCellH3^.Rdonl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldonlcol[k]]);
            PCellH3^.Rdonl[k].Delete(0);
           end
          else if (Ldonlcol[k] <> -1) then
           for j := 2 to PCellH3^.Rdonl[k].Count - 1 do
             PCellH3^.Rdonl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH3^.Rdonl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldonlcol[k],j+1]));
         end;

     // H4
        if PCellH4 <> nil then
         begin
          Lnh4lcol[k] := GetColumnNumber('sgmodeloutput', '*N NH4 loss lateral H4-'
           + inttostr(k));
          Lno3lcol[k] := GetColumnNumber('sgmodeloutput', '*N NO3 loss lateral H4-'
           + inttostr(k));
          Ldoclcol[k] := GetColumnNumber('sgmodeloutput', '*C DOC loss lateral H4-'
           + inttostr(k));
          Ldonlcol[k] := GetColumnNumber('sgmodeloutput', '*N DON loss lateral H4-'
           + inttostr(k));
          if (PCellH4^.Rnh4l[k].Count = 0) and (Lnh4lcol[k] <> -1) then
           begin
            PCellH4^.Rnh4l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lnh4lcol[k]]);
            PCellH4^.Rnh4l[k].Delete(0);
           end
          else if (Lnh4lcol[k] <> -1) then
           for j := 2 to PCellH4^.Rnh4l[k].Count - 1 do
             PCellH4^.Rnh4l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH4^.Rnh4l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lnh4lcol[k],j+1]));
          if (PCellH4^.Rno3l[k].Count = 0) and (Lno3lcol[k] <> -1) then
           begin
            PCellH4^.Rno3l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lno3lcol[k]]);
            PCellH4^.Rno3l[k].Delete(0);
           end
          else if (Lno3lcol[k] <> -1) then
           for j := 2 to PCellH4^.Rno3l[k].Count - 1 do
             PCellH4^.Rno3l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH4^.Rno3l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lno3lcol[k],j+1]));
          if (PCellH4^.Rdocl[k].Count = 0) and (Ldoclcol[k] <> -1) then
           begin
            PCellH4^.Rdocl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldoclcol[k]]);
            PCellH4^.Rdocl[k].Delete(0);
           end
          else if (Ldoclcol[k] <> -1) then
           for j := 2 to PCellH4^.Rdocl[k].Count - 1 do
             PCellH4^.Rdocl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH4^.Rdocl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldoclcol[k],j+1]));
          if (PCellH4^.Rdonl[k].Count = 0) and (Ldonlcol[k] <> -1) then
           begin
            PCellH4^.Rdonl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldonlcol[k]]);
            PCellH4^.Rdonl[k].Delete(0);
           end
          else if (Ldonlcol[k] <> -1) then
           for j := 2 to PCellH4^.Rdonl[k].Count - 1 do
             PCellH4^.Rdonl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH4^.Rdonl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldonlcol[k],j+1]));
         end;

     // H5
        if PCellH5 <> nil then
         begin
          Lnh4lcol[k] := GetColumnNumber('sgmodeloutput', '*N NH4 loss lateral H5-'
           + inttostr(k));
          Lno3lcol[k] := GetColumnNumber('sgmodeloutput', '*N NO3 loss lateral H5-'
           + inttostr(k));
          Ldoclcol[k] := GetColumnNumber('sgmodeloutput', '*C DOC loss lateral H5-'
           + inttostr(k));
          Ldonlcol[k] := GetColumnNumber('sgmodeloutput', '*N DON loss lateral H5-'
           + inttostr(k));
          if (PCellH5^.Rnh4l[k].Count = 0) and (Lnh4lcol[k] <> -1) then
           begin
            PCellH5^.Rnh4l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lnh4lcol[k]]);
            PCellH5^.Rnh4l[k].Delete(0);
           end
          else if (Lnh4lcol[k] <> -1) then
           for j := 2 to PCellH5^.Rnh4l[k].Count - 1 do
             PCellH5^.Rnh4l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH5^.Rnh4l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lnh4lcol[k],j+1]));
          if (PCellH5^.Rno3l[k].Count = 0) and (Lno3lcol[k] <> -1) then
           begin
            PCellH5^.Rno3l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lno3lcol[k]]);
            PCellH5^.Rno3l[k].Delete(0);
           end
          else if (Lno3lcol[k] <> -1) then
           for j := 2 to PCellH5^.Rno3l[k].Count - 1 do
             PCellH5^.Rno3l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH5^.Rno3l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lno3lcol[k],j+1]));
          if (PCellH5^.Rdocl[k].Count = 0) and (Ldoclcol[k] <> -1) then
           begin
            PCellH5^.Rdocl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldoclcol[k]]);
            PCellH5^.Rdocl[k].Delete(0);
           end
          else if (Ldoclcol[k] <> -1) then
           for j := 2 to PCellH5^.Rdocl[k].Count - 1 do
             PCellH5^.Rdocl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH5^.Rdocl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldoclcol[k],j+1]));
          if (PCellH5^.Rdonl[k].Count = 0) and (Ldonlcol[k] <> -1) then
           begin
            PCellH5^.Rdonl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldonlcol[k]]);
            PCellH5^.Rdonl[k].Delete(0);
           end
          else if (Ldonlcol[k] <> -1) then
           for j := 2 to PCellH5^.Rdonl[k].Count - 1 do
             PCellH5^.Rdonl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH5^.Rdonl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldonlcol[k],j+1]));
         end;

     // H6
        if PCellH6 <> nil then
         begin
          Lnh4lcol[k] := GetColumnNumber('sgmodeloutput', '*N NH4 loss lateral H6-'
           + inttostr(k));
          Lno3lcol[k] := GetColumnNumber('sgmodeloutput', '*N NO3 loss lateral H6-'
           + inttostr(k));
          Ldoclcol[k] := GetColumnNumber('sgmodeloutput', '*C DOC loss lateral H6-'
           + inttostr(k));
          Ldonlcol[k] := GetColumnNumber('sgmodeloutput', '*N DON loss lateral H6-'
           + inttostr(k));
          if (PCellH6^.Rnh4l[k].Count = 0) and (Lnh4lcol[k] <> -1) then
           begin
            PCellH6^.Rnh4l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lnh4lcol[k]]);
            PCellH6^.Rnh4l[k].Delete(0);
           end
          else if (Lnh4lcol[k] <> -1) then
           for j := 2 to PCellH6^.Rnh4l[k].Count - 1 do
             PCellH6^.Rnh4l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH6^.Rnh4l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lnh4lcol[k],j+1]));
          if (PCellH6^.Rno3l[k].Count = 0) and (Lno3lcol[k] <> -1) then
           begin
            PCellH6^.Rno3l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lno3lcol[k]]);
            PCellH6^.Rno3l[k].Delete(0);
           end
          else if (Lno3lcol[k] <> -1) then
           for j := 2 to PCellH6^.Rno3l[k].Count - 1 do
             PCellH6^.Rno3l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH6^.Rno3l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lno3lcol[k],j+1]));
          if (PCellH6^.Rdocl[k].Count = 0) and (Ldoclcol[k] <> -1) then
           begin
            PCellH6^.Rdocl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldoclcol[k]]);
            PCellH6^.Rdocl[k].Delete(0);
           end
          else if (Ldoclcol[k] <> -1) then
           for j := 2 to PCellH6^.Rdocl[k].Count - 1 do
             PCellH6^.Rdocl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH6^.Rdocl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldoclcol[k],j+1]));
          if (PCellH6^.Rdonl[k].Count = 0) and (Ldonlcol[k] <> -1) then
           begin
            PCellH6^.Rdonl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldonlcol[k]]);
            PCellH6^.Rdonl[k].Delete(0);
           end
          else if (Ldonlcol[k] <> -1) then
           for j := 2 to PCellH6^.Rdonl[k].Count - 1 do
             PCellH6^.Rdonl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH6^.Rdonl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldonlcol[k],j+1]));
         end;

     // H7
        if PCellH7 <> nil then
         begin
          Lnh4lcol[k] := GetColumnNumber('sgmodeloutput', '*N NH4 loss lateral H7-'
           + inttostr(k));
          Lno3lcol[k] := GetColumnNumber('sgmodeloutput', '*N NO3 loss lateral H7-'
           + inttostr(k));
          Ldoclcol[k] := GetColumnNumber('sgmodeloutput', '*C DOC loss lateral H7-'
           + inttostr(k));
          Ldonlcol[k] := GetColumnNumber('sgmodeloutput', '*N DON loss lateral H7-'
           + inttostr(k));
          if (PCellH7^.Rnh4l[k].Count = 0) and (Lnh4lcol[k] <> -1) then
           begin
            PCellH7^.Rnh4l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lnh4lcol[k]]);
            PCellH7^.Rnh4l[k].Delete(0);
           end
          else if (Lnh4lcol[k] <> -1) then
           for j := 2 to PCellH7^.Rnh4l[k].Count - 1 do
             PCellH7^.Rnh4l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH7^.Rnh4l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lnh4lcol[k],j+1]));
          if (PCellH7^.Rno3l[k].Count = 0) and (Lno3lcol[k] <> -1) then
           begin
            PCellH7^.Rno3l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lno3lcol[k]]);
            PCellH7^.Rno3l[k].Delete(0);
           end
          else if (Lno3lcol[k] <> -1) then
           for j := 2 to PCellH7^.Rno3l[k].Count - 1 do
             PCellH7^.Rno3l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH7^.Rno3l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lno3lcol[k],j+1]));
          if (PCellH7^.Rdocl[k].Count = 0) and (Ldoclcol[k] <> -1) then
           begin
            PCellH7^.Rdocl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldoclcol[k]]);
            PCellH7^.Rdocl[k].Delete(0);
           end
          else if (Ldoclcol[k] <> -1) then
           for j := 2 to PCellH7^.Rdocl[k].Count - 1 do
             PCellH7^.Rdocl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH7^.Rdocl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldoclcol[k],j+1]));
          if (PCellH7^.Rdonl[k].Count = 0) and (Ldonlcol[k] <> -1) then
           begin
            PCellH7^.Rdonl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldonlcol[k]]);
            PCellH7^.Rdonl[k].Delete(0);
           end
          else if (Ldonlcol[k] <> -1) then
           for j := 2 to PCellH7^.Rdonl[k].Count - 1 do
             PCellH7^.Rdonl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH7^.Rdonl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldonlcol[k],j+1]));
         end;

     // H8
        if PCellH8 <> nil then
         begin
          Lnh4lcol[k] := GetColumnNumber('sgmodeloutput', '*N NH4 loss lateral H8-'
           + inttostr(k));
          Lno3lcol[k] := GetColumnNumber('sgmodeloutput', '*N NO3 loss lateral H8-'
           + inttostr(k));
          Ldoclcol[k] := GetColumnNumber('sgmodeloutput', '*C DOC loss lateral H8-'
           + inttostr(k));
          Ldonlcol[k] := GetColumnNumber('sgmodeloutput', '*N DON loss lateral H8-'
           + inttostr(k));
          if (PCellH8^.Rnh4l[k].Count = 0) and (Lnh4lcol[k] <> -1) then
           begin
            PCellH8^.Rnh4l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lnh4lcol[k]]);
            PCellH8^.Rnh4l[k].Delete(0);
           end
          else if (Lnh4lcol[k] <> -1) then
            for j := 2 to PCellH8^.Rnh4l[k].Count - 1 do
             PCellH8^.Rnh4l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH8^.Rnh4l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lnh4lcol[k],j+1]));
          if (PCellH8^.Rno3l[k].Count = 0) and (Lno3lcol[k] <> -1) then
           begin
            PCellH8^.Rno3l[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Lno3lcol[k]]);
            PCellH8^.Rno3l[k].Delete(0);
           end
          else if (Lno3lcol[k] <> -1) then
            for j := 2 to PCellH8^.Rno3l[k].Count - 1 do
             PCellH8^.Rno3l[k].Strings[j] :=
                  floattostr(strtofloat(PCellH8^.Rno3l[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Lno3lcol[k],j+1]));
          if (PCellH8^.Rdocl[k].Count = 0) and (Ldoclcol[k] <> -1) then
           begin
            PCellH8^.Rdocl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldoclcol[k]]);
            PCellH8^.Rdocl[k].Delete(0);
           end
          else if (Ldoclcol[k] <> -1) then
            for j := 2 to PCellH8^.Rdocl[k].Count - 1 do
             PCellH8^.Rdocl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH8^.Rdocl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldoclcol[k],j+1]));
          if (PCellH8^.Rdonl[k].Count = 0) and (Ldonlcol[k] <> -1) then
           begin
            PCellH8^.Rdonl[k].AddStrings(FmDisplayData.SgModelOutput.Cols[Ldonlcol[k]]);
            PCellH8^.Rdonl[k].Delete(0);
           end
          else if (Ldonlcol[k] <> -1) then
            for j := 2 to PCellH8^.Rdonl[k].Count - 1 do
             PCellH8^.Rdonl[k].Strings[j] :=
                  floattostr(strtofloat(PCellH8^.Rdonl[k].Strings[j]) +
                  strtofloat(FmDisplayData.SgModelOutput.Cells[Ldonlcol[k],j+1]));
         end;
       end;
     end;
  finally
   Dispose(PCurrCell);
   if PCellH1 <> nil then Dispose(PCellH1);
   if PCellH2 <> nil then Dispose(PCellH2);
   if PCellH3 <> nil then Dispose(PCellH3);
   if PCellH4 <> nil then Dispose(PCellH4);
   if PCellH5 <> nil then Dispose(PCellH5);
   if PCellH6 <> nil then Dispose(PCellH6);
   if PCellH7 <> nil then Dispose(PCellH7);
   if PCellH8 <> nil then Dispose(PCellH8);
   BtnRun.Caption := 'Done';
  end;
end;

function TFmGridMEL.FindCellInList(RowNum, Colnum: integer): PCellProp;
var
 j: integer;
 cellfound: Boolean;
 PCurrCell: PCellProp;
begin
 j := 0;
 cellfound := false;
 repeat
   PCurrCell := FCellInfoList[j];
   if (PCurrCell.rowpos = rownum) and (PCurrCell.colpos = colnum) then
     cellfound := True;
   j := j + 1;
 until cellfound or (j = FCellInfoList.Count - 1);
 if cellfound then result := PCurrCell else result := nil;
end;

procedure TFmGridMEL.UpdateForm(Sender: TObject);
begin
  //check if files selected
  // update currentdirectory.
  CurrentDirectory := GetCurrentDir;
  if {(LedDEMFile.text <> '') and (LedVegClassFile.text <> '') and
     (LedSoilClassFile.text <> '') and (LedVeg1ParamFile.text <> '') and
     (LedVeg2ParamFile.text <> '') and (LedSoilPropFile.text <> '') and }
     (LedCellOrderFile.text <> ''){ and (LedDriver1File.Text <> '') and
     (LedDriver2File.Text <> '') }and (LedRunOptionsFile.text <> '')
     then BtnRun.Enabled := True;
end;

procedure TFmGridMEL.ReadCellOrderFile;
var
 CellOrderFile, Aline: TStringList;
 PACell: ^TCellProp;
 j, k: integer;
begin
  CellOrderFile := TstringList.create;
  Aline := Tstringlist.Create;
  Aline.Delimiter := ',';
  try
    CellOrderFile.LoadFromFile(FCellOrderFilename);
    for j := 1 to CellOrderFile.Count - 1 do  // Skip first line of file, it contains column headings
      begin
        Aline.DelimitedText := CellOrderFile[j];
        New(PACell);
        PACell^.RowPos := strtoint(Aline[0]);
        PACell^.ColPos := strtoint(Aline[1]);
        PACell^.elevation := strtofloat(Aline[2]);
        // FIX Need to enter Runposition here
{        PACellInfoItem^.SoilClass := Aline[3];
        PACellInfoItem^. := Aline[4];
        PACellInfoItem^.RowPos := Aline[5];
        PACellInfoItem^.RowPos := Aline[6];
        PACellInfoItem^.RowPos := Aline[7];        }
        PACell^.VegClass := strtoint(Aline[8]);
        case strtoint(Aline[8]) {VegClass} of
          501: PACell^.ParamFile := FVegInfo[0].Filenames.ParamFilename;
          502: PACell^.ParamFile := FVegInfo[1].Filenames.ParamFilename;
        end;
        case strtoint(Aline[8]) {VegClass} of
          501: PACell^.DriverFile := FVegInfo[0].Filenames.DriverFilename;
          502: PACell^.DriverFile := FVegInfo[1].Filenames.DriverFilename;
        end;
        PACell^.OuputFile := CurrentDirectory +
                '\Cell' + Aline[0] + '_' + Aline[1] + '.out';
        for k := 1 to 4 do    // Create lists to hold lateral inputs
         begin                
          PACell^.RDocl[k] := Tstringlist.Create;
          PACell^.RDonl[k] := Tstringlist.Create;
          PACell^.Rnh4l[k] := Tstringlist.Create;
          PACell^.Rno3l[k] := Tstringlist.Create;
         end;
        FCellInfoList.Add(PACell);
      end;
  finally
//   dispose(PACell);
   CellOrderFile.Clear;
   CellOrderFile.Free;
   Aline.Clear;
   Aline.Free;
  end;
end;

procedure TFmGridMEL.ReadRunOptionsFile;
var
 RunOptionsFile, Aline: TStringList;
 j: integer;
begin
  RunOptionsFile := TstringList.create;
  Aline := Tstringlist.Create;
  Aline.Delimiter := ',';
  try
    RunOptionsFile.LoadFromFile(FRunOptionsFilename);
    for j := 0 to RunOptionsFile.Count - 1 - 1 do  // One less than total count because first line of file is column headings
      begin
        Aline.DelimitedText := RunOptionsFile[j+1];  //Skip first line of the file because it contains column headings
        FVegInfo[j].VegType := strtoint(Aline[0]);
        FVegInfo[j].Filenames.ParamFilename := Aline[1];
        FVegInfo[j].Filenames.DriverFilename := Aline[2];
        FTimeStart := strtofloat(Aline[4]);
        FTimeStop := strtofloat(Aline[5]);
        FVegInfo[j].RunOptions.Time_step := strtofloat(Aline[6]);
        FVegInfo[j].RunOptions.DiscreteStep := strtofloat(Aline[7]);
        if (Aline[8][1] = 't') or (Aline[8][1] = 'T') then
           FVegInfo[j].RunOptions.NormalRun := True
        else
           FVegInfo[j].RunOptions.NormalRun := False;
        if (Aline[9][1] = 't') or (Aline[9][1] = 'T') then
           FVegInfo[j].RunOptions.RepeatDrivers := True
        else
           FVegInfo[j].RunOptions.RepeatDrivers := False;
//        FVegInfo[j].RunOptions.RepeatDrivers := Aline[9];
        FVegInfo[j].RunOptions.RepeatDriveTIme := strtoint(Aline[10]);
        if (Aline[11][1] = 't') or (Aline[11][1] = 'T') then
           FVegInfo[j].RunOptions.ResetStates := True
        else
           FVegInfo[j].RunOptions.ResetStates := False;
//        FVegInfo[j].RunOptions.ResetStates := Aline[11];
        FVegInfo[j].RunOptions.ResetStateTime := strtoint(Aline[12]);
        if (Aline[13][1] = 't') or (Aline[13][1] = 'T') then
           FVegInfo[j].RunOptions.RuntoSS := True
        else
           FVegInfo[j].RunOptions.RuntoSS := False;
//        FVegInfo[j].RunOptions.RuntoSS := Aline[13];
        FVegInfo[j].RunOptions.SSCriteria := strtofloat(Aline[14]);
        FVegInfo[j].RunOptions.SSTime := strtoint(Aline[15]);
        if (Aline[16][1] = 't') or (Aline[16][1] = 'T') then
           FVegInfo[j].RunOptions.HoldStatesConstant := True
        else
           FVegInfo[j].RunOptions.HoldStatesConstant := False;
//        FVegInfo[j].RunOptions.HoldStatesConstant := Aline[16];
        FVegInfo[j].RunOptions.Outputstep := strtoint(Aline[17]);
        FVegInfo[j].RunOptions.Outputoffset := strtoint(Aline[18]);
        if (Aline[17][1] = 't') or (Aline[17][1] = 'T') then
           FVegInfo[j].RunOptions.OutputEORonly := True
        else
           FVegInfo[j].RunOptions.OutputEORonly := False;
//        FVegInfo[j].RunOptions.OutputEORonly := Aline[19];
//        FVegInfo[j].RunOptions.OutputAnnually := Aline[7];
//        FVegInfo[j].RunOptions.OutputAnnuallyDay := Aline[7];
        if (Aline[20][1] = 't') or (Aline[20][1] = 'T') then
           FVegInfo[j].RunOptions.AppendOutputFile := True
        else
           FVegInfo[j].RunOptions.AppendOutputFile := False;
//        FVegInfo[j].RunOptions.AppendOutputFile := Aline[20];
//        FVegInfo[j].RunOptions.StepCounter := Aline[7];
 //       FVegInfo[j].RunOptions.OutCounter := Aline[7];
        FVegInfo[j].RunOptions.errormult := strtoint(Aline[21]);
      end;
  finally
   RunOptionsFile.Free;
   Aline.Free;
  end;
end;

procedure TFmGridMEL.LedCellOrderFileKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseCellOrderFile(Sender);
end;

procedure TFmGridMEL.LedDEMFileKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseDEMFile(Sender);
end;

procedure TFmGridMEL.LedDriver1FileKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseDriver1File(Sender);
end;

procedure TFmGridMEL.LedDriver2FileKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseDriver2File(Sender);
end;

procedure TFmGridMEL.LedRunOptionsFileKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseRunOptionsFile(Sender);
end;

procedure TFmGridMEL.LedSoilClassFileKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseSoilClassFile(Sender);
end;

procedure TFmGridMEL.LedSoilPropFileKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseSoilPropFile(Sender);
end;

procedure TFmGridMEL.LedVeg1ParamFileKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseVeg1ParamFile(Sender);
end;

procedure TFmGridMEL.LedVeg2ParamFileKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseVeg2ParamFile(Sender);
end;

procedure TFmGridMEL.LedVegClassFileKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = Chr(13)) then ChooseVegClassFile(Sender);
end;

procedure TFmGridMEL.ChooseDEMFile(Sender: TObject);
var
 ThisEdit:TLabeledEdit;
 ThisLabel: TBoundLabel;
begin
// If the user typed directly in the edit box
 if Sender is TLabeledEdit then
  begin
   ThisEdit := Sender as TLabeledEdit;
   FDEMFilename := ThisEdit.EditLabel.Caption;
   if not FileExists(FDEMFilename) then
    begin
     MessageDlg('Missing Data File: ' + FDEMFilename + '. Please reenter.',
                  mtError, [mbOK], 0);
     FDEMFilename := '';  // Set the filename to no value
     ThisEdit.EditLabel.Caption := FDEMFilename;
    end;
  end
 else  // User clicked on the label
  begin   // Show the open file dialog
   ThisLabel := Sender as TBoundLabel;
   ThisEdit := ThisLabel.Owner as TLabeledEdit;
   OpenDialog.Title := 'Select the DEM file';
   OpenDialog.DefaultExt := '.dat';
   OpenDialog.InitialDir := CurrentDirectory;
   OpenDialog.Options := [ofFileMustExist];
    // First set the default filename to the current file
   OpenDialog.filename := FDEMFilename;
    // Show the dialog and if the user clicks OK, set the new filename
   if OpenDialog.execute then
    begin
      FDEMFilename := OpenDialog.filename;
      ThisEdit.Text := FDEMFilename;
    end;
  end;
 UpdateForm(Sender);
end;

procedure TFmGridMEL.ChooseVegClassFile(Sender: TObject);
var
 ThisEdit:TLabeledEdit;
 ThisLabel: TBoundLabel;
begin
// If the user typed directly in the edit box
 if Sender is TLabeledEdit then
  begin
   ThisEdit := Sender as TLabeledEdit;
   FVegClassFilename := ThisEdit.EditLabel.Caption;
   if not FileExists(FVegClassFilename) then
    begin
     MessageDlg('Missing Data File: ' + FVegClassFilename + '. Please reenter.',
                  mtError, [mbOK], 0);
     FVegClassFilename := '';  // Set the filename to no value
     ThisEdit.EditLabel.Caption := FVegClassFilename;
    end;
  end
 else  // User clicked on the label
  begin   // Show the open file dialog
   ThisLabel := Sender as TBoundLabel;
   ThisEdit := ThisLabel.Owner as TLabeledEdit;
   OpenDialog.Title := 'Select the vegetation classification file';
   OpenDialog.DefaultExt := '.dat';
   OpenDialog.InitialDir := CurrentDirectory;
   OpenDialog.Options := [ofFileMustExist];
    // First set the default filename to the current file
   OpenDialog.filename := FVegClassFilename;
    // Show the dialog and if the user clicks OK, set the new filename
   if OpenDialog.execute then
    begin
      FVegClassFilename := OpenDialog.filename;
      ThisEdit.Text := FVegClassFilename;
    end;
  end;
 UpdateForm(Sender);
end;

procedure TFmGridMEL.ChooseVeg1ParamFile(Sender: TObject);
var
 ThisEdit:TLabeledEdit;
 ThisLabel: TBoundLabel;
begin
// If the user typed directly in the edit box
 if Sender is TLabeledEdit then
  begin
   ThisEdit := Sender as TLabeledEdit;
   FVeg1ParamFilename := ThisEdit.EditLabel.Caption;
   if not FileExists(FVeg1ParamFilename) then
    begin
     MessageDlg('Missing Data File: ' + FVeg1ParamFilename + '. Please reenter.',
                  mtError, [mbOK], 0);
     FVeg1ParamFilename := '';  // Set the filename to no value
     ThisEdit.EditLabel.Caption := FVeg1ParamFilename;
    end;
  end
 else  // User clicked on the label
  begin   // Show the open file dialog
   ThisLabel := Sender as TBoundLabel;
   ThisEdit := ThisLabel.Owner as TLabeledEdit;
   OpenDialog.Title := 'Select the parameter file for vegetation class 1';
   OpenDialog.DefaultExt := '.dat';
   OpenDialog.InitialDir := CurrentDirectory;
   OpenDialog.Options := [ofFileMustExist];
    // First set the default filename to the current file
   OpenDialog.filename := FVeg1ParamFilename;
    // Show the dialog and if the user clicks OK, set the new filename
   if OpenDialog.execute then
    begin
      FVeg1ParamFilename := OpenDialog.filename;
      ThisEdit.Text := FVeg1ParamFilename;
    end;
  end;
 UpdateForm(Sender);
end;

procedure TFmGridMEL.ChooseVeg2ParamFile(Sender: TObject);
var
 ThisEdit:TLabeledEdit;
 ThisLabel: TBoundLabel;
begin
// If the user typed directly in the edit box
 if Sender is TLabeledEdit then
  begin
   ThisEdit := Sender as TLabeledEdit;
   FVeg2ParamFilename := ThisEdit.EditLabel.Caption;
   if not FileExists(FVeg2ParamFilename) then
    begin
     MessageDlg('Missing Data File: ' + FVeg2ParamFilename + '. Please reenter.',
                  mtError, [mbOK], 0);
     FVeg2ParamFilename := '';  // Set the filename to no value
     ThisEdit.EditLabel.Caption := FVeg2ParamFilename;
    end;
  end
 else  // User clicked on the label
  begin   // Show the open file dialog
   ThisLabel := Sender as TBoundLabel;
   ThisEdit := ThisLabel.Owner as TLabeledEdit;
   OpenDialog.Title := 'Select the parameter file for vegetation class 2';
   OpenDialog.DefaultExt := '.dat';
   OpenDialog.InitialDir := CurrentDirectory;
   OpenDialog.Options := [ofFileMustExist];
    // First set the default filename to the current file
   OpenDialog.filename := FVeg2ParamFilename;
    // Show the dialog and if the user clicks OK, set the new filename
   if OpenDialog.execute then
    begin
      FVeg2ParamFilename := OpenDialog.filename;
      ThisEdit.Text := FVeg2ParamFilename;
    end;
  end;
 UpdateForm(Sender);
end;

procedure TFmGridMEL.ChooseSoilClassFile(Sender: TObject);
var
 ThisEdit:TLabeledEdit;
 ThisLabel: TBoundLabel;
begin
// If the user typed directly in the edit box
 if Sender is TLabeledEdit then
  begin
   ThisEdit := Sender as TLabeledEdit;
   FSoilClassFilename := ThisEdit.EditLabel.Caption;
   if not FileExists(FSoilClassFilename) then
    begin
     MessageDlg('Missing Data File: ' + FSoilClassFilename + '. Please reenter.',
                  mtError, [mbOK], 0);
     FSoilClassFilename := '';  // Set the filename to no value
     ThisEdit.EditLabel.Caption := FSoilClassFilename;
    end;
  end
 else  // User clicked on the label
  begin   // Show the open file dialog
   ThisLabel := Sender as TBoundLabel;
   ThisEdit := ThisLabel.Owner as TLabeledEdit;
   OpenDialog.Title := 'Select the soil classification file';
   OpenDialog.DefaultExt := '.dat';
   OpenDialog.InitialDir := CurrentDirectory;
   OpenDialog.Options := [ofFileMustExist];
    // First set the default filename to the current file
   OpenDialog.filename := FSoilClassFilename;
    // Show the dialog and if the user clicks OK, set the new filename
   if OpenDialog.execute then
    begin
      FSoilClassFilename := OpenDialog.filename;
      ThisEdit.Text := FSoilClassFilename;
    end;
  end;
 UpdateForm(Sender);
end;

procedure TFmGridMEL.ChooseSoilPropFile(Sender: TObject);
var
 ThisEdit:TLabeledEdit;
 ThisLabel: TBoundLabel;
begin
// If the user typed directly in the edit box
 if Sender is TLabeledEdit then
  begin
   ThisEdit := Sender as TLabeledEdit;
   FSoilPropFilename := ThisEdit.EditLabel.Caption;
   if not FileExists(FSoilPropFilename) then
    begin
     MessageDlg('Missing Data File: ' + FSoilPropFilename + '. Please reenter.',
                  mtError, [mbOK], 0);
     FSoilPropFilename := '';  // Set the filename to no value
     ThisEdit.EditLabel.Caption := FSoilPropFilename;
    end;
  end
 else  // User clicked on the label
  begin   // Show the open file dialog
   ThisLabel := Sender as TBoundLabel;
   ThisEdit := ThisLabel.Owner as TLabeledEdit;
   OpenDialog.Title := 'Select the soil properties file';
   OpenDialog.DefaultExt := '.dat';
   OpenDialog.InitialDir := CurrentDirectory;
   OpenDialog.Options := [ofFileMustExist];
    // First set the default filename to the current file
   OpenDialog.filename := FSoilPropFilename;
    // Show the dialog and if the user clicks OK, set the new filename
   if OpenDialog.execute then
    begin
      FSoilPropFilename := OpenDialog.filename;
      ThisEdit.Text := FSoilPropFilename;
    end;
  end;
 UpdateForm(Sender);
end;

procedure TFmGridMEL.Button2Click(Sender: TObject);
begin
  FmGridMEL.Close;
end;

procedure TFmGridMEL.ChooseCellOrderFile(Sender: TObject);
var
 ThisEdit:TLabeledEdit;
 ThisLabel: TBoundLabel;
begin
// If the user typed directly in the edit box
 if Sender is TLabeledEdit then
  begin
   ThisEdit := Sender as TLabeledEdit;
   FCellOrderFilename := ThisEdit.EditLabel.Caption;
   if not FileExists(FCellOrderFilename) then
    begin
     MessageDlg('Missing Data File: ' + FCellOrderFilename + '. Please reenter.',
                  mtError, [mbOK], 0);
     FCellOrderFilename := '';  // Set the filename to no value
     ThisEdit.EditLabel.Caption := FCellOrderFilename;
    end;
  end
 else  // User clicked on the label
  begin   // Show the open file dialog
   ThisLabel := Sender as TBoundLabel;
   ThisEdit := ThisLabel.Owner as TLabeledEdit;
   OpenDialog.Title := 'Select the cell order file';
   OpenDialog.DefaultExt := '.dat';
   OpenDialog.InitialDir := CurrentDirectory;
   OpenDialog.Options := [ofFileMustExist];
    // First set the default filename to the current file
   OpenDialog.filename := FCellOrderFilename;
    // Show the dialog and if the user clicks OK, set the new filename
   if OpenDialog.execute then
    begin
      FCellOrderFilename := OpenDialog.filename;
      ThisEdit.Text := FCellOrderFilename;
    end;
  end;
 UpdateForm(Sender);
end;

procedure TFmGridMEL.ChooseDriver1File(Sender: TObject);
var
 ThisEdit:TLabeledEdit;
 ThisLabel: TBoundLabel;
begin
// If the user typed directly in the edit box
 if Sender is TLabeledEdit then
  begin
   ThisEdit := Sender as TLabeledEdit;
   FDriver1Filename := ThisEdit.EditLabel.Caption;
   if not FileExists(FDriver1Filename) then
    begin
     MessageDlg('Missing Data File: ' + FDriver1Filename + '. Please reenter.',
                  mtError, [mbOK], 0);
     FDriver1Filename := '';  // Set the filename to no value
     ThisEdit.EditLabel.Caption := FDriver1Filename;
    end;
  end
 else  // User clicked on the label
  begin   // Show the open file dialog
   ThisLabel := Sender as TBoundLabel;
   ThisEdit := ThisLabel.Owner as TLabeledEdit;
   OpenDialog.Title := 'Select the driver file for vegetation class 1';
   OpenDialog.DefaultExt := '.drr';
   OpenDialog.InitialDir := CurrentDirectory;
   OpenDialog.Options := [ofFileMustExist];
    // First set the default filename to the current file
   OpenDialog.filename := FDriver1Filename;
    // Show the dialog and if the user clicks OK, set the new filename
   if OpenDialog.execute then
    begin
      FDriver1Filename := OpenDialog.filename;
      ThisEdit.Text := FDriver1Filename;
    end;
  end;
 UpdateForm(Sender);
end;

procedure TFmGridMEL.ChooseDriver2File(Sender: TObject);
var
 ThisEdit:TLabeledEdit;
 ThisLabel: TBoundLabel;
begin
// If the user typed directly in the edit box
 if Sender is TLabeledEdit then
  begin
   ThisEdit := Sender as TLabeledEdit;
   FDriver2Filename := ThisEdit.EditLabel.Caption;
   if not FileExists(FDriver2Filename) then
    begin
     MessageDlg('Missing Data File: ' + FDriver2Filename + '. Please reenter.',
                  mtError, [mbOK], 0);
     FDriver2Filename := '';  // Set the filename to no value
     ThisEdit.EditLabel.Caption := FDriver2Filename;
    end;
  end
 else  // User clicked on the label
  begin   // Show the open file dialog
   ThisLabel := Sender as TBoundLabel;
   ThisEdit := ThisLabel.Owner as TLabeledEdit;
   OpenDialog.Title := 'Select the driver file for vegetation class 2';
   OpenDialog.DefaultExt := '.drr';
   OpenDialog.InitialDir := CurrentDirectory;
   OpenDialog.Options := [ofFileMustExist];
    // First set the default filename to the current file
   OpenDialog.filename := FDriver2Filename;
    // Show the dialog and if the user clicks OK, set the new filename
   if OpenDialog.execute then
    begin
      FDriver2Filename := OpenDialog.filename;
      ThisEdit.Text := FDriver2Filename;
    end;
  end;
 UpdateForm(Sender);
end;

procedure TFmGridMEL.ChooseRunOptionsFile(Sender: TObject);
var
 ThisEdit:TLabeledEdit;
 ThisLabel: TBoundLabel;
begin
// If the user typed directly in the edit box
 if Sender is TLabeledEdit then
  begin
   ThisEdit := Sender as TLabeledEdit;
   FRunOptionsFilename := ThisEdit.EditLabel.Caption;
   if not FileExists(FRunOptionsFilename) then
    begin
     MessageDlg('Missing Data File: ' + FRunOptionsFilename + '. Please reenter.',
                  mtError, [mbOK], 0);
     FRunOptionsFilename := '';  // Set the filename to no value
     ThisEdit.EditLabel.Caption := FRunOptionsFilename;
    end;
  end
 else  // User clicked on the label
  begin   // Show the open file dialog
   ThisLabel := Sender as TBoundLabel;
   ThisEdit := ThisLabel.Owner as TLabeledEdit;
   OpenDialog.Title := 'Select the Run Options File file';
   OpenDialog.DefaultExt := '.bch';
   OpenDialog.InitialDir := CurrentDirectory;
   OpenDialog.Options := [ofFileMustExist];
    // First set the default filename to the current file
   OpenDialog.filename := FRunOptionsFilename;
    // Show the dialog and if the user clicks OK, set the new filename
   if OpenDialog.execute then
    begin
      FRunOptionsFilename := OpenDialog.filename;
      ThisEdit.Text := FRunOptionsFilename;
    end;
  end;
 UpdateForm(Sender);
end;

procedure TFmGridMEL.FileExit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TFmGridMEL.HelpAbout1Execute(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

end.
