{ This file defines new variable types and sets the limits on the various arrays
  used in the model code. All units should include this file. }
unit stypes;

{$MODE Delphi}

interface

uses sysutils, classes;

const
  maxstate = 150;   // The maximum number of state variables. The state variable
                   // array contains maxstate elements.
  maxdrive = 75;   // The maximum number of driver variables.
  maxprocess = maxstate + 350;  // The maximum number of process variables.
  maxparam = 500;     // The maximum number of parameters.
  maxresults = 102;
  maxtreatments = 8;
  stringlength = 25;
  maxVarObserved = 100;
  maxTimeMeasure = 100;
  maxmatrix = 10000;

type
  processtype = (ptGroup1, ptGroup2, ptGroup3, ptGroup4, ptGroup5);
//  processcolor = (clGreen, clTeal, clAqua, clBlue, clGray);
  TModelDef = record   // A structure used to hold the basic information
   modelname:string;   // about the model. Modelname is the name of
                       // model and appears on the title bar in the FmShellMain.
   versionnumber:string; // Used to keep track of the version number of the model.
   contactperson:string; // Used for the aboutbox
   contactaddress1:string; // Used for the aboutbox
   contactaddress2:string; // Used for the aboutbox
   contactaddress3:string; // Used for the aboutbox
   timeunit:string;    // Timeunit is the time step of the model, ie day, year.
   numdrive:integer;   // Numparam, numdrive, numstate and numprocess are the
   numstate:integer;  // total number of parameters, drivers, state variables,
   numprocess:integer;   // and processes, respectively.
   numparam:integer;
   end;

  Tparamvariable = record  // A structure to hold a parameter; it's name, value,
     name:string;         // and units. The name and units are for information
     value:double;        // only. They do not affect the running of the model.
     units:string;        // This structure type is also used for the
     symbol:string;       // driver variables.
  end;                     

  Tstatevariable = record  // A structure to hold a state variable; the name,
     name:string;          // value and units. As with Tparamvariable the name
     value:double;         // and units are for information only. HoldConstant
     units:string;         // determines if the state variable is held constant
     symbol:string;        // during the model run, i.e. dstatedt = 0.
     HoldConstant:Boolean;
     Reset:Boolean;
  end;

  Tprocessvariable = record  // A structure to hold a process variable; the
     name:string;            // name, value, and units. As above, the name and
     value:double;           // units are for information only. Parameters is
     units:string;           // number of parameters associated with this
     symbol:string;          // particular process. Every parameter must be
     parameters:integer;     // associated with a process. Ptype determines the
     ptype:processtype;      // color of the tab in the edit parameters page.
  end;                       

  Tallstates = record
     Cf:Tstatevariable;
     Cw:Tstatevariable;
     Cr:Tstatevariable;
     Nf:Tstatevariable;
     Nw:Tstatevariable;
     Nr:Tstatevariable;
  end;

  TCalSet = record
     HaveRunInfo: Boolean;
     ValidSet: Boolean;
     rownum: integer;
     Treatment: integer;
     State: string;
     StateIndex: integer;
     Parameter: string;
     ParamIndex: integer;
     Alpha: double;
     Beta: double;
     DeltaPar: double;
  end;

// State variable array types. yValueArray, glindx and glpbynp are used during
// integration. They hold temporary values of the state variables and therefore
// don't need to be of type Tstatevariable. This saves memory. However they
// must be the same size as statearray.
  yValueArray=array[1..maxstate] of double;
  glindx = ARRAY [1..maxstate] OF integer;
  glnpbynp = ARRAY [1..maxstate,1..maxstate] OF double;
  statearray=array[1..maxstate] of Tstatevariable;
// Driver variable array type. Note that the type is Tparamvariable.
  drivearray=array[1..maxdrive] of Tparamvariable;
// Parameter array type.
  paramarray=array[1..maxparam] of Tparamvariable;
// Process variable array type.
  processarray=array[1..maxprocess] of Tprocessvariable;
  calsetarray = array[1..maxstate] of Tcalset;
  stringarray = array[1..maxstate + 1] of string;

// The types of data which can be displayed in the dataform
  TDataType = (dtState, dtProcess, dtParameter);
// The possible axis' in the chart on the display form
  TAxis = (axLeft, axBottom);
// The display options for the output data in the display form
  TDisplayStyle = (dsChart, dsTable);
// The axis type for the chart on the display form
  TAxisType = (tyLinear, tyLog);
// The type of variable being looked for in the arrays
  TVarType = (vtDriver, vtState, vtParameter, vtProcess);

  TRunOptions = record
    NormalRun: Boolean;
    Time_step: double;
    DiscreteStep: double;
    RepeatDrivers: Boolean;
    RepeatDriveTime: double;
    ResetStates: Boolean;
    ResetStateTime: double;
    RuntoSS: Boolean;
    SSCriteria: double;
    SSTime: double;
    HoldStatesConstant: Boolean;  // Used in fuzzy calibrator
    Outputstep: double;      // The timestep specified by the user for output
    Outputoffset: double;    // No output for time less than outputoffset
    OutputEORonly: Boolean;  // output only if time = stop_time + Time_step
    OutputAnnually: Boolean;
    OutputAnnuallyDay: double;
    AppendOutputFile: Boolean;
    stepcounter: integer;
    outcounter: integer;   // The current number of timesteps since last output, output occurs when outcounter=outputfreq
    OutputFile: Boolean;
    WriteEvery: double;
    errormult: integer;
  end;

  TCalOptions = record
    UseSecondOutFile: Boolean;
    FinalOutFilename: string;
    OrigOutputStep: double;
    OrigOutputOffset: double;
    OutputEORonly: Boolean;
    caloutcounter: integer;
    caloutputfreq: integer;
    AppendFinalOutputFile: Boolean;
  end;

  TTreatResults = record
     Time: integer;
     IndexList: array[1..Maxstate] of integer;
     States: statearray;
  end;

  TDerivSet = record
     Time1: double;
     States1: statearray;
     Time2: double;
     States2: statearray;
  end;

  resultarray = array[1..maxresults] of TTreatResults;

  TTreatSet = record
     drivefilename: string;
     outputfilename: string;
     timestart: double;
     timestop: double;
     Options: TRunOptions;
     CalOptions: TCalOptions;
     TotalMeasurements: integer;
     measdata: resultarray;
     currmeasurement: integer;
     simdata: resultarray;
     currdrive: drivearray;
     currstate: statearray;
     derivstates: TDerivSet;
     NumCalSet: integer;
     CalSetIndexList: array[1..maxstate] of integer;
     FirstWrite: Boolean;
  end;

  TreatSetarray = array[1..maxtreatments] of TTreatSet;

// Ensemble Kalman Filter   
  EnKF2Dmat = array of array of double;
  EnKFvec = array of double;
  glindxEKF = array of integer;
  mat = array[1..MaxMatrix] of double;

  TVariableIncluded = array of Boolean;

  TNameArray = array of string;
  TTimeArray = array of double;
  TDataArray = array of double;
  T2dData = array of array of double;
  // Rows are time points, columns are variables

  TPertData = array of double;
  TPert = record
     NumPert: integer;
     Names: TNameArray;
     Data: EnKF2Dmat;
  end;

  TKstate = record
     NumTotKalman: integer;
     NumObservations: integer;
     Names: TNameArray;
     Units: TNameArray;
     Symbol: TNameArray;
     Time: TTimeArray;
     Z: EnKF2dmat;
     R: EnKF2dmat;
     Xmean: T2dData;
     Xmax: T2dData;
     Xmin: T2dData;
     Q: EnKF2Dmat;
     Ymean: T2dData;
     UnCorXmean: T2dData;
     UncorXmax: T2dData;
     UncorXmin: T2dData;
  end;

// Grid Shell Variables
  TGridCellInfo = record
    Row: integer;
    Column: integer;
    RunPosition: integer;
    VegClass: integer;
    SoilClass: integer;
    ParamFilname: string;
    Driverfilename: string;
    OutputFilename: string;
  end;

// Errors
  ERunTimeError = class(Exception);
  EIntegratorError = class(ERunTimeError);
  ETooManySteps = class(EIntegratorError);
  EIntStepTooSmall = class(EIntegratorError);
  EUserCancel = class(ERunTimeError);
  ECalculateError = class(EMathError);
  ECalcInitialProc = class(ECalculateError);
  ECalcNewValue = class(ECalculateError);
  ECalcNewProcess = class(ECalculateError);
  EStepError = class(Exception);
  EStepTooSmall = class(EStepError);
  EStepTooLarge = class(EStepError);
  EStepNotMultiple = class(EStepError);
implementation

end.
 
