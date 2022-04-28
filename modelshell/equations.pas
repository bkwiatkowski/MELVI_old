{ This unit defines the structure of the model. There are four functions. The
  first function, called counts, defines the number, names, and units of the
  model, the state variables, the process variables, the driver variables and
  the parameters. The second function, called processes, is the actual equations
  which make up the model. The third function, derivs, calculates the
  derivatives of state variables. And the fourth function, parcount, is used to
  automatically number the parameters consecutively. 
    The state variables, driver variables, process variables and parameters are
  all stored in global arrays, called stat, drive, proc, and par, respectively.
  The function counts accesses the global arrays directly but the other functions
  operate on copies of the global arrays. }
unit equations;

interface

uses  stypes, math, sysutils;

PROCEDURE counts;
PROCEDURE processes(time:double; dtime:double; var tdrive:drivearray;
                       var tpar:paramarray; var tstat:statearray;
                       var tproc:processarray; CalculateDiscrete:Boolean);
PROCEDURE derivs(t, drt:double; var tdrive:drivearray; var tpar:paramarray;
             var statevalue:yValueArray; VAR dydt:yValueArray);
function ParCount(processnum:integer) : integer;

var
  tproc:processarray;
  tstat:statearray;
  sensflag:boolean;
  newyear:Boolean = false;
  DayofYear: double = 0;
  h: array[1..4,1..4] of double;

implementation

uses frontend, calculate, options;

           { Do not make modifcations above this line. }
{*****************************************************************************}

{ This procedure defines the model. The number of parameters, state, driver and
  process variables are all set in this procedure. The model name, version
  number and time unit are also set here. This procedure accesses the global
  arrays containing the the parameters, state, driver and process variables and
  the global structure ModelDef directly, to save memory space. }
PROCEDURE counts;
var
 i,npar,CurrentProc:integer;
begin
{ Set the modelname, version and time unit. }
ModelDef.modelname := 'MELVI';
ModelDef.versionnumber := '2.8.5';
ModelDef.timeunit := 'day';
ModelDef.contactperson := 'Edward Rastetter';
ModelDef.contactaddress1 := 'The Ecosystems Center';
ModelDef.contactaddress2 := 'Marine Biological Laboratory';
ModelDef.contactaddress3 := 'Woods Hole, MA 02543';

{ Set the number of state variables in the model. The maximum number of state
  variables is maxstate, in unit stypes. }
ModelDef.numstate := 132;

{ Enter the name, units and symbol for each state variable. The maximum length
  of the state variable name is 17 characters and the maximum length for units
  and symbol is stringlength (specified in unit stypes) characters. }
 
 
with stat[1] do
 begin
    name:='biomass C';  units:='g C m-2';  symbol:='BC';
 end;
 
with stat[2] do
 begin
    name:='biomass N';  units:='g N m-2';  symbol:='BN';
 end;
 
with stat[3] do
 begin
    name:='biomass P';  units:='g P m-2';  symbol:='BP';
 end;
 
with stat[4] do
 begin
    name:='effort C';  units:='effort';  symbol:='VC';
 end;
 
with stat[5] do
 begin
    name:='effort N';  units:='effort';  symbol:='VN';
 end;
 
with stat[6] do
 begin
    name:='effort P';  units:='effort';  symbol:='VP';
 end;
 
with stat[7] do
 begin
    name:='sub effort CO2';  units:='effort';  symbol:='vCO2';
 end;
 
with stat[8] do
 begin
    name:='sub effort light';  units:='effort';  symbol:='vI';
 end;
 
with stat[9] do
 begin
    name:='sub effort H2O';  units:='effort';  symbol:='vW';
 end;
 
with stat[10] do
 begin
    name:='sub effort NH4';  units:='effort';  symbol:='vNH4';
 end;
 
with stat[11] do
 begin
    name:='sub effort NO3';  units:='effort';  symbol:='vNO3';
 end;
 
with stat[12] do
 begin
    name:='sub effort doN';  units:='effort';  symbol:='vdoN';
 end;
 
with stat[13] do
 begin
    name:='sub effort Nfix';  units:='effort';  symbol:='vNfix';
 end;
 
with stat[14] do
 begin
    name:='Phase I SOM C';  units:='g C m-2';  symbol:='DC';
 end;
 
with stat[15] do
 begin
    name:='Phase I SOM N';  units:='g N m-2';  symbol:='DN';
 end;
 
with stat[16] do
 begin
    name:='Phase I SOM P';  units:='g P m-2';  symbol:='DP';
 end;
 
with stat[17] do
 begin
    name:='Woody debris C';  units:='g C m-2';  symbol:='WC';
 end;
 
with stat[18] do
 begin
    name:='Woody debris N';  units:='g N m-2';  symbol:='WN';
 end;
 
with stat[19] do
 begin
    name:='Woody debris P';  units:='g P m-2';  symbol:='WP';
 end;
 
with stat[20] do
 begin
    name:='Phase II SOM C';  units:='g C m-2';  symbol:='SC';
 end;
 
with stat[21] do
 begin
    name:='Phase II SOM N';  units:='g N m-2';  symbol:='SN';
 end;
 
with stat[22] do
 begin
    name:='Phase II SOM P';  units:='g P m-2';  symbol:='SP';
 end;
 
with stat[23] do
 begin
    name:='NH4';  units:='g N m-2';  symbol:='ENH4';
 end;
 
with stat[24] do
 begin
    name:='NO3';  units:='g N m-2';  symbol:='ENO3';
 end;
 
with stat[25] do
 begin
    name:='PO4';  units:='g P m-2';  symbol:='EPO4';
 end;
 
with stat[26] do
 begin
    name:='P Primary min';  units:='g P m-2';  symbol:='Pa';
 end;
 
with stat[27] do
 begin
    name:='P non-Occluded';  units:='g P m-2';  symbol:='Pno';
 end;
 
with stat[28] do
 begin
    name:='P Occluded';  units:='g P m-2';  symbol:='Poccl';
 end;
 
with stat[29] do
 begin
    name:='water';  units:='mm H2O';  symbol:='W';
 end;
 
with stat[30] do
 begin
    name:='snow pack';  units:='mm H2O';  symbol:='WSnow';
 end;
 
with stat[31] do
 begin
    name:='Soil Heat';  units:='arbitrary';  symbol:='SQ';
 end;
 
with stat[32] do
 begin
    name:='canopy fraction';  units:='unitless';  symbol:='fc';
 end;
 
with stat[33] do
 begin
    name:='Ave requirement C';  units:='none';  symbol:='RCa';
 end;
 
with stat[34] do
 begin
    name:='Ave requirement N';  units:='none';  symbol:='RNa';
 end;
 
with stat[35] do
 begin
    name:='Ave requirement P';  units:='none';  symbol:='RPa';
 end;
 
with stat[36] do
 begin
    name:='Ave Acquisition C';  units:='none';  symbol:='UCa';
 end;
 
with stat[37] do
 begin
    name:='Ave Acquisition N';  units:='none';  symbol:='UNa';
 end;
 
with stat[38] do
 begin
    name:='Ave Acquisition P';  units:='none';  symbol:='UPa';
 end;
 
with stat[39] do
 begin
    name:='Degree day positive';  units:='oC day';  symbol:='DDayp';
 end;
 
with stat[40] do
 begin
    name:='Degree day negative';  units:='oC day';  symbol:='DDayn';
 end;
 
with stat[41] do
 begin
    name:='Cum GPP';  units:='g C m-2';  symbol:='CumGPP';
 end;
 
with stat[42] do
 begin
    name:='Cum NPP';  units:='g C m-2';  symbol:='CumNPP';
 end;
 
with stat[43] do
 begin
    name:='Cum NEP';  units:='g C m-2';  symbol:='CumNEP';
 end;
 
with stat[44] do
 begin
    name:='Cum UdoC';  units:='g C m-2';  symbol:='CumUdoC';
 end;
 
with stat[45] do
 begin
    name:='Cum LitC';  units:='g C m-2';  symbol:='CumLitC';
 end;
 
with stat[46] do
 begin
    name:='Cum LcWC';  units:='g C m-2';  symbol:='CumLcWC';
 end;
 
with stat[47] do
 begin
    name:='Cum Total Litter C';  units:='g C m-2';  symbol:='CumtotalLitC';
 end;
 
with stat[48] do
 begin
    name:='Cum Total Litter N';  units:='g N m-2';  symbol:='CumtotalLitN';
 end;
 
with stat[49] do
 begin
    name:='Cum Total Litter P';  units:='g P m-2';  symbol:='CumtotalLitP';
 end;
 
with stat[50] do
 begin
    name:='Cum Tot microb resp';  units:='g C m-2';  symbol:='CumRCmtotal';
 end;
 
with stat[51] do
 begin
    name:='Cum net N min';  units:='g N m-2';  symbol:='CumNmintot';
 end;
 
with stat[52] do
 begin
    name:='Cum net P min';  units:='g P m-2';  symbol:='CumPmintot';
 end;
 
with stat[53] do
 begin
    name:='Cum Total N uptake';  units:='g N m-2';  symbol:='CumUN';
 end;
 
with stat[54] do
 begin
    name:='Cum UNH4';  units:='g N m-2';  symbol:='CumUNH4';
 end;
 
with stat[55] do
 begin
    name:='Cum UNO3';  units:='g N m-2';  symbol:='CumUNO3';
 end;
 
with stat[56] do
 begin
    name:='Cum UdoN';  units:='g N m-2';  symbol:='CumUdoN';
 end;
 
with stat[57] do
 begin
    name:='Cum N fixation';  units:='g N m-2';  symbol:='CumUNfix';
 end;
 
with stat[58] do
 begin
    name:='Cum LitN';  units:='g N m-2';  symbol:='CumLitN';
 end;
 
with stat[59] do
 begin
    name:='Cum LcWN';  units:='g N m-2';  symbol:='CumLcWN';
 end;
 
with stat[60] do
 begin
    name:='Cum UPO4';  units:='g P m-2';  symbol:='CumUPO4';
 end;
 
with stat[61] do
 begin
    name:='Cum LitP';  units:='g P m-2';  symbol:='CumLitP';
 end;
 
with stat[62] do
 begin
    name:='Cum LcWP';  units:='g P m-2';  symbol:='CumLcWP';
 end;
 
with stat[63] do
 begin
    name:='Cum LcWCa';  units:='g C m-2';  symbol:='CumLcWCa';
 end;
 
with stat[64] do
 begin
    name:='Cum RCm';  units:='g C m-2';  symbol:='CumRCm';
 end;
 
with stat[65] do
 begin
    name:='Cum TiiC';  units:='g C m-2';  symbol:='CumTiiC';
 end;
 
with stat[66] do
 begin
    name:='Cum LcWNa';  units:='g N m-2';  symbol:='CumLcWNa';
 end;
 
with stat[67] do
 begin
    name:='Cum UNH4m';  units:='g N m-2';  symbol:='CumUNH4m';
 end;
 
with stat[68] do
 begin
    name:='Cum UNO3m';  units:='g N m-2';  symbol:='CumUNO3m';
 end;
 
with stat[69] do
 begin
    name:='Cum RNm';  units:='g N m-2';  symbol:='CumRNm';
 end;
 
with stat[70] do
 begin
    name:='Cum TiiN';  units:='g N m-2';  symbol:='CumTiiN';
 end;
 
with stat[71] do
 begin
    name:='Cum nonsym Nfix';  units:='g N m-2';  symbol:='CumNnsfix';
 end;
 
with stat[72] do
 begin
    name:='Cum LcWPa';  units:='g P m-2';  symbol:='CumLcWPa';
 end;
 
with stat[73] do
 begin
    name:='Cum UPO4m';  units:='g P m-2';  symbol:='CumUPO4m';
 end;
 
with stat[74] do
 begin
    name:='Cum RPm';  units:='g P m-2';  symbol:='CumRPm';
 end;
 
with stat[75] do
 begin
    name:='Cum TiiP';  units:='g P m-2';  symbol:='CumTiiP';
 end;
 
with stat[76] do
 begin
    name:='Cum MiiC';  units:='g Cm-2';  symbol:='CumMiiC';
 end;
 
with stat[77] do
 begin
    name:='Cum MiiN';  units:='g N m-2';  symbol:='CumMiiN';
 end;
 
with stat[78] do
 begin
    name:='Cum MiiP';  units:='g P m-2';  symbol:='CumMiiP';
 end;
 
with stat[79] do
 begin
    name:='Cum INH4';  units:='g N m-2';  symbol:='CumINH4';
 end;
 
with stat[80] do
 begin
    name:='Cum LNH4';  units:='g N m-2';  symbol:='CumLNH4';
 end;
 
with stat[81] do
 begin
    name:='Cum nitrification';  units:='g N m-2';  symbol:='CumNitr';
 end;
 
with stat[82] do
 begin
    name:='Cum INO3';  units:='g N m-2';  symbol:='CumINO3';
 end;
 
with stat[83] do
 begin
    name:='Cum LNO3';  units:='g N m-2';  symbol:='CumLNO3';
 end;
 
with stat[84] do
 begin
    name:='Cum DNtr';  units:='g N m-2';  symbol:='CumDNtr';
 end;
 
with stat[85] do
 begin
    name:='Cum Paw';  units:='g P m-2';  symbol:='CumPaw';
 end;
 
with stat[86] do
 begin
    name:='Cum Pnow';  units:='g P m-2';  symbol:='CumPnow';
 end;
 
with stat[87] do
 begin
    name:='Cum IPO4';  units:='g P m-2';  symbol:='CumIPO4';
 end;
 
with stat[88] do
 begin
    name:='Cum LPO4';  units:='g P m-2';  symbol:='CumLPO4';
 end;
 
with stat[89] do
 begin
    name:='Cum PO4P';  units:='g P m-2';  symbol:='CumPO4P';
 end;
 
with stat[90] do
 begin
    name:='Cum IPa';  units:='g P m-2';  symbol:='CumIPa';
 end;
 
with stat[91] do
 begin
    name:='Cum Pocclw';  units:='g P m-2';  symbol:='CumPocclw';
 end;
 
with stat[92] do
 begin
    name:='Cum Pnos';  units:='g P m-2';  symbol:='CumPnos';
 end;
 
with stat[93] do
 begin
    name:='Cum IdoC';  units:='g C m-2';  symbol:='CumIdoC';
 end;
 
with stat[94] do
 begin
    name:='Cum IdoN';  units:='g C m-2';  symbol:='CumIdoN';
 end;
 
with stat[95] do
 begin
    name:='Cum LdoC';  units:='g C m-2';  symbol:='CumLdoC';
 end;
 
with stat[96] do
 begin
    name:='Cum LdoN';  units:='g N m-2';  symbol:='CumLdoN';
 end;
 
with stat[97] do
 begin
    name:='Cum UW';  units:='mm H2O';  symbol:='CumUW';
 end;
 
with stat[98] do
 begin
    name:='Cum runoff';  units:='mm H2O';  symbol:='CumRO';
 end;
 
with stat[99] do
 begin
    name:='Cum precipitation';  units:='mm H2O';  symbol:='CumPpt';
 end;
 
with stat[100] do
 begin
    name:='Cum interception';  units:='mm H2O';  symbol:='CumIntr';
 end;
 
with stat[101] do
 begin
    name:='Cum rainfall';  units:='mm H2O';  symbol:='CumRfl';
 end;
 
with stat[102] do
 begin
    name:='Cum snowfall';  units:='mm H2O';  symbol:='CumSfl';
 end;
 
with stat[103] do
 begin
    name:='Cum snowmelt';  units:='mm H2O';  symbol:='CumSm';
 end;
 
with stat[104] do
 begin
    name:='Cum run in';  units:='mm H2O';  symbol:='CumRin';
 end;
 
with stat[105] do
 begin
    name:='Cum Rin doC';  units:='g C m-2 day-1';  symbol:='CumIRindoC';
 end;
 
with stat[106] do
 begin
    name:='Cum Rin doN';  units:='g N m-2 day-1';  symbol:='CumIRindoN';
 end;
 
with stat[107] do
 begin
    name:='Cum Rin NH4';  units:='g N m-2 day-1';  symbol:='CumIRinNH4';
 end;
 
with stat[108] do
 begin
    name:='Cum Rin NO3';  units:='g N m-2 day-1';  symbol:='CumIRinNO3';
 end;
 
with stat[109] do
 begin
    name:='Cum Rin PO4';  units:='g P m-2 day-1';  symbol:='CumIRinPO4';
 end;
 
with stat[110] do
 begin
    name:='Cum Overland flow';  units:='mm H2O';  symbol:='CumROvf';
 end;
 
with stat[111] do
 begin
    name:='Cum Overland doC';  units:='g C m-2';  symbol:='CumROvfdoC';
 end;
 
with stat[112] do
 begin
    name:='Cum Overland doN';  units:='g N m-2';  symbol:='CumROvfdoN';
 end;
 
with stat[113] do
 begin
    name:='Cum Overland NH4';  units:='g N m-2';  symbol:='CumROvfNH4';
 end;
 
with stat[114] do
 begin
    name:='Cum Overland NO3';  units:='g N m-2';  symbol:='CumROvfNO3';
 end;
 
with stat[115] do
 begin
    name:='Cum Overland PO4';  units:='g P m-2';  symbol:='CumROvfPO4';
 end;
 
with stat[116] do
 begin
    name:='LAI peak season';  units:='m2 m-2';  symbol:='LAIpeak';
 end;
 
with stat[117] do
 begin
    name:='Cum BC fire loss';  units:='g C m-2';  symbol:='CumfBC';
 end;
 
with stat[118] do
 begin
    name:='Cum BN fire loss';  units:='g N m-2';  symbol:='CumfBN';
 end;
 
with stat[119] do
 begin
    name:='Cum BP fire loss';  units:='g P m-2';  symbol:='CumfBP';
 end;
 
with stat[120] do
 begin
    name:='Cum WC fire loss';  units:='g C m-2';  symbol:='CumfWC';
 end;
 
with stat[121] do
 begin
    name:='Cum WN fire loss';  units:='g N m-2';  symbol:='CumfWN';
 end;
 
with stat[122] do
 begin
    name:='Cum WP fire loss';  units:='g P m-2';  symbol:='CumfWP';
 end;
 
with stat[123] do
 begin
    name:='Cum DC fire loss';  units:='g C m-2';  symbol:='CumfDC';
 end;
 
with stat[124] do
 begin
    name:='Cum DN fire loss';  units:='g N m-2';  symbol:='CumfDN';
 end;
 
with stat[125] do
 begin
    name:='Cum DP fire loss';  units:='g P m-2';  symbol:='CumfDP';
 end;
 
with stat[126] do
 begin
    name:='Cum TotC volatilized fire';  units:='g C m-2';  symbol:='CumFCvt';
 end;
 
with stat[127] do
 begin
    name:='Cum TotN volatilized fire';  units:='g N m-2';  symbol:='CumFNvt';
 end;
 
with stat[128] do
 begin
    name:='Cum TotP volatilized fire';  units:='g P m-2';  symbol:='CumFPvt';
 end;
 
with stat[129] do
 begin
    name:='Cum C FineLit to Debris';  units:='g C m-2';  symbol:='CumLitCDebris';
 end;
 
with stat[130] do
 begin
    name:='Cum N FineLit to Debris';  units:='g N m-2';  symbol:='CumLitNDebris';
 end;
 
with stat[131] do
 begin
    name:='Cum P FineLit to Debris';  units:='g P m-2';  symbol:='CumLitPDebris';
 end;
 
with stat[132] do
 begin
    name:='BC peak season';  units:='g C m-2';  symbol:='BCpeak';
 end;

{ Set the total number of processes in the model. The first numstate processes
  are the derivatives of the state variables. The maximum number of processes is
  maxparam, in unit stypes. }
ModelDef.numprocess := ModelDef.numstate + 208;

{ For each process, set proc[i].parameters equal to the number of parameters
  associated with that process, and set IsDiscrete to true or false. After each
  process, set the name, units, and symbol for all parameters associated with
  that process. Note that Parcount returns the total number of parameters in
  all previous processes. }
 
CurrentProc := ModelDef.numstate + 1;
With proc[CurrentProc] do
   begin
      name       := 'Temperature ave';
      units       := 'oC';
      symbol       := 'Ta';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 2;
With proc[CurrentProc] do
   begin
      name       := 'Biomass';
      units       := 'g DW m-2';
      symbol       := 'Bt';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 3;
With proc[CurrentProc] do
   begin
      name       := 'Biomass Active';
      units       := 'g DW m-2';
      symbol       := 'Ba';
      parameters       := 5;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='Allometry alpha';  units:='';  symbol:='alphaB';
 end;
with par[npar + 2] do
 begin
    name:='Maximum Ba';  units:='g DW m-2';  symbol:='Bamax';
 end;
with par[npar + 3] do
 begin
    name:='Ba:Bt slope';  units:='none';  symbol:='gammaB';
 end;
with par[npar + 4] do
 begin
    name:='leaf area per DW';  units:='m2 g-1 DW';  symbol:='a_sla';
 end;
with par[npar + 5] do
 begin
    name:='C:DW ratio';  units:='g C g-1 DW';  symbol:='qC';
 end;
 
CurrentProc := ModelDef.numstate + 4;
With proc[CurrentProc] do
   begin
      name       := 'Biomass leaf';
      units       := 'g DW m-2';
      symbol       := 'BL';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 5;
With proc[CurrentProc] do
   begin
      name       := 'Biomass root';
      units       := 'g DW m-2';
      symbol       := 'BR';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 6;
With proc[CurrentProc] do
   begin
      name       := 'Biomass woody';
      units       := 'g DW m-2';
      symbol       := 'BW';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 7;
With proc[CurrentProc] do
   begin
      name       := 'leaf area';
      units       := 'm2 m-2';
      symbol       := 'L';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 8;
With proc[CurrentProc] do
   begin
      name       := 'Leaf area max';
      units       := 'm2 m-2';
      symbol       := 'L_max';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 9;
With proc[CurrentProc] do
   begin
      name       := 'canopy fraction growth';
      units       := 'fraction day-1';
      symbol       := 'Gfc';
      parameters       := 6;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='evergreen canopy fraction';  units:='unitless';  symbol:='fcmin';
 end;
with par[npar + 2] do
 begin
    name:='canopy growth rate';  units:='deg-1 C day-1';  symbol:='alphaGfc';
 end;
with par[npar + 3] do
 begin
    name:='canopy growth water sens';  units:='unitless';  symbol:='betaGfc';
 end;
with par[npar + 4] do
 begin
    name:='wilting amplitude';  units:='unitless';  symbol:='gammaw';
 end;
with par[npar + 5] do
 begin
    name:='degree day bud break';  units:='deg C';  symbol:='Ddbud';
 end;
with par[npar + 6] do
 begin
    name:='canopy feedback exp';  units:='unitless';  symbol:='epsilonfc';
 end;
 
CurrentProc := ModelDef.numstate + 10;
With proc[CurrentProc] do
   begin
      name       := 'canopy growth water resp';
      units       := 'unitless';
      symbol       := 'deltaGcT';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 11;
With proc[CurrentProc] do
   begin
      name       := 'canopy fraction senescence';
      units       := 'fraction day-1';
      symbol       := 'Lfc';
      parameters       := 3;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='day of year fall starts';  units:='day';  symbol:='Dfs';
 end;
with par[npar + 2] do
 begin
    name:='litter temperature rate';  units:='day-1';  symbol:='chicT';
 end;
with par[npar + 3] do
 begin
    name:='litter moisture rate';  units:='day-1';  symbol:='chicW';
 end;
 
CurrentProc := ModelDef.numstate + 12;
With proc[CurrentProc] do
   begin
      name       := 'canopy litter water resp';
      units       := 'unitless';
      symbol       := 'deltaLcW';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 13;
With proc[CurrentProc] do
   begin
      name       := 'VPD';
      units       := 'MPa';
      symbol       := 'Delta_E';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 14;
With proc[CurrentProc] do
   begin
      name       := 'soil potential';
      units       := 'MPa';
      symbol       := 'psiS';
      parameters       := 24;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='field capacity for plants';  units:='fraction of soil vol';  symbol:='theta_fp';
 end;
with par[npar + 2] do
 begin
    name:='field capacity for runoff';  units:='fraction of soil vol';  symbol:='theta_fro';
 end;
with par[npar + 3] do
 begin
    name:='wilting point';  units:='fraction of soil vol';  symbol:='theta_w';
 end;
with par[npar + 4] do
 begin
    name:='field potential';  units:='MPa';  symbol:='psi_f';
 end;
with par[npar + 5] do
 begin
    name:='wilting potential';  units:='MPa';  symbol:='psi_w';
 end;
with par[npar + 6] do
 begin
    name:='rooting depth';  units:='m';  symbol:='z0';
 end;
with par[npar + 7] do
 begin
    name:='soil porosity';  units:='fraction of soil vol';  symbol:='phis';
 end;
with par[npar + 8] do
 begin
    name:='bulk density';  units:='Mg m-3';  symbol:='rho_s';
 end;
with par[npar + 9] do
 begin
    name:='soil drain rate';  units:='day-1';  symbol:='drain';
 end;
with par[npar + 10] do
 begin
    name:='non-leaf sfc area';  units:='m2 m-2';  symbol:='NLsfc';
 end;
with par[npar + 11] do
 begin
    name:='branch exponent';  units:='none';  symbol:='NLe';
 end;
with par[npar + 12] do
 begin
    name:='Mid wood biomass';  units:='g DW m-2';  symbol:='MBW';
 end;
with par[npar + 13] do
 begin
    name:='intercept volume';  units:='mm m-2';  symbol:='Intv';
 end;
with par[npar + 14] do
 begin
    name:='NH4 sorption cap';  units:='g N Mg-1 dry soil';  symbol:='SNH4';
 end;
with par[npar + 15] do
 begin
    name:='NH4 affinity const';  units:='umol NH4 L-1';  symbol:='etaNH4';
 end;
with par[npar + 16] do
 begin
    name:='NO3 sorption cap';  units:='g N Mg-1 dry soil';  symbol:='SNO3';
 end;
with par[npar + 17] do
 begin
    name:='NO3 affinity const';  units:='umol NH4 L-1';  symbol:='etaNO3';
 end;
with par[npar + 18] do
 begin
    name:='PO4 sorption cap';  units:='g PO4 Mg-1 dry soil';  symbol:='SPO4';
 end;
with par[npar + 19] do
 begin
    name:='PO4 partition const';  units:='g sorbed g-1 solution';  symbol:='etaPO4';
 end;
with par[npar + 20] do
 begin
    name:='depth: hydraulically active';  units:='m';  symbol:='zW';
 end;
with par[npar + 21] do
 begin
    name:='PaDOC C:N cutoff';  units:='gC/gN';  symbol:='qPadoC';
 end;
with par[npar + 22] do
 begin
    name:='Nfix critical LAI';  units:='m2 m-2';  symbol:='Lcrit';
 end;
with par[npar + 23] do
 begin
    name:='doN runoff par';  units:='mm-1';  symbol:='aLdoN';
 end;
with par[npar + 24] do
 begin
    name:='Nfix Lcrit sens';  units:='none';  symbol:='eNfix';
 end;
 
CurrentProc := ModelDef.numstate + 15;
With proc[CurrentProc] do
   begin
      name       := 'P bal Apatite weathering';
      units       := 'g P m-2 day-1';
      symbol       := 'Paw';
      parameters       := 1;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='apatite weathering rate';  units:='day-1';  symbol:='rPaw';
 end;
 
CurrentProc := ModelDef.numstate + 16;
With proc[CurrentProc] do
   begin
      name       := 'P bal PO4 precipitation';
      units       := 'g P m-2 day-1';
      symbol       := 'PO4P';
      parameters       := 4;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='PO4 precip rate';  units:='day-1';  symbol:='rPO4P';
 end;
with par[npar + 2] do
 begin
    name:='non-occld weath rate';  units:='day-1';  symbol:='rPnow';
 end;
with par[npar + 3] do
 begin
    name:='No-occld stab rate';  units:='day-1';  symbol:='rPnos';
 end;
with par[npar + 4] do
 begin
    name:='occlud weathering rate';  units:='day-1';  symbol:='rPocclw';
 end;
 
CurrentProc := ModelDef.numstate + 17;
With proc[CurrentProc] do
   begin
      name       := 'P bal Non-occl weathrng';
      units       := 'g P m-2 day-1';
      symbol       := 'Pnow';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 18;
With proc[CurrentProc] do
   begin
      name       := 'P bal occld weathering';
      units       := 'g P m-2 day-1';
      symbol       := 'Pocclw';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 19;
With proc[CurrentProc] do
   begin
      name       := 'P bal non-occld stablz';
      units       := 'g P m-2 day-1';
      symbol       := 'Pnos';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 20;
With proc[CurrentProc] do
   begin
      name       := 'canopy cond soil limited';
      units       := 'mm H2O MPa-1 hr-1';
      symbol       := 'c_cs';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 21;
With proc[CurrentProc] do
   begin
      name       := 'C bal Ps: Light limit';
      units       := 'g C m-2 day-1';
      symbol       := 'PsIrr';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 22;
With proc[CurrentProc] do
   begin
      name       := 'C bal Ps: CO2 limit';
      units       := 'g C m-2 day-1';
      symbol       := 'PsC';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 23;
With proc[CurrentProc] do
   begin
      name       := 'C bal Ps: Water limit';
      units       := 'g C m-2 day-1';
      symbol       := 'PsW';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 24;
With proc[CurrentProc] do
   begin
      name       := 'C bal photosynthesis';
      units       := 'g C m-2 day-1';
      symbol       := 'UC';
      parameters       := 13;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='Neg leaching flag';  units:='none';  symbol:='FlagNegLeach';
 end;
with par[npar + 2] do
 begin
    name:='PET-root sensitivity';  units:='m2 soil m-1 root';  symbol:='kE';
 end;
with par[npar + 3] do
 begin
    name:='Ps CO2 rate constant';  units:='g C effort-1 day-1';  symbol:='gC';
 end;
with par[npar + 4] do
 begin
    name:='Ta Ps CA';  units:='oC';  symbol:='TaPs';
 end;
with par[npar + 5] do
 begin
    name:='Tb Ps CA';  units:='oC';  symbol:='TbPs';
 end;
with par[npar + 6] do
 begin
    name:='T exp a Ps CA';  units:='oC-1';  symbol:='aPs';
 end;
with par[npar + 7] do
 begin
    name:='T exp b Ps CA';  units:='oC-1';  symbol:='bPs';
 end;
with par[npar + 8] do
 begin
    name:='Ps light constant';  units:='g C effort-1 day-1';  symbol:='gI';
 end;
with par[npar + 9] do
 begin
    name:='light half sat';  units:='g C MJ-1';  symbol:='HSI';
 end;
with par[npar + 10] do
 begin
    name:='light extinct';  units:='none';  symbol:='kI';
 end;
with par[npar + 11] do
 begin
    name:='Ppt projection exp';  units:='mm-1';  symbol:='aPpt';
 end;
with par[npar + 12] do
 begin
    name:='UW Ps scaler';  units:='none';  symbol:='scg';
 end;
with par[npar + 13] do
 begin
    name:='Q10 resp';  units:='none';  symbol:='Q10R';
 end;
 
CurrentProc := ModelDef.numstate + 25;
With proc[CurrentProc] do
   begin
      name       := 'W bal water uptake';
      units       := 'mm day-1';
      symbol       := 'UW';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 26;
With proc[CurrentProc] do
   begin
      name       := 'potential ET';
      units       := 'mm day-1';
      symbol       := 'PET';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 27;
With proc[CurrentProc] do
   begin
      name       := 'aqueous NH4';
      units       := 'umol NH4 L-1';
      symbol       := 'NH4aq';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 28;
With proc[CurrentProc] do
   begin
      name       := 'N bal NH4 uptake';
      units       := 'g N m-2 day-1';
      symbol       := 'UNH4';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 29;
With proc[CurrentProc] do
   begin
      name       := 'aqueous NO3';
      units       := 'umol NO3 L-1';
      symbol       := 'NO3aq';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 30;
With proc[CurrentProc] do
   begin
      name       := 'N bal NO3 uptake';
      units       := 'g N m-2 day-1';
      symbol       := 'UNO3';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 31;
With proc[CurrentProc] do
   begin
      name       := 'Plant avail doC';
      units       := 'umol C L-1';
      symbol       := 'PaDoC';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 32;
With proc[CurrentProc] do
   begin
      name       := 'doC uptake';
      units       := 'g C m-2 day-1';
      symbol       := 'UdoC';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 33;
With proc[CurrentProc] do
   begin
      name       := 'N bal doN uptake';
      units       := 'g N m-2 day-1';
      symbol       := 'UdoN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 34;
With proc[CurrentProc] do
   begin
      name       := 'N bal N fixation';
      units       := 'g N m-2 day-1';
      symbol       := 'UNfix';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 35;
With proc[CurrentProc] do
   begin
      name       := 'N bal total N uptake';
      units       := 'g N m-2 day-1';
      symbol       := 'UN';
      parameters       := 16;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='NH4 uptake const';  units:='g N effort-1 day-1';  symbol:='gNH4';
 end;
with par[npar + 2] do
 begin
    name:='NH4 half sat const';  units:='umol L-1';  symbol:='kNH4';
 end;
with par[npar + 3] do
 begin
    name:='Q10 NH4 uptake';  units:='none';  symbol:='Q10NH4';
 end;
with par[npar + 4] do
 begin
    name:='NO3 uptake const';  units:='g N effort-1 day-1';  symbol:='gNO3';
 end;
with par[npar + 5] do
 begin
    name:='NO3 half sat const';  units:='umol L-1';  symbol:='kNO3';
 end;
with par[npar + 6] do
 begin
    name:='Q10 NO3 uptake';  units:='none';  symbol:='Q10NO3';
 end;
with par[npar + 7] do
 begin
    name:='Soil PadoC fraction';  units:='g N';  symbol:='bdoC';
 end;
with par[npar + 8] do
 begin
    name:='doC uptake const';  units:='g C effort-1 day-1';  symbol:='gdoC';
 end;
with par[npar + 9] do
 begin
    name:='doC half sat const';  units:='umol L-1';  symbol:='kdoC';
 end;
with par[npar + 10] do
 begin
    name:='Q10 doC uptake';  units:='none';  symbol:='Q10doC';
 end;
with par[npar + 11] do
 begin
    name:='N fix rate const';  units:='g N effort-1 day-1';  symbol:='gNfix';
 end;
with par[npar + 12] do
 begin
    name:='Q10 N fix';  units:='none';  symbol:='Q10Nfix';
 end;
with par[npar + 13] do
 begin
    name:='NH4 C cost';  units:='g C g-1 N';  symbol:='NH4Ccost';
 end;
with par[npar + 14] do
 begin
    name:='NO3 C cost';  units:='g C g-1 N';  symbol:='NO3Ccost';
 end;
with par[npar + 15] do
 begin
    name:='doN C cost';  units:='g C g-1 N';  symbol:='doNCcost';
 end;
with par[npar + 16] do
 begin
    name:='N fix C cost';  units:='g C g-1 N';  symbol:='NfixCcost';
 end;
 
CurrentProc := ModelDef.numstate + 36;
With proc[CurrentProc] do
   begin
      name       := 'aqueous PO4';
      units       := 'umol PO4 L-1';
      symbol       := 'PO4aq';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 37;
With proc[CurrentProc] do
   begin
      name       := 'P bal PO4 uptake';
      units       := 'g P m-2 day-1';
      symbol       := 'UPO4';
      parameters       := 3;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='PO4 uptake const';  units:='g P effort-1 day-1';  symbol:='gPO4';
 end;
with par[npar + 2] do
 begin
    name:='PO4 half sat const';  units:='umol L-1';  symbol:='kPO4';
 end;
with par[npar + 3] do
 begin
    name:='Q10 PO4 uptake';  units:='none';  symbol:='Q10PO4';
 end;
 
CurrentProc := ModelDef.numstate + 38;
With proc[CurrentProc] do
   begin
      name       := 'actual N conc.';
      units       := 'g N g-1 DW';
      symbol       := 'aqN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 39;
With proc[CurrentProc] do
   begin
      name       := 'optimum N conc.';
      units       := 'g N g-1 DW';
      symbol       := 'qN';
      parameters       := 7;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='leaf N:DW';  units:='g N g-1 DW';  symbol:='qNL';
 end;
with par[npar + 2] do
 begin
    name:='Wood N:DW';  units:='g N g-1 DW';  symbol:='qNW';
 end;
with par[npar + 3] do
 begin
    name:='root N:DW';  units:='g N g-1 DW';  symbol:='qNR';
 end;
with par[npar + 4] do
 begin
    name:='leaf litter N:DW';  units:='g N g-1 DW';  symbol:='qNLl';
 end;
with par[npar + 5] do
 begin
    name:='wood litter N:DW';  units:='g N g-1 DW';  symbol:='qNWl';
 end;
with par[npar + 6] do
 begin
    name:='root litter N:DW';  units:='g N g-1 DW';  symbol:='qNRl';
 end;
with par[npar + 7] do
 begin
    name:='stoichio feedback';  units:='none';  symbol:='kq';
 end;
 
CurrentProc := ModelDef.numstate + 40;
With proc[CurrentProc] do
   begin
      name       := 'actual P conc.';
      units       := 'g P g-1 DW';
      symbol       := 'aqP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 41;
With proc[CurrentProc] do
   begin
      name       := 'optimum P conc.';
      units       := 'g P g-1 DW';
      symbol       := 'qP';
      parameters       := 6;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='leaf P:DW';  units:='g P g-1 DW';  symbol:='qPL';
 end;
with par[npar + 2] do
 begin
    name:='Wood P:DW';  units:='g P g-1 DW';  symbol:='qPW';
 end;
with par[npar + 3] do
 begin
    name:='root P:DW';  units:='g P g-1 DW';  symbol:='qPR';
 end;
with par[npar + 4] do
 begin
    name:='leaf litter P:DW';  units:='g P g-1 DW';  symbol:='qPLl';
 end;
with par[npar + 5] do
 begin
    name:='wood litter P:DW';  units:='g P g-1 DW';  symbol:='qPWl';
 end;
with par[npar + 6] do
 begin
    name:='root litter P:DW';  units:='g P g-1 DW';  symbol:='qPRl';
 end;
 
CurrentProc := ModelDef.numstate + 42;
With proc[CurrentProc] do
   begin
      name       := 'C bal Litter C';
      units       := 'g C m-2 day-1';
      symbol       := 'LitC';
      parameters       := 3;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='leaf turnover evergreen';  units:='day-1';  symbol:='maL';
 end;
with par[npar + 2] do
 begin
    name:='BW turnover';  units:='day-1';  symbol:='mW';
 end;
with par[npar + 3] do
 begin
    name:='root turnover';  units:='day-1';  symbol:='maR';
 end;
 
CurrentProc := ModelDef.numstate + 43;
With proc[CurrentProc] do
   begin
      name       := 'N bal Litter N';
      units       := 'g N m-2 day-1';
      symbol       := 'LitN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 44;
With proc[CurrentProc] do
   begin
      name       := 'P bal Litter P';
      units       := 'g P m-2 day-1';
      symbol       := 'LitP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 45;
With proc[CurrentProc] do
   begin
      name       := 'C bal Fine-Debris Litter';
      units       := 'g C m-2 day-1';
      symbol       := 'LitCDebris';
      parameters       := 3;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='Standing dead fine litt frac';  units:='fraction';  symbol:='fDebris';
 end;
with par[npar + 2] do
 begin
    name:='C:N LitCDebris';  units:='g C g-1 N';  symbol:='qNLDebris';
 end;
with par[npar + 3] do
 begin
    name:='C:P LitCDebris';  units:='g C g-1 P';  symbol:='qPLDebris';
 end;
 
CurrentProc := ModelDef.numstate + 46;
With proc[CurrentProc] do
   begin
      name       := 'N bal Fine-Debris Litter';
      units       := 'g N m-2 day-1';
      symbol       := 'LitNDebris';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 47;
With proc[CurrentProc] do
   begin
      name       := 'P bal Fine-Debris Litter';
      units       := 'g P m-2 day-1';
      symbol       := 'LitPDebris';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 48;
With proc[CurrentProc] do
   begin
      name       := 'C coarse litter';
      units       := 'g C m-2 day-1';
      symbol       := 'LcWC';
      parameters       := 4;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='coarse woody litter';  units:='day-1';  symbol:='mcw';
 end;
with par[npar + 2] do
 begin
    name:='wood litter exp';  units:='none';  symbol:='mcwex';
 end;
with par[npar + 3] do
 begin
    name:='coarse litter N:DW';  units:='g N g-1 DW';  symbol:='qNWwl';
 end;
with par[npar + 4] do
 begin
    name:='coarse litter P:DW';  units:='g P g-1 DW';  symbol:='qPWwl';
 end;
 
CurrentProc := ModelDef.numstate + 49;
With proc[CurrentProc] do
   begin
      name       := 'N coarse litter';
      units       := 'g N m-2 day-1';
      symbol       := 'LcWN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 50;
With proc[CurrentProc] do
   begin
      name       := 'P coarse litter';
      units       := 'g P m-2 day-1';
      symbol       := 'LcWP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 51;
With proc[CurrentProc] do
   begin
      name       := 'C CWD activation';
      units       := 'g C m-2 day-1';
      symbol       := 'LcWCa';
      parameters       := 1;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='CWD turnover';  units:='day-1';  symbol:='omega';
 end;
 
CurrentProc := ModelDef.numstate + 52;
With proc[CurrentProc] do
   begin
      name       := 'N CWD activation';
      units       := 'g N m-2 day-1';
      symbol       := 'LcWNa';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 53;
With proc[CurrentProc] do
   begin
      name       := 'P CWD activation';
      units       := 'g P m-2 day-1';
      symbol       := 'LcWPa';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 54;
With proc[CurrentProc] do
   begin
      name       := 'maint C req';
      units       := 'g C m-2 day-1';
      symbol       := 'RCPm';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 55;
With proc[CurrentProc] do
   begin
      name       := 'C bal total plant resp';
      units       := 'g C m-2 day-1';
      symbol       := 'RCPt';
      parameters       := 4;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='active resp';  units:='g C g-1 N day-1';  symbol:='rma';
 end;
with par[npar + 2] do
 begin
    name:='woody resp';  units:='g C g-1 N day-1';  symbol:='rmw';
 end;
with par[npar + 3] do
 begin
    name:='sapwd:heartwd parti exp';  units:='m2 g-1 DW';  symbol:='krmw';
 end;
with par[npar + 4] do
 begin
    name:='growth resp';  units:='fraction';  symbol:='rg';
 end;
 
CurrentProc := ModelDef.numstate + 56;
With proc[CurrentProc] do
   begin
      name       := 'N-use eff';
      units       := 'unitless';
      symbol       := 'NUE';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 57;
With proc[CurrentProc] do
   begin
      name       := 'P-use eff';
      units       := 'unitless';
      symbol       := 'PUE';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 58;
With proc[CurrentProc] do
   begin
      name       := 'H2O-use eff';
      units       := 'unitless';
      symbol       := 'WUE';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 59;
With proc[CurrentProc] do
   begin
      name       := 'allocatable effort';
      units       := 'effort g-1 DW';
      symbol       := 'Vstar';
      parameters       := 6;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='acclim rate';  units:='day-1';  symbol:='acc';
 end;
with par[npar + 2] do
 begin
    name:='requirement turnover';  units:='day-1';  symbol:='tau';
 end;
with par[npar + 3] do
 begin
    name:='minimum effort';  units:='effort';  symbol:='chi0';
 end;
with par[npar + 4] do
 begin
    name:='C sub effort acclim rate';  units:='day-1';  symbol:='omegavC';
 end;
with par[npar + 5] do
 begin
    name:='N sub effort acclim rate';  units:='day-1';  symbol:='omegavN';
 end;
with par[npar + 6] do
 begin
    name:='max req to up index';  units:='none';  symbol:='lambda';
 end;
 
CurrentProc := ModelDef.numstate + 60;
With proc[CurrentProc] do
   begin
      name       := 'growth C req';
      units       := 'g C m-2 day-1';
      symbol       := 'RCg';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 61;
With proc[CurrentProc] do
   begin
      name       := 'growth N req';
      units       := 'g N m-2 day-1';
      symbol       := 'RNg';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 62;
With proc[CurrentProc] do
   begin
      name       := 'growth P req';
      units       := 'g P m-2 day-1';
      symbol       := 'RPg';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 63;
With proc[CurrentProc] do
   begin
      name       := 'marginal yield NH4';
      units       := 'g N effort-1 m-2 day-1';
      symbol       := 'yNH4';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 64;
With proc[CurrentProc] do
   begin
      name       := 'marginal yield NO3';
      units       := 'g N effort-1 m-2 day-1';
      symbol       := 'yNO3';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 65;
With proc[CurrentProc] do
   begin
      name       := 'marginal yield doN';
      units       := 'g N effort-1 m-2 day-1';
      symbol       := 'ydoN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 66;
With proc[CurrentProc] do
   begin
      name       := 'marginal yield Nfix';
      units       := 'g N effort-1 m-2 day-1';
      symbol       := 'yNfix';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 67;
With proc[CurrentProc] do
   begin
      name       := 'marginal yield CO2';
      units       := 'g C effort-1 m-2 day-1';
      symbol       := 'yCO2';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 68;
With proc[CurrentProc] do
   begin
      name       := 'marginal yield light';
      units       := 'g C effort-1 m-2 day-1';
      symbol       := 'yI';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 69;
With proc[CurrentProc] do
   begin
      name       := 'marginal yield H2O';
      units       := 'g C effort-1 m-2 day-1';
      symbol       := 'yW';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 70;
With proc[CurrentProc] do
   begin
      name       := 'average C yield';
      units       := 'g C effort-1 m-2 day-1';
      symbol       := 'yCa';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 71;
With proc[CurrentProc] do
   begin
      name       := 'average N yield';
      units       := 'g N effort-1 m-2 day-1';
      symbol       := 'yNa';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 72;
With proc[CurrentProc] do
   begin
      name       := 'Total C req';
      units       := 'g C m-2 day-1';
      symbol       := 'RCt';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 73;
With proc[CurrentProc] do
   begin
      name       := 'Total N req';
      units       := 'g N m-2 day-1';
      symbol       := 'RNt';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 74;
With proc[CurrentProc] do
   begin
      name       := 'Total P req';
      units       := 'g P m-2 day-1';
      symbol       := 'RPt';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 75;
With proc[CurrentProc] do
   begin
      name       := 'Mean req:uptake';
      units       := 'none';
      symbol       := 'phi';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 76;
With proc[CurrentProc] do
   begin
      name       := 'mesophyll CO2';
      units       := 'ppm';
      symbol       := 'Ci';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 77;
With proc[CurrentProc] do
   begin
      name       := 'Total effort';
      units       := 'none';
      symbol       := 'VTot';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 78;
With proc[CurrentProc] do
   begin
      name       := 'Total root effort';
      units       := 'none';
      symbol       := 'VR';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 79;
With proc[CurrentProc] do
   begin
      name       := 'Total leaf effort';
      units       := 'none';
      symbol       := 'VL';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 80;
With proc[CurrentProc] do
   begin
      name       := 'Total soil C';
      units       := 'g C/m2';
      symbol       := 'SoCt';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 81;
With proc[CurrentProc] do
   begin
      name       := 'Total soil N';
      units       := 'g N/m2';
      symbol       := 'SoNt';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 82;
With proc[CurrentProc] do
   begin
      name       := 'Total soil P';
      units       := 'g P/m2';
      symbol       := 'SoPt';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 83;
With proc[CurrentProc] do
   begin
      name       := 'C bal NPP';
      units       := 'g C m-2 day-1';
      symbol       := 'NPP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 84;
With proc[CurrentProc] do
   begin
      name       := 'W bal runoff';
      units       := 'mm day-1';
      symbol       := 'Ro';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 85;
With proc[CurrentProc] do
   begin
      name       := 'NH4 leach';
      units       := 'g N m-2 day-1';
      symbol       := 'LNH4';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 86;
With proc[CurrentProc] do
   begin
      name       := 'NO3 leach';
      units       := 'g N m-2 day-1';
      symbol       := 'LNO3';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 87;
With proc[CurrentProc] do
   begin
      name       := 'C bal doC leach';
      units       := 'g C m-2 day-1';
      symbol       := 'LdoC';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 88;
With proc[CurrentProc] do
   begin
      name       := 'N bal doN leach';
      units       := 'g N m-2 day-1';
      symbol       := 'LdoN';
      parameters       := 1;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='C:N dom leach';  units:='g C g-1 N';  symbol:='qLdom';
 end;
 
CurrentProc := ModelDef.numstate + 89;
With proc[CurrentProc] do
   begin
      name       := 'N bal total N leach';
      units       := 'g N m-2 day-1';
      symbol       := 'LNtot';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 90;
With proc[CurrentProc] do
   begin
      name       := 'P bal PO4 leach';
      units       := 'g P m-2 day-1';
      symbol       := 'LPO4';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 91;
With proc[CurrentProc] do
   begin
      name       := 'thetaN';
      units       := 'g C/g N';
      symbol       := 'thetaN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 92;
With proc[CurrentProc] do
   begin
      name       := 'thetaP';
      units       := 'g C/g P';
      symbol       := 'thetaP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 93;
With proc[CurrentProc] do
   begin
      name       := 'N bal Micr NH4 uptake';
      units       := 'g N m-2 day-1';
      symbol       := 'UNH4m';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 94;
With proc[CurrentProc] do
   begin
      name       := 'N bal Micr NO3 uptake';
      units       := 'g N m-2 day-1';
      symbol       := 'UNO3m';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 95;
With proc[CurrentProc] do
   begin
      name       := 'N bal tot Micr N uptake';
      units       := 'g N m-2 day-1';
      symbol       := 'UNmtot';
      parameters       := 9;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='microb return C:N';  units:='g C g-1 N';  symbol:='phiN';
 end;
with par[npar + 2] do
 begin
    name:='Phase II OM C:N';  units:='g C g-1 N';  symbol:='qNSii';
 end;
with par[npar + 3] do
 begin
    name:='NH4 up par m';  units:='g N m-2 day-1';  symbol:='alphaNH4';
 end;
with par[npar + 4] do
 begin
    name:='NH4 half sat m';  units:='umol N L-1';  symbol:='kNH4m';
 end;
with par[npar + 5] do
 begin
    name:='NO3 up par m';  units:='g N m-2 day-1';  symbol:='alphaNO3';
 end;
with par[npar + 6] do
 begin
    name:='NO3 half sat m';  units:='umol N L-1';  symbol:='kNO3m';
 end;
with par[npar + 7] do
 begin
    name:='C:N Plant avail dom';  units:='g C g-1 N';  symbol:='qdom';
 end;
with par[npar + 8] do
 begin
    name:='Nitrifica rate';  units:='g N m-2 day-1';  symbol:='rrNitr';
 end;
with par[npar + 9] do
 begin
    name:='Nitrifica half sat const';  units:='umol N L-1';  symbol:='kNitr';
 end;
 
CurrentProc := ModelDef.numstate + 96;
With proc[CurrentProc] do
   begin
      name       := 'P bal Micr P uptake';
      units       := 'g P m-2 day-1';
      symbol       := 'UPO4m';
      parameters       := 4;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='microb return C:P';  units:='g C g-1 P';  symbol:='phiP';
 end;
with par[npar + 2] do
 begin
    name:='Phase II OM C:P';  units:='g C g-1 P';  symbol:='qPSii';
 end;
with par[npar + 3] do
 begin
    name:='PO4 up par m';  units:='g P m-2 day-1';  symbol:='alphaPO4';
 end;
with par[npar + 4] do
 begin
    name:='PO4 half sat m';  units:='umol P L-1';  symbol:='kPO4m';
 end;
 
CurrentProc := ModelDef.numstate + 97;
With proc[CurrentProc] do
   begin
      name       := 'total C consump';
      units       := 'g C m-2 day-1';
      symbol       := 'MC';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 98;
With proc[CurrentProc] do
   begin
      name       := 'total N consump';
      units       := 'g N m-2 day-1';
      symbol       := 'MN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 99;
With proc[CurrentProc] do
   begin
      name       := 'total P consump';
      units       := 'g P m-2 day-1';
      symbol       := 'MP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 100;
With proc[CurrentProc] do
   begin
      name       := 'C efficiency';
      units       := 'none';
      symbol       := 'LambdaC';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 101;
With proc[CurrentProc] do
   begin
      name       := 'N efficiency';
      units       := 'none';
      symbol       := 'LambdaN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 102;
With proc[CurrentProc] do
   begin
      name       := 'P efficiency';
      units       := 'none';
      symbol       := 'LambdaP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 103;
With proc[CurrentProc] do
   begin
      name       := 'C bal Microbial resp';
      units       := 'g C m-2 day-1';
      symbol       := 'RCm';
      parameters       := 8;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='max C eff';  units:='none';  symbol:='xiC';
 end;
with par[npar + 2] do
 begin
    name:='N pref coef';  units:='none';  symbol:='psiN';
 end;
with par[npar + 3] do
 begin
    name:='P pref coef';  units:='none';  symbol:='psiP';
 end;
with par[npar + 4] do
 begin
    name:='metabolic const';  units:='day-1';  symbol:='psiC';
 end;
with par[npar + 5] do
 begin
    name:='Q10 microb resp';  units:='none';  symbol:='Q10m';
 end;
with par[npar + 6] do
 begin
    name:='opt moisture';  units:='pore fraction';  symbol:='Wopt';
 end;
with par[npar + 7] do
 begin
    name:='J parameter';  units:='none';  symbol:='Jmoist';
 end;
with par[npar + 8] do
 begin
    name:='minimum moist';  units:='pore fraction';  symbol:='Wmin';
 end;
 
CurrentProc := ModelDef.numstate + 104;
With proc[CurrentProc] do
   begin
      name       := 'N bal gross N min';
      units       := 'g N m-2 day-1';
      symbol       := 'RNm';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 105;
With proc[CurrentProc] do
   begin
      name       := 'P bal gross P min';
      units       := 'g P m-2 day-1';
      symbol       := 'RPm';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 106;
With proc[CurrentProc] do
   begin
      name       := 'C bal Phase II tran';
      units       := 'g C m-2 day-1';
      symbol       := 'TiiC';
      parameters       := 1;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='Si->Sii transit. modifier';  units:='none';  symbol:='aTii';
 end;
 
CurrentProc := ModelDef.numstate + 107;
With proc[CurrentProc] do
   begin
      name       := 'N bal Phase II tran';
      units       := 'g N m-2 day-1';
      symbol       := 'TiiN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 108;
With proc[CurrentProc] do
   begin
      name       := 'P bal Phase II tran';
      units       := 'g P m-2 day-1';
      symbol       := 'TiiP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 109;
With proc[CurrentProc] do
   begin
      name       := 'C bal Phase II min';
      units       := 'g C m-2 day-1';
      symbol       := 'MiiC';
      parameters       := 1;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='Sii min rate modifier';  units:='none';  symbol:='aMii';
 end;
 
CurrentProc := ModelDef.numstate + 110;
With proc[CurrentProc] do
   begin
      name       := 'N bal Phase II min';
      units       := 'g N m-2 day-1';
      symbol       := 'MiiN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 111;
With proc[CurrentProc] do
   begin
      name       := 'P bal Phase II min';
      units       := 'g P m-2 day-1';
      symbol       := 'MiiP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 112;
With proc[CurrentProc] do
   begin
      name       := 'N Bal Non Sym N fix';
      units       := 'g N m-2 day-1';
      symbol       := 'Nnsfix';
      parameters       := 3;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='lichen rate const';  units:='g N m-2 day-1';  symbol:='betaNfix';
 end;
with par[npar + 2] do
 begin
    name:='soil Nfix rate const';  units:='g2 N g-2 C day-1';  symbol:='gammaNfix';
 end;
with par[npar + 3] do
 begin
    name:='Nfix crit C:N';  units:='g C g-1 N';  symbol:='qSfix';
 end;
 
CurrentProc := ModelDef.numstate + 113;
With proc[CurrentProc] do
   begin
      name       := 'N bal Nitrification';
      units       := 'g N m-2 day-1';
      symbol       := 'Nitr';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 114;
With proc[CurrentProc] do
   begin
      name       := 'delta effort total';
      units       := 'effort g-2 DW day-1';
      symbol       := 'dVtot';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 115;
With proc[CurrentProc] do
   begin
      name       := 'N bal total Ndep';
      units       := 'g N m-2 day-1';
      symbol       := 'Ndept';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 116;
With proc[CurrentProc] do
   begin
      name       := 'N bal net N min';
      units       := 'g N m-2 day-1';
      symbol       := 'netNmin';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 117;
With proc[CurrentProc] do
   begin
      name       := 'P bal net P min';
      units       := 'g P m-2 day-1';
      symbol       := 'netPmin';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 118;
With proc[CurrentProc] do
   begin
      name       := 'C bal NEP';
      units       := 'g C m-2 day-1';
      symbol       := 'NEP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 119;
With proc[CurrentProc] do
   begin
      name       := 'N bal Net ecos';
      units       := 'g N m-2 day-1';
      symbol       := 'NeNb';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 120;
With proc[CurrentProc] do
   begin
      name       := 'P bal Net ecos';
      units       := 'g P m-2 day-1';
      symbol       := 'NePB';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 121;
With proc[CurrentProc] do
   begin
      name       := 'C bal Net ecos';
      units       := 'g C m-2 day-1';
      symbol       := 'NeCB';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 122;
With proc[CurrentProc] do
   begin
      name       := 'W bal Net ecos';
      units       := 'mm H2O m-2 day-1';
      symbol       := 'NEWB';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 123;
With proc[CurrentProc] do
   begin
      name       := 'Cum Net Eco C bal';
      units       := 'g C m-2';
      symbol       := 'CumNeCB';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 124;
With proc[CurrentProc] do
   begin
      name       := 'Cum Net Eco N bal';
      units       := 'g C m-2';
      symbol       := 'CumNeNB';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 125;
With proc[CurrentProc] do
   begin
      name       := 'Cum Net Eco P bal';
      units       := 'g C m-2';
      symbol       := 'CumNePB';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 126;
With proc[CurrentProc] do
   begin
      name       := 'Cum Net Eco W bal';
      units       := 'g C m-2';
      symbol       := 'CumNeWB';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 127;
With proc[CurrentProc] do
   begin
      name       := 'biomass w full canopy';
      units       := 'g C m-2';
      symbol       := 'Btstar';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 128;
With proc[CurrentProc] do
   begin
      name       := 'Canopy litter';
      units       := 'g C m-2 day-1';
      symbol       := 'LL';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 129;
With proc[CurrentProc] do
   begin
      name       := 'Snow melt';
      units       := 'mm day-1';
      symbol       := 'Sm';
      parameters       := 5;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='short wave coef';  units:='none';  symbol:='SWC';
 end;
with par[npar + 2] do
 begin
    name:='longwave coef';  units:='none';  symbol:='sB';
 end;
with par[npar + 3] do
 begin
    name:='latent heat of fusion H2O';  units:='MJ mm-1 m-2';  symbol:='LHF';
 end;
with par[npar + 4] do
 begin
    name:='convective coef';  units:='mm oC-1 day-1';  symbol:='CC';
 end;
with par[npar + 5] do
 begin
    name:='snow crit temp';  units:='oC';  symbol:='Tcrit';
 end;
 
CurrentProc := ModelDef.numstate + 130;
With proc[CurrentProc] do
   begin
      name       := 'Day of year';
      units       := 'day';
      symbol       := 'Doy';
      parameters       := 4;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='degree day start';  units:='day';  symbol:='Ddds';
 end;
with par[npar + 2] do
 begin
    name:='time offset';  units:='days';  symbol:='Tly';
 end;
with par[npar + 3] do
 begin
    name:='latitude';  units:='degrees';  symbol:='lat';
 end;
with par[npar + 4] do
 begin
    name:='Doy divisor';  units:='days';  symbol:='DoyD';
 end;
 
CurrentProc := ModelDef.numstate + 131;
With proc[CurrentProc] do
   begin
      name       := 'Day length';
      units       := 'hr day-1';
      symbol       := 'Dl';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 132;
With proc[CurrentProc] do
   begin
      name       := 'declination';
      units       := 'radians';
      symbol       := 'delta';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 133;
With proc[CurrentProc] do
   begin
      name       := 'Interception';
      units       := 'mm day-1';
      symbol       := 'Intr';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 134;
With proc[CurrentProc] do
   begin
      name       := 'requirement ratio CA';
      units       := 'none';
      symbol       := 'OmegaC';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 135;
With proc[CurrentProc] do
   begin
      name       := 'requirement ratio N';
      units       := 'none';
      symbol       := 'OmegaN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 136;
With proc[CurrentProc] do
   begin
      name       := 'requirement ratio PO4';
      units       := 'none';
      symbol       := 'OmegaP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 137;
With proc[CurrentProc] do
   begin
      name       := 'dUC dvCO2';
      units       := 'g C m-2 day-1 effort-1';
      symbol       := 'dUCdvCO2';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 138;
With proc[CurrentProc] do
   begin
      name       := 'dUC dvW';
      units       := 'g C m-2 day-1 effort-1';
      symbol       := 'dUCdvW';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 139;
With proc[CurrentProc] do
   begin
      name       := 'dUC dvI';
      units       := 'g C m-2 day-1 effort-1';
      symbol       := 'dUCdvI';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 140;
With proc[CurrentProc] do
   begin
      name       := 'dUC dvNH4';
      units       := 'g N m-2 day-1 effort-1';
      symbol       := 'dUNdvNH4';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 141;
With proc[CurrentProc] do
   begin
      name       := 'dUC dvNO3';
      units       := 'g N m-2 day-1 effort-1';
      symbol       := 'dUNdvNO3';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 142;
With proc[CurrentProc] do
   begin
      name       := 'dUC dvdoN';
      units       := 'g N m-2 day-1 effort-1';
      symbol       := 'dUNdvdoN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 143;
With proc[CurrentProc] do
   begin
      name       := 'dUC dvNfix';
      units       := 'g N m-2 day-1 effort-1';
      symbol       := 'dUNdvNfix';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 144;
With proc[CurrentProc] do
   begin
      name       := 'dUC dVC';
      units       := 'g C m-2 day-1 effort-1';
      symbol       := 'dUCdVC';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 145;
With proc[CurrentProc] do
   begin
      name       := 'kickstarter: C effort';
      units       := 'effort day-1';
      symbol       := 'chiC';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 146;
With proc[CurrentProc] do
   begin
      name       := 'kickstarter: N effort';
      units       := 'effort day-1';
      symbol       := 'chiN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 147;
With proc[CurrentProc] do
   begin
      name       := 'kickstarter: P effort';
      units       := 'effort day-1';
      symbol       := 'chiP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 148;
With proc[CurrentProc] do
   begin
      name       := 'kickstarter: CO2 subeffort';
      units       := 'effort day-1';
      symbol       := 'zetaCO2';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 149;
With proc[CurrentProc] do
   begin
      name       := 'kickstarter: W subeffort';
      units       := 'effort day-1';
      symbol       := 'zetaW';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 150;
With proc[CurrentProc] do
   begin
      name       := 'kickstarter: I subeffort';
      units       := 'effort day-1';
      symbol       := 'zetaI';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 151;
With proc[CurrentProc] do
   begin
      name       := 'kickstarter: NH4 subeffort';
      units       := 'effort day-1';
      symbol       := 'zetaNH4';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 152;
With proc[CurrentProc] do
   begin
      name       := 'kickstarter: NO3 subeffort';
      units       := 'effort day-1';
      symbol       := 'zetaNO3';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 153;
With proc[CurrentProc] do
   begin
      name       := 'kickstarter: Nfix subeffort';
      units       := 'effort day-1';
      symbol       := 'zetaNfix';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 154;
With proc[CurrentProc] do
   begin
      name       := 'kickstarter: doN subeffort';
      units       := 'effort day-1';
      symbol       := 'zetadoN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 155;
With proc[CurrentProc] do
   begin
      name       := 'rooting depth: actual';
      units       := 'm';
      symbol       := 'z';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 156;
With proc[CurrentProc] do
   begin
      name       := 'near root depletion factor';
      units       := 'none';
      symbol       := 'betanRd';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 157;
With proc[CurrentProc] do
   begin
      name       := 'between root half distance';
      units       := 'none';
      symbol       := 'Rd';
      parameters       := 1;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='Max between root half distance';  units:='none';  symbol:='Rdmax';
 end;
 
CurrentProc := ModelDef.numstate + 158;
With proc[CurrentProc] do
   begin
      name       := 'Root Length';
      units       := 'm m-2 soil';
      symbol       := 'Rl';
      parameters       := 6;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='specific root length';  units:='m g-1 DW';  symbol:='asRl';
 end;
with par[npar + 2] do
 begin
    name:='root radius';  units:='m';  symbol:='Rr';
 end;
with par[npar + 3] do
 begin
    name:='diffusion NH4';  units:='m2 d-1';  symbol:='DNH4';
 end;
with par[npar + 4] do
 begin
    name:='diffusion NO3';  units:='m2 d-1';  symbol:='DNO3';
 end;
with par[npar + 5] do
 begin
    name:='diffusion dom';  units:='m2 d-1';  symbol:='Ddom';
 end;
with par[npar + 6] do
 begin
    name:='diffusion PO4';  units:='m2 d-1';  symbol:='DPO4';
 end;
 
CurrentProc := ModelDef.numstate + 159;
With proc[CurrentProc] do
   begin
      name       := 'Denitrification';
      units       := 'g N m-2 day-1';
      symbol       := 'DNtr';
      parameters       := 3;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='Dntr rate constant';  units:='g N m-2 day-1';  symbol:='aDNtr';
 end;
with par[npar + 2] do
 begin
    name:='minimum moisture';  units:='fraction soil volume';  symbol:='thetaD';
 end;
with par[npar + 3] do
 begin
    name:='half sat DNtr';  units:='umol L-1';  symbol:='kDNtr';
 end;
 
CurrentProc := ModelDef.numstate + 160;
With proc[CurrentProc] do
   begin
      name       := 'Depth of peat';
      units       := 'm';
      symbol       := 'Dop';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 161;
With proc[CurrentProc] do
   begin
      name       := 'Soil volume per g C';
      units       := 'm3/g C';
      symbol       := 'VpC';
      parameters       := 2;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='initial SOC';  units:='g C/m2';  symbol:='SoC0';
 end;
with par[npar + 2] do
 begin
    name:='initial depth of peat';  units:='m';  symbol:='Dop0';
 end;
 
CurrentProc := ModelDef.numstate + 162;
With proc[CurrentProc] do
   begin
      name       := 'Unfrozen soil fraction';
      units       := 'fraction';
      symbol       := 'fuf';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 163;
With proc[CurrentProc] do
   begin
      name       := 'Soil temperature';
      units       := 'oC';
      symbol       := 'Ts';
      parameters       := 15;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='downward cond';  units:='none';  symbol:='kds';
 end;
with par[npar + 2] do
 begin
    name:='radiation term';  units:='heat m2 day MJ-1';  symbol:='bI';
 end;
with par[npar + 3] do
 begin
    name:='cond organic soil';  units:='heat oC-1';  symbol:='kso';
 end;
with par[npar + 4] do
 begin
    name:='cond mineral soil';  units:='heat oC-1';  symbol:='ksm';
 end;
with par[npar + 5] do
 begin
    name:='cond water';  units:='heat oC-1';  symbol:='kw';
 end;
with par[npar + 6] do
 begin
    name:='snow conductance';  units:='heat oC-1 mm-1';  symbol:='ksnow';
 end;
with par[npar + 7] do
 begin
    name:='heat cap organic soil';  units:='oC heat-1';  symbol:='cso';
 end;
with par[npar + 8] do
 begin
    name:='heat cap min soil';  units:='oC heat-1';  symbol:='csm';
 end;
with par[npar + 9] do
 begin
    name:='heat cap water';  units:='oC heat-1';  symbol:='cw';
 end;
with par[npar + 10] do
 begin
    name:='zero low limit';  units:='heat';  symbol:='Ql';
 end;
with par[npar + 11] do
 begin
    name:='zero high limit';  units:='heat';  symbol:='Qh';
 end;
with par[npar + 12] do
 begin
    name:='deep soil temperature';  units:='oC';  symbol:='Tds';
 end;
with par[npar + 13] do
 begin
    name:='snow albedo';  units:='fraction';  symbol:='alpha';
 end;
with par[npar + 14] do
 begin
    name:='distance Dop to deep soil';  units:='m';  symbol:='zTds';
 end;
with par[npar + 15] do
 begin
    name:='depth of surface soil T';  units:='m';  symbol:='zTs';
 end;
 
CurrentProc := ModelDef.numstate + 164;
With proc[CurrentProc] do
   begin
      name       := 'Upward cond';
      units       := 'heat oC-1';
      symbol       := 'ks';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 165;
With proc[CurrentProc] do
   begin
      name       := 'Upward thaw cond';
      units       := 'heat oC-1';
      symbol       := 'kst';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 166;
With proc[CurrentProc] do
   begin
      name       := 'Upward frozen cond';
      units       := 'heat oC-1';
      symbol       := 'ksf';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 167;
With proc[CurrentProc] do
   begin
      name       := 'Frozen heat cap';
      units       := 'oC heat-1';
      symbol       := 'aQl';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 168;
With proc[CurrentProc] do
   begin
      name       := 'Thawed heat cap';
      units       := 'oC heat-1';
      symbol       := 'aQh';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 169;
With proc[CurrentProc] do
   begin
      name       := 'Snowfall';
      units       := 'mm';
      symbol       := 'Sfl';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 170;
With proc[CurrentProc] do
   begin
      name       := 'Rainfall';
      units       := 'mm';
      symbol       := 'Rfl';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 171;
With proc[CurrentProc] do
   begin
      name       := 'Volumetric water content';
      units       := 'unitless';
      symbol       := 'theta';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 172;
With proc[CurrentProc] do
   begin
      name       := 'Overland flow:Runin';
      units       := 'mm H2O day-1';
      symbol       := 'OvfR';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 173;
With proc[CurrentProc] do
   begin
      name       := 'Overland doC:Runin';
      units       := 'g C m-2 day-1';
      symbol       := 'ROvfdoC';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 174;
With proc[CurrentProc] do
   begin
      name       := 'Overland doN:Runin';
      units       := 'g N m-2 day-1';
      symbol       := 'ROvfdoN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 175;
With proc[CurrentProc] do
   begin
      name       := 'Overland NH4:Runin';
      units       := 'g N m-2 day-1';
      symbol       := 'ROvfNH4';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 176;
With proc[CurrentProc] do
   begin
      name       := 'Overland NO3:Runin';
      units       := 'g N m-2 day-1';
      symbol       := 'ROvfNO3';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 177;
With proc[CurrentProc] do
   begin
      name       := 'Overland PO4:Runin';
      units       := 'g P m-2 day-1';
      symbol       := 'ROvfPO4';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 178;
With proc[CurrentProc] do
   begin
      name       := 'Overland flow:Infil';
      units       := 'mm H2O day-1';
      symbol       := 'OvfI';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 179;
With proc[CurrentProc] do
   begin
      name       := 'Depth of thaw';
      units       := 'm';
      symbol       := 'Dot';
      parameters       := 7;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='Initial moisture';  units:='unitless';  symbol:='thetab';
 end;
with par[npar + 2] do
 begin
    name:='Initial soil T';  units:='oC';  symbol:='Tsb';
 end;
with par[npar + 3] do
 begin
    name:='Dot par1';  units:='oC-1';  symbol:='xi1';
 end;
with par[npar + 4] do
 begin
    name:='Dot par2';  units:='oC-1';  symbol:='xi2';
 end;
with par[npar + 5] do
 begin
    name:='Dot par3';  units:='m oC-1';  symbol:='xi3';
 end;
with par[npar + 6] do
 begin
    name:='Dot par4';  units:='m';  symbol:='xi4';
 end;
with par[npar + 7] do
 begin
    name:='Dot par5';  units:='m oC-1';  symbol:='xi5';
 end;
 
CurrentProc := ModelDef.numstate + 180;
With proc[CurrentProc] do
   begin
      name       := 'Calibration';
      units       := 'none';
      symbol       := 'calib';
      parameters       := 68;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='calibrate';  units:='none';  symbol:='calibrate';
 end;
with par[npar + 2] do
 begin
    name:='Secondary calibrate';  units:='none';  symbol:='calibrate2nd';
 end;
with par[npar + 3] do
 begin
    name:='Peak season BC target';  units:='g C m-2';  symbol:='BCpeaktar';
 end;
with par[npar + 4] do
 begin
    name:='gI target';  units:='effort';  symbol:='gItar';
 end;
with par[npar + 5] do
 begin
    name:='LAI peak target';  units:='m2 m-2';  symbol:='LAItar';
 end;
with par[npar + 6] do
 begin
    name:='GPP target';  units:='g C m-2 yr-1';  symbol:='GPPtar';
 end;
with par[npar + 7] do
 begin
    name:='NPP target';  units:='g C m-2 yr-1';  symbol:='NPPtar';
 end;
with par[npar + 8] do
 begin
    name:='Fast Fine Lit C target';  units:='g C m-2 yr-1';  symbol:='LitCtar';
 end;
with par[npar + 9] do
 begin
    name:='LcWC target';  units:='g C m-2 yr-1';  symbol:='LcWCtar';
 end;
with par[npar + 10] do
 begin
    name:='veg NH4 uptake target';  units:='g N m-2 yr-1';  symbol:='UNH4tar';
 end;
with par[npar + 11] do
 begin
    name:='veg NO3 uptake target';  units:='g N m-2 yr-1';  symbol:='UNO3tar';
 end;
with par[npar + 12] do
 begin
    name:='veg doN uptake target';  units:='g N m-2 yr-1';  symbol:='UdoNtar';
 end;
with par[npar + 13] do
 begin
    name:='Nfix target';  units:='g N m-2 yr-1';  symbol:='UNfixtar';
 end;
with par[npar + 14] do
 begin
    name:='Fast Fine Lit N target';  units:='g N m-2 yr-1';  symbol:='LitNtar';
 end;
with par[npar + 15] do
 begin
    name:='LcWN target';  units:='g N m-2 yr-1';  symbol:='LcWNtar';
 end;
with par[npar + 16] do
 begin
    name:='veg PO4 uptake target';  units:='g P m-2 yr-1';  symbol:='UPO4tar';
 end;
with par[npar + 17] do
 begin
    name:='Fast Fine Lit P target';  units:='g P m-2 yr-1';  symbol:='LitPtar';
 end;
with par[npar + 18] do
 begin
    name:='LcWP target';  units:='g P m-2 yr-1';  symbol:='LcWPtar';
 end;
with par[npar + 19] do
 begin
    name:='LcWCa target';  units:='g C m-2 yr-1';  symbol:='LcWCatar';
 end;
with par[npar + 20] do
 begin
    name:='LcWNa target';  units:='g N m-2 yr-1';  symbol:='LcWNatar';
 end;
with par[npar + 21] do
 begin
    name:='LcWPa target';  units:='g P m-2 yr-1';  symbol:='LcWPatar';
 end;
with par[npar + 22] do
 begin
    name:='Phase I resp tar';  units:='g C m-2 yr-1';  symbol:='RCmtar';
 end;
with par[npar + 23] do
 begin
    name:='TiiC target';  units:='g C m-2 yr-1';  symbol:='TiiCtar';
 end;
with par[npar + 24] do
 begin
    name:='microbe NH4 uptake target';  units:='g N m-2 yr-1';  symbol:='UNH4mtar';
 end;
with par[npar + 25] do
 begin
    name:='microbe NO3 uptake target';  units:='g N m-2 yr-1';  symbol:='UNO3mtar';
 end;
with par[npar + 26] do
 begin
    name:='Phase I N min target';  units:='g N m-2 yr-1';  symbol:='RNmtar';
 end;
with par[npar + 27] do
 begin
    name:='nonsymb Nfix target';  units:='g N m-2 yr-1';  symbol:='Nnsfixtar';
 end;
with par[npar + 28] do
 begin
    name:='microbe PO4 uptake target';  units:='g P m-2 yr-1';  symbol:='UPO4mtar';
 end;
with par[npar + 29] do
 begin
    name:='Phase I P min target';  units:='g P m-2 yr-1';  symbol:='RPmtar';
 end;
with par[npar + 30] do
 begin
    name:='MiiC target';  units:='g C m-2 yr-1';  symbol:='MiiCtar';
 end;
with par[npar + 31] do
 begin
    name:='LNH4 target';  units:='g N m-2 yr-1';  symbol:='LNH4tar';
 end;
with par[npar + 32] do
 begin
    name:='Nitrification target';  units:='g N m-2 yr-1';  symbol:='Nitrtar';
 end;
with par[npar + 33] do
 begin
    name:='LNO3 target';  units:='g N m-2 yr-1';  symbol:='LNO3tar';
 end;
with par[npar + 34] do
 begin
    name:='Denitr target';  units:='g N m-2 yr-1';  symbol:='DNtrtar';
 end;
with par[npar + 35] do
 begin
    name:='Paw target';  units:='g P m-2 yr-1';  symbol:='Pawtar';
 end;
with par[npar + 36] do
 begin
    name:='Pnow target';  units:='g P m-2 yr-1';  symbol:='Pnowtar';
 end;
with par[npar + 37] do
 begin
    name:='LPO4 target';  units:='g P m-2 yr-1';  symbol:='LPO4tar';
 end;
with par[npar + 38] do
 begin
    name:='PO4P target';  units:='g P m-2 yr-1';  symbol:='PO4Ptar';
 end;
with par[npar + 39] do
 begin
    name:='Pocclw target';  units:='g P m-2 yr-1';  symbol:='PocclWtar';
 end;
with par[npar + 40] do
 begin
    name:='Pnos target';  units:='g P m-2 yr-1';  symbol:='Pnostar';
 end;
with par[npar + 41] do
 begin
    name:='LdoC target';  units:='g C m-2 yr-1';  symbol:='LdoCtar';
 end;
with par[npar + 42] do
 begin
    name:='UW target';  units:='mm H2O yr-1';  symbol:='UWtar';
 end;
with par[npar + 43] do
 begin
    name:='Ro target';  units:='mm H2O yr-1';  symbol:='Rotar';
 end;
with par[npar + 44] do
 begin
    name:='Intercept target';  units:='mm H2O yr-1';  symbol:='Intrtar';
 end;
with par[npar + 45] do
 begin
    name:='VC target';  units:='effort';  symbol:='VCtar';
 end;
with par[npar + 46] do
 begin
    name:='VN target';  units:='effort';  symbol:='VNtar';
 end;
with par[npar + 47] do
 begin
    name:='VP target';  units:='effort';  symbol:='VPtar';
 end;
with par[npar + 48] do
 begin
    name:='vCO2 target';  units:='effort';  symbol:='vCO2tar';
 end;
with par[npar + 49] do
 begin
    name:='vI target';  units:='effort';  symbol:='vItar';
 end;
with par[npar + 50] do
 begin
    name:='vW target';  units:='effort';  symbol:='vWtar';
 end;
with par[npar + 51] do
 begin
    name:='vNH4 target';  units:='effort';  symbol:='vNH4tar';
 end;
with par[npar + 52] do
 begin
    name:='vNO3 target';  units:='effort';  symbol:='vNO3tar';
 end;
with par[npar + 53] do
 begin
    name:='vDON target';  units:='effort';  symbol:='vDONtar';
 end;
with par[npar + 54] do
 begin
    name:='vNfix target';  units:='effort';  symbol:='vNfixtar';
 end;
with par[npar + 55] do
 begin
    name:='C Fine-Debris Tar';  units:='g C m-2 yr-1';  symbol:='LitCDebristar';
 end;
with par[npar + 56] do
 begin
    name:='N Fine-Debris Tar';  units:='g N m-2 yr-1';  symbol:='LitNDebristar';
 end;
with par[npar + 57] do
 begin
    name:='P Fine-Debris Tar';  units:='g P m-2 yr-1';  symbol:='LitPDebristar';
 end;
with par[npar + 58] do
 begin
    name:='Fire BC loss tar';  units:='g C m-2 yr-1';  symbol:='FBCtar';
 end;
with par[npar + 59] do
 begin
    name:='Fire BN loss tar';  units:='g N m-2 yr-1';  symbol:='FBNtar';
 end;
with par[npar + 60] do
 begin
    name:='Fire BP loss tar';  units:='g P m-2 yr-1';  symbol:='FBPtar';
 end;
with par[npar + 61] do
 begin
    name:='Fire WC loss tar';  units:='g C m-2 yr-1';  symbol:='FWCtar';
 end;
with par[npar + 62] do
 begin
    name:='Fire WN loss tar';  units:='g N m-2 yr-1';  symbol:='FWNtar';
 end;
with par[npar + 63] do
 begin
    name:='Fire WP loss tar';  units:='g P m-2 yr-1';  symbol:='FWPtar';
 end;
with par[npar + 64] do
 begin
    name:='Fire DC loss tar';  units:='g C m-2 yr-1';  symbol:='FDCtar';
 end;
with par[npar + 65] do
 begin
    name:='Fire DN loss tar';  units:='g N m-2 yr-1';  symbol:='FDNtar';
 end;
with par[npar + 66] do
 begin
    name:='Fire DP loss tar';  units:='g P m-2 yr-1';  symbol:='FDPtar';
 end;
with par[npar + 67] do
 begin
    name:='Fire totN volatilized tar';  units:='g N m-2 yr-1';  symbol:='FNvttar';
 end;
with par[npar + 68] do
 begin
    name:='Fire totP volatilized tar';  units:='g P m-2 yr-1';  symbol:='FPvttar';
 end;
 
CurrentProc := ModelDef.numstate + 181;
With proc[CurrentProc] do
   begin
      name       := 'Soil temp water response';
      units       := 'dum';
      symbol       := 'deltaDW';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 182;
With proc[CurrentProc] do
   begin
      name       := 'Years since 2005';
      units       := 'year';
      symbol       := 'Yf';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 183;
With proc[CurrentProc] do
   begin
      name       := 'CO2 deviation';
      units       := 'ppm';
      symbol       := 'SCdev';
      parameters       := 3;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='Flag: Project CO2?';  units:='none';  symbol:='FlagCa';
 end;
with par[npar + 2] do
 begin
    name:='CO2 dev slope';  units:='ppm/year';  symbol:='mCdev';
 end;
with par[npar + 3] do
 begin
    name:='CO2 dev intercept';  units:='ppm';  symbol:='bCdev';
 end;
 
CurrentProc := ModelDef.numstate + 184;
With proc[CurrentProc] do
   begin
      name       := 'Temp Summer Dev';
      units       := 'deg C';
      symbol       := 'STdev';
      parameters       := 8;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='Flag: Project Temp?';  units:='none';  symbol:='FlagT';
 end;
with par[npar + 2] do
 begin
    name:='summer T dev max';  units:='deg C';  symbol:='VSTd';
 end;
with par[npar + 3] do
 begin
    name:='Flag: 3.5 deg change?';  units:='none';  symbol:='FlagT35';
 end;
with par[npar + 4] do
 begin
    name:='Deltaz per time';  units:='m day-1';  symbol:='deltaz';
 end;
with par[npar + 5] do
 begin
    name:='Phase II C conc';  units:='g C m-1';  symbol:='Ccpii';
 end;
with par[npar + 6] do
 begin
    name:='Primary min P conc';  units:='g P m-1';  symbol:='PcPa';
 end;
with par[npar + 7] do
 begin
    name:='Pno P conc';  units:='g P m-1';  symbol:='PcPno';
 end;
with par[npar + 8] do
 begin
    name:='Poccl P conc';  units:='g P m-1';  symbol:='PcPoccl';
 end;
 
CurrentProc := ModelDef.numstate + 185;
With proc[CurrentProc] do
   begin
      name       := 'Temp Winter Dev';
      units       := 'deg C';
      symbol       := 'WTdev';
      parameters       := 1;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='winter T dev slope';  units:='deg C';  symbol:='VWTd';
 end;
 
CurrentProc := ModelDef.numstate + 186;
With proc[CurrentProc] do
   begin
      name       := 'Ppt Simulated';
      units       := 'mm H2O';
      symbol       := 'Pptsim';
      parameters       := 7;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='Simulate Ppt?';  units:='none';  symbol:='FlagSimPpt';
 end;
with par[npar + 2] do
 begin
    name:='DOY Sum Ppt begin';  units:='day';  symbol:='DoySPpts';
 end;
with par[npar + 3] do
 begin
    name:='DOY Sum Ppt end';  units:='day';  symbol:='DoySPpte';
 end;
with par[npar + 4] do
 begin
    name:='Frac Dry Summer days';  units:='fraction';  symbol:='phiSdry';
 end;
with par[npar + 5] do
 begin
    name:='Summer Ppt intensity';  units:='mm-1 H2O';  symbol:='alphaSPpt';
 end;
with par[npar + 6] do
 begin
    name:='Frac Dry Winter days';  units:='fraction';  symbol:='phiWdry';
 end;
with par[npar + 7] do
 begin
    name:='Winter Ppt intensity';  units:='mm-1 H2O';  symbol:='alphaWPpt';
 end;
 
CurrentProc := ModelDef.numstate + 187;
With proc[CurrentProc] do
   begin
      name       := 'Ppt Summer Dev';
      units       := 'mm H2O';
      symbol       := 'SPdev';
      parameters       := 3;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='Flag: Project precip?';  units:='none';  symbol:='FlagPpt';
 end;
with par[npar + 2] do
 begin
    name:='summer Ppt dev max';  units:='mm H2O';  symbol:='VSPd';
 end;
with par[npar + 3] do
 begin
    name:='summer Ppt dev half sat';  units:='year';  symbol:='kSPd';
 end;
 
CurrentProc := ModelDef.numstate + 188;
With proc[CurrentProc] do
   begin
      name       := 'Ppt Winter Dev';
      units       := 'mm H2O';
      symbol       := 'WPdev';
      parameters       := 3;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='winter Ppt dev max';  units:='mm H2O';  symbol:='VWPd';
 end;
with par[npar + 2] do
 begin
    name:='winter Ppt dev half sat';  units:='year';  symbol:='kWPd';
 end;
with par[npar + 3] do
 begin
    name:='Days with precip';  units:='days';  symbol:='DaysPpt';
 end;
 
CurrentProc := ModelDef.numstate + 189;
With proc[CurrentProc] do
   begin
      name       := 'Projected CO2';
      units       := 'ppm';
      symbol       := 'Proj_Ca';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 190;
With proc[CurrentProc] do
   begin
      name       := 'Projected Tmax';
      units       := 'deg C';
      symbol       := 'Proj_Tmax';
      parameters       := 8;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='T winter slope';  units:='oC yr-1';  symbol:='Tmwi';
 end;
with par[npar + 2] do
 begin
    name:='T spring slope';  units:='oC yr-1';  symbol:='Tmsp';
 end;
with par[npar + 3] do
 begin
    name:='T summer slope';  units:='oC yr-1';  symbol:='Tmsu';
 end;
with par[npar + 4] do
 begin
    name:='T fall   slope';  units:='oC yr-1';  symbol:='Tmfa';
 end;
with par[npar + 5] do
 begin
    name:='T winter half sat';  units:='days';  symbol:='Tkwi';
 end;
with par[npar + 6] do
 begin
    name:='T spring half sat';  units:='days';  symbol:='Tksp';
 end;
with par[npar + 7] do
 begin
    name:='T summer half sat';  units:='days';  symbol:='Tksu';
 end;
with par[npar + 8] do
 begin
    name:='T fall   half sat';  units:='days';  symbol:='Tkfa';
 end;
 
CurrentProc := ModelDef.numstate + 191;
With proc[CurrentProc] do
   begin
      name       := 'Projected Tmin';
      units       := 'deg C';
      symbol       := 'Proj_Tmin';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 192;
With proc[CurrentProc] do
   begin
      name       := 'Projected Precip';
      units       := 'mm H2O';
      symbol       := 'Proj_Ppt';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 193;
With proc[CurrentProc] do
   begin
      name       := 'Ave grow season Temp';
      units       := 'oC';
      symbol       := 'Tave';
      parameters       := 1;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='calib Tave';  units:='oC';  symbol:='Tave0';
 end;
 
CurrentProc := ModelDef.numstate + 194;
With proc[CurrentProc] do
   begin
      name       := 'Biomass C Fire loss';
      units       := 'g C m-2 day-1';
      symbol       := 'FBC';
      parameters       := 12;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='fire interval';  units:='yrs';  symbol:='FI';
 end;
with par[npar + 2] do
 begin
    name:='day of fire';  units:='Julian day';  symbol:='DOYfire';
 end;
with par[npar + 3] do
 begin
    name:='fire leaf biomass loss';  units:='fraction';  symbol:='ffLL';
 end;
with par[npar + 4] do
 begin
    name:='fire woody biomass loss';  units:='fraction';  symbol:='ffWL';
 end;
with par[npar + 5] do
 begin
    name:='fire woody debris loss';  units:='fraction';  symbol:='ffWDL';
 end;
with par[npar + 6] do
 begin
    name:='fire Phase I soil loss';  units:='fraction';  symbol:='ffSL';
 end;
with par[npar + 7] do
 begin
    name:='fraction FBN volatilized';  units:='fraction';  symbol:='fBNv';
 end;
with par[npar + 8] do
 begin
    name:='fraction FBP volatilized';  units:='fraction';  symbol:='fBPv';
 end;
with par[npar + 9] do
 begin
    name:='fraction FWN volatilized';  units:='fraction';  symbol:='fWNv';
 end;
with par[npar + 10] do
 begin
    name:='fraction FWP volatilized';  units:='fraction';  symbol:='fWPv';
 end;
with par[npar + 11] do
 begin
    name:='fraction FDN volatilized';  units:='fraction';  symbol:='fDNv';
 end;
with par[npar + 12] do
 begin
    name:='fraction FDP volatilized';  units:='fraction';  symbol:='fDPv';
 end;
 
CurrentProc := ModelDef.numstate + 195;
With proc[CurrentProc] do
   begin
      name       := 'Biomass N Fire loss';
      units       := 'g N m-2 day-1';
      symbol       := 'FBN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 196;
With proc[CurrentProc] do
   begin
      name       := 'Biomass P Fire loss';
      units       := 'g P m-2 day-1';
      symbol       := 'FBP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 197;
With proc[CurrentProc] do
   begin
      name       := 'Debris C Fire loss';
      units       := 'g C m-2 day-1';
      symbol       := 'FWC';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 198;
With proc[CurrentProc] do
   begin
      name       := 'Debris N Fire loss';
      units       := 'g N m-2 day-1';
      symbol       := 'FWN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 199;
With proc[CurrentProc] do
   begin
      name       := 'Debris P Fire loss';
      units       := 'g P m-2 day-1';
      symbol       := 'FWP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 200;
With proc[CurrentProc] do
   begin
      name       := 'Phase I C Fire loss';
      units       := 'g C m-2 day-1';
      symbol       := 'FDC';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 201;
With proc[CurrentProc] do
   begin
      name       := 'Phase I N Fire loss';
      units       := 'g N m-2 day-1';
      symbol       := 'FDN';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 202;
With proc[CurrentProc] do
   begin
      name       := 'Phase I P Fire loss';
      units       := 'g P m-2 day-1';
      symbol       := 'FDP';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 203;
With proc[CurrentProc] do
   begin
      name       := 'Fire NO3 deposition';
      units       := 'g N m-2 day-1';
      symbol       := 'FNO3';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 204;
With proc[CurrentProc] do
   begin
      name       := 'Fire PO4 deposition';
      units       := 'g P m-2 day-1';
      symbol       := 'FPO4';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 205;
With proc[CurrentProc] do
   begin
      name       := 'Fire N volatilized';
      units       := 'g N m-2 day-1';
      symbol       := 'FNvol';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 206;
With proc[CurrentProc] do
   begin
      name       := 'Fire P volatilized';
      units       := 'g P m-2 day-1';
      symbol       := 'FPvol';
      parameters       := 0;
      ptype       := ptGroup1;
   end;
 
CurrentProc := ModelDef.numstate + 207;
With proc[CurrentProc] do
   begin
      name       := 'Standing dead LAI Equiv';
      units       := 'm2 m-2';
      symbol       := 'LW';
      parameters       := 1;
      ptype       := ptGroup1;
   end;
npar:=ParCount(CurrentProc);
with par[npar + 1] do
 begin
    name:='specif dead leaf area';  units:='m2 gDW';  symbol:='a_sdla';
 end;
 
CurrentProc := ModelDef.numstate + 208;
With proc[CurrentProc] do
   begin
      name       := 'Projected Runin';
      units       := 'mm H2O day-1';
      symbol       := 'Proj_Rin';
      parameters       := 0;
      ptype       := ptGroup1;
   end;

{ Set the total number of drivers in the model. The maximum number of drivers is
  maxdrive, in unit stypes. }
ModelDef.numdrive := 19;

{ Set the names, units, and symbols of the drivers. The maximum length for the
  name, units and symbol is 20 characters. }
 
with drive[1] do
 begin
    name:='D: total short wave';  units:='MJ m-2 day-1';  symbol:='Isw';
 end;
 
with drive[2] do
 begin
    name:='D: Temperature min';  units:='oC';  symbol:='Tmin';
 end;
 
with drive[3] do
 begin
    name:='D: Temperature max';  units:='oC';  symbol:='Tmax';
 end;
 
with drive[4] do
 begin
    name:='D: carbon dioxide';  units:='umol mol-1';  symbol:='Ca';
 end;
 
with drive[5] do
 begin
    name:='D: NH4 input';  units:='g N m-2 day-1';  symbol:='INH4';
 end;
 
with drive[6] do
 begin
    name:='D: NO3 input';  units:='g N m-2 day-1';  symbol:='INO3';
 end;
 
with drive[7] do
 begin
    name:='D: PO4 input';  units:='g P m-2 day-1';  symbol:='IPO4';
 end;
 
with drive[8] do
 begin
    name:='D: Precipitation';  units:='mm day-1';  symbol:='Ppt';
 end;
 
with drive[9] do
 begin
    name:='D: doC input';  units:='g C m-2 day-1';  symbol:='IdoC';
 end;
 
with drive[10] do
 begin
    name:='D: doN input';  units:='g N m-2 day-1';  symbol:='IdoN';
 end;
 
with drive[11] do
 begin
    name:='D: apatite input';  units:='g P m-2 day-1';  symbol:='IPa';
 end;
 
with drive[12] do
 begin
    name:='D: Run in';  units:='mm H2O day-1';  symbol:='Rin';
 end;
 
with drive[13] do
 begin
    name:='D: doC in Run in';  units:='g C m-2 day-1';  symbol:='IRindoC';
 end;
 
with drive[14] do
 begin
    name:='D: doN in Run in';  units:='g N m-2 day-1';  symbol:='IRindoN';
 end;
 
with drive[15] do
 begin
    name:='D: NH4 in Run in';  units:='g N m-2 day-1';  symbol:='IRinNH4';
 end;
 
with drive[16] do
 begin
    name:='D: NO3 in Run in';  units:='g N m-2 day-1';  symbol:='IRinNO3';
 end;
 
with drive[17] do
 begin
    name:='D: PO4 in Run in';  units:='g P m-2 day-1';  symbol:='IRinPO4';
 end;
 
with drive[18] do
 begin
    name:='D: Observed series 1';  units:='various';  symbol:='OTS1';
 end;
 
with drive[19] do
 begin
    name:='D: Observed series 2';  units:='various';  symbol:='OTS2';
 end;

{ The first numstate processes are the derivatives of the state variables. The
  code sets the names, units and symbols accordingly.}
for i:= 1 to ModelDef.numstate do proc[i].name:='d'+stat[i].name+'dt';
for i:= 1 to ModelDef.numstate do proc[i].units := stat[i].units + 't-1';
for i:= 1 to ModelDef.numstate do proc[i].symbol := 'd' + stat[i].symbol + 'dt';

{ Code to sum up the total number of parameters in the model. Do not change the
  next few lines. }
ModelDef.numparam := 0;
for i := 1 to ModelDef.NumProcess do
  ModelDef.numparam := ModelDef.numparam + proc[i].parameters;

end; // counts procedure


{ A procedure to calculate the value of all states and processes at the current
  time. This function accesses time, state variables and process variables by
  reference, ie it uses the same array as the calling routine. It does not use
  the global variables time, stat and proc because values calculated during
  integration might later be discarded. It does access the global variables par,
  drive and ModelDef directly because those values are not modified.

  The model equations are written using variable names which correspond to the
  actual name instead of using the global arrays (i.e. SoilWater instead of
  stat[7].value). This makes it necessary to switch all values into local
  variables, do all the calculations and then put everything back into the
  global variables. Lengthy but worth it in terms of readability of the code. }

// Choose either GlobalPs, ArcticPs, or none here so the appropriate Ps model is compiled below.
{$DEFINE none}

PROCEDURE processes(time:double; dtime:double; var tdrive:drivearray;
                       var tpar:paramarray; var tstat:statearray;
                       var tproc:processarray; CalculateDiscrete:Boolean);
{$IFDEF GlobalPs}
const
// Global Ps parameters
 x1 = 11.04;             x2 = 0.03;
 x5 = 0.216;             x6 = 0.6;
 x7 = 3.332;             x8 = 0.004;
 x9 = 1.549;             x10 = 1.156;
 gammastar = 0;          kCO2 = 995.4;  }
{$ENDIF}

// Modify constant above (line above "procedure processes..." line )to specify
// which Ps model and it's constants should be compiled. Choosing a Ps model
// automatically includes the Et and Misc constants (i.e. Gem is assumed).

{$IFDEF ArcticPs}
const
// Arctic Ps parameters
x1 = 0.192;	x2 = 0.125;
x5 = 2.196;	x6 = 50.41;
x7 = 0.161;	x8 = 14.78;
x9 = 1.146;
gammastar = 0.468;	kCO2 = 500.3;
{$ENDIF}

{$IFDEF ArcticPs OR GlobalPs}
//const
// General Et parameters
aE1 = 0.0004;    aE2 = 150;  aE3 = 1.21;   aE4 = 6.11262E5;

// Other constants
cp = 1.012E-9; //specific heat air MJ kg-1 oC-1
sigmaSB = 4.9e-9; //stefan-Boltzmann MJ m-2 day-1 K-4
S0 = 117.5; //solar constant MJ m-2 day-1
bHI1 =0.23;
bHI2 =0.48;
mw = 2.99; //kg h2o MJ-1
alphaMS = 2; //mm oC-1 day-1                                 }
{$ENDIF}

var
{ List the variable names you are going to use here. Generally, this list
  includes all the symbols you defined in the procedure counts above. The order
  in which you list them does not matter. }
{States}
BC, dBCdt, 
BN, dBNdt, 
BP, dBPdt, 
VC, dVCdt, 
VN, dVNdt, 
VP, dVPdt, 
vCO2, dvCO2dt, 
vI, dvIdt, 
vW, dvWdt, 
vNH4, dvNH4dt, 
vNO3, dvNO3dt, 
vdoN, dvdoNdt, 
vNfix, dvNfixdt, 
DC, dDCdt, 
DN, dDNdt, 
DP, dDPdt, 
WC, dWCdt, 
WN, dWNdt, 
WP, dWPdt, 
SC, dSCdt, 
SN, dSNdt, 
SP, dSPdt, 
ENH4, dENH4dt, 
ENO3, dENO3dt, 
EPO4, dEPO4dt, 
Pa, dPadt, 
Pno, dPnodt, 
Poccl, dPoccldt, 
W, dWdt, 
WSnow, dWSnowdt, 
SQ, dSQdt, 
fc, dfcdt, 
RCa, dRCadt, 
RNa, dRNadt, 
RPa, dRPadt, 
UCa, dUCadt, 
UNa, dUNadt, 
UPa, dUPadt, 
DDayp, dDDaypdt, 
DDayn, dDDayndt, 
CumGPP, dCumGPPdt, 
CumNPP, dCumNPPdt, 
CumNEP, dCumNEPdt, 
CumUdoC, dCumUdoCdt, 
CumLitC, dCumLitCdt, 
CumLcWC, dCumLcWCdt, 
CumtotalLitC, dCumtotalLitCdt, 
CumtotalLitN, dCumtotalLitNdt, 
CumtotalLitP, dCumtotalLitPdt, 
CumRCmtotal, dCumRCmtotaldt, 
CumNmintot, dCumNmintotdt, 
CumPmintot, dCumPmintotdt, 
CumUN, dCumUNdt, 
CumUNH4, dCumUNH4dt, 
CumUNO3, dCumUNO3dt, 
CumUdoN, dCumUdoNdt, 
CumUNfix, dCumUNfixdt, 
CumLitN, dCumLitNdt, 
CumLcWN, dCumLcWNdt, 
CumUPO4, dCumUPO4dt, 
CumLitP, dCumLitPdt, 
CumLcWP, dCumLcWPdt, 
CumLcWCa, dCumLcWCadt, 
CumRCm, dCumRCmdt, 
CumTiiC, dCumTiiCdt, 
CumLcWNa, dCumLcWNadt, 
CumUNH4m, dCumUNH4mdt, 
CumUNO3m, dCumUNO3mdt, 
CumRNm, dCumRNmdt, 
CumTiiN, dCumTiiNdt, 
CumNnsfix, dCumNnsfixdt, 
CumLcWPa, dCumLcWPadt, 
CumUPO4m, dCumUPO4mdt, 
CumRPm, dCumRPmdt, 
CumTiiP, dCumTiiPdt, 
CumMiiC, dCumMiiCdt, 
CumMiiN, dCumMiiNdt, 
CumMiiP, dCumMiiPdt, 
CumINH4, dCumINH4dt, 
CumLNH4, dCumLNH4dt, 
CumNitr, dCumNitrdt, 
CumINO3, dCumINO3dt, 
CumLNO3, dCumLNO3dt, 
CumDNtr, dCumDNtrdt, 
CumPaw, dCumPawdt, 
CumPnow, dCumPnowdt, 
CumIPO4, dCumIPO4dt, 
CumLPO4, dCumLPO4dt, 
CumPO4P, dCumPO4Pdt, 
CumIPa, dCumIPadt, 
CumPocclw, dCumPocclwdt, 
CumPnos, dCumPnosdt, 
CumIdoC, dCumIdoCdt, 
CumIdoN, dCumIdoNdt, 
CumLdoC, dCumLdoCdt, 
CumLdoN, dCumLdoNdt, 
CumUW, dCumUWdt, 
CumRO, dCumROdt, 
CumPpt, dCumPptdt, 
CumIntr, dCumIntrdt, 
CumRfl, dCumRfldt, 
CumSfl, dCumSfldt, 
CumSm, dCumSmdt, 
CumRin, dCumRindt, 
CumIRindoC, dCumIRindoCdt, 
CumIRindoN, dCumIRindoNdt, 
CumIRinNH4, dCumIRinNH4dt, 
CumIRinNO3, dCumIRinNO3dt, 
CumIRinPO4, dCumIRinPO4dt, 
CumROvf, dCumROvfdt, 
CumROvfdoC, dCumROvfdoCdt, 
CumROvfdoN, dCumROvfdoNdt, 
CumROvfNH4, dCumROvfNH4dt, 
CumROvfNO3, dCumROvfNO3dt, 
CumROvfPO4, dCumROvfPO4dt, 
LAIpeak, dLAIpeakdt, 
CumfBC, dCumfBCdt, 
CumfBN, dCumfBNdt, 
CumfBP, dCumfBPdt, 
CumfWC, dCumfWCdt, 
CumfWN, dCumfWNdt, 
CumfWP, dCumfWPdt, 
CumfDC, dCumfDCdt, 
CumfDN, dCumfDNdt, 
CumfDP, dCumfDPdt, 
CumFCvt, dCumFCvtdt, 
CumFNvt, dCumFNvtdt, 
CumFPvt, dCumFPvtdt, 
CumLitCDebris, dCumLitCDebrisdt, 
CumLitNDebris, dCumLitNDebrisdt, 
CumLitPDebris, dCumLitPDebrisdt, 
BCpeak, dBCpeakdt, 

{processes and associated parameters}
Ta, 
Bt, 
Ba, alphaB, Bamax, gammaB, a_sla, qC, 
BL, 
BR, 
BW, 
L, 
L_max, 
Gfc, fcmin, alphaGfc, betaGfc, gammaw, Ddbud, epsilonfc, 
deltaGcT, 
Lfc, Dfs, chicT, chicW, 
deltaLcW, 
Delta_E, 
psiS, theta_fp, theta_fro, theta_w, psi_f, psi_w, z0, phis, rho_s, drain, NLsfc, NLe, MBW, Intv, SNH4, etaNH4, SNO3, etaNO3, SPO4, etaPO4, zW, qPadoC, Lcrit, aLdoN, eNfix, 
Paw, rPaw, 
PO4P, rPO4P, rPnow, rPnos, rPocclw, 
Pnow, 
Pocclw, 
Pnos, 
c_cs, 
PsIrr, 
PsC, 
PsW, 
UC, FlagNegLeach, kE, gC, TaPs, TbPs, aPs, bPs, gI, HSI, kI, aPpt, scg, Q10R, 
UW, 
PET, 
NH4aq, 
UNH4, 
NO3aq, 
UNO3, 
PaDoC, 
UdoC, 
UdoN, 
UNfix, 
UN, gNH4, kNH4, Q10NH4, gNO3, kNO3, Q10NO3, bdoC, gdoC, kdoC, Q10doC, gNfix, Q10Nfix, NH4Ccost, NO3Ccost, doNCcost, NfixCcost, 
PO4aq, 
UPO4, gPO4, kPO4, Q10PO4, 
aqN, 
qN, qNL, qNW, qNR, qNLl, qNWl, qNRl, kq, 
aqP, 
qP, qPL, qPW, qPR, qPLl, qPWl, qPRl, 
LitC, maL, mW, maR, 
LitN, 
LitP, 
LitCDebris, fDebris, qNLDebris, qPLDebris, 
LitNDebris, 
LitPDebris, 
LcWC, mcw, mcwex, qNWwl, qPWwl, 
LcWN, 
LcWP, 
LcWCa, omega, 
LcWNa, 
LcWPa, 
RCPm, 
RCPt, rma, rmw, krmw, rg, 
NUE, 
PUE, 
WUE, 
Vstar, acc, tau, chi0, omegavC, omegavN, lambda, 
RCg, 
RNg, 
RPg, 
yNH4, 
yNO3, 
ydoN, 
yNfix, 
yCO2, 
yI, 
yW, 
yCa, 
yNa, 
RCt, 
RNt, 
RPt, 
phi, 
Ci, 
VTot, 
VR, 
VL, 
SoCt, 
SoNt, 
SoPt, 
NPP, 
Ro, 
LNH4, 
LNO3, 
LdoC, 
LdoN, qLdom, 
LNtot, 
LPO4, 
thetaN, 
thetaP, 
UNH4m, 
UNO3m, 
UNmtot, phiN, qNSii, alphaNH4, kNH4m, alphaNO3, kNO3m, qdom, rrNitr, kNitr, 
UPO4m, phiP, qPSii, alphaPO4, kPO4m, 
MC, 
MN, 
MP, 
LambdaC, 
LambdaN, 
LambdaP, 
RCm, xiC, psiN, psiP, psiC, Q10m, Wopt, Jmoist, Wmin, 
RNm, 
RPm, 
TiiC, aTii, 
TiiN, 
TiiP, 
MiiC, aMii, 
MiiN, 
MiiP, 
Nnsfix, betaNfix, gammaNfix, qSfix, 
Nitr, 
dVtot, 
Ndept, 
netNmin, 
netPmin, 
NEP, 
NeNb, 
NePB, 
NeCB, 
NEWB, 
CumNeCB, 
CumNeNB, 
CumNePB, 
CumNeWB, 
Btstar, 
LL, 
Sm, SWC, sB, LHF, CC, Tcrit, 
Doy, Ddds, Tly, lat, DoyD, 
Dl, 
delta, 
Intr, 
OmegaC, 
OmegaN, 
OmegaP, 
dUCdvCO2, 
dUCdvW, 
dUCdvI, 
dUNdvNH4, 
dUNdvNO3, 
dUNdvdoN, 
dUNdvNfix, 
dUCdVC, 
chiC, 
chiN, 
chiP, 
zetaCO2, 
zetaW, 
zetaI, 
zetaNH4, 
zetaNO3, 
zetaNfix, 
zetadoN, 
z, 
betanRd, 
Rd, Rdmax, 
Rl, asRl, Rr, DNH4, DNO3, Ddom, DPO4, 
DNtr, aDNtr, thetaD, kDNtr, 
Dop, 
VpC, SoC0, Dop0, 
fuf, 
Ts, kds, bI, kso, ksm, kw, ksnow, cso, csm, cw, Ql, Qh, Tds, alpha, zTds, zTs, 
ks, 
kst, 
ksf, 
aQl, 
aQh, 
Sfl, 
Rfl, 
theta, 
OvfR, 
ROvfdoC, 
ROvfdoN, 
ROvfNH4, 
ROvfNO3, 
ROvfPO4, 
OvfI, 
Dot, thetab, Tsb, xi1, xi2, xi3, xi4, xi5, 
calib, calibrate, calibrate2nd, BCpeaktar, gItar, LAItar, GPPtar, NPPtar, LitCtar, LcWCtar, UNH4tar, UNO3tar, UdoNtar, UNfixtar, LitNtar, LcWNtar, UPO4tar, LitPtar, LcWPtar, LcWCatar, LcWNatar, LcWPatar, RCmtar, TiiCtar, UNH4mtar, UNO3mtar, RNmtar, Nnsfixtar, UPO4mtar, RPmtar, MiiCtar, LNH4tar, Nitrtar, LNO3tar, DNtrtar, Pawtar, Pnowtar, LPO4tar, PO4Ptar, PocclWtar, Pnostar, LdoCtar, UWtar, Rotar, Intrtar, VCtar, VNtar, VPtar, vCO2tar, vItar, vWtar, vNH4tar, vNO3tar, vDONtar, vNfixtar, LitCDebristar, LitNDebristar, LitPDebristar, FBCtar, FBNtar, FBPtar, FWCtar, FWNtar, FWPtar, FDCtar, FDNtar, FDPtar, FNvttar, FPvttar, 
deltaDW, 
Yf, 
SCdev, FlagCa, mCdev, bCdev, 
STdev, FlagT, VSTd, FlagT35, deltaz, Ccpii, PcPa, PcPno, PcPoccl, 
WTdev, VWTd, 
Pptsim, FlagSimPpt, DoySPpts, DoySPpte, phiSdry, alphaSPpt, phiWdry, alphaWPpt, 
SPdev, FlagPpt, VSPd, kSPd, 
WPdev, VWPd, kWPd, DaysPpt, 
Proj_Ca, 
Proj_Tmax, Tmwi, Tmsp, Tmsu, Tmfa, Tkwi, Tksp, Tksu, Tkfa, 
Proj_Tmin, 
Proj_Ppt, 
Tave, Tave0, 
FBC, FI, DOYfire, ffLL, ffWL, ffWDL, ffSL, fBNv, fBPv, fWNv, fWPv, fDNv, fDPv, 
FBN, 
FBP, 
FWC, 
FWN, 
FWP, 
FDC, 
FDN, 
FDP, 
FNO3, 
FPO4, 
FNvol, 
FPvol, 
LW, a_sdla, 
Proj_Rin, 

{drivers}
Isw, 
Tmin, 
Tmax, 
Ca, 
INH4, 
INO3, 
IPO4, 
Ppt, 
IdoC, 
IdoN, 
IPa, 
Rin, 
IRindoC, 
IRindoN, 
IRinNH4, 
IRinNO3, 
IRinPO4, 
OTS1, 
OTS2, 

{Other double}
dum, 
Thf, 
InfS, 
InfR, 
divsr, 
R, 
S, 
C, 
Tsnow, 
alphaVC, 
alphaVN, 
alphaVP, 
etaVC, 
etaVN, 
etaVP, 
lambdavCO2, 
lambdavI, 
lambdavW, 
lambdavNH4, 
lambdavNO3, 
lambdavNfix, 
lambdavdoN, 
muvCO2, 
muvI, 
muvW, 
muvNH4, 
muvNO3, 
muvNfix, 
muvdoN, 
deltaPII, 
UNttar, 
kPa

:double; {Final double}

{Other integers}
npar, j, jj, kk,  tnum:integer;

{ Boolean Variables }


{ Functions or procedures }
Function FindBtstar:double;
var
 deltaB, rhs, currBtstar: double;
begin
  deltaB:=Bt/10;
  currBtstar:=Bt-deltaB;      // Subtract deltaB because deltaB is added first thing in the loop
  repeat
    currBtstar:=currBtstar+deltaB;
    rhs:=Bt + currBtstar*Bamax*VL*(1-fc)*gammaB/(power(power(Bamax,alphaB)+power(gammaB*currBtstar,alphaB),1/alphaB));
      if (rhs>currBtstar) and (deltaB<0) then
        deltaB:=-abs(currBtstar-rhs)
      else
        if (rhs<currBtstar) and (deltaB>0) then deltaB:=-abs(currBtstar-rhs)
        else deltaB:=abs(currBtstar-rhs);
  until (abs(currBtstar-rhs)<epsilon);// or (deltaB<epsilon);
  FindBtstar:=currBtstar;
end;

Procedure Assess_Allometry(VC,vW:double);
begin {allometry}
   {Biomass (DW)}
   Bt:= BC/qC;
   if Bt<1e-6 then Bt:=1e-6;
   {canopy allocation}
   VL:=VC*(1-vW);
   {Root allocation}
   VR:=1-VL;
   {find Ba & BtStar = biomass if canopy were full}
///   Bs:=Bamax-B0;
{   y:=VL*(1-fc);
{   a:=gammaB;
   b:=Bs-gammaB*Bt-Bamax*y*gammaB;
   c:=-Bs*Bt-B0*Bs*y;
   Btstar:=(-b+sqrt(b*b-4*a*c))/(2*a);
   Btstar:=min(Btstar, Bt/(1-y));}
   
   Btstar:=FindBtstar;
   Ba:=Bamax*gammaB*Btstar/(power(power(Bamax,alphaB)+power(gammaB*Btstar,alphaB),1/alphaB));
   if Ba>Btstar then Ba:=Btstar;
   {woody biomass}
   BW:=max(0,Btstar-Ba);
   {leaf biomass}
   BL:=fc*VL*Ba;
   {leaf area}
   L:=a_sla*BL;
   LW:=a_sdla*WC/qC;
   {full-canopy leaf area}
   L_max:=a_sla*VL*Ba;
   {root biomass}
   BR:=VR*Ba;
   {root length}
   Rl:=BR*asRl;
   {average between-root half distance}
   if Rl>0 then Rd:=sqrt(z/pi/Rl) else Rd:=999;
   if Rd>Rdmax then Rd:=Rdmax;
   {near-root depletion factor}
   betanRd:=sqr(sqr(Rd))-sqr(sqr(Rr))-4*sqr(sqr(Rd))*(ln(Rd)-ln(Rr));
   betanRd:=betanRd/(2*sqr(sqr(Rd)-sqr(Rr)));
   betanRd:=(1+betanRd)/(4*pi); 
end;  {allometry}

Function fCi(P,g,k,Gamma,Ca:double):double;
Var b,c:double;
begin
   if g>0 then
     begin
       b:=(P/g)+k-Ca;
       c:=-(k*Ca+P*Gamma/g);
       fCi:=0.5*(-b+sqrt(sqr(b)-4*c));
     end
   else fCI:=Gamma;
end;

Function photosynthesis(vc,vi,vw:double; var PsC, PsIrr, PsW:double):double;
var dum, c_cmax, CiI, CiC, fpt, PIx, PCx, kC, gamma:double;
begin
   Gamma:=42*exp(9.46*(Ta-25)/(Ta+273.2)); //McMurtrie et al (1992) Eq 18.
   kC:=310*exp(23.956*(Ta-25)/(Ta+273.2)); //McMurtrie et al (1992) Eq 18.
   kC:=kC*(1+200/(155*exp(14.509*(Ta-25)/(Ta+273.2))));//McMurtrie et al (1992) Eq 18. in umol/mol
   
   if fuf>0 then UW:=Dl*Delta_e*(L*kE*Rl*fuf*vw/VR/z0/phis)*(Psis-Psi_w)/abs(Psi_w) 
   else UW:=0;
   if UW<0 then UW:=0;
   if Dl>0 then c_cs:=UW/(Dl*Delta_e) else c_cs:=0;
   if Ta> 0 then 
     fpt:=exp(0.0397*(Tave-Tave0))/((1+exp(-aPs*exp(0.0165*(Tave-Tave0))*(Ta-TaPs)))*(1+exp(bPs*(Ta-TbPs))))
   else 
     fpt:=0;
   // Based on Hikosaka et al, Eq 5. Modified based on acclimation 
   if VL=0 then
     begin
       PIx:=0;
       PCx:=0;
     end
   else
     begin
       if Dl>0 then PIx:=L*gI*(vi/VL)*ln((HSI+Isw/Dl)/(HSI+Isw*exp(-kI*(L+LW))/Dl))/(kI*(L+LW))
       else PIx:=0;
       PCx:=gC*(vc/VL)*L;
     end; 
	 
	 // Check on where the exp(0.0397 came from. Showed up in v2.6.4, Ps temp response change but it's not listed in Ed's notes???
	 
   PsC:= Dl*fpt*PCx*(5*Ca/7+gamma)/(kC+5*Ca/7); 
   PsIrr:= Dl*fpt*PIx*(5*Ca/7+gamma)/(2*gamma+5*Ca/7);
   PsW:= Dl*fpt*scg*c_cs*2*Ca/7;                                                     
   dum:=min(PsC,min(PsIrr,PsW));                                                    
   if (DL>0) and (fpt>0) then c_cs:=7*dum/DL/fpt/scg/2/Ca else c_cs:=0;                            
   UW:=c_cs*DL*Delta_e;                                                                 
   Photosynthesis:=min(PsC,min(PsIrr,PsW));
end;


function uptake(Cbar,vi,km,D,g,Q10,psi:double):double;
var
  V,Croot,Cbar2:double;
begin
   if Rl>0 then
     begin
        Cbar2:=Cbar; if Cbar2<0 then Cbar2:=0;
        V:=g*(vi/VR)*Rl*power(Q10,Ts/10);
        Croot:=km-Cbar2-V*betanRd/(D*Rl*psi);
        Croot:=0.5*(-Croot+sqrt(sqr(Croot)+4*km*Cbar2));
        Uptake:=V*Croot/(km+Croot);
     end
   else uptake:=0;
end;{uptake}

function Nfix(vi,g,Q10:double):double;
begin
  Nfix:=g*(vi/VR)*power(Q10,Ts/10)*Rl/(1+power(L_max/Lcrit, Lcrit*eNfix));
end;{Nfix}

function water_tension(W:double):double;
var
   b,psi_i,m,n,theta_i:double;
begin
{Clapp & Hornberger 1978 WRR 14:601 water tension, assume air entry at 0.92 WFPS}
  theta_i:=0.92*phis;
  b:=-ln(psi_f/psi_w)/ln(theta_fp/theta_w);
  psi_i:=psi_f*power(theta_i/theta_fp,-b); 
  m:=(11.5-b)*psi_i/0.0736; 
  n:= 0.84-0.08*b/(11.5-b); 
  if theta<theta_i then water_tension:=psi_f*power(theta/theta_fp,-b)
  else water_tension:= -m*(theta/phis-n)*(theta/phis-1);
end; {water_tension}

function Tdev(DOY, Tywi, Tysp, Tysu, Tyfa:double):double;
var
   y1,y2,y3,y4,a,b,c,d:double;

Begin
   if DOY<91.25 then
      begin
          y1:=Tywi; y2:=Tysp; y3:=(Tysp-Tyfa)/182.5; y4:=(Tysu-Tywi)/182.5;
          a:= 2.632275710316e-6*y1-2.632275710316e-6*y2
                +1.20097579283168e-4*y3+1.20097579283168e-4*y4;
         b:= -3.60292737849503e-4*y1+3.60292737849503e-4*y2
               -2.19178082191781e-2*y3-1.0958904109589e-2*y4;
         c:=y3; 
         d:=y1;
     end;
   if (DOY>91.25) and (DOY<182.5) then
      begin
          y1:=Tysp; y2:=Tysu; y3:=(Tysu-Tywi)/182.5; y4:=(Tyfa-Tysp)/182.5;
          a:= 2.632275710316e-6*y1-2.632275710316e-6*y2
                +1.20097579283167e-4*y3+1.20097579283167e-4*y4;
         b:= -1.08087821354851e-3*y1+1.08087821354851e-3*y2
              -5.47945205479451e-2*y3-4.38356164383561e-2*y4;
         c:= 0.131506849315068*y1-0.131506849315068*y2+8*y3+5*y4; 
         d:=-4*y1+5*y2-365*y3-182.5*y4;
     end; 
   if (DOY>182.5) and (DOY<273.75) then
      begin
          y1:=Tysu; y2:=Tyfa; y3:=(Tyfa-Tysp)/182.5; y4:=(Tywi-Tysu)/182.5;
          a:= 2.632275710316e-6*y1-2.632275710316e-6*y2
                +1.20097579283167e-4*y3+1.20097579283167e-4*y4;
         b:= -1.8014636892475e-3*y1+1.8014636892475e-3*y2
              -8.76712328767117e-2*y3-7.67123287671228e-2*y4;
         c:= 0.394520547945203*y1-0.394520547945203*y2+21*y3+16*y4; 
         d:=-27*y1+28*y2-1642.5*y3-1095*y4;
     end; 
   if (DOY>273.75) then
      begin
          y1:=Tyfa; y2:=Tywi; y3:=(Tywi-Tysu)/182.5; y4:=(Tysp-Tyfa)/182.5;
          a:= 2.632275710316e-6*y1-2.632275710316e-6*y2
                +1.20097579283167e-4*y3+1.20097579283167e-4*y4;
         b:= -2.52204916494648e-3*y1+2.52204916494648e-3*y2
              -0.120547945205478*y3-0.109589041095889*y4;
         c:= 0.7890410958904*y1-0.789041095890401*y2+40*y3+33*y4; 
         d:=-80*y1+81*y2-4380*y3-3285*y4;
     end;
   Tdev:=a*DOY*DOY*DOY+b*DOY*DOY+c*DOY+d;
end;


begin
{ Copy the drivers from the global array, drive, into the local variables. }
Isw := tdrive[1].value;
Tmin := tdrive[2].value;
Tmax := tdrive[3].value;
Ca := tdrive[4].value;
INH4 := tdrive[5].value;
INO3 := tdrive[6].value;
IPO4 := tdrive[7].value;
Ppt := tdrive[8].value;
IdoC := tdrive[9].value;
IdoN := tdrive[10].value;
IPa := tdrive[11].value;
Rin := tdrive[12].value;
IRindoC := tdrive[13].value;
IRindoN := tdrive[14].value;
IRinNH4 := tdrive[15].value;
IRinNO3 := tdrive[16].value;
IRinPO4 := tdrive[17].value;
OTS1 := tdrive[18].value;
OTS2 := tdrive[19].value;

{ Copy the state variables from the global array into the local variables. }
BC := tstat[1].value;
BN := tstat[2].value;
BP := tstat[3].value;
VC := tstat[4].value;
VN := tstat[5].value;
VP := tstat[6].value;
vCO2 := tstat[7].value;
vI := tstat[8].value;
vW := tstat[9].value;
vNH4 := tstat[10].value;
vNO3 := tstat[11].value;
vdoN := tstat[12].value;
vNfix := tstat[13].value;
DC := tstat[14].value;
DN := tstat[15].value;
DP := tstat[16].value;
WC := tstat[17].value;
WN := tstat[18].value;
WP := tstat[19].value;
SC := tstat[20].value;
SN := tstat[21].value;
SP := tstat[22].value;
ENH4 := tstat[23].value;
ENO3 := tstat[24].value;
EPO4 := tstat[25].value;
Pa := tstat[26].value;
Pno := tstat[27].value;
Poccl := tstat[28].value;
W := tstat[29].value;
WSnow := tstat[30].value;
SQ := tstat[31].value;
fc := tstat[32].value;
RCa := tstat[33].value;
RNa := tstat[34].value;
RPa := tstat[35].value;
UCa := tstat[36].value;
UNa := tstat[37].value;
UPa := tstat[38].value;
DDayp := tstat[39].value;
DDayn := tstat[40].value;
CumGPP := tstat[41].value;
CumNPP := tstat[42].value;
CumNEP := tstat[43].value;
CumUdoC := tstat[44].value;
CumLitC := tstat[45].value;
CumLcWC := tstat[46].value;
CumtotalLitC := tstat[47].value;
CumtotalLitN := tstat[48].value;
CumtotalLitP := tstat[49].value;
CumRCmtotal := tstat[50].value;
CumNmintot := tstat[51].value;
CumPmintot := tstat[52].value;
CumUN := tstat[53].value;
CumUNH4 := tstat[54].value;
CumUNO3 := tstat[55].value;
CumUdoN := tstat[56].value;
CumUNfix := tstat[57].value;
CumLitN := tstat[58].value;
CumLcWN := tstat[59].value;
CumUPO4 := tstat[60].value;
CumLitP := tstat[61].value;
CumLcWP := tstat[62].value;
CumLcWCa := tstat[63].value;
CumRCm := tstat[64].value;
CumTiiC := tstat[65].value;
CumLcWNa := tstat[66].value;
CumUNH4m := tstat[67].value;
CumUNO3m := tstat[68].value;
CumRNm := tstat[69].value;
CumTiiN := tstat[70].value;
CumNnsfix := tstat[71].value;
CumLcWPa := tstat[72].value;
CumUPO4m := tstat[73].value;
CumRPm := tstat[74].value;
CumTiiP := tstat[75].value;
CumMiiC := tstat[76].value;
CumMiiN := tstat[77].value;
CumMiiP := tstat[78].value;
CumINH4 := tstat[79].value;
CumLNH4 := tstat[80].value;
CumNitr := tstat[81].value;
CumINO3 := tstat[82].value;
CumLNO3 := tstat[83].value;
CumDNtr := tstat[84].value;
CumPaw := tstat[85].value;
CumPnow := tstat[86].value;
CumIPO4 := tstat[87].value;
CumLPO4 := tstat[88].value;
CumPO4P := tstat[89].value;
CumIPa := tstat[90].value;
CumPocclw := tstat[91].value;
CumPnos := tstat[92].value;
CumIdoC := tstat[93].value;
CumIdoN := tstat[94].value;
CumLdoC := tstat[95].value;
CumLdoN := tstat[96].value;
CumUW := tstat[97].value;
CumRO := tstat[98].value;
CumPpt := tstat[99].value;
CumIntr := tstat[100].value;
CumRfl := tstat[101].value;
CumSfl := tstat[102].value;
CumSm := tstat[103].value;
CumRin := tstat[104].value;
CumIRindoC := tstat[105].value;
CumIRindoN := tstat[106].value;
CumIRinNH4 := tstat[107].value;
CumIRinNO3 := tstat[108].value;
CumIRinPO4 := tstat[109].value;
CumROvf := tstat[110].value;
CumROvfdoC := tstat[111].value;
CumROvfdoN := tstat[112].value;
CumROvfNH4 := tstat[113].value;
CumROvfNO3 := tstat[114].value;
CumROvfPO4 := tstat[115].value;
LAIpeak := tstat[116].value;
CumfBC := tstat[117].value;
CumfBN := tstat[118].value;
CumfBP := tstat[119].value;
CumfWC := tstat[120].value;
CumfWN := tstat[121].value;
CumfWP := tstat[122].value;
CumfDC := tstat[123].value;
CumfDN := tstat[124].value;
CumfDP := tstat[125].value;
CumFCvt := tstat[126].value;
CumFNvt := tstat[127].value;
CumFPvt := tstat[128].value;
CumLitCDebris := tstat[129].value;
CumLitNDebris := tstat[130].value;
CumLitPDebris := tstat[131].value;
BCpeak := tstat[132].value;

{ And now copy the parameters into the local variables. No need to copy the
  processes from the global array into local variables. Process values will be
  calculated by this procedure.

  Copy the parameters for each process separately using the function ParCount
  to keep track of the number of parameters in the preceeding processes.
  npar now contains the number of parameters in the preceding processes.
  copy the value of the first parameter of this process into it's local
  variable }
npar:=ParCount(ModelDef.numstate + 3);
alphaB := par[npar + 1].value;
Bamax := par[npar + 2].value;
gammaB := par[npar + 3].value;
a_sla := par[npar + 4].value;
qC := par[npar + 5].value;

npar:=ParCount(ModelDef.numstate + 9);
fcmin := par[npar + 1].value;
alphaGfc := par[npar + 2].value;
betaGfc := par[npar + 3].value;
gammaw := par[npar + 4].value;
Ddbud := par[npar + 5].value;
epsilonfc := par[npar + 6].value;
 
npar:=ParCount(ModelDef.numstate + 11);
Dfs := par[npar + 1].value;
chicT := par[npar + 2].value;
chicW := par[npar + 3].value;
 
npar:=ParCount(ModelDef.numstate + 14);
theta_fp := par[npar + 1].value;
theta_fro := par[npar + 2].value;
theta_w := par[npar + 3].value;
psi_f := par[npar + 4].value;
psi_w := par[npar + 5].value;
z0 := par[npar + 6].value;
phis := par[npar + 7].value;
rho_s := par[npar + 8].value;
drain := par[npar + 9].value;
NLsfc := par[npar + 10].value;
NLe := par[npar + 11].value;
MBW := par[npar + 12].value;
Intv := par[npar + 13].value;
SNH4 := par[npar + 14].value;
etaNH4 := par[npar + 15].value;
SNO3 := par[npar + 16].value;
etaNO3 := par[npar + 17].value;
SPO4 := par[npar + 18].value;
etaPO4 := par[npar + 19].value;
zW := par[npar + 20].value;
qPadoC := par[npar + 21].value;
Lcrit := par[npar + 22].value;
aLdoN := par[npar + 23].value;
eNfix := par[npar + 24].value;
 
npar:=ParCount(ModelDef.numstate + 15);
rPaw := par[npar + 1].value;
 
npar:=ParCount(ModelDef.numstate + 16);
rPO4P := par[npar + 1].value;
rPnow := par[npar + 2].value;
rPnos := par[npar + 3].value;
rPocclw := par[npar + 4].value;
 
npar:=ParCount(ModelDef.numstate + 24);
FlagNegLeach := par[npar + 1].value;
kE := par[npar + 2].value;
gC := par[npar + 3].value;
TaPs := par[npar + 4].value;
TbPs := par[npar + 5].value;
aPs := par[npar + 6].value;
bPs := par[npar + 7].value;
gI := par[npar + 8].value;
HSI := par[npar + 9].value;
kI := par[npar + 10].value;
aPpt := par[npar + 11].value;
scg := par[npar + 12].value;
Q10R := par[npar + 13].value;
 
npar:=ParCount(ModelDef.numstate + 35);
gNH4 := par[npar + 1].value;
kNH4 := par[npar + 2].value;
Q10NH4 := par[npar + 3].value;
gNO3 := par[npar + 4].value;
kNO3 := par[npar + 5].value;
Q10NO3 := par[npar + 6].value;
bdoC := par[npar + 7].value;
gdoC := par[npar + 8].value;
kdoC := par[npar + 9].value;
Q10doC := par[npar + 10].value;
gNfix := par[npar + 11].value;
Q10Nfix := par[npar + 12].value;
NH4Ccost := par[npar + 13].value;
NO3Ccost := par[npar + 14].value;
doNCcost := par[npar + 15].value;
NfixCcost := par[npar + 16].value;
 
npar:=ParCount(ModelDef.numstate + 37);
gPO4 := par[npar + 1].value;
kPO4 := par[npar + 2].value;
Q10PO4 := par[npar + 3].value;
 
npar:=ParCount(ModelDef.numstate + 39);
qNL := par[npar + 1].value;
qNW := par[npar + 2].value;
qNR := par[npar + 3].value;
qNLl := par[npar + 4].value;
qNWl := par[npar + 5].value;
qNRl := par[npar + 6].value;
kq := par[npar + 7].value;
 
npar:=ParCount(ModelDef.numstate + 41);
qPL := par[npar + 1].value;
qPW := par[npar + 2].value;
qPR := par[npar + 3].value;
qPLl := par[npar + 4].value;
qPWl := par[npar + 5].value;
qPRl := par[npar + 6].value;
 
npar:=ParCount(ModelDef.numstate + 42);
maL := par[npar + 1].value;
mW := par[npar + 2].value;
maR := par[npar + 3].value;
 
npar:=ParCount(ModelDef.numstate + 45);
fDebris := par[npar + 1].value;
qNLDebris := par[npar + 2].value;
qPLDebris := par[npar + 3].value;
 
npar:=ParCount(ModelDef.numstate + 48);
mcw := par[npar + 1].value;
mcwex := par[npar + 2].value;
qNWwl := par[npar + 3].value;
qPWwl := par[npar + 4].value;
 
npar:=ParCount(ModelDef.numstate + 51);
omega := par[npar + 1].value;
 
npar:=ParCount(ModelDef.numstate + 55);
rma := par[npar + 1].value;
rmw := par[npar + 2].value;
krmw := par[npar + 3].value;
rg := par[npar + 4].value;
 
npar:=ParCount(ModelDef.numstate + 59);
acc := par[npar + 1].value;
tau := par[npar + 2].value;
chi0 := par[npar + 3].value;
omegavC := par[npar + 4].value;
omegavN := par[npar + 5].value;
lambda := par[npar + 6].value;
 
npar:=ParCount(ModelDef.numstate + 88);
qLdom := par[npar + 1].value;
 
npar:=ParCount(ModelDef.numstate + 95);
phiN := par[npar + 1].value;
qNSii := par[npar + 2].value;
alphaNH4 := par[npar + 3].value;
kNH4m := par[npar + 4].value;
alphaNO3 := par[npar + 5].value;
kNO3m := par[npar + 6].value;
qdom := par[npar + 7].value;
rrNitr := par[npar + 8].value;
kNitr := par[npar + 9].value;
 
npar:=ParCount(ModelDef.numstate + 96);
phiP := par[npar + 1].value;
qPSii := par[npar + 2].value;
alphaPO4 := par[npar + 3].value;
kPO4m := par[npar + 4].value;
 
npar:=ParCount(ModelDef.numstate + 103);
xiC := par[npar + 1].value;
psiN := par[npar + 2].value;
psiP := par[npar + 3].value;
psiC := par[npar + 4].value;
Q10m := par[npar + 5].value;
Wopt := par[npar + 6].value;
Jmoist := par[npar + 7].value;
Wmin := par[npar + 8].value;
 
npar:=ParCount(ModelDef.numstate + 106);
aTii := par[npar + 1].value;
 
npar:=ParCount(ModelDef.numstate + 109);
aMii := par[npar + 1].value;
 
npar:=ParCount(ModelDef.numstate + 112);
betaNfix := par[npar + 1].value;
gammaNfix := par[npar + 2].value;
qSfix := par[npar + 3].value;
 
npar:=ParCount(ModelDef.numstate + 129);
SWC := par[npar + 1].value;
sB := par[npar + 2].value;
LHF := par[npar + 3].value;
CC := par[npar + 4].value;
Tcrit := par[npar + 5].value;
 
npar:=ParCount(ModelDef.numstate + 130);
Ddds := par[npar + 1].value;
Tly := par[npar + 2].value;
lat := par[npar + 3].value;
DoyD := par[npar + 4].value;
 
npar:=ParCount(ModelDef.numstate + 157);
Rdmax := par[npar + 1].value;
 
npar:=ParCount(ModelDef.numstate + 158);
asRl := par[npar + 1].value;
Rr := par[npar + 2].value;
DNH4 := par[npar + 3].value;
DNO3 := par[npar + 4].value;
Ddom := par[npar + 5].value;
DPO4 := par[npar + 6].value;
 
npar:=ParCount(ModelDef.numstate + 159);
aDNtr := par[npar + 1].value;
thetaD := par[npar + 2].value;
kDNtr := par[npar + 3].value;
 
npar:=ParCount(ModelDef.numstate + 161);
SoC0 := par[npar + 1].value;
Dop0 := par[npar + 2].value;
 
npar:=ParCount(ModelDef.numstate + 163);
kds := par[npar + 1].value;
bI := par[npar + 2].value;
kso := par[npar + 3].value;
ksm := par[npar + 4].value;
kw := par[npar + 5].value;
ksnow := par[npar + 6].value;
cso := par[npar + 7].value;
csm := par[npar + 8].value;
cw := par[npar + 9].value;
Ql := par[npar + 10].value;
Qh := par[npar + 11].value;
Tds := par[npar + 12].value;
alpha := par[npar + 13].value;
zTds := par[npar + 14].value;
zTs := par[npar + 15].value;
 
npar:=ParCount(ModelDef.numstate + 179);
thetab := par[npar + 1].value;
Tsb := par[npar + 2].value;
xi1 := par[npar + 3].value;
xi2 := par[npar + 4].value;
xi3 := par[npar + 5].value;
xi4 := par[npar + 6].value;
xi5 := par[npar + 7].value;
 
npar:=ParCount(ModelDef.numstate + 180);
calibrate := par[npar + 1].value;
calibrate2nd := par[npar + 2].value;
BCpeaktar := par[npar + 3].value;
gItar := par[npar + 4].value;
LAItar := par[npar + 5].value;
GPPtar := par[npar + 6].value;
NPPtar := par[npar + 7].value;
LitCtar := par[npar + 8].value;
LcWCtar := par[npar + 9].value;
UNH4tar := par[npar + 10].value;
UNO3tar := par[npar + 11].value;
UdoNtar := par[npar + 12].value;
UNfixtar := par[npar + 13].value;
LitNtar := par[npar + 14].value;
LcWNtar := par[npar + 15].value;
UPO4tar := par[npar + 16].value;
LitPtar := par[npar + 17].value;
LcWPtar := par[npar + 18].value;
LcWCatar := par[npar + 19].value;
LcWNatar := par[npar + 20].value;
LcWPatar := par[npar + 21].value;
RCmtar := par[npar + 22].value;
TiiCtar := par[npar + 23].value;
UNH4mtar := par[npar + 24].value;
UNO3mtar := par[npar + 25].value;
RNmtar := par[npar + 26].value;
Nnsfixtar := par[npar + 27].value;
UPO4mtar := par[npar + 28].value;
RPmtar := par[npar + 29].value;
MiiCtar := par[npar + 30].value;
LNH4tar := par[npar + 31].value;
Nitrtar := par[npar + 32].value;
LNO3tar := par[npar + 33].value;
DNtrtar := par[npar + 34].value;
Pawtar := par[npar + 35].value;
Pnowtar := par[npar + 36].value;
LPO4tar := par[npar + 37].value;
PO4Ptar := par[npar + 38].value;
PocclWtar := par[npar + 39].value;
Pnostar := par[npar + 40].value;
LdoCtar := par[npar + 41].value;
UWtar := par[npar + 42].value;
Rotar := par[npar + 43].value;
Intrtar := par[npar + 44].value;
VCtar := par[npar + 45].value;
VNtar := par[npar + 46].value;
VPtar := par[npar + 47].value;
vCO2tar := par[npar + 48].value;
vItar := par[npar + 49].value;
vWtar := par[npar + 50].value;
vNH4tar := par[npar + 51].value;
vNO3tar := par[npar + 52].value;
vDONtar := par[npar + 53].value;
vNfixtar := par[npar + 54].value;
LitCDebristar := par[npar + 55].value;
LitNDebristar := par[npar + 56].value;
LitPDebristar := par[npar + 57].value;
FBCtar := par[npar + 58].value;
FBNtar := par[npar + 59].value;
FBPtar := par[npar + 60].value;
FWCtar := par[npar + 61].value;
FWNtar := par[npar + 62].value;
FWPtar := par[npar + 63].value;
FDCtar := par[npar + 64].value;
FDNtar := par[npar + 65].value;
FDPtar := par[npar + 66].value;
FNvttar := par[npar + 67].value;
FPvttar := par[npar + 68].value;
 
npar:=ParCount(ModelDef.numstate + 183);
FlagCa := par[npar + 1].value;
mCdev := par[npar + 2].value;
bCdev := par[npar + 3].value;
 
npar:=ParCount(ModelDef.numstate + 184);
FlagT := par[npar + 1].value;
VSTd := par[npar + 2].value;
FlagT35 := par[npar + 3].value;
deltaz := par[npar + 4].value;
Ccpii := par[npar + 5].value;
PcPa := par[npar + 6].value;
PcPno := par[npar + 7].value;
PcPoccl := par[npar + 8].value;
 
npar:=ParCount(ModelDef.numstate + 185);
VWTd := par[npar + 1].value;
 
npar:=ParCount(ModelDef.numstate + 186);
FlagSimPpt := par[npar + 1].value;
DoySPpts := par[npar + 2].value;
DoySPpte := par[npar + 3].value;
phiSdry := par[npar + 4].value;
alphaSPpt := par[npar + 5].value;
phiWdry := par[npar + 6].value;
alphaWPpt := par[npar + 7].value;
 
npar:=ParCount(ModelDef.numstate + 187);
FlagPpt := par[npar + 1].value;
VSPd := par[npar + 2].value;
kSPd := par[npar + 3].value;
 
npar:=ParCount(ModelDef.numstate + 188);
VWPd := par[npar + 1].value;
kWPd := par[npar + 2].value;
DaysPpt := par[npar + 3].value;
 
npar:=ParCount(ModelDef.numstate + 190);
Tmwi := par[npar + 1].value;
Tmsp := par[npar + 2].value;
Tmsu := par[npar + 3].value;
Tmfa := par[npar + 4].value;
Tkwi := par[npar + 5].value;
Tksp := par[npar + 6].value;
Tksu := par[npar + 7].value;
Tkfa := par[npar + 8].value;
 
npar:=ParCount(ModelDef.numstate + 193);
Tave0 := par[npar + 1].value;
 
npar:=ParCount(ModelDef.numstate + 194);
FI := par[npar + 1].value;
DOYfire := par[npar + 2].value;
ffLL := par[npar + 3].value;
ffWL := par[npar + 4].value;
ffWDL := par[npar + 5].value;
ffSL := par[npar + 6].value;
fBNv := par[npar + 7].value;
fBPv := par[npar + 8].value;
fWNv := par[npar + 9].value;
fWPv := par[npar + 10].value;
fDNv := par[npar + 11].value;
fDPv := par[npar + 12].value;
 
npar:=ParCount(ModelDef.numstate + 207);
a_sdla := par[npar + 1].value;
 
dBCdt := -999;
dBNdt := -999;
dBPdt := -999;
dVCdt := -999;
dVNdt := -999;
dVPdt := -999;
dvCO2dt := -999;
dvIdt := -999;
dvWdt := -999;
dvNH4dt := -999;
dvNO3dt := -999;
dvdoNdt := -999;
dvNfixdt := -999;
dDCdt := -999;
dDNdt := -999;
dDPdt := -999;
dWCdt := -999;
dWNdt := -999;
dWPdt := -999;
dSCdt := -999;
dSNdt := -999;
dSPdt := -999;
dENH4dt := -999;
dENO3dt := -999;
dEPO4dt := -999;
dPadt := -999;
dPnodt := -999;
dPoccldt := -999;
dWdt := -999;
dWSnowdt := -999;
dSQdt := -999;
dfcdt := -999;
dRCadt := -999;
dRNadt := -999;
dRPadt := -999;
dUCadt := -999;
dUNadt := -999;
dUPadt := -999;
dDDaypdt := -999;
dDDayndt := -999;
dCumGPPdt := -999;
dCumNPPdt := -999;
dCumNEPdt := -999;
dCumUdoCdt := -999;
dCumLitCdt := -999;
dCumLcWCdt := -999;
dCumtotalLitCdt := -999;
dCumtotalLitNdt := -999;
dCumtotalLitPdt := -999;
dCumRCmtotaldt := -999;
dCumNmintotdt := -999;
dCumPmintotdt := -999;
dCumUNdt := -999;
dCumUNH4dt := -999;
dCumUNO3dt := -999;
dCumUdoNdt := -999;
dCumUNfixdt := -999;
dCumLitNdt := -999;
dCumLcWNdt := -999;
dCumUPO4dt := -999;
dCumLitPdt := -999;
dCumLcWPdt := -999;
dCumLcWCadt := -999;
dCumRCmdt := -999;
dCumTiiCdt := -999;
dCumLcWNadt := -999;
dCumUNH4mdt := -999;
dCumUNO3mdt := -999;
dCumRNmdt := -999;
dCumTiiNdt := -999;
dCumNnsfixdt := -999;
dCumLcWPadt := -999;
dCumUPO4mdt := -999;
dCumRPmdt := -999;
dCumTiiPdt := -999;
dCumMiiCdt := -999;
dCumMiiNdt := -999;
dCumMiiPdt := -999;
dCumINH4dt := -999;
dCumLNH4dt := -999;
dCumNitrdt := -999;
dCumINO3dt := -999;
dCumLNO3dt := -999;
dCumDNtrdt := -999;
dCumPawdt := -999;
dCumPnowdt := -999;
dCumIPO4dt := -999;
dCumLPO4dt := -999;
dCumPO4Pdt := -999;
dCumIPadt := -999;
dCumPocclwdt := -999;
dCumPnosdt := -999;
dCumIdoCdt := -999;
dCumIdoNdt := -999;
dCumLdoCdt := -999;
dCumLdoNdt := -999;
dCumUWdt := -999;
dCumROdt := -999;
dCumPptdt := -999;
dCumIntrdt := -999;
dCumRfldt := -999;
dCumSfldt := -999;
dCumSmdt := -999;
dCumRindt := -999;
dCumIRindoCdt := -999;
dCumIRindoNdt := -999;
dCumIRinNH4dt := -999;
dCumIRinNO3dt := -999;
dCumIRinPO4dt := -999;
dCumROvfdt := -999;
dCumROvfdoCdt := -999;
dCumROvfdoNdt := -999;
dCumROvfNH4dt := -999;
dCumROvfNO3dt := -999;
dCumROvfPO4dt := -999;
dLAIpeakdt := -999;
dCumfBCdt := -999;
dCumfBNdt := -999;
dCumfBPdt := -999;
dCumfWCdt := -999;
dCumfWNdt := -999;
dCumfWPdt := -999;
dCumfDCdt := -999;
dCumfDNdt := -999;
dCumfDPdt := -999;
dCumFCvtdt := -999;
dCumFNvtdt := -999;
dCumFPvtdt := -999;
dCumLitCDebrisdt := -999;
dCumLitNDebrisdt := -999;
dCumLitPDebrisdt := -999;
dBCpeakdt := -999;
Ta := -999;
Bt := -999;
Ba := -999;
BL := -999;
BR := -999;
BW := -999;
L := -999;
L_max := -999;
Gfc := -999;
deltaGcT := -999;
Lfc := -999;
deltaLcW := -999;
Delta_E := -999;
psiS := -999;
Paw := -999;
PO4P := -999;
Pnow := -999;
Pocclw := -999;
Pnos := -999;
c_cs := -999;
PsIrr := -999;
PsC := -999;
PsW := -999;
UC := -999;
UW := -999;
PET := -999;
NH4aq := -999;
UNH4 := -999;
NO3aq := -999;
UNO3 := -999;
PaDoC := -999;
UdoC := -999;
UdoN := -999;
UNfix := -999;
UN := -999;
PO4aq := -999;
UPO4 := -999;
aqN := -999;
qN := -999;
aqP := -999;
qP := -999;
LitC := -999;
LitN := -999;
LitP := -999;
LitCDebris := -999;
LitNDebris := -999;
LitPDebris := -999;
LcWC := -999;
LcWN := -999;
LcWP := -999;
LcWCa := -999;
LcWNa := -999;
LcWPa := -999;
RCPm := -999;
RCPt := -999;
NUE := -999;
PUE := -999;
WUE := -999;
Vstar := -999;
RCg := -999;
RNg := -999;
RPg := -999;
yNH4 := -999;
yNO3 := -999;
ydoN := -999;
yNfix := -999;
yCO2 := -999;
yI := -999;
yW := -999;
yCa := -999;
yNa := -999;
RCt := -999;
RNt := -999;
RPt := -999;
phi := -999;
Ci := -999;
VTot := -999;
VR := -999;
VL := -999;
SoCt := -999;
SoNt := -999;
SoPt := -999;
NPP := -999;
Ro := -999;
LNH4 := -999;
LNO3 := -999;
LdoC := -999;
LdoN := -999;
LNtot := -999;
LPO4 := -999;
thetaN := -999;
thetaP := -999;
UNH4m := -999;
UNO3m := -999;
UNmtot := -999;
UPO4m := -999;
MC := -999;
MN := -999;
MP := -999;
LambdaC := -999;
LambdaN := -999;
LambdaP := -999;
RCm := -999;
RNm := -999;
RPm := -999;
TiiC := -999;
TiiN := -999;
TiiP := -999;
MiiC := -999;
MiiN := -999;
MiiP := -999;
Nnsfix := -999;
Nitr := -999;
dVtot := -999;
Ndept := -999;
netNmin := -999;
netPmin := -999;
NEP := -999;
NeNb := -999;
NePB := -999;
NeCB := -999;
NEWB := -999;
CumNeCB := -999;
CumNeNB := -999;
CumNePB := -999;
CumNeWB := -999;
Btstar := -999;
LL := -999;
Sm := -999;
Doy := -999;
Dl := -999;
delta := -999;
Intr := -999;
OmegaC := -999;
OmegaN := -999;
OmegaP := -999;
dUCdvCO2 := -999;
dUCdvW := -999;
dUCdvI := -999;
dUNdvNH4 := -999;
dUNdvNO3 := -999;
dUNdvdoN := -999;
dUNdvNfix := -999;
dUCdVC := -999;
chiC := -999;
chiN := -999;
chiP := -999;
zetaCO2 := -999;
zetaW := -999;
zetaI := -999;
zetaNH4 := -999;
zetaNO3 := -999;
zetaNfix := -999;
zetadoN := -999;
z := -999;
betanRd := -999;
Rd := -999;
Rl := -999;
DNtr := -999;
Dop := -999;
VpC := -999;
fuf := -999;
Ts := -999;
ks := -999;
kst := -999;
ksf := -999;
aQl := -999;
aQh := -999;
Sfl := -999;
Rfl := -999;
theta := -999;
OvfR := -999;
ROvfdoC := -999;
ROvfdoN := -999;
ROvfNH4 := -999;
ROvfNO3 := -999;
ROvfPO4 := -999;
OvfI := -999;
Dot := -999;
calib := -999;
deltaDW := -999;
Yf := -999;
SCdev := -999;
STdev := -999;
WTdev := -999;
Pptsim := -999;
SPdev := -999;
WPdev := -999;
Proj_Ca := -999;
Proj_Tmax := -999;
Proj_Tmin := -999;
Proj_Ppt := -999;
Tave := -999;
FBC := -999;
FBN := -999;
FBP := -999;
FWC := -999;
FWN := -999;
FWP := -999;
FDC := -999;
FDN := -999;
FDP := -999;
FNO3 := -999;
FPO4 := -999;
FNvol := -999;
FPvol := -999;
LW := -999;
Proj_Rin := -999;
 
{ Enter the equations to calculate the processes here, using the local variable
  names defined above. }

// Initialize discrete variables
FBC:=0;
FBN:=0;
FBP:=0;
FWC:=0;
FWN:=0;
FWP:=0;
FDC:=0;
FDN:=0;
FDP:=0;
FNvol:=0;
FPvol:=0;
FNO3:=0;
FPO4:=0;

// drivers to specify ENH4, ENO3, EPO4, and W
if INH4<0 then ENH4:=-INH4;
if INO3<0 then ENH4:=-INO3;
if IPO4<0 then ENH4:=-IPO4;
if Ppt<0 then W:=-Ppt;

// Correct impossible negative values caused by round off error
if BC < 1e-6 then BC:=1e-6;
if BN < 1e-6 then BN:=1e-6;
if BP < 1e-6 then BP:=1e-6;
if DC < 1e-6 then DC:=1e-6;
if DN < 1e-6 then DN:=1e-6;
if DP < 1e-6 then DP:=1e-6;
if WC < 1e-6 then WC:=1e-6;
if WN < 1e-6 then WN:=1e-6;
if WP < 1e-6 then WP:=1e-6;
if SC < 1e-6 then SC:=1e-6;
if SN < 1e-6 then SN:=1e-6;
if SP < 1e-6 then SP:=1e-6;
if ENH4 < 0 then ENH4:=0;
if ENO3 < 0 then ENO3:=0;
if EPO4 < 0 then EPO4:=0;
if Pa < 0 then Pa:=0;
if Pno < 0 then Pno:=0;
if Poccl < 0 then Poccl:=0;
if W < 1e-6 then W:=1e-6;
if WSnow < 0 then WSnow:=0;
if fc<fcmin then fc:=fcmin;
if fc>1 then fc:=1;

{correct round off for V}
Vstar:=0;
if VC>0 then Vstar:=Vstar+VC else VC:=0;
if VN>0 then Vstar:=Vstar+VN else VN:=0;
if VP>0 then Vstar:=Vstar+VP else VP:=0;

VC:=VC/Vstar;            tstat[4].value:=VC;
VN:=VN/Vstar;            tstat[5].value:=VN;
VP:=VP/Vstar;            tstat[6].value:=VP;
Vtot:=VC+VN+VP;

if vCO2<=epsilon then vCO2:=epsilon;
if vI<=epsilon then vI:=epsilon;
if vW<=epsilon then vW:=epsilon;
Vstar:= vCO2+vI+vW;

{Vstar:=0;
if vCO2>0 then Vstar:=Vstar+vCO2 else vCO2:=0;
if vI>0 then   Vstar:=Vstar+vI   else vI:=0;
if vW>0 then   Vstar:=Vstar+vW   else vW:=0;   }

vCO2:=vCO2/Vstar;            tstat[7].value:=vCO2;
vI:=vI/Vstar;                tstat[8].value:=vI;
vW:=vW/Vstar;                tstat[9].value:=vW;

Vstar:=0;
if vNH4>0 then  Vstar:=Vstar+vNH4  else vNH4:=0;
if vNO3>0 then  Vstar:=Vstar+vNO3  else vNO3:=0;
if vdoN>0 then  Vstar:=Vstar+vdoN  else vdoN:=0; 
if vNfix>0 then Vstar:=Vstar+vNfix else vNfix:=0;

vNH4:=vNH4/Vstar;            tstat[10].value:=vNH4;
vNO3:=vNO3/Vstar;            tstat[11].value:=vNO3;
vdoN:=vdoN/Vstar;            tstat[12].value:=vdoN; 
vNfix:=vNfix/Vstar;          tstat[13].value:=vNfix;

{This isn't used. Ppt is actually set in the calculatediscrete section. It's here so that the Proj_Ppt<>-999 on day 0}
Proj_Ppt:=Ppt;
Proj_Rin:=0;

{Set rooting depth}
z:=z0;

{Julian Day}
Doy:= tan(pi*((time+Tly)/DoyD -0.50001));
Doy:= arctan(Doy)+pi/2;
Doy:=trunc((DoyD/pi)*Doy+0.9);

{Day length}
delta:=0.1303*pi*cos(2*pi*(Doy-172)/365);
Dl:=-tan(lat*pi/180)*tan(delta);
if Dl>1 then Dl:=1;
if Dl<-1 then Dl:=-1;
Dl:= 24*arccos(Dl)/pi;

Ndept:=INH4+INO3+IdoN;
VpC:=Dop0/SoC0;
Dop:=VpC*(DC+SC-SoC0) + Dop0;

{Discrete processes}
Sm:=0;
Intr:=0;

{Projected climate}
Tave:=Tave0;
Yf := int(time/365);
if Yf>=1 then
  begin
   if Doy<>365 then
    begin
     SCdev:=mCdev*(Yf-1+Doy/365);
     STdev:=VSTd*(Yf-1+Doy/365);
    end
   else
    begin
     SCdev:=mCdev*(Yf-1);
     STdev:=VSTd*(Yf-1); 
    end;
   WTdev:=0;
   if abs(FlagPpt)=1 then
      begin
       if FlagPpt<0 then
         SPdev:=(Yf+1)*OTS1/100+(100-(Yf+1))*Ppt/100-Ppt
	   else 
         SPdev:=(Yf+1)*OTS2/100+(100-(Yf+1))*Ppt/100-Ppt;
       WPdev:=0;
      end
   else
      begin
       SPdev:=0;
       WPdev:=0;
      end;
  end
else   // Year=2004 baseline
  begin
    SCdev:=0;
    STdev:=0;
    WTdev:=0;
    SPdev:=0;
    WPdev:=0;
  end;

// Calculate new climate. Ppt done in calculatediscrete section.
if FlagCa>0 then 
   Ca:=Ca+SCdev;
if FlagT>0 then
 begin
   if FlagT35>0 then
     STdev:=STdev
   else
//     STdev:=Tdev(Doy,Tywi*(time-1)/365/(50+(time-1)/365), Tysp*(time-1)/365/(50+(time-1)/365), Tysu*(time-1)/365/(50+(time-1)/365), Tyfa*(time-1)/365/(50+(time-1)/365));
     STdev:=Tdev(Doy,Tmwi*time/(Tkwi+time), Tmsp*time/(Tksp+time), Tmsu*time/(Tksu+time), Tmfa*time/(Tkfa+time));
   Tmin:=Tmin+STdev;
   Tmax:=Tmax+STdev;
   Tave:=Tave0+STdev;
 end;

{Projected Rin}
if abs(FlagPpt)>0 then
    Rin:=Rin+0.002*Yf*Rin*FlagPpt;

      
// Save changes to climate so you can see them in the output file
Proj_Tmin:=Tmin;
Proj_Tmax:=Tmax;
Proj_Ca:=Ca;
Proj_Rin:=Rin;

{Depth of thaw}
if Tsb<100 then
     Dot:=max(0,(xi1*thetab+xi2)*(xi3*Tsb+xi4)*Ddayp+xi5*Ddayn)/100
else DOT:=zW;
{Rooting depth and Phase II soil - see discrete section}

{Average daily temperature}
Ta:=(Tmax+Tmin)/2;

{Soil Water fraction}
theta:= W/1000/z;
if theta>phis then theta:=phis;
if theta<theta_w then theta:=theta_w;

{unfrozen soil fraction}
fuf:=min(zW,Dot)/zW; 

{soil heat conductance calculations}
C:=min(Dop/z,1);

kst:=kw*theta + kso*(1-phis)*C + ksm*(1-phis)*(1-C);
ksf:=1.06*kst+0.0121;
aQh:= cw*theta + cso*(1-phis)*C + csm*(1-phis)*(1-C);
aQl:=0.88*aQh-0.139;

{soil temperature}
if SQ<Ql then Ts:=(SQ-Ql)/aQl
else 
  if SQ>Qh then Ts:=(SQ-Qh)/aQh
  else Ts:=0;

if Ts>0 then ks:=kst
else ks:=ksf;

{Run in excess}
if Rin>(1000*z*phis-W) then 
   OvfR:=Rin-1000*z*phis+W
else
   OvfR:=0;
if Rin>0 then
 begin
  ROvfdoC:=IRindoC*OvfR/Rin;
  ROvfdoN:=IRindoN*OvfR/Rin;
  ROvfNH4:=IRinNH4*OvfR/Rin;
  ROvfNO3:=IRinNO3*OvfR/Rin;
  ROvfPO4:=IRinPO4*OvfR/Rin;
 end
else
 begin
  ROvfdoC:=0;
  ROvfdoN:=0;
  ROvfNH4:=0;
  ROvfNO3:=0;
  ROvfPO4:=0;
 end;            
   
{soil water tension}
PsiS:=water_tension(max(1e-6,W));
{Peak vapor pressure deficit}
Delta_e:=6.1078e-4*(exp(17.269*Tmax/(237.3+Tmax))-exp(17.269*Tmin/(237.3+Tmin)));

{aqueous NH4,  14e-6 converts umoles to g N}
NH4aq:= ENH4 - 14E-6*W*etaNH4 - z*rho_s*SNH4; 
NH4aq:= NH4aq + sqrt(sqr(NH4aq)+ 56e-6*W*ENH4*etaNH4);
NH4aq:=NH4aq/(28e-6*W);
{aqueous NO3}
NO3aq:= ENO3 - 14E-6*W*etaNO3 - z*rho_s*SNO3; 
NO3aq:= NO3aq + sqrt(sqr(NO3aq)+ 56e-6*W*ENO3*etaNO3);
NO3aq:=NO3aq/(28e-6*W);
{doC}
PadoC:= bdoC*qDOM*(DN*qPadoC/DC)/12e-6/W;

{aqueous PO4, 31e-6 converts umoles to g P}
PO4aq:= EPO4 - 31E-6*W*etaPO4 - z*rho_s*SPO4; 
PO4aq:= PO4aq + sqrt(sqr(PO4aq)+ 124e-6*W*EPO4*etaPO4);
PO4aq:=PO4aq/(62e-6*W); 

{Canopy fraction; included in allometric table}
deltaGcT:=min(power((theta-theta_w)/(theta_fp-theta_w),betaGfc),1);
deltaLcW:=max(min((gammaw*theta_w-theta)/(gammaw-1),1),0);
if (DDayp>Ddbud) and (theta>gammaw*theta_w) and (Doy<Dfs) then
  Gfc:=alphaGfc*Ta*deltaGcT*power(1-fc,epsilonfc)
else
  Gfc:=0;
if Doy=1 then Gfc:=0;
if (Doy<=Dfs) then
  Lfc:=(chicW*deltaLcW)*power(fc-fcmin,epsilonfc)
else
  Lfc:=chicT*(1-0.99*fc)*(fc-fcmin)+(chicW*deltaLcW)*power(fc-fcmin,epsilonfc);

Assess_Allometry(VC,vW);
UC:= photosynthesis(VC*vCO2,VC*vI,VC*vW,PsC,PsIrr,PsW);

{optimum element concentrations; included in allometric table}
qN:=(VL*Ba*(qNL-qNLl)+qNLl*BL+qNW*BW+qNR*BR)/Bt;{corrected for not full canopy}
qP:=(VL*Ba*(qPL-qPLl)+qPLl*BL+qPW*BW+qPR*BR)/Bt;{corrected for not full canopy}

{actual element concentrations; included in allometric table}
aqN:=BN/Bt;
aqP:=BP/Bt;

{WUE}
if UW>0 then WUE:=UC/UW else WUE:=0;

UNH4:=uptake(NH4aq,VN*vNH4,kNH4,DNH4,gNH4,Q10NH4,14e-3);
UNO3:=uptake(NO3aq,VN*vNO3,kNO3,DNO3,gNO3,Q10NO3,14e-3);
UdoC:=uptake(PadoC,VN*vdoN,kdoC,Ddom,gdoC,Q10doC,12e-3);
UdoN:=UdoC/qdom;
UNfix:=Nfix(VN*vNfix,gNfix,Q10Nfix);
UN:=UNH4+UNO3+UdoN+UNfix;
UPO4:=uptake(PO4aq,VP,kPO4,DPO4,gPO4,Q10PO4,31e-3);


{Assess dUCdvCO2}
dUCdvCO2:= (photosynthesis(VC*(vCO2+0.001),VC*vI,VC*vW,PsC,PsIrr,PsW)-UC)/(0.001);

{Assess dUCdvW}
dUCdvW:= (photosynthesis(VC*vCO2,VC*vI,VC*(vW+0.001),PsC,PsIrr,PsW)-UC)/(0.001);

{Assess dUCdvI}
dUCdvI:= (photosynthesis(VC*vCO2,VC*(vI+0.001),VC*vW,PsC,PsIrr,PsW)-UC)/(0.001);

{Assess dUNdvNH4}
dUNdvNH4:=(uptake(NH4aq,VN*(vNH4+0.001),kNH4,DNH4,gNH4,Q10NH4,14e-3)-UNH4)/(0.001);

{Assess dUNdvNO3}
dUNdvNO3:=(uptake(NO3aq,VN*(vNO3+0.001),kNO3,DNO3,gNO3,Q10NO3,14e-3)-UNO3)/(0.001);

{Assess dUNdvdoN}
dUNdvdoN:=uptake(PadoC,VN*(vdoN+0.001),kdoC,Ddom,gdoC,Q10doC,12e-3);
dUNdvdoN:=(dUNdvdoN/qdom-UdoN)/(0.001);

{Assess dUNdvNfix}
dUNdvNfix:=(Nfix(VN*(vNfix+0.001),gNfix,Q10Nfix)-UNfix)/(0.001); 

{Assess dUCdVC}
dUCdVC:= (photosynthesis((VC+0.001)*vCO2,(VC+0.001)*vI,(VC+0.001)*vW,PsC,PsIrr,PsW)-UC)/0.001;

UC:= photosynthesis(VC*vCO2,VC*vI,VC*vW,PsC,PsIrr,PsW);// need recall to calculate PET, UW, c_cs, Ci correctly 

Paw:= rPaw*Pa;                            {apatite weathering}

PO4P:=rPO4P*PO4aq;                        {PO4 precipitation to nonoccluded}
Pnow:=rPnow*Pno;                          {release of non occl to PO4}
Pocclw:=rPocclw*Poccl;                    {weathering on occluded to non occl}
Pnos:=rPnos*Pno;                          {stabilization of non occ to occl}

{litter}
LL:=Lfc*VL*Ba+maL*VL*Ba*fcmin;

if BW>1 then LcWC:=mcw *power(BW,mcwex)
else LcWC:=mcw *BW;
LcWN:=LcWC*qNWwl;
LcWP:=LcWC*qPWwl;
LcWC:=BC*LcWC/Bt;

LitC:=BC*(mW*BW + maR*BR + LL)/Bt;
LitN:=BN*(mW*BW*qNWl + maR*BR*qNRl + LL*qNLl)/(qN*Bt); 
LitP:=BP*(mW*BW*qPWl + maR*BR*qPRl + LL*qPLl)/(qP*Bt);

if fDebris>0 then
 begin
   LitCDebris:=fDebris*LL;
   LitNDebris:=LitCDebris/qNLDebris;
   LitPDebris:=LitCDebris/qPLDebris;
 end
else
 begin
   LitCDebris:=0;
   LitNDebris:=0;
   LitPDebris:=0;
 end;

LitC:=LitC-LitCDebris;
LitN:=LitN-LitNDebris;
LitP:=LitP-LitPDebris;

if LitC<0 then LitC:=0;
if LitN<0 then LitN:=0;
if LitP<0 then LitP:=0;


{NUE & PUE}
NUE:=(LitC+LitCDebris+LcWC)/(LitN+LitNDebris+LcWN);
PUE:=(LitC+LitCDebris+LcWC)/(LitP+LitPDebris+LcWP);

{Resource requirement}
{for maintenance}
RCPm:=power(Q10R,Ta/10)*(rma*qNL*BL+rmw*qNW*BW*exp(-BW*krmw));
RCPm:=RCPm + power(Q10R,Ts/10)*rma*qNR*BR;
RCPm:=RCPm*aqN/qN;

NPP:=UC-RCPm-UNH4*NH4Ccost-UNO3*NO3Ccost-UdoN*doNCcost-UNfix*NfixCcost;

{Respiration}
RCPt:=RCPm+UNH4*NH4Ccost+UNO3*NO3Ccost+UdoN*doNCcost+UNfix*NfixCcost;
if NPP>0 then 
  begin
    NPP:=NPP/(1+rg);
    RCPt:=RCPt+rg*NPP;
  end;

{Resource requirement, replacement growth}
RCg:= (LitC+LitCDebris+LcWC)*(1+rg)*power(sqrt((BN/(Bt*qN))*(BP/(Bt*qP))),kq);
RNg:= (LitN+LitNDebris+LcWN)*power(qN*Bt/BN,kq);
RPg:= (LitP+LitPDebris+LcWP)*power(qP*Bt/BP,kq);

{total requirement}
{nitrogen}
RNt:= RNg;
{Phosphorus}
RPt:= RPg;
{carbon}
RCt:=RCPm + RCg + UNH4*NH4Ccost+UNO3*NO3Ccost+UdoN*doNCcost+UNfix*NfixCcost;

{C yield} 
if UC=0 then
  begin
    yCO2:=0;
    yI:=0;
    yW:=0;
  end
else
begin
if UC=PsC then
  begin
      yCO2:=dUCdvCO2/VC; 
      yI:=yCO2*max((1-(PsIrr-PsC)/PsC),0); 
      yW:=yCO2*max((1-(PsW-PsC)/PsC),0);
  end; 
if UC=PsIrr then
  begin
      yI:=dUCdvI/VC;
      yCO2:=yI*max((1-(PsC-PsIrr)/PsIrr),0); 
      yW:=yI*max((1-(PsW-PsIrr)/PsIrr),0);
  end; 
if UC=PsW then
  begin
      yW:=dUCdvW/VC; 
      yI:=yW*max((1-(PsIrr-PsW)/PsW),0); 
      yCO2:=yW*max((1-(PsC-PsW)/PsW),0);
  end;
end;

{N yield}
//if dUCdVC>0 then
  begin
    yNH4:=dUNdvNH4/(VN+dUNdvNH4*NH4Ccost/(dUCdVC+1e-6)); {yield}
    yNO3:=dUNdvNO3/(VN+dUNdvNO3*NO3Ccost/(dUCdVC+1e-6)); {yield}
    ydoN:=dUNdvdoN/(VN+dUNdvdoN*doNCcost/(dUCdVC+1e-6));{yield}
    yNfix:=dUNdvNfix/(VN+dUNdvNfix*NfixCcost/(dUCdVC+1e-6)); {yield}
  end;
{else
  begin
    yNH4:=1; 
    yNO3:=1; 
    ydoN:=1; 
    yNfix:=1;
  end;}

yCa:=yCO2*vCO2+yI*vI+yW*vW;
yNa:=yNH4*vNH4+yNO3*vNO3+yNfix*vNfix+ydoN*vdoN;

if (vCO2<chi0) and (yCO2=max(yCO2,max(yW,yI))) then
  begin
    lambdavCO2:=chi0;
    muvCO2:=0;
  end
else
  begin
    lambdavCO2:=0;
    muvCO2:=1;
  end;

if (vW<chi0) and (yW=max(yCO2,max(yW,yI))) then
  begin
    lambdavW:=chi0;
    muvW:=0;
  end
else
  begin
    lambdavW:=0;
    muvW:=1;
  end;

if (vI<chi0) and (yI=max(yCO2,max(yW,yI))) then
  begin
    lambdavI:=chi0;
    muvI:=0;
  end
else
  begin
    lambdavI:=0;
    muvI:=1;
  end;

if (vNH4<chi0) and (yNH4=max(yNH4,max(yNO3,max(yNfix,ydoN)))) then
  begin
    lambdavNH4:=chi0;
    muvNH4:=0;
  end
else
  begin
    lambdavNH4:=0;
    muvNH4:=1;
  end;

if (vNO3<chi0) and (yNO3=max(yNH4,max(yNO3,max(yNfix,ydoN)))) then
  begin
    lambdavNO3:=chi0;
    muvNO3:=0;
  end
else
  begin
    lambdavNO3:=0;
    muvNO3:=1;
  end;

if (vNfix<chi0) and (yNfix=max(yNH4,max(yNO3,max(yNfix,ydoN)))) then
  begin
    lambdavNfix:=chi0;
    muvNfix:=0;
  end
else
  begin
    lambdavNfix:=0;
    muvNfix:=1;
  end;

if (vdoN<chi0) and (ydoN=max(yNH4,max(yNO3,max(yNfix,ydoN)))) then
  begin
    lambdavdoN:=chi0;
    muvdoN:=0;
  end
else
  begin
    lambdavdoN:=0;
    muvdoN:=1;
  end;

zetaCO2:= lambdavCO2-(lambdavCO2+lambdavI+lambdavW)*(muvCO2*vCO2)/(muvCO2*vCO2+muvI*vI+muvW*vW);
zetaW:= lambdavW-(lambdavCO2+lambdavI+lambdavW)*(muvW*vW)/(muvCO2*vCO2+muvI*vI+muvW*vW);
zetaI:= lambdavI-(lambdavCO2+lambdavI+lambdavW)*(muvI*vI)/(muvCO2*vCO2+muvI*vI+muvW*vW);

zetaNH4:= lambdavNH4-(lambdavNH4+lambdavNO3+lambdavNfix+lambdavdoN)*(muvNH4*vNH4)/
                 (muvNH4*vNH4+muvNO3*vNO3+muvNfix*vNfix+muvdoN*vdoN);
zetaNO3:= lambdavNO3-(lambdavNH4+lambdavNO3+lambdavNfix+lambdavdoN)*(muvNO3*vNO3)/
                 (muvNH4*vNH4+muvNO3*vNO3+muvNfix*vNfix+muvdoN*vdoN);
zetaNfix:= lambdavNfix-(lambdavNH4+lambdavNO3+lambdavNfix+lambdavdoN)*(muvNfix*vNfix)/
                 (muvNH4*vNH4+muvNO3*vNO3+muvNfix*vNfix+muvdoN*vdoN);
zetadoN:= lambdavdoN-(lambdavNH4+lambdavNO3+lambdavNfix+lambdavdoN)*(muvdoN*vdoN)/
                 (muvNH4*vNH4+muvNO3*vNO3+muvNfix*vNfix+muvdoN*vdoN);

if yCa<>0 then
  begin
	dvCO2dt:=omegavC*(yCO2/yCa-1)+zetaCO2;
	dvWdt:=omegavC*(yW/yCa-1)+zetaW;
	dvIdt:=omegavC*(yI/yCa-1)+zetaI;
  end
else
  begin
	dvCO2dt:=0;
	dvWdt:=0;
	dvIdt:=0;
  end;

if yNa<>0 then
  begin
	dvNH4dt:=omegavN*(yNH4/yNa-1)+zetaNH4;
	dvNO3dt:=omegavN*(yNO3/yNa-1)+zetaNO3;
	dvNfixdt:=omegavN*(yNfix/yNa-1)+zetaNfix;
	dvdoNdt:=omegavN*(ydoN/yNa-1)+zetadoN;
  end
else
  begin
	dvNH4dt:=0;
	dvNO3dt:=0;
	dvNfixdt:=0;
	dvdoNdt:=0;
  end;

OmegaC:=(lambda*RCa +UCa)/(RCa +lambda*UCa);
OmegaN:=(lambda*RNa +UNa)/(RNa +lambda*UNa);
OmegaP:=(lambda*RPa +UPa)/(RPa +lambda*UPa);

if (VC<chi0) and (OmegaC>1) then 
  begin
    alphaVC:=chi0;
    etaVC:=0;
  end
else
  begin
    alphaVC:=0;
    etaVC:=1;
  end;

if (VN<chi0) and (OmegaN>1) then 
  begin
    alphaVN:=chi0;
    etaVN:=0;
  end
else
  begin
    alphaVN:=0;
    etaVN:=1;
  end;

if (VP<chi0) and (OmegaP>1) then 
  begin
    alphaVP:=chi0;
    etaVP:=0;
  end
else
  begin
    alphaVP:=0;
    etaVP:=1;
  end;

chiC:=alphaVC-(alphaVC+alphaVN+alphaVP)*(etaVC*VC)/(etaVC*VC+etaVN*VN*etaVP*VP);
chiN:=alphaVN-(alphaVC+alphaVN+alphaVP)*(etaVN*VN)/(etaVC*VC+etaVN*VN*etaVP*VP);
chiP:=alphaVP-(alphaVC+alphaVN+alphaVP)*(etaVP*VP)/(etaVC*VC+etaVN*VN*etaVP*VP);


phi:=1;
phi:=phi*power(1/OmegaC,VC);
phi:=phi*power(1/OmegaN,VN);
phi:=phi*power(1/OmegaP,VP);

dVCdt  :=acc*ln(phi*OmegaC)*VC+chiC;
dVNdt  :=acc*ln(phi*OmegaN)*VN+chiN;   
dVPdt  :=acc*ln(phi*OmegaP)*VP+chiP;

dVtot:=dVCdt+dVNdt+dVPdt;

{leaching}
Ro:=fuf*DRAIN*(W - theta_fro*1000*z); {mm H2O = kg H2O/m2 = L/m2}
if Ro<0 then Ro:=0;
LNH4:=Ro*NH4aq*14e-6;
LNO3:=Ro*NO3aq*14e-6;
LdoN:=Ro*aLdoN*DN*DC; 
LdoC:=LdoN*qLdom;
//LdoC:=Ro*aLdoC*DC;
//LdoN:=LdoC/qLdom;
LPO4:=Ro*PO4aq*31e-6;
LNtot:=LNH4+LNO3+LdoN;

{microbial processes}
SoCt:=DC+SC+WC;
SoNt:=DN+SN+WN;
SoPt:=DP+SP+WP;

if z>0 then
   R:=W/(z*1000*phis)
else
   R:=0; 
S:=Wopt-Wmin;
deltadW:=1-sqr((Wopt-R)/S)/(1+Jmoist*((R-Wmin)/S));
deltadW:=deltadW*power(Q10m,Ts/10);
if deltadW<1e-9 then deltadW:=1e-9;
LcWCa:=omega*deltadW*WC;
LcWNa:=omega*deltadW*WN;
LcWPa:=omega*deltadW*WP;
TiiC:=deltadW*aTii*DC*DN*DP;
TiiN:=TiiC/qNSii;
TiiP:=TiiC/qPSii;
MiiC:=deltadW*aMii*SC;
MiiN:=deltadW*aMii*SN;
MiiP:=deltadW*aMii*SP;
if BW>0 then Nnsfix:=deltadW*betaNfix*power(BW,NLe)*(1-MBW/sqrt(sqr(MBW)+sqr(BW)))
else Nnsfix:=0;
if DC/DN>qSfix then Nnsfix:=Nnsfix+deltadW*gammaNfix*(DC/DN-qSfix)*DC;
thetaN:=phiN/xiC;
thetaP:=phiP/xiC;
UNH4m:=deltadW*alphaNH4*sqr(DC)*NH4aq/(thetaN*DN*(kNH4m+NH4aq));
UNO3m:=deltadW*alphaNO3*sqr(DC)*NO3aq/(thetaN*DN*(kNO3m+NO3aq));
UPO4m:=deltadW*alphaPO4*sqr(DC)*PO4aq/(thetaP*DP*(kPO4m+PO4aq));
UNmtot:=UNH4m+UNO3m;
MC:=deltadW*psiC*DC;
MN:=deltadW*psiN*DN+UNH4m+UNO3m+Nnsfix;
MP:=deltadW*psiP*DP+UPO4m;
divsr:=thetaN*MN*thetaP*MP+MC*thetaP*MP+MC*thetaN*MN;
LambdaC:=xiC*thetaN*MN*thetaP*MP/divsr;
LambdaN:=MC*thetaP*MP/divsr;
LambdaP:=MC*thetaN*MN/divsr;
RCm:=MC*(1-LambdaC);
RNm:=MN*(1-LambdaN);
RPm:=MP*(1-LambdaP);
Nitr:= deltadW*rrNitr*NH4aq/(kNitr+NH4aq);
DNtr:=max(aDNtr*(theta-thetaD)*Power(Q10m,Ts/10)*NO3aq/(kDNtr+NO3aq),0);
netNmin:=RNm-UNH4m-UNO3m+MiiN;
netPmin:=RPm-UPO4m+MiiP;

NEP:=NPP-RCm-MiiC;
NeCB:=NPP-RCm+IdoC+IRindoC-LdoC-MiiC-ROvfdoC;
NeNb:=INH4+INO3+IdoN+IRindoN+IRinNH4+IRinNO3+UNfix+Nnsfix-LNH4-LNO3-LdoN-DNtr-ROvfdoN-ROvfNH4-ROvfNO3;
NePB:=IPa+IPO4+IRinPO4-LPO4-ROvfPO4;
NeWB:=Ppt+Rin-UW-Intr-Ro-OvfR;

CumNeCB:=CumNPP-CumRCm+CumIdoC+CumIRindoC-CumLdoC-CumMiiC-CumROvfdoC;
CumNeNB:=CumINH4+CumINO3+CumIdoN+CumIRindoN+CumIRinNH4+CumIRinNO3+CumUNfix+CumNnsfix-CumLNH4-CumLNO3-CumLdoC/qLdom-CumDNtr-CumROvfdoN-CumROvfNH4-CumROvfNO3;
CumNePB:=CumIPa+CumIPO4+CumIRinPO4-CumLPO4-CumROvfPO4;
CumNeWB:=CumPpt+CumRin-CumUW-CumIntr-CumRo-CumROvf;


if CalculateDiscrete then
 begin

 {time step size}
      R:=FmOptions.RunOptions.discretestep;

{fire losses}
  if (FI<>0) or (DOYfire<>0) then
  begin
   if YF>=0 then
   begin
   If ((YF+1)/FI - trunc((YF+1)/FI) < 0.0001) and (abs(DOY-DOYfire) < 0.001) then
     begin
       fc:=fc*ffLL; // Decrease canopy fraction by the same amount as leaves
       FBC:=R*(ffLL*BL*qC+ffWL*BW*qC);
       FBN:=R*(ffLL*BL*qNL+ffWL*BW*qNW);
       FBP:=R*(ffLL*BL*qPL+ffWL*BW*qPW);
       FWC:=R*ffWDL*WC;
       FWN:=FWC*WN/WC;
       FWP:=FWC*WP/WC;
       FDC:=R*ffSL*DC;
       FDN:=FDC*DN/DC;
       FDP:=FDC*DP/DC;
	   FNvol:=fBNv*FBN+fWNv*FWN+fDNv*FDN;
	   FPvol:=fBPv*FBP+fWPv*FWP+fDPv*FDP;
	   FNO3:=FBN+FDN+FWN-FNvol;
	   FPO4:=FBP+FDP+FWP-FPvol;
         
       if not(FmOptions.RunOptions.HoldStatesConstant) then
         begin
       if stat[FmCalculate.GetArrayIndex(vtstate,'BC')].holdconstant=false then
       BC:=BC-FBC;
       if stat[FmCalculate.GetArrayIndex(vtstate,'BN')].holdconstant=false then
       BN:=BN-FBN;
       if stat[FmCalculate.GetArrayIndex(vtstate,'BP')].holdconstant=false then
       BP:=BP-FBP;
       if stat[FmCalculate.GetArrayIndex(vtstate,'WC')].holdconstant=false then
       WC:=WC-FWC;
       if stat[FmCalculate.GetArrayIndex(vtstate,'WN')].holdconstant=false then
       WN:=WN-FWN;
       if stat[FmCalculate.GetArrayIndex(vtstate,'WP')].holdconstant=false then
       WP:=WP-FWP;
       if stat[FmCalculate.GetArrayIndex(vtstate,'DC')].holdconstant=false then
       DC:=DC-FDC;
       if stat[FmCalculate.GetArrayIndex(vtstate,'DN')].holdconstant=false then
       DN:=DN-FDN;
       if stat[FmCalculate.GetArrayIndex(vtstate,'DP')].holdconstant=false then
       DP:=DP-FDP;
       if stat[FmCalculate.GetArrayIndex(vtstate,'ENO3')].holdconstant=false then
       ENO3:=ENO3+FNO3;
       if stat[FmCalculate.GetArrayIndex(vtstate,'EPO4')].holdconstant=false then
       EPO4:=EPO4+FPO4;

       if stat[FmCalculate.GetArrayIndex(vtstate,'CumfBC')].holdconstant=false then
       CumfBC:=CumfBC+FBC;
       if stat[FmCalculate.GetArrayIndex(vtstate,'CumfBN')].holdconstant=false then
       CumfBN:=CumfBN+FBN;
       if stat[FmCalculate.GetArrayIndex(vtstate,'CumfBP')].holdconstant=false then
       CumfBP:=CumfBP+FBP;
       if stat[FmCalculate.GetArrayIndex(vtstate,'CumfWC')].holdconstant=false then
       CumfWC:=CumfWC+FWC;
       if stat[FmCalculate.GetArrayIndex(vtstate,'CumfWN')].holdconstant=false then
       CumfWN:=CumfWN+FWN;
       if stat[FmCalculate.GetArrayIndex(vtstate,'CumfWP')].holdconstant=false then
       CumfWP:=CumfWP+FWP;
       if stat[FmCalculate.GetArrayIndex(vtstate,'CumfDC')].holdconstant=false then
       CumfDC:=CumfDC+FDC;
       if stat[FmCalculate.GetArrayIndex(vtstate,'CumfDN')].holdconstant=false then
       CumfDN:=CumfDN+FDN;
       if stat[FmCalculate.GetArrayIndex(vtstate,'CumfDP')].holdconstant=false then
       CumfDP:=CumfDP+FDP;
       if stat[FmCalculate.GetArrayIndex(vtstate,'CumFCvt')].holdconstant=false then
       CumFCvt:=CumFCvt+FBC+FWC+FDC;
       if stat[FmCalculate.GetArrayIndex(vtstate,'CumFNvt')].holdconstant=false then
       CumFNvt:=CumFNvt+FNvol;
       if stat[FmCalculate.GetArrayIndex(vtstate,'CumFCvt')].holdconstant=false then
       CumFPvt:=CumFPvt+FPvol;
         end;
	  end;
   end;  
  end;


{Degree days}//Needs to be modified to match Delbart and Picard 2007 model, careful of Dot calculation
      if Doy<=Ddds then DDayp:=0;
      if (Doy>=Ddds) and (Wsnow=0) then DDayp:=DDayp+max(Ta,0)*R
      else DDayp:=DDayp;

      if Doy<=227 then DDayn:=0;
      if (Doy>=227) then DDayn:=DDayn+min(Ta,0)*R
      else DDayn:=DDayn;

     {Peak leaf area and biomass}
      if L > LAIpeak then LAIpeak := L;
      if BC > BCpeak then BCpeak := BC;

     {Increasing active layer}
      z:=z+deltaz*time;
      SC:=SC+deltaz*time*Ccpii;
      SN:=SC/qNSii;
      SP:=SC/qPSii;
      Pa:=Pa+deltaz*time*PcPa;
      Pno:=Pno+deltaz*time*PcPno;
      Poccl:=Poccl+deltaz*time*PcPoccl;

     {Save the new values}
      par[FmCalculate.GetArrayIndex(vtparameter,'z0')].value:=z;
      stat[FmCalculate.GetArrayIndex(vtstate,'SC')].value:=SC;
      stat[FmCalculate.GetArrayIndex(vtstate,'SN')].value:=SN;
      stat[FmCalculate.GetArrayIndex(vtstate,'SP')].value:=SP;
      stat[FmCalculate.GetArrayIndex(vtstate,'Pa')].value:=Pa;
      stat[FmCalculate.GetArrayIndex(vtstate,'Pno')].value:=Pno;
      stat[FmCalculate.GetArrayIndex(vtstate,'Poccl')].value:=Poccl;

     {Simulated Precipitation}
      if FlagSimPpt=1 then
      begin
       if Yf>=1 then
        begin
         if (Doy>=DoySPpts) and (Doy<=DoySPpte) then
           begin
             if random>phiSdry then Ppt:= -ln(1-random)/alphaSPpt
             else Ppt := 0;
           end
         else
           begin
             if random>phiWdry then Ppt:= -ln(1-random)/alphaWPpt
             else Ppt := 0;
           end;
         Pptsim:=Ppt; 
        end;
      end;

     {Projected climate}
     // Calculate new climate. Temp and CO2 done above.
      if abs(FlagPpt)>0 then
        if Ppt>0 then
          begin
            Ppt:=Ppt+SPdev;            
          end;
 
     // Save changes to climate so you can see them in the output file
      Proj_Ppt:=Ppt; 

      {Interception}
      if stat[FmCalculate.GetArrayIndex(vtstate,'CumPpt')].holdconstant=false then
         CumPpt:=CumPpt+Ppt*R;
      if Bw=0 then
        Intr:=Intv*L
      else
        Intr:= Intv*(NLsfc*power(BW,NLe)*(1-MBW/sqrt(sqr(MBW)+sqr(Bw)))+L);

      if Ppt > Intr then
         begin
            Thf:= Ppt*R - Intr;
         end
      else
         begin
            Intr:=Ppt*R;
            Thf:=0;
         end;
      if stat[FmCalculate.GetArrayIndex(vtstate,'CumIntr')].holdconstant=false then
         CumIntr:=CumIntr+Intr;
      
      {Rain or Snow}
      OvfI:=0;
      if Ta>Tcrit then  
        begin
          Rfl:=Ppt;
          Sfl := 0;
          InfR:= max(0,min(Thf,(1000*phis*z-W)));
          OvfI:=max(0,Thf-InfR);
          if stat[FmCalculate.GetArrayIndex(vtstate,'W')].holdconstant=false then
             W:=W + InfR;
          if stat[FmCalculate.GetArrayIndex(vtstate,'CumRO')].holdconstant=false then
             CumRO:=CumRO+OvfI;
        end
      else
        begin
          Sfl:=Ppt;
          Rfl:=0;
          if stat[FmCalculate.GetArrayIndex(vtstate,'WSnow')].holdconstant=false then
             WSnow:=WSnow+Thf;
        end;
      if stat[FmCalculate.GetArrayIndex(vtstate,'CumRfl')].holdconstant=false then
         CumRfl:= CumRfl + Rfl;
      if stat[FmCalculate.GetArrayIndex(vtstate,'CumSfl')].holdconstant=false then
         CumSfl:= CumSfl + Sfl;

     {snow melt}
      if Ta > 2.5 then Tsnow:=0 else Tsnow:=Ta-2.5; {snow sfc temperature}
      {Dunne and Leopold 1978}
      Sm:=SWC*Isw*exp(-kI*L)+4.9e-9*(1-exp(-kI*L))*(SB*power(273.15+Ta,4)                 
               -power(273.15+Tsnow,4));//4.9e-9 Stefan-Boltzmann constant
      if Ta>0 then Sm:=Sm/LHF+CC*Ta else Sm:=0;
      if Sm>WSnow     
         then Sm:=WSnow;
      if Sm<0 then Sm:=0;
      InfS:= max(0,min(Sm,(1000*phis*z-W)));
      OvfI:=OvfI+max(0,Sm-InfS);
      if stat[FmCalculate.GetArrayIndex(vtstate,'W')].holdconstant=false then
         W:=W + InfS;
      if stat[FmCalculate.GetArrayIndex(vtstate,'CumRO')].holdconstant=false then
         CumRO:=CumRO+OvfI;
      if stat[FmCalculate.GetArrayIndex(vtstate,'WSnow')].holdconstant=false then
         WSnow:= Wsnow - Sm;
      if stat[FmCalculate.GetArrayIndex(vtstate,'CumSm')].holdconstant=false then
         CumSm:=CumSm+Sm;

      {soil heat}
      if WSnow>0 then dum:=bI*(1-alpha)*Isw else dum := bI*Isw*exp(-kI*L);
      if stat[FmCalculate.GetArrayIndex(vtstate,'SQ')].holdconstant=false then
         SQ:=SQ + R*((Tds-Ts)*kds/(zTds+Dot) + (Ta-Ts)/zTs/(1/ks + WSnow*0.01/ksnow) + dum)/z;

 {calibrate}
      if (calibrate>-100) and (abs(Doy-365)<0.001) then
        begin
       
         FlagNegLeach:=0;
   
         IF CALIBRATE <0 THEN
          begin
           calibrate :=-calibrate*exp(-5e-6*time);
          end;
 
     // v2.8.5 begin
         if calibrate2nd=1 then
           begin
              dum:=0.1*(BCpeaktar-BCpeak)/BCpeaktar;
              FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'BC')].value:=
                  (1+dum)*FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'BC')].value; 
              FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'BN')].value:=
                  (1+dum)*FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'BN')].value; 
              FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'BP')].value:=
                  (1+dum)*FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'BP')].value;  
           end;		  
     // *********
         // Correct for rounding errors coming from Excel to Pascal 
          qNSii:=SC/SN;      // Not saved to global par array, v2.4.17
          qPSii:=SC/SP;      // Not saved to global par array, v2.4.17
		  
          phiN:=qNSii;       // Not saved to global par array, v2.4.17
          phiP:=qPSii;       // Not saved to global par array, v2.4.17

 // v2.8.5 
          LcwNatar:=CumLcWNa;
          LcWPatar:=CumLcWPa;
          FBNtar:=CumfBN;
          FBPtar:=CumfBP;
// ********
          if CumLcWCa>0 then LcwNatar:=CumLcWNa * LcWCatar / CumLcWCa;
          if CumLcWCa>0 then LcWPatar:=CumLcWPa * LcWCatar / CumLcWCa;
          if CumfBC>0 then FBNtar:=CumfBN * FBCtar / CumfBC;
          if CumfBC>0 then FBPtar:=CumfBP * FBCtar / CumfBC;

          if fDebris>0 then
           begin
            LitCDebristar:=LcwCatar+FWCtar-LcWCtar;
            LitNDebristar:=LcWNatar+FWNtar-LcWNtar;
            LitPDebristar:=LcWPatar+FWPtar-LcWPtar;
           end;

          LitCtar:=NPPtar+UdoNtar*qDOM-LcWCtar-FBCtar-LitCDebristar;
          LitNtar:=UNH4tar+UNO3tar+UdoNtar+UNfixtar-LcWNtar-FBNtar-LitNDebristar;   
          LitPtar:=UPO4tar-LcWPtar-FBPtar-LitPDebristar;

          RCmtar:=LitCtar+LcWCatar+cumIdoC+cumIRinDOC-UdoNtar*qDOM-TiiCtar-LdoCtar-CumROvfdoC-FDCtar;
          RNmtar:=LitNtar+LcWNatar+UNH4mtar+UNO3mtar+cumIdoN+cumIRinDON+Nnsfixtar-UdoNtar-LdoCtar/qLDOM-TiiCtar/qNSii-CumROvfdoN-FDNtar;
          RPmtar:=LitPtar+LcWPatar+UPO4mtar-TiiCtar/qPSii-FDPtar;

          MiiCtar:=TiiCtar;

          PAWtar:=cumIPa; 
          Pocclwtar:=Pnostar;
          Pnowtar:=Pocclwtar+PO4Ptar-Pnostar;

   // v2.8.5 
          dum:=DNtrtar;
          DNtrtar:=CUMIdoN+cumINH4+cumINO3+cumIRindoN+cumIRinNH4+cumIRinNO3+UNfixtar+Nnsfixtar
           -cumROvfdoN-cumROvfNH4-cumROvfNO3-LNH4tar-LNO3tar-LdoCtar/qLdom
           -cumFNvt;
          if DNtrtar<0 then DNtrtar:=dum;   

         if Nitrtar>0 then  
            Nitrtar:=-cumINO3-cumIRinNO3+UNO3tar+UNO3mtar+LNO3tar+DNtrtar+CumROvfNO3-((1-fBNv)*CumfBN+(1-fDNv)*CumfDN+(1-fWNv)*CumfWN)
          else
            LNO3tar:=cumINO3+cumIRinNO3-UNO3tar-UNO3mtar+Nitrtar-DNtrtar-CumROvfNO3+((1-fBNv)*CumfBN+(1-fDNv)*CumfDN+(1-fWNv)*CumfWN);
          LNH4tar:=cumINH4+cumIRinNH4-UNH4tar-UNH4mtar+RNmtar-Nitrtar+MiiCtar/qNSii-CumROvfNH4;
          LPO4tar:=Pawtar+Pnowtar+cumIPO4+cumIRinPO4-UPO4tar-UPO4mtar+RPmtar-PO4Ptar+MiiCtar/qPSii-CumROvfPO4+((1-fBPv)*CumfBP+(1-fDPv)*CumfDP+(1-fWPv)*CumfWP);

         
          if LNH4tar<0 then FlagNegLeach:=1;
          if LNO3tar<0 then FlagNegLeach:=1;
          if LPO4tar<0 then FlagNegLeach:=1;
          if DNtrtar<0 then FlagNegLeach:=1;
//********
          if VN>VNtar then 
            begin
              qNL:=qNL*(1-calibrate); 
              qNW:=qNW*(1-calibrate); 
              qNR:=qNR*(1-calibrate);
            end
          else 
            begin
              qNL:=qNL*(1+calibrate); 
              qNW:=qNW*(1+calibrate); 
              qNR:=qNR*(1+calibrate);
            end; 

          if VP>VPtar then 
            begin
              qPL:=qPL*(1-calibrate); 
              qPW:=qPW*(1-calibrate); 
              qPR:=qPR*(1-calibrate);
            end
          else 
            begin
              qPL:=qPL*(1+calibrate); 
              qPW:=qPW*(1+calibrate); 
              qPR:=qPR*(1+calibrate);
            end;


          if CumGPP>GPPtar then
            begin
              scg:= scg*(1-calibrate);
            end
          else
            begin
              scg:= scg*(1+calibrate);
            end;

          if LAIpeak > LAItar then
           begin
 //            scg:= scg *(1+calibrate)
             gC:=gC*(1+calibrate);
             gi:=gi*(1+calibrate);
           end
          else
           begin
             gC:=gC*(1-calibrate);
             gi:=gi*(1-calibrate);
 //            scg:= scg *(1-calibrate);
           end;    

               		  
          if CumUW>=UWtar then
            begin
              ke:= ke*(1-calibrate);
              scg:=scg*(1+0.5*calibrate);//here
  	        end
          else 
            begin
        	  ke:= ke*(1+calibrate);
              scg:=scg*(1-0.5*calibrate);//here
            end;		
          			
 
   {       vW:=max(0,(VR-VN-VP)/VC);
          vCO2:=(1-vW)*vCO2/(vCO2+vI);
          vI:=1-vCO2-vW; 		  
           
           if CumGPP<GPPtar then
            begin
              gNH4:=gNH4*(1+calibrate);
              gNO3:=gNO3*(1+calibrate);
              gNFix:=gNfix*(1+calibrate);
              gdoC:=gdoC*(1+calibrate);	
              gPO4:=gPO4*(1+calibrate);	  
            end			
          else   
	        begin
              gNH4:=gNH4*(1-calibrate);
              gNO3:=gNO3*(1-calibrate);
              gNFix:=gNfix*(1-calibrate);
              gdoC:=gdoC*(1-calibrate);
              gPO4:=gPO4*(1-calibrate);	  
	        end;}

         If CumGPP-CumNPP>GPPtar-NPPtar then 
            begin
              rma:=rma*(1-calibrate);//here
              rmw:=rmw*(1-calibrate);//here
            end
          else 
            begin
              rma:=rma*(1+calibrate);//here
              rmw:=rmw*(1+calibrate);//here
            end; 

          If CumLitC>LitCtar then 
            begin
              maL:=maL*(1-calibrate);
              maR:=maR*(1-calibrate); 
              mW:=mW*(1-calibrate);
            end
          else 
            begin
              maL:=maL*(1+calibrate);
              maR:=maR*(1+calibrate); 
              mW:=mW*(1+calibrate);
            end;
			
          If (CumLitN>0) and (CumLitN>LitNtar) then 
            begin
              qNWl:= qNWl *(1-calibrate);
              qNRl:= qNRl *(1-calibrate); 
              qNLl:= qNLl *(1-calibrate);
            end
          else 
            begin
              qNWl:= qNWl *(1+calibrate);
              qNRl:= qNRl *(1+calibrate); 
              qNLl:= qNLl *(1+calibrate);
            end;
         
          If (CumLitP>0) and (CumLitP>LitPtar) then 
            begin
              qPWl:= qPWl *(1-calibrate);
              qPRl:= qPRl *(1-calibrate); 
              qPLl:= qPLl *(1-calibrate);
            end
          else 
            begin
              qPWl:= qPWl *(1+calibrate);
              qPRl:= qPRl *(1+calibrate); 
              qPLl:= qPLl *(1+calibrate);
            end; 
			
          if CumLitCDebris>LitCDebristar then
            fDebris:=fDebris*(1-calibrate)
          else
            fDebris:=fDebris*(1+calibrate);
	      if fDebris>1 then fDebris:=1;
	  	

          if CumLcWC>LcWCtar then mcw:=mcw*(1-calibrate)
          else                    mcw:=mcw*(1+calibrate);
          if CumLcWN>LcWNtar then qNWwl:=qNWwl*(1-calibrate)
          else                    qNWwl:=qNWwl*(1+calibrate);
          if CumLcWP>LcWPtar then qPWwl:=qPWwl*(1-calibrate)
          else                    qPWwl:=qPWwl*(1+calibrate);

          if CumLcWCa>LcWCatar then omega:=omega*(1-calibrate)
          else                      omega:=omega*(1+calibrate);


//***************************************************************************************************
 { If calNtar > 0 then
    begin
          UNttar:=UNH4tar+UNO3tar+UDONtar+UNfixtar;
          if vNH4>vNH4tar then
            begin
               UNO3tar :=UNO3tar+ UNH4tar*calibrate* UNO3tar/(UNttar-UNH4tar);
               UDONtar :=UDONtar+ UNH4tar*calibrate* UDONtar/(UNttar-UNH4tar);
               UNfixtar:=UNfixtar+UNH4tar*calibrate*UNfixtar/(UNttar-UNH4tar);
               UNH4tar :=UNH4tar*(1-calibrate);
             end;

          if vNO3>vNO3tar then
            begin
               UNH4tar:= UNH4tar+ UNO3tar*calibrate* UNH4tar/(UNttar-UNO3tar);
               UDONtar:= UDONtar+ UNO3tar*calibrate* UDONtar/(UNttar-UNO3tar);
               UNfixtar:=UNfixtar+UNO3tar*calibrate*UNfixtar/(UNttar-UNO3tar);
               UNO3tar:= UNO3tar*(1-calibrate);
             end;

          if vDON>vDONtar then
            begin
               UNH4tar:= UNH4tar+ UDONtar*calibrate* UNH4tar/(UNttar-UDONtar);
               UNO3tar:= UNO3tar+ UDONtar*calibrate* UNO3tar/(UNttar-UDONtar);
               UNfixtar:=UNfixtar+UDONtar*calibrate*UNfixtar/(UNttar-UDONtar);
               UDONtar:= UDONtar*(1-calibrate);
             end;

          if vNfix>vNfixtar then
            begin
               UNH4tar:=UNH4tar+UNfixtar*calibrate*UNH4tar/(UNttar-UNfixtar);
               UNO3tar:=UNO3tar+UNfixtar*calibrate*UNO3tar/(UNttar-UNfixtar);
               UDONtar:=UDONtar+UNfixtar*calibrate*UDONtar/(UNttar-UNfixtar);
               UNfixtar:=UNfixtar*(1-calibrate);
            end;
     end;
             }
//***************************************************************************************************
          If UNh4tar= 0 then gnh4:=0
          else
           begin
            if CumUNH4>UNH4tar then 
                   gNH4:=gNH4*(1-1.5*calibrate)//here      1.3
            else  
              begin
               if (CumUNO3>=Uno3tar) and (CumUNfix>=UNfixtar) and (CumUdoN>=UdoNtar) then
                 gNH4:=gNH4*(1+1.5*calibrate);//here
              end;
           end;

          
          If UNo3tar= 0 then gno3:=0
          else
           begin
            if CumUNO3>UNO3tar then 
              gNO3:=gNO3*(1-1.5*calibrate)//here        1.2
            else   
              begin
               if (CumUNfix>=UNfixtar) and (CumUdoN>=UdoNtar) then
                gNO3:=gNO3*(1+1.5*calibrate);//here
               end;
           end;
          
          If UdoNtar= 0 then gdoC:=0
          else
           begin
            if CumUdoN>UdoNtar then 
               gdoC:=gdoC*(1-1.5*calibrate)//here       1.1
            else   
              begin
               if (CumUNfix>=UNfixtar) then
                gdoC:=gdoC*(1+1.5*calibrate); //here
              end;
          end;
          
          If UNfixtar= 0 then gnfix:=0
          else
           begin
             if CumUNfix>UNfixtar then 
               gNfix:=gNfix*(1-calibrate)
            else   
              begin
                gNfix:=gNfix*(1+calibrate);
              end;
           end;

          if CumUPO4>UPO4tar then 
               gPO4:=gPO4*(1-calibrate)
          else gPO4:=gPO4*(1+calibrate);

//******************************************************************************************************

          if CumRCm>RCmtar then psiC := psiC*(1-calibrate)
          else                psiC := psiC*(1+calibrate);

          if CumTiiC>TiiCtar then    aTii:=aTii*(1-calibrate)
          else                       aTii:=aTii*(1+calibrate); 

          if CumMiiC>MiiCtar then    aMii:=aMii*(1-calibrate)
           else                       aMii:=aMii*(1+calibrate);
// v2.8.5
{ PIIflag was set to 0, this code never ran, removed it
      if (PIIflag>0.8) then
      begin
          if CumMiiC>MiiCtar then 
           begin
              DC:=FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DC')].value+
                 FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'SC')].value*calibrate;
              DN:=FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DN')].value+
                 FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'SN')].value*calibrate;
              DP:=FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DP')].value+
                 FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'SP')].value*calibrate;
              SC:=FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'SC')].value*(1-calibrate);
              SN:=FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'SN')].value*(1-calibrate);
              SP:=FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'SP')].value*(1-calibrate);
           end
          else
           begin
              deltaPII:=min(DC*SN/(SC*DN),DC*SP/(SC*DP));
              deltaPII:=min(DN*SC/(SN*DC),deltaPII);
              deltaPII:=min(DN*SP/(SN*DP),deltaPII);
              deltaPII:=min(DP*SC/(SP*DC),deltaPII);
              deltaPII:=min(DP*SN/(SP*DN),deltaPII);
              deltaPII:=min(1,deltaPII);
              DC:=FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DC')].value-
                 FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DC')].value*calibrate*deltaPII;
              DN:=FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DN')].value-
                 FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DN')].value*calibrate*deltaPII;
              DP:=FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DP')].value-
                 FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DP')].value*calibrate*deltaPII;
              SC:=FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'SC')].value+
                 FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DC')].value*calibrate*deltaPII;
              SN:=FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'SN')].value+
                 FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DN')].value*calibrate*deltaPII;
              SP:=FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'SP')].value+
                 FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DP')].value*calibrate*deltaPII;
           end;
      end;
}      
// ****************
          SoC0:=DC+SC;
		  
          if CumUNH4m>UNH4mtar then  alphaNH4:=alphaNH4*(1-calibrate)
          else                       alphaNH4:=alphaNH4*(1+calibrate);

          if CumUNO3m>UNO3mtar then  alphaNO3:=alphaNO3*(1-calibrate)
          else                       alphaNO3:=alphaNO3*(1+calibrate);

          if CumRNm>RNmtar then psiN := psiN*(1-calibrate)
          else                  psiN := psiN*(1+calibrate);

          if Nnsfixtar = 0 then gammaNfix:=0;
          if CumNnsfix>Nnsfixtar then gammaNfix:= gammaNfix *(1-calibrate)
          else                        gammaNfix:= gammaNfix *(1+calibrate);

          if CumUPO4m>UPO4mtar then  alphaPO4:=alphaPO4*(1-calibrate)
          else                       alphaPO4:=alphaPO4*(1+calibrate); 

          if CumRPm>RPmtar-(CumLcWPa-LcWPatar) then psiP := psiP*(1-calibrate)
          else                  psiP := psiP*(1+calibrate);

          if CumLNH4>LNH4tar then 
		   begin
			gNH4:=gNH4*(1+0.5*calibrate/(1-0.5*calibrate));
			alphaNH4:=alphaNH4*(1+0.5*calibrate/(1-0.5*calibrate));
			rrNitr:=rrNitr*(1+0.5*calibrate/(1-0.5*calibrate));
		   end
          else
		   begin
			gNH4:=gNH4*(1-0.5*calibrate/(1+0.5*calibrate));
			alphaNH4:=alphaNH4*(1-0.5*calibrate/(1+0.5*calibrate));
			rrNitr:=rrNitr*(1-0.5*calibrate/(1+0.5*calibrate));
		   end;                    
          if CumLNO3>LNO3tar then 
		   begin
			gNO3:=gNO3*(1+0.5*calibrate/(1-0.5*calibrate));
			alphaNO3:=alphaNO3*(1+0.5*calibrate/(1-0.5*calibrate));
			aDNtr:=aDNtr*(1+0.5*calibrate/(1-0.5*calibrate));
		   end
          else 
		   begin
			gNO3:=gNO3*(1-0.5*calibrate/(1+0.5*calibrate));
			alphaNO3:=alphaNO3*(1-0.5*calibrate/(1+0.5*calibrate));
			aDNtr:=aDNtr*(1-0.5*calibrate/(1+0.5*calibrate));
		   end;
	     if CumLPO4>LPO4tar then 
		   begin
			gPO4:=gPO4*(1+0.5*calibrate/(1-0.5*calibrate));
			alphaPO4:=alphaPO4*(1+0.5*calibrate/(1-0.5*calibrate));
		   end
         else  
		   begin
			gPO4:=gPO4*(1-0.5*calibrate/(1+0.5*calibrate));
			alphaPO4:=alphaPO4*(1-0.5*calibrate/(1+0.5*calibrate));
		   end;

	
          if CumLdoN<LdoCtar/qLdom then aLdoN:=aLdoN*(1+calibrate)
          else                          aLdoN:=aLdoN*(1-calibrate);
          
          if Nitrtar=0 then rrNitr:=0;
          if CumNitr>Nitrtar then rrNitr:=rrNitr*(1-calibrate)
          else                    rrNitr:=rrNitr*(1+calibrate);
          
          if  DNtrtar=0 then aDNtr:= 0;
          if CumDNtr>DNtrtar then aDNtr:= aDNtr *(1-calibrate)
          else                    aDNtr:= aDNtr *(1+calibrate); 

          if CumPaw>Pawtar then rPaw:=rPaw*(1-calibrate)
          else                  rPaw:=rPaw*(1+calibrate); 

          if CumPnow>Pnowtar then rPnow:=rPnow*(1-calibrate)
          else                    rPnow:=rPnow*(1+calibrate);       

          if CumPO4P>PO4Ptar then rPO4P:= rPO4P *(1-calibrate)
          else                    rPO4P:= rPO4P *(1+calibrate);

          if CumPocclw>Pocclwtar then rPocclw:=rPocclw*(1-calibrate)
          else                        rPocclw:=rPocclw*(1+calibrate);

          if CumPnos>Pnostar then rPnos:= rPnos *(1-calibrate)
          else                    rPnos:= rPnos *(1+calibrate); 
 
          if CumIntr>Intrtar then Intv:= Intv *(1-calibrate)
          else                  Intv:= Intv *(1+calibrate);

 {         if gI>gItar then
            VCtar:=VCtar*(1-calibrate)
          else
            VCtar:=VCtar*(1+calibrate);
		  VCtar:=min(0.95,max(0.05,VCtar));
          FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'VC')].value:= VCtar;
          VNtar:=(1-VCtar)/2;
          VPtar:=VNtar; 
          FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'VN')].value:= VNtar;
          FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'VP')].value:= VPtar;}

 
 {Fire Parameters}
          if CumfBC>FBCtar then
             begin
               ffWL:=ffWL*(1-calibrate);
               ffLL:=ffLL*(1-calibrate);
             end
	      else
             begin
               ffWL:=ffWL*(1+calibrate);
               ffLL:=ffLL*(1+calibrate);
             end;
          if CumfWC>FWCtar then ffWDL:=ffWDL*(1-calibrate)
		          else           ffWDL:=ffWDL*(1+calibrate);
          if CumfDC>FDCtar then ffSL:=ffSL*(1-calibrate)
		          else           ffSL:=ffSL*(1+calibrate);
			 
       // You can't burn more than there is
          if ffLL>1 then ffLL:=1;
          if ffWL>1 then ffWL:=1;
          if ffWDL>1 then ffWDL:=1;
          if ffSL>1 then ffSL:=1;
				  
          if CumLitNDebris>LitNDebristar then qNLdebris:=qNLdebris*(1+calibrate)
                  else           qNLdebris:=qNLdebris*(1-calibrate);
          if CumLitPDebris>LitPDebristar then qPLdebris:=qPLdebris*(1+calibrate)
                  else           qPLdebris:=qPLdebris*(1-calibrate);				  

		  
          if CumFNvt>FNvttar then
            begin
	            fBNv:=fBNv*(1-calibrate);
	            fWNv:=fWNv*(1-calibrate);
                fDNv:=fDNv*(1-calibrate);
            end
          else
            begin
	            fBNv:=fBNv*(1+calibrate);
                fWNv:=fWNv*(1+calibrate);
                fDNv:=fDNv*(1+calibrate);
            end;

          if CumFPvt>FPvttar then
             begin
 	            fBPv:=fBPv*(1-calibrate);
 	            fWPv:=fWPv*(1-calibrate);
                fDPv:=fDPv*(1-calibrate);
             end
           else
             begin
 	            fBPv:=fBPv*(1+calibrate);
                fWPv:=fWPv*(1+calibrate);
                fDPv:=fDPv*(1+calibrate);
             end;

       // You can't volatilize more than there is
          if fBNv>1 then fBNv:=1;
          if fWNv>1 then fWNv:=1;
          if fDNv>1 then fDNv:=1;
          if fBPv>1 then fBPv:=1;
          if fWPv>1 then fWPv:=1;
          if fDPv>1 then fDPv:=1;

 
// v2.8.5
     //     FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'BC')].value:=BC;


          par[FmCalculate.GetArrayIndex(vtparameter,'FlagNegLeach')].value:=FlagNegLeach;
// ********
          par[FmCalculate.GetArrayIndex(vtparameter,'ffWL')].value:=ffWL;
          par[FmCalculate.GetArrayIndex(vtparameter,'ffLL')].value:=ffLL;
          par[FmCalculate.GetArrayIndex(vtparameter,'ffWDL')].value:=ffWDL;
          par[FmCalculate.GetArrayIndex(vtparameter,'ffSL')].value:=ffSL;
          par[FmCalculate.GetArrayIndex(vtparameter,'fBNv')].value:=fBNv;
          par[FmCalculate.GetArrayIndex(vtparameter,'fBPv')].value:=fBPv;
          par[FmCalculate.GetArrayIndex(vtparameter,'fWNv')].value:=fWNv;
          par[FmCalculate.GetArrayIndex(vtparameter,'fWPv')].value:=fWPv;
          par[FmCalculate.GetArrayIndex(vtparameter,'fDNv')].value:=fDNv;
          par[FmCalculate.GetArrayIndex(vtparameter,'fDPv')].value:=fDPv;
                 
          par[FmCalculate.GetArrayIndex(vtparameter,'kNH4')].value:=kNH4;
          par[FmCalculate.GetArrayIndex(vtparameter,'kNO3')].value:=kNO3;
          par[FmCalculate.GetArrayIndex(vtparameter,'kdoC')].value:=kdoC;
          par[FmCalculate.GetArrayIndex(vtparameter,'NfixCcost')].value:=NfixCcost;
         
          par[FmCalculate.GetArrayIndex(vtparameter,'gC')].value:=gC;
          par[FmCalculate.GetArrayIndex(vtparameter,'gI')].value:=gI; 
          par[FmCalculate.GetArrayIndex(vtparameter,'kE')].value:=kE;
          par[FmCalculate.GetArrayIndex(vtparameter,'scg')].value:=scg;
          par[FmCalculate.GetArrayIndex(vtparameter,'rma')].value:= rma;
          par[FmCalculate.GetArrayIndex(vtparameter,'rmw')].value:= rmw; 
          par[FmCalculate.GetArrayIndex(vtparameter,'maL')].value:= maL;
          par[FmCalculate.GetArrayIndex(vtparameter,'mW')].value:= mW;
          par[FmCalculate.GetArrayIndex(vtparameter,'maR')].value:= maR; 
          par[FmCalculate.GetArrayIndex(vtparameter,'mcw')].value:= mcw; 
          par[FmCalculate.GetArrayIndex(vtparameter,'gNH4')].value:=gNH4;
          par[FmCalculate.GetArrayIndex(vtparameter,'gNO3')].value:=gNO3;
          par[FmCalculate.GetArrayIndex(vtparameter,'gdoC')].value:=gdoC;
          par[FmCalculate.GetArrayIndex(vtparameter,'gNfix')].value:=gNfix; 
          par[FmCalculate.GetArrayIndex(vtparameter,'qNLl')].value:= qNLl;
          par[FmCalculate.GetArrayIndex(vtparameter,'qNWl')].value:= qNWl;
          par[FmCalculate.GetArrayIndex(vtparameter,'qNRl')].value:= qNRl; 
          par[FmCalculate.GetArrayIndex(vtparameter,'qNL')].value:= qNL;
          par[FmCalculate.GetArrayIndex(vtparameter,'qNW')].value:= qNW;
          par[FmCalculate.GetArrayIndex(vtparameter,'qNR')].value:= qNR;
          par[FmCalculate.GetArrayIndex(vtparameter,'gPO4')].value:=gPO4; 
          par[FmCalculate.GetArrayIndex(vtparameter,'qPLl')].value:= qPLl;
          par[FmCalculate.GetArrayIndex(vtparameter,'qPWl')].value:= qPWl;
          par[FmCalculate.GetArrayIndex(vtparameter,'qPR')].value:= qPR; 
          par[FmCalculate.GetArrayIndex(vtparameter,'qPL')].value:= qPL;
          par[FmCalculate.GetArrayIndex(vtparameter,'qPW')].value:= qPW;
          par[FmCalculate.GetArrayIndex(vtparameter,'qPRl')].value:= qPRl;
          par[FmCalculate.GetArrayIndex(vtparameter,'omega')].value:= omega; 
          par[FmCalculate.GetArrayIndex(vtparameter,'psiC')].value:= psiC; 
          par[FmCalculate.GetArrayIndex(vtparameter,'aTii')].value:= aTii; 

 
          par[FmCalculate.GetArrayIndex(vtparameter,'UNH4tar')].value:= UNH4tar;
          par[FmCalculate.GetArrayIndex(vtparameter,'UNO3tar')].value:= UNO3tar;
          par[FmCalculate.GetArrayIndex(vtparameter,'UdoNtar')].value:= UDONtar;
          par[FmCalculate.GetArrayIndex(vtparameter,'UNfixtar')].value:= UNfixtar;

// v2.8.5
{
          if PIIflag>0.8 then
          begin
            FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'SC')].value:=SC;
            FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'SN')].value:=SN;
            FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'SP')].value:=SP;
            FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DC')].value:=DC;
            FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DN')].value:=DN;
            FmShellMain.initialstates[FmCalculate.GetArrayIndex(vtstate,'DP')].value:=DP;
          end;
}
// ***********
          par[FmCalculate.GetArrayIndex(vtparameter,'SoC0')].value:=SoC0;
          par[FmCalculate.GetArrayIndex(vtparameter,'alphaNH4')].value:=alphaNH4;
          par[FmCalculate.GetArrayIndex(vtparameter,'alphaNO3')].value:= alphaNO3; 
          par[FmCalculate.GetArrayIndex(vtparameter,'psiN')].value:= psiN; 
          par[FmCalculate.GetArrayIndex(vtparameter,'gammaNfix')].value:=gammaNfix; 
          par[FmCalculate.GetArrayIndex(vtparameter,'alphaPO4')].value:= alphaPO4; 
          par[FmCalculate.GetArrayIndex(vtparameter,'psiP')].value:= psiP; 
          par[FmCalculate.GetArrayIndex(vtparameter,'aMii')].value:= aMii; 
          par[FmCalculate.GetArrayIndex(vtparameter,'rrNitr')].value:= rrNitr; 
          par[FmCalculate.GetArrayIndex(vtparameter,'aDNtr')].value:=aDntr; 
          par[FmCalculate.GetArrayIndex(vtparameter,'rPaw')].value:= rPaw; 
          par[FmCalculate.GetArrayIndex(vtparameter,'rPnow')].value:= rPnow; 
          par[FmCalculate.GetArrayIndex(vtparameter, 'rPO4P')].value := rPo4p; 
          par[FmCalculate.GetArrayIndex(vtparameter,'rPocclw')].value:= rPocclw; 
          par[FmCalculate.GetArrayIndex(vtparameter, 'rPnos')].value := rPnos; 
          par[FmCalculate.GetArrayIndex(vtparameter,'aLdoN')].value:= aLdoN;
          par[FmCalculate.GetArrayIndex(vtparameter,'drain')].value:= DRAIN; 
          par[FmCalculate.GetArrayIndex(vtparameter,'Intv')].value:= Intv; 
          par[FmCalculate.GetArrayIndex(vtparameter,'qNSii')].value:=qNSii; 
          par[FmCalculate.GetArrayIndex(vtparameter,'qPSii')].value:=qPSii; 
          par[FmCalculate.GetArrayIndex(vtparameter,'phiN')].value:= phiN; 
          par[FmCalculate.GetArrayIndex(vtparameter,'phiP')].value:= phiP;
          par[FmCalculate.GetArrayIndex(vtparameter,'qNWwl')].value:= qNWwl; 
          par[FmCalculate.GetArrayIndex(vtparameter,'qPWwl')].value:= qPWwl;
          par[FmCalculate.GetArrayIndex(vtparameter,'fDebris')].value:= fDebris;
          par[FmCalculate.GetArrayIndex(vtparameter,'qNLDebris')].value:= qNLDebris;
          par[FmCalculate.GetArrayIndex(vtparameter,'qPLDebris')].value:= qPLDebris;		  
          par[FmCalculate.GetArrayIndex(vtparameter,'LcWNatar')].value:= LcWNatar;
          par[FmCalculate.GetArrayIndex(vtparameter,'LcWPatar')].value:= LcWPatar;		  
        end;
   end;
if CalculateDiscrete then
begin
// Add any discrete processes here
end; //discrete processes


{ Now calculate the derivatives of the state variables. If the holdConstant
  portion of the state variable is set to true then set the derivative equal to
  zero. }
if (tstat[1].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dBCdt := 0
else
 dBCdt   :=NPP+UdoC-LitC-LitCDebris-LcWC{-FBC};
 
if (tstat[2].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dBNdt := 0
else
 dBNdt   :=UNH4+UNO3+UdoN+UNfix-LitN-LitNDebris-LcWN{-FBN};
 
if (tstat[3].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dBPdt := 0
else
 dBPdt   :=UPO4-LitP-LitPDebris-LcWP{-FBP};
 
if (tstat[4].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dVCdt := 0
else
 dVCdt   :=dVCdt;
 
if (tstat[5].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dVNdt := 0
else
 dVNdt   :=dVNdt;
 
if (tstat[6].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dVPdt := 0
else
 dVPdt   :=dVPdt;
 
if (tstat[7].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dvCO2dt := 0
else
 dvCO2dt :=dvCO2dt;
 
if (tstat[8].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dvIdt := 0
else
 dvIdt   :=dvIdt;
 
if (tstat[9].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dvWdt := 0
else
 dvWdt   :=dvWdt;
 
if (tstat[10].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dvNH4dt := 0
else
 dvNH4dt :=dvNH4dt;
 
if (tstat[11].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dvNO3dt := 0
else
 dvNO3dt :=dvNO3dt;
 
if (tstat[12].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dvdoNdt := 0
else
 dvdoNdt :=dvdoNdt;
 
if (tstat[13].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dvNfixdt := 0
else
 dvNfixdt:=dvNfixdt;
 
if (tstat[14].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dDCdt := 0
else
 dDCdt:=LitC+LcWCa+IdoC-UdoC-RCm-TiiC-LdoC+IRindoC-ROvfdoC{-FDC};
 
if (tstat[15].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dDNdt := 0
else
 dDNdt:=LitN+LcWNa+UNH4m+UNO3m+IdoN+Nnsfix-RNm-UdoN-LdoN-TiiN+IRindoN-ROvfdoN{-FDN};
 
if (tstat[16].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dDPdt := 0
else
 dDPdt:=LitP+LcWPa+UPO4m-RPm-TiiP{-FDP};
 
if (tstat[17].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dWCdt := 0
else
 dWCdt:=LcWC-LcWCa+LitCDebris{-FWC};
 
if (tstat[18].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dWNdt := 0
else
 dWNdt:=LcWN-LcWNa+LitNDebris{-FWN};
 
if (tstat[19].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dWPdt := 0
else
 dWPdt:=LcWP-LcWPa+LitPDebris{-FWP};
 
if (tstat[20].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dSCdt := 0
else
 dSCdt:=TiiC-MiiC;
 
if (tstat[21].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dSNdt := 0
else
 dSNdt:=TiiN-MiiN;
 
if (tstat[22].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dSPdt := 0
else
 dSPdt:=TiiP-MiiP;
 
if (tstat[23].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dENH4dt := 0
else
 dENH4dt:=INH4-UNH4-LNH4-UNH4m+RNm-Nitr+MiiN+IRinNH4-ROvfNH4;
 
if (tstat[24].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dENO3dt := 0
else
 dENO3dt:=INO3-UNO3-LNO3-UNO3m+Nitr-DNtr+IRinNO3-ROvfNO3{+FNO3};
 
if (tstat[25].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dEPO4dt := 0
else
 dEPO4dt:=Paw+Pnow+IPO4-UPO4-LPO4-UPO4m+RPm-PO4P+MiiP+IRinPO4-ROvfPO4{+FPO4};
 
if (tstat[26].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dPadt := 0
else
 dPadt:=IPa-Paw;
 
if (tstat[27].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dPnodt := 0
else
 dPnodt:=Pocclw+PO4P-Pnow-Pnos;
 
if (tstat[28].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dPoccldt := 0
else
 dPoccldt:=Pnos-Pocclw;
 
if (tstat[29].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dWdt := 0
else
 dWdt:=-UW-Ro+Rin-OvfR;
 
if (tstat[30].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dWSnowdt := 0
else
 dWsnowdt:=0 ;
 
if (tstat[31].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dSQdt := 0
else
 dSQdt:=0;
 
if (tstat[32].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dfcdt := 0
else
 dfcdt:=Gfc-Lfc;
 
if (tstat[33].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dRCadt := 0
else
 dRCadt:=tau*(RCt-RCa);
 
if (tstat[34].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dRNadt := 0
else
 dRNadt:=tau*(RNt-RNa);
 
if (tstat[35].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dRPadt := 0
else
 dRPadt:=tau*(RPt-RPa);
 
if (tstat[36].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dUCadt := 0
else
 dUCadt:=tau*(UC-UCa);
 
if (tstat[37].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dUNadt := 0
else
 dUNadt:=tau*(UN-UNa);
 
if (tstat[38].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dUPadt := 0
else
 dUPadt:=tau*(UPO4-UPa);
 
if (tstat[39].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dDDaypdt := 0
else
 dDDaypdt:=0;
 
if (tstat[40].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dDDayndt := 0
else
 dDDayndt:=0;
 
if (tstat[41].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumGPPdt := 0
else
 dCumGPPdt:=UC;
 
if (tstat[42].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumNPPdt := 0
else
 dCumNPPdt:=NPP;
 
if (tstat[43].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumNEPdt := 0
else
 dCumNEPdt:=NEP;
 
if (tstat[44].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumUdoCdt := 0
else
 dCumUdoCdt:=UdoC;
 
if (tstat[45].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLitCdt := 0
else
 dCumLitCdt:=LitC;
 
if (tstat[46].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLcWCdt := 0
else
 dCumLcWCdt:=LcWC;
 
if (tstat[47].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumtotalLitCdt := 0
else
 dCumtotalLitCdt:=LitC+LcWC+LitCDebris;
 
if (tstat[48].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumtotalLitNdt := 0
else
 dCumtotalLitNdt:=LitN+LcWN+LitNDebris;
 
if (tstat[49].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumtotalLitPdt := 0
else
 dCumtotalLitPdt:=LitP+LcWP+LitPDebris;
 
if (tstat[50].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumRCmtotaldt := 0
else
 dCumRCmtotaldt:=RCm+MiiC;
 
if (tstat[51].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumNmintotdt := 0
else
 dCumNmintotdt:=-UNH4m-UNO3m+RNm+MiiN;
 
if (tstat[52].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumPmintotdt := 0
else
 dCumPmintotdt:=-UPO4m+RPm+MiiP;
 
if (tstat[53].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumUNdt := 0
else
 dCumUNdt:=UNH4+UNO3+UdoN+UNfix;
 
if (tstat[54].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumUNH4dt := 0
else
 dCumUNH4dt:=UNH4;
 
if (tstat[55].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumUNO3dt := 0
else
 dCumUNO3dt:=UNO3;
 
if (tstat[56].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumUdoNdt := 0
else
 dCumUdoNdt:=UdoN;
 
if (tstat[57].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumUNfixdt := 0
else
 dCumUNfixdt:=UNfix;
 
if (tstat[58].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLitNdt := 0
else
 dCumLitNdt:=LitN;
 
if (tstat[59].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLcWNdt := 0
else
 dCumLcWNdt:=LcWN;
 
if (tstat[60].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumUPO4dt := 0
else
 dCumUPO4dt:=UPO4;
 
if (tstat[61].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLitPdt := 0
else
 dCumLitPdt:=LitP;
 
if (tstat[62].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLcWPdt := 0
else
 dCumLcWPdt:=LcWP;
 
if (tstat[63].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLcWCadt := 0
else
 dCumLcWCadt:=LcWCa;
 
if (tstat[64].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumRCmdt := 0
else
 dCumRCmdt:=RCm;
 
if (tstat[65].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumTiiCdt := 0
else
 dCumTiiCdt:=TiiC;
 
if (tstat[66].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLcWNadt := 0
else
 dCumLcWNadt:=LcWNa;
 
if (tstat[67].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumUNH4mdt := 0
else
 dCumUNH4mdt:=UNH4m;
 
if (tstat[68].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumUNO3mdt := 0
else
 dCumUNO3mdt:=UNO3m;
 
if (tstat[69].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumRNmdt := 0
else
 dCumRNmdt:=RNm;
 
if (tstat[70].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumTiiNdt := 0
else
 dCumTiiNdt:=TiiN;
 
if (tstat[71].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumNnsfixdt := 0
else
 dCumNnsfixdt:=Nnsfix;
 
if (tstat[72].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLcWPadt := 0
else
 dCumLcWPadt:=LcWPa;
 
if (tstat[73].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumUPO4mdt := 0
else
 dCumUPO4mdt:=UPO4m;
 
if (tstat[74].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumRPmdt := 0
else
 dCumRPmdt:=RPm;
 
if (tstat[75].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumTiiPdt := 0
else
 dCumTiiPdt:=TiiP;
 
if (tstat[76].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumMiiCdt := 0
else
 dCumMiiCdt:=MiiC;
 
if (tstat[77].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumMiiNdt := 0
else
 dCumMiiNdt:=MiiN;
 
if (tstat[78].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumMiiPdt := 0
else
 dCumMiiPdt:=MiiP;
 
if (tstat[79].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumINH4dt := 0
else
 dCumINH4dt:=INH4;
 
if (tstat[80].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLNH4dt := 0
else
 dCumLNH4dt:=LNH4;
 
if (tstat[81].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumNitrdt := 0
else
 dCumNitrdt:=Nitr;
 
if (tstat[82].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumINO3dt := 0
else
 dCumINO3dt:=INO3;
 
if (tstat[83].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLNO3dt := 0
else
 dCumLNO3dt:=LNO3;
 
if (tstat[84].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumDNtrdt := 0
else
 dCumDNtrdt:=DNtr;
 
if (tstat[85].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumPawdt := 0
else
 dCumPawdt:=Paw;
 
if (tstat[86].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumPnowdt := 0
else
 dCumPnowdt:=Pnow;
 
if (tstat[87].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumIPO4dt := 0
else
 dCumIPO4dt:=IPO4;
 
if (tstat[88].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLPO4dt := 0
else
 dCumLPO4dt:=LPO4;
 
if (tstat[89].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumPO4Pdt := 0
else
 dCumPO4Pdt:=PO4P;
 
if (tstat[90].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumIPadt := 0
else
 dCumIPadt:=IPa;
 
if (tstat[91].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumPocclwdt := 0
else
 dCumPocclwdt:=Pocclw;
 
if (tstat[92].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumPnosdt := 0
else
 dCumPnosdt:=Pnos;
 
if (tstat[93].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumIdoCdt := 0
else
 dCumIdoCdt:=IdoC;
 
if (tstat[94].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumIdoNdt := 0
else
 dCumIdoNdt:=IdoN;
 
if (tstat[95].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLdoCdt := 0
else
 dCumLdoCdt:=LdoC;
 
if (tstat[96].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLdoNdt := 0
else
 dCumLdoNdt:=LdoN;
 
if (tstat[97].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumUWdt := 0
else
 dCumUWdt:=UW;
 
if (tstat[98].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumROdt := 0
else
 dCumRodt:=Ro+OvfR;
 
if (tstat[99].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumPptdt := 0
else
 dCumPptdt:=0;
 
if (tstat[100].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumIntrdt := 0
else
 dCumIntrdt:=0;
 
if (tstat[101].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumRfldt := 0
else
 dCumRfldt:=0;
 
if (tstat[102].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumSfldt := 0
else
 dCumSfldt:=0;
 
if (tstat[103].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumSmdt := 0
else
 dCumSmdt:=0;
 
if (tstat[104].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumRindt := 0
else
 dCumRindt:=Rin;//???
 
if (tstat[105].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumIRindoCdt := 0
else
 dCumIRindoCdt:=IRindoC;
 
if (tstat[106].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumIRindoNdt := 0
else
 dCumIRindoNdt:=IRindoN;
 
if (tstat[107].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumIRinNH4dt := 0
else
 dCumIRinNH4dt:=IRinNH4;
 
if (tstat[108].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumIRinNO3dt := 0
else
 dCumIRinNO3dt:=IRinNO3;
 
if (tstat[109].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumIRinPO4dt := 0
else
 dCumIRinPO4dt:=IRinPO4;
 
if (tstat[110].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumROvfdt := 0
else
 dCumROvfdt:=OvfR;
 
if (tstat[111].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumROvfdoCdt := 0
else
 dCumROvfdoCdt:=ROvfdoC;
 
if (tstat[112].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumROvfdoNdt := 0
else
 dCumROvfdoNdt:=ROvfdoN;
 
if (tstat[113].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumROvfNH4dt := 0
else
 dCumROvfNH4dt:=ROvfNH4;
 
if (tstat[114].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumROvfNO3dt := 0
else
 dCumROvfNO3dt:=ROvfNO3;
 
if (tstat[115].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumROvfPO4dt := 0
else
 dCumROvfPO4dt:=ROvfPO4;
 
if (tstat[116].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dLAIpeakdt := 0
else
 dLAIpeakdt:=0;
 
if (tstat[117].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumfBCdt := 0
else
 dCumFBCdt:=0;
 
if (tstat[118].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumfBNdt := 0
else
 dCumFBNdt:=0;
 
if (tstat[119].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumfBPdt := 0
else
 dCumFBPdt:=0;
 
if (tstat[120].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumfWCdt := 0
else
 dCumFWCdt:=0;
 
if (tstat[121].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumfWNdt := 0
else
 dCumFWNdt:=0;
 
if (tstat[122].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumfWPdt := 0
else
 dCumFWPdt:=0;
 
if (tstat[123].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumfDCdt := 0
else
 dCumFDCdt:=0;
 
if (tstat[124].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumfDNdt := 0
else
 dCumFDNdt:=0;
 
if (tstat[125].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumfDPdt := 0
else
 dCumFDPdt:=0;
 
if (tstat[126].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumFCvtdt := 0
else
 dCumFCvtdt:=0;
 
if (tstat[127].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumFNvtdt := 0
else
 dCumFNvtdt:=0;
 
if (tstat[128].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumFPvtdt := 0
else
 dCumFPvtdt:=0;
 
if (tstat[129].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLitCDebrisdt := 0
else
 dCumLitCDebrisdt:=LitCDebris;
 
if (tstat[130].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLitNDebrisdt := 0
else
 dCumLitNDebrisdt:=LitNDebris;
 
if (tstat[131].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dCumLitPDebrisdt := 0
else
 dCumLitPDebrisdt:=LitPDebris;
 
if (tstat[132].HoldConstant) and (FmOptions.RunOptions.HoldStatesConstant) then
 dBCpeakdt := 0
else
 dBCpeakdt:=0; // v2.8.5
 

{ Now that the calculations are complete, assign everything back into the arrays
  so the rest of the code can access the values calculated here. (Local variables
  are destroyed at the end of the procedure).

  Put the state variables back into the global arrays in case the state variable
  was manually changed in this procedure (e.g. discrete state variables or steady state
  calculations).   }
tstat[1].value := BC;
tstat[2].value := BN;
tstat[3].value := BP;
tstat[4].value := VC;
tstat[5].value := VN;
tstat[6].value := VP;
tstat[7].value := vCO2;
tstat[8].value := vI;
tstat[9].value := vW;
tstat[10].value := vNH4;
tstat[11].value := vNO3;
tstat[12].value := vdoN;
tstat[13].value := vNfix;
tstat[14].value := DC;
tstat[15].value := DN;
tstat[16].value := DP;
tstat[17].value := WC;
tstat[18].value := WN;
tstat[19].value := WP;
tstat[20].value := SC;
tstat[21].value := SN;
tstat[22].value := SP;
tstat[23].value := ENH4;
tstat[24].value := ENO3;
tstat[25].value := EPO4;
tstat[26].value := Pa;
tstat[27].value := Pno;
tstat[28].value := Poccl;
tstat[29].value := W;
tstat[30].value := WSnow;
tstat[31].value := SQ;
tstat[32].value := fc;
tstat[33].value := RCa;
tstat[34].value := RNa;
tstat[35].value := RPa;
tstat[36].value := UCa;
tstat[37].value := UNa;
tstat[38].value := UPa;
tstat[39].value := DDayp;
tstat[40].value := DDayn;
tstat[41].value := CumGPP;
tstat[42].value := CumNPP;
tstat[43].value := CumNEP;
tstat[44].value := CumUdoC;
tstat[45].value := CumLitC;
tstat[46].value := CumLcWC;
tstat[47].value := CumtotalLitC;
tstat[48].value := CumtotalLitN;
tstat[49].value := CumtotalLitP;
tstat[50].value := CumRCmtotal;
tstat[51].value := CumNmintot;
tstat[52].value := CumPmintot;
tstat[53].value := CumUN;
tstat[54].value := CumUNH4;
tstat[55].value := CumUNO3;
tstat[56].value := CumUdoN;
tstat[57].value := CumUNfix;
tstat[58].value := CumLitN;
tstat[59].value := CumLcWN;
tstat[60].value := CumUPO4;
tstat[61].value := CumLitP;
tstat[62].value := CumLcWP;
tstat[63].value := CumLcWCa;
tstat[64].value := CumRCm;
tstat[65].value := CumTiiC;
tstat[66].value := CumLcWNa;
tstat[67].value := CumUNH4m;
tstat[68].value := CumUNO3m;
tstat[69].value := CumRNm;
tstat[70].value := CumTiiN;
tstat[71].value := CumNnsfix;
tstat[72].value := CumLcWPa;
tstat[73].value := CumUPO4m;
tstat[74].value := CumRPm;
tstat[75].value := CumTiiP;
tstat[76].value := CumMiiC;
tstat[77].value := CumMiiN;
tstat[78].value := CumMiiP;
tstat[79].value := CumINH4;
tstat[80].value := CumLNH4;
tstat[81].value := CumNitr;
tstat[82].value := CumINO3;
tstat[83].value := CumLNO3;
tstat[84].value := CumDNtr;
tstat[85].value := CumPaw;
tstat[86].value := CumPnow;
tstat[87].value := CumIPO4;
tstat[88].value := CumLPO4;
tstat[89].value := CumPO4P;
tstat[90].value := CumIPa;
tstat[91].value := CumPocclw;
tstat[92].value := CumPnos;
tstat[93].value := CumIdoC;
tstat[94].value := CumIdoN;
tstat[95].value := CumLdoC;
tstat[96].value := CumLdoN;
tstat[97].value := CumUW;
tstat[98].value := CumRO;
tstat[99].value := CumPpt;
tstat[100].value := CumIntr;
tstat[101].value := CumRfl;
tstat[102].value := CumSfl;
tstat[103].value := CumSm;
tstat[104].value := CumRin;
tstat[105].value := CumIRindoC;
tstat[106].value := CumIRindoN;
tstat[107].value := CumIRinNH4;
tstat[108].value := CumIRinNO3;
tstat[109].value := CumIRinPO4;
tstat[110].value := CumROvf;
tstat[111].value := CumROvfdoC;
tstat[112].value := CumROvfdoN;
tstat[113].value := CumROvfNH4;
tstat[114].value := CumROvfNO3;
tstat[115].value := CumROvfPO4;
tstat[116].value := LAIpeak;
tstat[117].value := CumfBC;
tstat[118].value := CumfBN;
tstat[119].value := CumfBP;
tstat[120].value := CumfWC;
tstat[121].value := CumfWN;
tstat[122].value := CumfWP;
tstat[123].value := CumfDC;
tstat[124].value := CumfDN;
tstat[125].value := CumfDP;
tstat[126].value := CumFCvt;
tstat[127].value := CumFNvt;
tstat[128].value := CumFPvt;
tstat[129].value := CumLitCDebris;
tstat[130].value := CumLitNDebris;
tstat[131].value := CumLitPDebris;
tstat[132].value := BCpeak;

{  Put all process values into process variable array. The first numstate
  processes are the derivatives of the state variables (Calculated above).}
tproc[1].value := dBCdt;
tproc[2].value := dBNdt;
tproc[3].value := dBPdt;
tproc[4].value := dVCdt;
tproc[5].value := dVNdt;
tproc[6].value := dVPdt;
tproc[7].value := dvCO2dt;
tproc[8].value := dvIdt;
tproc[9].value := dvWdt;
tproc[10].value := dvNH4dt;
tproc[11].value := dvNO3dt;
tproc[12].value := dvdoNdt;
tproc[13].value := dvNfixdt;
tproc[14].value := dDCdt;
tproc[15].value := dDNdt;
tproc[16].value := dDPdt;
tproc[17].value := dWCdt;
tproc[18].value := dWNdt;
tproc[19].value := dWPdt;
tproc[20].value := dSCdt;
tproc[21].value := dSNdt;
tproc[22].value := dSPdt;
tproc[23].value := dENH4dt;
tproc[24].value := dENO3dt;
tproc[25].value := dEPO4dt;
tproc[26].value := dPadt;
tproc[27].value := dPnodt;
tproc[28].value := dPoccldt;
tproc[29].value := dWdt;
tproc[30].value := dWSnowdt;
tproc[31].value := dSQdt;
tproc[32].value := dfcdt;
tproc[33].value := dRCadt;
tproc[34].value := dRNadt;
tproc[35].value := dRPadt;
tproc[36].value := dUCadt;
tproc[37].value := dUNadt;
tproc[38].value := dUPadt;
tproc[39].value := dDDaypdt;
tproc[40].value := dDDayndt;
tproc[41].value := dCumGPPdt;
tproc[42].value := dCumNPPdt;
tproc[43].value := dCumNEPdt;
tproc[44].value := dCumUdoCdt;
tproc[45].value := dCumLitCdt;
tproc[46].value := dCumLcWCdt;
tproc[47].value := dCumtotalLitCdt;
tproc[48].value := dCumtotalLitNdt;
tproc[49].value := dCumtotalLitPdt;
tproc[50].value := dCumRCmtotaldt;
tproc[51].value := dCumNmintotdt;
tproc[52].value := dCumPmintotdt;
tproc[53].value := dCumUNdt;
tproc[54].value := dCumUNH4dt;
tproc[55].value := dCumUNO3dt;
tproc[56].value := dCumUdoNdt;
tproc[57].value := dCumUNfixdt;
tproc[58].value := dCumLitNdt;
tproc[59].value := dCumLcWNdt;
tproc[60].value := dCumUPO4dt;
tproc[61].value := dCumLitPdt;
tproc[62].value := dCumLcWPdt;
tproc[63].value := dCumLcWCadt;
tproc[64].value := dCumRCmdt;
tproc[65].value := dCumTiiCdt;
tproc[66].value := dCumLcWNadt;
tproc[67].value := dCumUNH4mdt;
tproc[68].value := dCumUNO3mdt;
tproc[69].value := dCumRNmdt;
tproc[70].value := dCumTiiNdt;
tproc[71].value := dCumNnsfixdt;
tproc[72].value := dCumLcWPadt;
tproc[73].value := dCumUPO4mdt;
tproc[74].value := dCumRPmdt;
tproc[75].value := dCumTiiPdt;
tproc[76].value := dCumMiiCdt;
tproc[77].value := dCumMiiNdt;
tproc[78].value := dCumMiiPdt;
tproc[79].value := dCumINH4dt;
tproc[80].value := dCumLNH4dt;
tproc[81].value := dCumNitrdt;
tproc[82].value := dCumINO3dt;
tproc[83].value := dCumLNO3dt;
tproc[84].value := dCumDNtrdt;
tproc[85].value := dCumPawdt;
tproc[86].value := dCumPnowdt;
tproc[87].value := dCumIPO4dt;
tproc[88].value := dCumLPO4dt;
tproc[89].value := dCumPO4Pdt;
tproc[90].value := dCumIPadt;
tproc[91].value := dCumPocclwdt;
tproc[92].value := dCumPnosdt;
tproc[93].value := dCumIdoCdt;
tproc[94].value := dCumIdoNdt;
tproc[95].value := dCumLdoCdt;
tproc[96].value := dCumLdoNdt;
tproc[97].value := dCumUWdt;
tproc[98].value := dCumROdt;
tproc[99].value := dCumPptdt;
tproc[100].value := dCumIntrdt;
tproc[101].value := dCumRfldt;
tproc[102].value := dCumSfldt;
tproc[103].value := dCumSmdt;
tproc[104].value := dCumRindt;
tproc[105].value := dCumIRindoCdt;
tproc[106].value := dCumIRindoNdt;
tproc[107].value := dCumIRinNH4dt;
tproc[108].value := dCumIRinNO3dt;
tproc[109].value := dCumIRinPO4dt;
tproc[110].value := dCumROvfdt;
tproc[111].value := dCumROvfdoCdt;
tproc[112].value := dCumROvfdoNdt;
tproc[113].value := dCumROvfNH4dt;
tproc[114].value := dCumROvfNO3dt;
tproc[115].value := dCumROvfPO4dt;
tproc[116].value := dLAIpeakdt;
tproc[117].value := dCumfBCdt;
tproc[118].value := dCumfBNdt;
tproc[119].value := dCumfBPdt;
tproc[120].value := dCumfWCdt;
tproc[121].value := dCumfWNdt;
tproc[122].value := dCumfWPdt;
tproc[123].value := dCumfDCdt;
tproc[124].value := dCumfDNdt;
tproc[125].value := dCumfDPdt;
tproc[126].value := dCumFCvtdt;
tproc[127].value := dCumFNvtdt;
tproc[128].value := dCumFPvtdt;
tproc[129].value := dCumLitCDebrisdt;
tproc[130].value := dCumLitNDebrisdt;
tproc[131].value := dCumLitPDebrisdt;
tproc[132].value := dBCpeakdt;

{ Now the remaining processes. Be sure to number the processes the same here as
  you did in the procedure counts above. }
tproc[ModelDef.numstate + 1].value := Ta;
tproc[ModelDef.numstate + 2].value := Bt;
tproc[ModelDef.numstate + 3].value := Ba;
tproc[ModelDef.numstate + 4].value := BL;
tproc[ModelDef.numstate + 5].value := BR;
tproc[ModelDef.numstate + 6].value := BW;
tproc[ModelDef.numstate + 7].value := L;
tproc[ModelDef.numstate + 8].value := L_max;
tproc[ModelDef.numstate + 9].value := Gfc;
tproc[ModelDef.numstate + 10].value := deltaGcT;
tproc[ModelDef.numstate + 11].value := Lfc;
tproc[ModelDef.numstate + 12].value := deltaLcW;
tproc[ModelDef.numstate + 13].value := Delta_E;
tproc[ModelDef.numstate + 14].value := psiS;
tproc[ModelDef.numstate + 15].value := Paw;
tproc[ModelDef.numstate + 16].value := PO4P;
tproc[ModelDef.numstate + 17].value := Pnow;
tproc[ModelDef.numstate + 18].value := Pocclw;
tproc[ModelDef.numstate + 19].value := Pnos;
tproc[ModelDef.numstate + 20].value := c_cs;
tproc[ModelDef.numstate + 21].value := PsIrr;
tproc[ModelDef.numstate + 22].value := PsC;
tproc[ModelDef.numstate + 23].value := PsW;
tproc[ModelDef.numstate + 24].value := UC;
tproc[ModelDef.numstate + 25].value := UW;
tproc[ModelDef.numstate + 26].value := PET;
tproc[ModelDef.numstate + 27].value := NH4aq;
tproc[ModelDef.numstate + 28].value := UNH4;
tproc[ModelDef.numstate + 29].value := NO3aq;
tproc[ModelDef.numstate + 30].value := UNO3;
tproc[ModelDef.numstate + 31].value := PaDoC;
tproc[ModelDef.numstate + 32].value := UdoC;
tproc[ModelDef.numstate + 33].value := UdoN;
tproc[ModelDef.numstate + 34].value := UNfix;
tproc[ModelDef.numstate + 35].value := UN;
tproc[ModelDef.numstate + 36].value := PO4aq;
tproc[ModelDef.numstate + 37].value := UPO4;
tproc[ModelDef.numstate + 38].value := aqN;
tproc[ModelDef.numstate + 39].value := qN;
tproc[ModelDef.numstate + 40].value := aqP;
tproc[ModelDef.numstate + 41].value := qP;
tproc[ModelDef.numstate + 42].value := LitC;
tproc[ModelDef.numstate + 43].value := LitN;
tproc[ModelDef.numstate + 44].value := LitP;
tproc[ModelDef.numstate + 45].value := LitCDebris;
tproc[ModelDef.numstate + 46].value := LitNDebris;
tproc[ModelDef.numstate + 47].value := LitPDebris;
tproc[ModelDef.numstate + 48].value := LcWC;
tproc[ModelDef.numstate + 49].value := LcWN;
tproc[ModelDef.numstate + 50].value := LcWP;
tproc[ModelDef.numstate + 51].value := LcWCa;
tproc[ModelDef.numstate + 52].value := LcWNa;
tproc[ModelDef.numstate + 53].value := LcWPa;
tproc[ModelDef.numstate + 54].value := RCPm;
tproc[ModelDef.numstate + 55].value := RCPt;
tproc[ModelDef.numstate + 56].value := NUE;
tproc[ModelDef.numstate + 57].value := PUE;
tproc[ModelDef.numstate + 58].value := WUE;
tproc[ModelDef.numstate + 59].value := Vstar;
tproc[ModelDef.numstate + 60].value := RCg;
tproc[ModelDef.numstate + 61].value := RNg;
tproc[ModelDef.numstate + 62].value := RPg;
tproc[ModelDef.numstate + 63].value := yNH4;
tproc[ModelDef.numstate + 64].value := yNO3;
tproc[ModelDef.numstate + 65].value := ydoN;
tproc[ModelDef.numstate + 66].value := yNfix;
tproc[ModelDef.numstate + 67].value := yCO2;
tproc[ModelDef.numstate + 68].value := yI;
tproc[ModelDef.numstate + 69].value := yW;
tproc[ModelDef.numstate + 70].value := yCa;
tproc[ModelDef.numstate + 71].value := yNa;
tproc[ModelDef.numstate + 72].value := RCt;
tproc[ModelDef.numstate + 73].value := RNt;
tproc[ModelDef.numstate + 74].value := RPt;
tproc[ModelDef.numstate + 75].value := phi;
tproc[ModelDef.numstate + 76].value := Ci;
tproc[ModelDef.numstate + 77].value := VTot;
tproc[ModelDef.numstate + 78].value := VR;
tproc[ModelDef.numstate + 79].value := VL;
tproc[ModelDef.numstate + 80].value := SoCt;
tproc[ModelDef.numstate + 81].value := SoNt;
tproc[ModelDef.numstate + 82].value := SoPt;
tproc[ModelDef.numstate + 83].value := NPP;
tproc[ModelDef.numstate + 84].value := Ro;
tproc[ModelDef.numstate + 85].value := LNH4;
tproc[ModelDef.numstate + 86].value := LNO3;
tproc[ModelDef.numstate + 87].value := LdoC;
tproc[ModelDef.numstate + 88].value := LdoN;
tproc[ModelDef.numstate + 89].value := LNtot;
tproc[ModelDef.numstate + 90].value := LPO4;
tproc[ModelDef.numstate + 91].value := thetaN;
tproc[ModelDef.numstate + 92].value := thetaP;
tproc[ModelDef.numstate + 93].value := UNH4m;
tproc[ModelDef.numstate + 94].value := UNO3m;
tproc[ModelDef.numstate + 95].value := UNmtot;
tproc[ModelDef.numstate + 96].value := UPO4m;
tproc[ModelDef.numstate + 97].value := MC;
tproc[ModelDef.numstate + 98].value := MN;
tproc[ModelDef.numstate + 99].value := MP;
tproc[ModelDef.numstate + 100].value := LambdaC;
tproc[ModelDef.numstate + 101].value := LambdaN;
tproc[ModelDef.numstate + 102].value := LambdaP;
tproc[ModelDef.numstate + 103].value := RCm;
tproc[ModelDef.numstate + 104].value := RNm;
tproc[ModelDef.numstate + 105].value := RPm;
tproc[ModelDef.numstate + 106].value := TiiC;
tproc[ModelDef.numstate + 107].value := TiiN;
tproc[ModelDef.numstate + 108].value := TiiP;
tproc[ModelDef.numstate + 109].value := MiiC;
tproc[ModelDef.numstate + 110].value := MiiN;
tproc[ModelDef.numstate + 111].value := MiiP;
tproc[ModelDef.numstate + 112].value := Nnsfix;
tproc[ModelDef.numstate + 113].value := Nitr;
tproc[ModelDef.numstate + 114].value := dVtot;
tproc[ModelDef.numstate + 115].value := Ndept;
tproc[ModelDef.numstate + 116].value := netNmin;
tproc[ModelDef.numstate + 117].value := netPmin;
tproc[ModelDef.numstate + 118].value := NEP;
tproc[ModelDef.numstate + 119].value := NeNb;
tproc[ModelDef.numstate + 120].value := NePB;
tproc[ModelDef.numstate + 121].value := NeCB;
tproc[ModelDef.numstate + 122].value := NEWB;
tproc[ModelDef.numstate + 123].value := CumNeCB;
tproc[ModelDef.numstate + 124].value := CumNeNB;
tproc[ModelDef.numstate + 125].value := CumNePB;
tproc[ModelDef.numstate + 126].value := CumNeWB;
tproc[ModelDef.numstate + 127].value := Btstar;
tproc[ModelDef.numstate + 128].value := LL;
tproc[ModelDef.numstate + 129].value := Sm;
tproc[ModelDef.numstate + 130].value := Doy;
tproc[ModelDef.numstate + 131].value := Dl;
tproc[ModelDef.numstate + 132].value := delta;
tproc[ModelDef.numstate + 133].value := Intr;
tproc[ModelDef.numstate + 134].value := OmegaC;
tproc[ModelDef.numstate + 135].value := OmegaN;
tproc[ModelDef.numstate + 136].value := OmegaP;
tproc[ModelDef.numstate + 137].value := dUCdvCO2;
tproc[ModelDef.numstate + 138].value := dUCdvW;
tproc[ModelDef.numstate + 139].value := dUCdvI;
tproc[ModelDef.numstate + 140].value := dUNdvNH4;
tproc[ModelDef.numstate + 141].value := dUNdvNO3;
tproc[ModelDef.numstate + 142].value := dUNdvdoN;
tproc[ModelDef.numstate + 143].value := dUNdvNfix;
tproc[ModelDef.numstate + 144].value := dUCdVC;
tproc[ModelDef.numstate + 145].value := chiC;
tproc[ModelDef.numstate + 146].value := chiN;
tproc[ModelDef.numstate + 147].value := chiP;
tproc[ModelDef.numstate + 148].value := zetaCO2;
tproc[ModelDef.numstate + 149].value := zetaW;
tproc[ModelDef.numstate + 150].value := zetaI;
tproc[ModelDef.numstate + 151].value := zetaNH4;
tproc[ModelDef.numstate + 152].value := zetaNO3;
tproc[ModelDef.numstate + 153].value := zetaNfix;
tproc[ModelDef.numstate + 154].value := zetadoN;
tproc[ModelDef.numstate + 155].value := z;
tproc[ModelDef.numstate + 156].value := betanRd;
tproc[ModelDef.numstate + 157].value := Rd;
tproc[ModelDef.numstate + 158].value := Rl;
tproc[ModelDef.numstate + 159].value := DNtr;
tproc[ModelDef.numstate + 160].value := Dop;
tproc[ModelDef.numstate + 161].value := VpC;
tproc[ModelDef.numstate + 162].value := fuf;
tproc[ModelDef.numstate + 163].value := Ts;
tproc[ModelDef.numstate + 164].value := ks;
tproc[ModelDef.numstate + 165].value := kst;
tproc[ModelDef.numstate + 166].value := ksf;
tproc[ModelDef.numstate + 167].value := aQl;
tproc[ModelDef.numstate + 168].value := aQh;
tproc[ModelDef.numstate + 169].value := Sfl;
tproc[ModelDef.numstate + 170].value := Rfl;
tproc[ModelDef.numstate + 171].value := theta;
tproc[ModelDef.numstate + 172].value := OvfR;
tproc[ModelDef.numstate + 173].value := ROvfdoC;
tproc[ModelDef.numstate + 174].value := ROvfdoN;
tproc[ModelDef.numstate + 175].value := ROvfNH4;
tproc[ModelDef.numstate + 176].value := ROvfNO3;
tproc[ModelDef.numstate + 177].value := ROvfPO4;
tproc[ModelDef.numstate + 178].value := OvfI;
tproc[ModelDef.numstate + 179].value := Dot;
tproc[ModelDef.numstate + 180].value := calib;
tproc[ModelDef.numstate + 181].value := deltaDW;
tproc[ModelDef.numstate + 182].value := Yf;
tproc[ModelDef.numstate + 183].value := SCdev;
tproc[ModelDef.numstate + 184].value := STdev;
tproc[ModelDef.numstate + 185].value := WTdev;
tproc[ModelDef.numstate + 186].value := Pptsim;
tproc[ModelDef.numstate + 187].value := SPdev;
tproc[ModelDef.numstate + 188].value := WPdev;
tproc[ModelDef.numstate + 189].value := Proj_Ca;
tproc[ModelDef.numstate + 190].value := Proj_Tmax;
tproc[ModelDef.numstate + 191].value := Proj_Tmin;
tproc[ModelDef.numstate + 192].value := Proj_Ppt;
tproc[ModelDef.numstate + 193].value := Tave;
tproc[ModelDef.numstate + 194].value := FBC;
tproc[ModelDef.numstate + 195].value := FBN;
tproc[ModelDef.numstate + 196].value := FBP;
tproc[ModelDef.numstate + 197].value := FWC;
tproc[ModelDef.numstate + 198].value := FWN;
tproc[ModelDef.numstate + 199].value := FWP;
tproc[ModelDef.numstate + 200].value := FDC;
tproc[ModelDef.numstate + 201].value := FDN;
tproc[ModelDef.numstate + 202].value := FDP;
tproc[ModelDef.numstate + 203].value := FNO3;
tproc[ModelDef.numstate + 204].value := FPO4;
tproc[ModelDef.numstate + 205].value := FNvol;
tproc[ModelDef.numstate + 206].value := FPvol;
tproc[ModelDef.numstate + 207].value := LW;
tproc[ModelDef.numstate + 208].value := Proj_Rin;

end;  // End of processes procedure


       { Do not make any modifications to code below this line. }
{****************************************************************************}


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

{ This procedure supplies the derivatives of the state variables to the
  integrator. Since the integrator deals only with the values of the variables
  and not there names, units or the state field HoldConstant, this procedure
  copies the state values into a temporary state array and copies the value of
  HoldConstant into the temporary state array and passes this temporary state
  array to the procedure processes. }
PROCEDURE derivs(t, drt:double; var tdrive:drivearray; var tpar:paramarray;
             var statevalue:yValueArray; VAR dydt:yValueArray);
var
   i:integer;
   tempproc:processarray;
   tempstate:statearray;
begin
   tempstate := stat;  // Copy names, units and HoldConstant to tempstate
  // Copy current values of state variables into tempstate
   for i := 1 to ModelDef.numstate do tempstate[i].value := statevalue[i];
  // Calculate the process values
   processes(t, drt, tdrive, tpar, tempstate, tempproc, false);
  // Put process values into dydt array to get passed back to the integrator.
   for i:= 1 to ModelDef.numstate do dydt[i]:=tempproc[i].value;
end;  // end of derivs procedure

end.
