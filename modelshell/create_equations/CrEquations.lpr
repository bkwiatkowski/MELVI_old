program CrEquations;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Crmain, stypes;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmMain, FmMain);
  Application.Run;
end.

