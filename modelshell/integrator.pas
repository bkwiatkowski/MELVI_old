{ Code to implement a 4th order Runge-Kutta integrator with adaptive step size
  control. This integrator is used to integrate the differential equations in
  the equations.pas file. }
unit integrator;

interface

uses sysutils, Dialogs, stypes, equations, fileio;

PROCEDURE RK4_5_Integrator(VAR ystart: yValueArray; nvar: integer;
       t1,t2,dr: double; var stepguess:double;
       var tdrive:drivearray; var tpar:paramarray);

implementation

uses frontend, math, Options;

PROCEDURE RungeKutta(y: yValueArray; n: integer; t,tstep,dr: double; flag:boolean;
   VAR yout, dydt: yValueArray; var tpar:paramarray; var tdrive:drivearray);

VAR
   i: integer;

   ytemp,k,S:yValueArray;
BEGIN
  try
    if flag then
      begin
        derivs(t,dr,tdrive,tpar,y,k);                       // calk dirivs
        dydt:=k;                                           // save 1st k for next call
      end
    else k:=dydt;                                           // use k input to routine
    for i:=1 to n do S[i]:=k[i]/6;                          // 1st k

    FOR i := 1 to n DO ytemp[i] := y[i] + tstep*k[i]/2;     // half step 1
    derivs(t+tstep/2,dr+tstep/2,tdrive,tpar,ytemp,k);       // calk dirivs
    for i:=1 to n do S[i]:=S[i]+k[i]/3;                     // 2nd k

    FOR i := 1 to n DO ytemp[i] := y[i] + tstep*k[i]/2;     // half step 2
    derivs(t+tstep/2,dr+tstep/2,tdrive,tpar,ytemp,k);       // calk dirivs
    for i:=1 to n do S[i]:=S[i]+k[i]/3;                     // 3rd k

    FOR i := 1 to n DO ytemp[i] := y[i] + tstep*k[i];       // full step
    derivs(t+tstep,dr+tstep,tdrive,tpar,ytemp,k);           // calk dirivs
    for i:=1 to n do S[i]:=S[i]+k[i]/6;                     // 4th k

    for i:=1 to n do yout[i]:=y[i]+S[i]*tstep;              // estimate new y
  except
   raise;
  end;
END;


PROCEDURE TimestepControl(VAR y:yValueArray; n: integer; VAR t, dr: double;
      steptry: double; VAR stepdid,stepnext: double;
      var tdrive:drivearray; var tpar:paramarray; var index: integer);
Const
  eps= 0.0001;
VAR
   tstep, emax,scale: double;
   ytemp,yout,epsilon,dydt:yValueArray;
   i:integer;
   flag,flag2:boolean;
   tdrivehalftime:drivearray;
Begin
  emax:=2*eps;
  flag:=true;
  flag2:=true;
  while flag2 do
    begin
      counttt:=counttt+1;
      tstep:=steptry/2;
      if (t = t+tstep) THEN
       raise EIntStepTooSmall.Create('Step size too small in TimestepControl '
         + 'Error caused by state variable, ' + stat[index].name + '.');      { TODO -cVisual : Remove word state from error message so that the actual variable name shows up in the box. On some computers it gets cut off. }
      RungeKutta(y, n, t,tstep,dr, Flag, ytemp, dydt, tpar, tdrive);
      flag:=false;
      tdrivehalftime:=tdrive;
      GetCurrentDrivers(dr+tstep,tdrivehalftime);
      RungeKutta(ytemp, n, t+tstep, tstep, dr+tstep, TRUE, yout, epsilon, tpar, tdrivehalftime);
      RungeKutta(y, n, t, steptry, dr, FALSE, ytemp, dydt, tpar, tdrive);
      emax:=0;
      index:=0;
      For i:=1 to n do
        begin
          epsilon[i]:=yout[i]-ytemp[i];
          scale := FmOptions.RunOptions.Errormult*max(abs(y[i]), eps)+1e-30;
          if emax < abs(epsilon[i])/scale then
            begin
              emax:=abs(epsilon[i])/scale;
              index:=i;
            end;
        end;
      stepdid:=steptry;
      if emax>eps then  stepnext:=0.9*steptry*power(eps/emax,1/4)
      else
        begin
          if emax>0 then stepnext:=1.1*steptry*power(eps/emax,1/5)
          else stepnext:=2*steptry;
          flag2:=false;
        end;
      steptry:=stepnext;
    end; {while}
  For i:=1 to n do y[i]:=yout[i]+epsilon[i]/15;
End;


PROCEDURE RK4_5_Integrator(VAR ystart: yValueArray; nvar: integer;
       t1,t2,dr: double; var stepguess:double;
       var tdrive:drivearray; var tpar:paramarray);

VAR
   numstep,i, index: integer;
   t,stepnext,stepdid,step, stepmin:double;
   y: yValueArray;
BEGIN
   stepmin:=0.000001*FmOptions.RunOptions.Time_step;
   t := t1;
   step := stepguess;
   y := ystart;
   Numstep:=0;
   While t<t2 do
     begin
       GetCurrentDrivers(dr,tdrive);
       if t+step>t2 then step:=t2-t; // avoid overstep
       TimestepControl(y,nvar,t,dr,step,stepdid,stepnext,tdrive,tpar,index);
       t:=t+stepdid;
       dr:=dr+stepdid;
       step:=stepnext;
       ystart := y;
       stepguess := stepnext;
       Numstep:=numstep+1;
       IF (abs(stepnext) < stepmin) THEN
             raise EIntStepTooSmall.Create('Step size too small in Integrator. '
             + 'Error caused by state variable, ' + stat[index].name + '.');
       IF numstep>10000 THEN
             raise EIntStepTooSmall.Create('Too many steps in Integrator. '
             + 'Error caused by state variable, ' + stat[index].name + '.');
     end;
   END;


end.
