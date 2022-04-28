{ Contains the code for all file input and output. All I/O should be done using
  these procedures. There is one exception. In the note.pas file the drivers are
  written to the driver file directly from the memo component. It is done that
  way because the memo component has a function to write to a file and using
  that function is easier then converting the text in the memo to the correct
  form for the writedriverfile function. }
unit fileiogrid;
 
interface

uses sysutils, classes, Dialogs, stypes;

type  TAction = (flRead, flWrite);

// Grid shell I/O
function ReadDEMFile(fname:string; 

end.
