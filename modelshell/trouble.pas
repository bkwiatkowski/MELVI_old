unit TROUBLE;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type

  { TFmTrouble }

  TFmTrouble = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    MoContact: TMemo;
    BtnOK: TButton;
    MmError: TMemo;
    procedure Image1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FmTrouble: TFmTrouble;

implementation

{$R *.lfm}

procedure TFmTrouble.FormShow(Sender: TObject);
begin
 // SetWindowPos(FmTrouble.Handle, HWND_TOPMOST,
   //           0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TFmTrouble.Image1Click(Sender: TObject);
begin
  MessageDlg('Take a break, go get a snack.', mtConfirmation, [mbOK],0);
end;

end.

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TFmTrouble = class(TForm)
    Image1: TImage;
    MoContact: TMemo;
    BtnOK: TButton;
    MmError: TMemo;
    procedure Image1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FmTrouble: TFmTrouble;

implementation

{$R *.lfm}

procedure TFmTrouble.FormShow(Sender: TObject);
begin
 // SetWindowPos(FmTrouble.Handle, HWND_TOPMOST,
   //           0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TFmTrouble.Image1Click(Sender: TObject);
begin
  MessageDlg('Take a break, go get a snack.', mtConfirmation, [mbOK],0);
end;

end.
