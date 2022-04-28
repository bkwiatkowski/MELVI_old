{ This form displays a typical About Box. It contains the model name, version
  number and contact information. }
unit aboutbox;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type

  { TFmAbout }

  TFmAbout = class(TForm)
    BtnOK: TButton;
    Panel3: TPanel;
    Panel1: TPanel;
    LblVersion: TLabel;
    LblModel: TLabel;
    MoContact: TMemo;
    Memo1: TMemo;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo2: TMemo;
    Memo3: TMemo;
    Label4: TLabel;
    Label5: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FmAbout: TFmAbout;

implementation

{$R *.lfm}

{ TFmAbout }


end.
