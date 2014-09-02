unit Unit1;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, XpresParser, FormOut;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    edXpr: TSynEdit;
    procedure Button1Click(Sender: TObject);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  frmOut.Show;  //muestra consola
  edXpr.Highlighter := xlex;
  Compilar('', edXpr.Lines);
  if Perr.HayError then begin
    Perr.Show;
    exit;
  end;
end;

end.

