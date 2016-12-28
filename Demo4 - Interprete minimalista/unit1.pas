unit Unit1;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Parser;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    edXpr: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    cxp : TCompiler;
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  cxp.Compilar('', edXpr.Lines);
  if cxp.HayError then begin
    If cxp.ErrorLine <> 0 Then begin
      edXpr.CaretX:=cxp.ErrorCol;
      edXpr.CaretY:=cxp.ErrorLine;
      edXpr.Invalidate;
    end;
    cxp.ShowError;
    edXpr.SetFocus;
  end else begin
//    frmOut.Show;  //muestra consola
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cxp := TCompiler.Create;  //Crea una instancia del compilador
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  cxp.Destroy;
end;

end.

