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
    SynEdit1: TSynEdit;
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
  cxp.Compilar('', SynEdit1.Lines);
  if cxp.HayError then begin
    If cxp.ErrorLine <> 0 Then begin
      SynEdit1.CaretX:=cxp.ErrorCol;
      SynEdit1.CaretY:=cxp.ErrorLine;
      SynEdit1.Invalidate;
    end;
    cxp.ShowError;
    SynEdit1.SetFocus;
  end else begin
//    frmOut.Show;  //muestra consola
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cxp := TCompiler.Create;  //Crea una instancia del compilador
  SynEdit1.Highlighter:=cxp.xLex;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  cxp.Destroy;
end;

end.

