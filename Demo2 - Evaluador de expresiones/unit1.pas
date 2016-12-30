unit Unit1;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Forms, Dialogs, XpresParser, XpresTypes;
type

  { TEvaluador }
  TEvaluador = class(TCompilerBase)
  private
    procedure entero_mult_entero;
    procedure entero_suma_entero;
  public
    tipEnt : Ttype;
    constructor Create; override;
  end;

  { TForm1 }
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    cxp : TEvaluador;
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

{ TEvaluador }

procedure TEvaluador.entero_mult_entero;
begin
  Res.valInt := p1.valInt * p2.valInt;
  Res.typ := tipEnt;
end;

procedure TEvaluador.entero_suma_entero;
begin
  Res.valInt := p1.valInt + p2.valInt;
  Res.typ := tipEnt;
end;

constructor TEvaluador.Create;
var
  oprEntSum, oprEntMul: TOperator;
begin
  inherited Create;
  //Configura la sintaxis del lexer
  xLex.ClearMethodTables;
  xLex.DefTokContent('[0-9]', '[0-9]*', tkNumber);
  xLex.AddSymbSpec('+',  tkOperator);   //Importante
  xLex.AddSymbSpec('*',  tkOperator);   //Importante
  xLex.Rebuild;
  //Crea operación de suma entre enteros
  ClearTypes;
  tipEnt := CreateType('t_ent',t_integer,1);
  oprEntSum := tipEnt.CreateOperator('+',1,'suma');
  oprEntMul := tipEnt.CreateOperator('*',2,'suma');
  oprEntSum.CreateOperation(tipEnt, @entero_suma_entero);
  oprEntMul.CreateOperation(tipEnt, @entero_mult_entero);
  //Calcula una suma
  cIn.NewContextFromTxt('2+3*4', '');
  GetExpression(0);
  if HayError then ShowError;
  ShowMessage(IntToStr(res.valInt));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cxp := TEvaluador.Create;  //Crea una instancia del compilador
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  cxp.Destroy;
end;

end.

