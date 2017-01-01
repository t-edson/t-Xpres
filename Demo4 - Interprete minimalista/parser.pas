{Ejemplo minimalista de como se puede implementar un intérprete, con el mínimo código.
Este intérprete, solo define el tipo de dato "String", y una sola función "Eureka", que
muestra un mensaje en Pantalla.
En esta unidad se incluye al Intérprete y al Parser.
Para simplificar la unidad se han omitido rutinas importantes, como la evalución de
expresiones. Es por eso que muchas rutinas las implementa la librería por defecto.}
unit Parser;
{$mode objfpc}{$H+}
interface
uses
  Classes, XpresTypes, Dialogs, XPresParser, XpresElements;

type

 { TCompiler }

  TCompiler = class(TCompilerBase)
  private
    tipStr : TxType;
    procedure fun_puts(fun: TxpFun);
  public
    procedure Compilar(NombArc: string; LinArc: Tstrings);
    constructor Create; override;
  end;


implementation

procedure TCompiler.Compilar(NombArc: string; LinArc: Tstrings);
//Compila el contenido de un archivo a ensamblador
begin
  Perr.IniError;
  cIn.ClearAll;     //elimina todos los Contextos de entrada
  ExprLevel := 0;  //inicia
  //compila el archivo abierto
  cIn.NewContextFromFile(NombArc,LinArc);   //Crea nuevo contenido
  SkipWhites;
  while not cIn.Eof do begin
    //se espera una expresión o estructura
    GetExpression(0);
    if perr.HayError then exit;   //aborta
    //busca delimitador
    SkipWhites;
    if EOExpres then begin //encontró delimitador de expresión
      cIn.Next;   //lo toma
      SkipWhites;  //quita espacios
    end;
  end;
  if Perr.HayError then exit;
  cIn.RemoveContext;   //es necesario para dejar limpio
end;

constructor TCompiler.Create;
begin
  inherited Create;
  ClearTypes;
  //Se crea un único tipo.
  tipStr:=CreateType('string',t_string,-1);
  CreateSysFunction('eureka', tipStr, @fun_puts);
end;

procedure TCompiler.fun_puts(fun :TxpFun);
begin
  ShowMessage('Eureka');
end;

end.
