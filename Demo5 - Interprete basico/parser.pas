{}
unit Parser;
{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, LCLType, Dialogs, lclProc, Graphics, SynEditHighlighter,
  SynFacilBasic,
  XpresTypes, XPresParser, Interprete;

type

 { TCompiler }

  TCompiler = class(TInterprete)
  private
    procedure CompilarArc;
  public
    procedure Compilar(NombArc: string; LinArc: Tstrings);
    constructor Create; override;
    destructor Destroy; override;
  end;

//procedure Compilar(NombArc: string; LinArc: Tstrings);
var
  cxp : TCompiler;

implementation

//Métodos OVERRIDE
procedure TCompiler.CompilarArc;
//Compila un programa en el contexto actual
begin
//  CompilarAct;
  Perr.Clear;
  if cIn.Eof then begin
    GenError('Se esperaba "begin", "var", "type" o "const".');
    exit;
  end;
  cIn.SkipWhites;
  //empiezan las declaraciones
  Cod_StartData;  //debe definirse en el "Interprete.pas"
  if cIn.tokL = 'var' then begin
    cIn.Next;    //lo toma
    while (cIn.tokL <>'begin') and (cIn.tokL <>'const') and
          (cIn.tokL <>'type') and (cIn.tokL <>'var') do begin
      CompileVarDeclar;
      if pErr.HayError then exit;;
    end;
  end;
  if cIn.tokL = 'begin' then begin
    Cod_StartProgram;
    cIn.Next;   //coge "begin"
    //codifica el contenido
    SkipWhites;
    while not cIn.Eof and (cIn.tokL<>'end') do begin
      //se espera una expresión o estructura
      GetExpressionE(0);
      if perr.HayError then exit;   //aborta
      //busca delimitador
      SkipWhites;
      if EOExpres then begin //encontró delimitador de expresión
        cIn.Next;   //lo toma
        SkipWhites;  //quita espacios
      end;
    end;
    if Perr.HayError then exit;
    if cIn.Eof then begin
      GenError('Inesperado fin de archivo. Se esperaba "end".');
      exit;       //sale
    end;
    if cIn.tokL <> 'end' then begin  //verifica si termina el programa
      GenError('Se esperaba "end".');
      exit;       //sale
    end;
    cIn.Next;   //coge "end"
  end else begin
    GenError('Se esperaba "begin", "var", "type" o "const".');
    exit;
  end;
  Cod_EndProgram;
end;
procedure TCompiler.Compilar(NombArc: string; LinArc: Tstrings);
//Compila el contenido de un archivo a ensamblador
begin
  //se pone en un "try" para capturar errores y para tener un punto salida de salida
  //único
  try
    ejecProg := true;  //marca bandera

    Perr.IniError;
    ClearVars;       //limpia las variables
    ClearFuncs;      //limpia las funciones
    cIn.ClearAll;     //elimina todos los Contextos de entrada
    ExprLevel := 0;  //inicia
    //compila el archivo abierto

    cIn.NewContextFromFile(NombArc,LinArc);   //Crea nuevo contenido
    if PErr.HayError then exit;
    CompilarArc;     //puede dar error

    cIn.RemoveContext;   //es necesario por dejar limpio
    if PErr.HayError then exit;   //sale
  finally
    ejecProg := false;
    //tareas de finalización
    //como actualizar estado
  end;
end;

constructor TCompiler.Create;
begin
  inherited Create;
  //se puede definir la sintaxis aquí o dejarlo para StartSyntax()
  StartSyntax;   //Debe hacerse solo una vez al inicio
end;

destructor TCompiler.Destroy;
begin
  inherited Destroy;
end;

initialization
  //Es necesario crear solo una instancia del compilador.
  cxp := TCompiler.Create;  //Crea una instancia del compilador

finalization
  cxp.Destroy;
end.

