{
}
//{$DEFINE mode_inter}  //mode_inter->Modo intérprete  mode_comp->Modo compilador
unit Parser;
{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, LCLType, Dialogs, lclProc, Graphics, SynEditHighlighter,
  SynFacilBasic,
  XpresTypes, XPresParser, FormOut;

type

 { TCompiler }

  TCompiler = class(TCompilerBase)
  private
    tkStruct   : TSynHighlighterAttributes;
    tkExpDelim : TSynHighlighterAttributes;
    tkBlkDelim : TSynHighlighterAttributes;
    tkOthers   : TSynHighlighterAttributes;
    procedure CompilarArc;
    procedure TipDefecString(var Op: TOperand; tokcad: string); override;
  public
    mem   : TStringList;   //Para almacenar el código de salida del compilador
    procedure ShowOperand(const Op: TOperand);
    procedure ShowResult;
    procedure StartSyntax;
    procedure Compilar(NombArc: string; LinArc: Tstrings);
    constructor Create; override;
    destructor Destroy; override;
  end;

//procedure Compilar(NombArc: string; LinArc: Tstrings);
var
  cxp : TCompiler;

implementation

//Funciones de acceso al compilador. Facilitan el acceso de forma resumida.
procedure Code(cod: string);
begin
  cxp.mem.Add(cod);
end;
procedure GenError(msg: string);
begin
  cxp.GenError(msg);
end;
function HayError: boolean;
begin
  Result := cxp.HayError;
end;
{Incluye el código del compilador. Aquí tendrá acceso a todas las variables públicas
 de XPresParser}
{$I GenCod.pas}
//Métodos OVERRIDE
procedure TCompiler.TipDefecString(var Op: TOperand; tokcad: string);
//Devuelve el tipo de cadena encontrado en un token
var
  i: Integer;
  x: TType;
begin
  Op.catTyp := t_string;   //es cadena
  Op.size:=length(tokcad);
  //toma el texto
  Op.valStr := copy(cIn.tok,2, length(cIn.tok)-2);   //quita comillas
  //////////// Verifica si hay tipos string definidos ////////////
  if length(Op.valStr)=1 then begin
    Op.typ := tipChr;
  end else
    Op.typ :=nil;  //no hay otro tipo
end;
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
      GetExpression(0);
      if perr.HayError then exit;   //aborta
      //busca delimitador
      SkipWhites;
      if EOExpres then begin //encontró delimitador de expresión
        cIn.Next;   //lo toma
        SkipWhites;  //quita espacios
      end;
    end;
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
  if ejecProg then begin
    GenError('Ya se está compilando un programa actualmente.');
    exit;  //sale directamente
  end;
  try
    ejecProg := true;  //marca bandera

    Perr.IniError;
    ClearVars;       //limpia las variables
    ClearFuncs;      //limpia las funciones
    mem.Clear;       //limpia salida
    cIn.ClearAll;     //elimina todos los Contextos de entrada
    ExprLevel := 0;  //inicia
    //compila el archivo abierto

  //  con := PosAct;   //Guarda posición y referencia a contenido actual
    cIn.NewContextFromFile(NombArc,LinArc);   //Crea nuevo contenido
    if PErr.HayError then exit;
    CompilarArc;     //puede dar error

    cIn.QuitaContexEnt;   //es necesario por dejar limpio
    if PErr.HayError then exit;   //sale
  //  PosAct := con;   //recupera el contenido actual

  //  PPro.GenArchivo(ArcSal);
    ShowResult;  //muestra el resultado
  finally
    ejecProg := false;
    //tareas de finalización
    //como actualizar estado
  end;
end;

//procedure TCompilerBase.ShowError
procedure TCompiler.ShowOperand(const Op: TOperand);
//muestra un operando por pantalla
var
  tmp: String;
begin
  tmp := 'Result ' + CategName(Op.typ.cat) + '(' + Op.typ.name + ') = ';
  case Op.Typ.cat of
  t_integer: frmOut.puts(tmp + IntToStr(Op.ReadInt));
  t_float :  frmOut.puts(tmp + FloatToStr(Op.ReadFloat));
  t_string:  frmOut.puts(tmp + Op.ReadStr);
  t_boolean: if Op.ReadBool then frmOut.puts(tmp + 'TRUE')
             else frmOut.puts(tmp + 'FALSE');
  end;
end;
procedure TCompiler.ShowResult;
//muestra el resultado de la última exprersión evaluada
begin
{  case res.estOp of
  NO_STORED : frmOut.puts('Resultado no almacen.');
  else  //se supone que está en un estado válido
    ShowOperand(res);
  end;}
end;

constructor TCompiler.Create;
begin
  inherited Create;
  mem := TStringList.Create;  //crea lista para almacenar ensamblador

  ///////////define la sintaxis del compilador
  //crea y guarda referencia a los atributos
  tkEol      := xLex.tkEol;
  tkIdentif  := xLex.tkIdentif;
  tkKeyword  := xLex.tkKeyword;
  tkNumber   := xLex.tkNumber;
  tkString   := xLex.tkString;
  //personalizados
  tkOperator := xLex.NewTokType('Operador'); //personalizado
  tkBoolean  := xLex.NewTokType('Boolean');  //personalizado
  tkSysFunct := xLex.NewTokType('SysFunct'); //funciones del sistema
  tkExpDelim := xLex.NewTokType('ExpDelim');//delimitador de expresión ";"
  tkBlkDelim := xLex.NewTokType('BlkDelim'); //delimitador de bloque
  tkType     := xLex.NewTokType('Types');    //personalizado
  tkStruct   := xLex.NewTokType('Struct');   //personalizado
  tkOthers   := xLex.NewTokType('Others');   //personalizado
  //Configura atributos
  tkKeyword.Style := [fsBold];     //en negrita
  tkBlkDelim.Foreground:=clGreen;
  tkBlkDelim.Style := [fsBold];     //en negrita
  tkStruct.Foreground:=clGreen;
  tkStruct.Style := [fsBold];     //en negrita
  //inicia la configuración
  xLex.ClearMethodTables;           //limpìa tabla de métodos
  xLex.ClearSpecials;               //para empezar a definir tokens
  //crea tokens por contenido
  xLex.DefTokIdentif('[$A-Za-z_]', '[A-Za-z0-9_]*');
  xLex.DefTokContent('[0-9]', '[0-9.]*', tkNumber);
  //define palabras claves
  xLex.AddIdentSpecList('THEN var type', tkKeyword);
  xLex.AddIdentSpecList('program public private method const', tkKeyword);
  xLex.AddIdentSpecList('class create destroy sub do begin', tkKeyword);
  xLex.AddIdentSpecList('END ELSE ELSIF', tkBlkDelim);
  xLex.AddIdentSpecList('true false', tkBoolean);
  xLex.AddIdentSpecList('IF FOR', tkStruct);
  xLex.AddIdentSpecList('and or xor not', tkOperator);
  xLex.AddIdentSpecList('int float char string bool', tkType);
  //símbolos especiales
  xLex.AddSymbSpec('+',  tkOperator);
  xLex.AddSymbSpec('-',  tkOperator);
  xLex.AddSymbSpec('*',  tkOperator);
  xLex.AddSymbSpec('/',  tkOperator);
  xLex.AddSymbSpec('\',  tkOperator);
  xLex.AddSymbSpec('%',  tkOperator);
  xLex.AddSymbSpec('**', tkOperator);
  xLex.AddSymbSpec('=',  tkOperator);
  xLex.AddSymbSpec('>',  tkOperator);
  xLex.AddSymbSpec('>=', tkOperator);
  xLex.AddSymbSpec('<;', tkOperator);
  xLex.AddSymbSpec('<=', tkOperator);
  xLex.AddSymbSpec('<>', tkOperator);
  xLex.AddSymbSpec('<=>',tkOperator);
  xLex.AddSymbSpec(':=', tkOperator);
  xLex.AddSymbSpec(';', tkExpDelim);
  xLex.AddSymbSpec('(',  tkOthers);
  xLex.AddSymbSpec(')',  tkOthers);
  xLex.AddSymbSpec(':',  tkOthers);
  xLex.AddSymbSpec(',',  tkOthers);
  //crea tokens delimitados
  xLex.DefTokDelim('''','''', tkString);
  xLex.DefTokDelim('"','"', tkString);
  xLex.DefTokDelim('//','', xLex.tkComment);
  xLex.DefTokDelim('/\*','\*/', xLex.tkComment, tdMulLin);
  //define bloques de sintaxis
  xLex.AddBlock('{','}');
  xLex.Rebuild;   //es necesario para terminar la definición

  StartSyntax;   //Debe hacerse solo una vez al inicio
end;

destructor TCompiler.Destroy;
begin
  mem.Free;  //libera
  inherited Destroy;
end;

initialization
  //Es necesario crear solo una instancia del compilador.
  cxp := TCompiler.Create;  //Crea una instancia del compilador

finalization
  cxp.Destroy;
end.

