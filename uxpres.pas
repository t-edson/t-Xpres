unit uXpres;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, fgl, SynHighlighterFacil, Forms, LCLType,
  SynEditHighlighter,  //Para mostrar mensajes con Application.MessageBox()
  XpresBas, Globales;

type
  //categorías básicas de tipo de datos
  TtipDato=(
    t_integer,  //es entero
    t_float,    //es de coma flotante
    t_string,   //cadena
    t_boolean,  //booleano
    t_enum      //enumerado
  );

  //Operador
  TOperat = record
    txt: string;    //cadena del operador '+', '-', '++', ...
    jer: byte;      //jerarquía
    nom: string;    //nombre de la operación (suma, resta)
    idx: integer;   //ubicación dentro de un arreglo
  end;

  //Categoría de Operando
  CatOperan = (
    coConst,     //mono-operando constante
    coConstExp,  //expresión constante
    coVariable,  //variable
    coExpres     //expresión
  );

  TType = class;

  { TOperand }
  //Operando
  TOperand = object
    nom: string;
    simple: boolean;  //indica si el tipo de dato es simple o compuesto
  	CatTyp: tTipDato; //Categoría de Tipo de dato
    sType: string;    //Tipo de dato (en cadena)
    typ   : TType;    //referencia al tipo de dato
    size:  integer;   //tamaño del operando en bytes
    cat: CatOperan;   //Categoría de operando
//    cons: boolean;     //indica si es constante o variable
  	dir_ini: integer; //Direción física de inicio (necesario para compilar)
    txt: string;     //Texto del operador o expresión
    //valores del operando
    valFloat: extended; //VAlor en caso de que sea un flotante
    valInt: Int64;     //valor en caso de que sea un entero
    valUInt: Int64;    //valor en caso de que sea un entero sin signo
    function NombreTipo: string;  //nombre de tipo
    procedure Load;   //carga el operador en registro o pila
  end;

  TProcLoadOperand = procedure(var Op: TOperand) of object;
  TProcExecOperat = procedure(var Op1: TOperand; opr: TOperat; Op2: TOperand) of object;

  //Tipo operación
  TOperation = record
    Operat: TOperat;    //operador
    OperadType: string; //tipo de operador
    procOperat: TProcExecOperat;  //Procesamiento de la operación
    //Si "procOperat" es NIL, se deben usar estos campos para procesar la oepración
    CodForConst: string;  //código para ejecutar con Operando constante
    CodForVar: string;    //código para ejecutar con Operando variable
    CodForExpr: string;    //código para ejecutar con Operando expresión
  end;

  //"Tipos de datos"
  { TType }

  TType = class
    name : string;      //nombre del tipo ("int8", "int16", ...)
    cat  : TtipDato;    //categoría del tipo (numérico. cadena, etc)
    size : smallint;    //tamaño en bytes del tipo
    idx  : smallint;    //ubicación dentro de la matriz de tipos
    amb  : TFaSynBlock; //ámbito de validez del tipo
    procLoad: TProcLoadOperand;    //Procesamiento de carga
    codLoad: string;   //código de carga de operando. Se usa si "procLoad" es NIL.
    Operators: array of TOperat;      //operadores soportados
    Operations: array of TOperation;  //operaciones soportadas. Debería haber tantos como
                                      //Num. Operadores * Num.Tipos compatibles.
    procedure DefineLoadOperand(codLoad0: string);  //Define carga de un operando
//    procedure DefineLoadOperand(procLoad0: TProcExecOperat);  //Define carga de un operando
    function CreateOperator(txt0: string; jer0: byte; name0: string): integer; //Crea operador
    function CreateOperation(idOperator: integer; OperadType: string;
                             codCons, codVar, codExp: string): integer;  //Crea operación
    function DefinedOperator(Opr: string): boolean;  //indica si el operador está definido
    function JerarOperator(Opr: string): integer;  //devuelve la jerarquía del operador
    constructor Create;
    destructor Destroy; override;
  end;


  //Lista de tipos
  TTypes = specialize TFPGObjectList<TType>; //lista de bloques

  { TCompiler }
  {Define un objeto compilador}
  TCompiler = class   //objeto compilador
    procedure int8OperationsprocSum(var Op1: TOperand; opr: TOperat;
      Op2: TOperand);
    procedure int8procLoad(var Op: TOperand);
  private
    lex : TSynFacilSyn;
//    tipo: TtipDato;   //indica el tipo de dato que se esta procesando
    types: TTypes;   //lista de tipos
//    oper: string;      //indica el operador actual
    //tipos de tokens
    tkIdentif : TSynHighlighterAttributes;
    tkKeyword : TSynHighlighterAttributes;
    tkNumber  : TSynHighlighterAttributes;
    tkOperator: TSynHighlighterAttributes;
    tkDelimiter: TSynHighlighterAttributes;
    tkType    : TSynHighlighterAttributes;
    tkOthers  : TSynHighlighterAttributes;
    procedure Code(cod: string);  //Genera código de salida
    function CapturaDelim: boolean;
    procedure CompilarArc(NomArc: String; LinArc: Tstrings);
    procedure CompileVarDeclar;
    function Evaluar(Op1: TOperand; opr: string; Op2: TOperand): TOperand;
    function GAEE(jerar: Integer): TOperand;
    Function LeePosContAct: TPosCont;
    procedure FijPosContAct(pc:TPosCont);
    function TokAct: string;
    procedure Cod_CargarOperNumerico(const Op: TOperand);
    procedure Cod_DecremOperanNumerico(const Opdo: TOperand);
    procedure Cod_DefinirVariable(varName, varType: string);
    procedure Cod_EndProgram;
    procedure Cod_IncremOperanNumerico(const Opdo: TOperand);
    procedure Cod_CargarConstNumerica(const Op: TOperand);
    procedure Cod_SalvarOperanNumerico(const Opdo: TOperand);
    procedure Cod_StartProgram;
    function Cod_SumaNumerica(const Op1: TOperand; const Op2: TOperand): TOperand;
  public
    PErr : TPError;   //Objeto de Error
    mem: TStringList;   //tetso de salida del compilador
    constructor Create;
    destructor Destroy; override;
    procedure Iniciar(lex0: TSynFacilSyn);   //Prepara la secuencia de preprocesamiento
    //Manejo de tipos
    procedure ClearTypes;
    function CreateType(nom0: string; cat0: TtipDato; siz0: smallint): TType;
    //rutinas de entrada
    procedure NuevoContexEntTxt(txt: string; arc0: String);
    procedure NuevoContexEntArc(arc0: String);
    procedure NuevoContexEntArc(arc0: String;   //versión que recibe TStringList
                            lins: Tstrings);
    procedure QuitaContexEnt;   //quita contexto de entrada actual

    function cogOperador: string;
    function CogOperando: TOperand;

    property PosAct: TPosCont read LeePosContAct write FijPosContAct;
    procedure Compilar(NombArc: string; LinArc: Tstrings; lex0: TSynFacilSyn);
    //Procesamiento de errores
    function HayError: boolean;  //indica si hay error actual
    function ArcError: string;
    procedure GenTemplCompiler;
  private
    ConsE: TListaCont;      //Lista de contextos de entrada
    //Variables del Contexto actual
    cEnt : TContexto;   //referencia al contexto de entrada actual
  end;


implementation

{ TType }

procedure TType.DefineLoadOperand(codLoad0: string);
begin

end;
{Permite crear un nuevo ooperador soportado por este tipo de datos. Si hubo error,
devuelve -1. En caso normal devuelve un índice al Operador}
function TType.CreateOperator(txt0: string; jer0: byte; name0: string): integer;
var
  n: Integer;
  r: TOperat;  //operador
  i: Integer;
begin
  //verifica nombre
  if DefinedOperator(txt0) then begin
    Result := -1;  //indica que hubo error
    exit;
  end;
  //agrega
  r.txt:=txt0;
  r.jer:=jer0;
  r.nom:=name0;
  n := high(Operators)+1;
  setlength(Operators,n+1);
  Operators[n] := r;
  Result := n;
end;
function TType.CreateOperation(idOperator: integer; OperadType: string;
  codCons, codVar, codExp: string): integer;
var
  r: TOperation;
  n: Integer;
begin
  //agrega
  r.Operat := Operators[idOperator];
  r.OperadType:=OperadType;
  r.CodForConst:=codCons;
  r.CodForVar:=codVar;
  r.CodForExpr:=codExp;

  n := high(Operations)+1;
  setlength(Operations,n+1);
  Operations[n] := r;
  Result := n;
end;
function TType.DefinedOperator(Opr: string): boolean;
var
  i: Integer;
begin
  Result := false;
  for i:=0 to high(Operators) do begin
    if Operators[i].txt = Opr then begin
      exit(true); //está definido
    end;
  end;
end;
function TType.JerarOperator(Opr: string): integer;
var
  i: Integer;
begin
  Result := -1;   //indica que no se encuentra
  for i:=0 to high(Operators) do begin
    if Operators[i].txt = Opr then begin
      exit(Operators[i].jer); //está definido
    end;
  end;
end;

constructor TType.Create;
begin
  setlength(Operators,0);  //inicia
  setlength(Operations,0); //inicia
end;
destructor TType.Destroy;
begin
  inherited Destroy;
end;

{ TOperand }

function TOperand.NombreTipo: string;
begin
   case CatTyp of
   t_integer: Result := 'Numérico';
   t_float: Result := 'Flotante';
   t_string: Result := 'Numérico';
   t_boolean: Result := 'Booelano';
   t_enum: Result := 'Enumerado';
   else Result := 'Desconocido';
   end;
end;
procedure TOperand.Load;
begin
  typ.procLoad(self);
end;

{ TCompiler }

constructor TCompiler.Create;
begin
  mem := TStringList.Create;
  PErr.IniError;   //inicia motor de errores
  //Crea lista de Contextos
  ConsE := TListaCont.Create(true);  //crea contenedor de Contextos, con control de objetos.
  cEnt := nil;
  //Inicia lista de tipos
  types := TTypes.Create(true);
end;
destructor TCompiler.Destroy;
begin
  types.Free;
  //Limpia lista de Contextos
  ConsE.Free;
  mem.Free;  //libera
  inherited Destroy;
end;
procedure TCompiler.Iniciar(lex0: TSynFacilSyn);
var
  int8: TType;
  iOpe: Integer;
  iSum: Integer;
begin
  lex := lex0;    //asigna lexer
  //guarda referencia a los atributos
  tkIdentif := lex.tkIdentif;
  tkKeyword := lex.tkKeyword;
  tkNumber := lex.tkNumber;
  tkOperator := lex.GetAttribByName('Operator');  //se debe haber creado
  tkDelimiter:= lex.GetAttribByName('Delimiter'); //se debe haber creado
  tkType     := lex.GetAttribByName('Types');    //se debe haber creado
  tkOthers   := lex.GetAttribByName('Others');   //se debe haber creado

  ConsE.Clear;          //elimina todos los Contextos de entrada

  //Crea tipos y operaciones
  ClearTypes;
  int8:=CreateType('int8',t_integer,1);
  int8.procLoad:=@int8procLoad;
  iOpe:=int8.CreateOperator('+',5,'suma');
  iSum := int8.CreateOperation(iOpe,'int8','','','');
  int8.Operations[iSum].procOperat:=@int8OperationsprocSum;

//  int8:=CreateType('int16',t_integer,1);
end;
function TCompiler.TokAct: string; inline;
//Devuelve el token actual, ignorando la caja.
begin
  Result:=lowercase(cEnt.tok);
end;
function TCompiler.CapturaDelim: boolean;
//Verifica si sigue un delimitador de expresión. Si encuentra devuelve false.
begin
  cEnt.CapBlancos;
  if cEnt.tokType=tkDelimiter then begin //encontró
    cEnt.Next;   //pasa al siguiente
    exit(true);
  end else if tokAct = 'end' then begin   //es un error
    //detect apero no lo toma
    exit(true);  //sale con error
  end else begin   //es un error
    Perr.GenError('Se esperaba ";"', PosAct);
    exit(false);  //sale con error
  end;
end;
function TCompiler.LeePosContAct: TPosCont;
//Devuelve Contexto actual y su posición
begin
    Result.fCon := cEnt;
    if cEnt = nil then begin
      //aún no hay Contexto definido
      Result.fil  := 1;
      Result.col  := 1;
      Result.arc  := '';
      Result.nlin := 0;
    end else begin
      Result.fil  := cEnt.fil;
      Result.col  := cEnt.col;
      Result.arc  := cEnt.arc;
      Result.nlin := cEnt.nlin;
    end;
end;
procedure TCompiler.FijPosContAct(pc: TPosCont);
//Fija Contexto actual y su posición
begin
    cEnt := pc.fCon;
    if cEnt = nil then begin
      //no tiene un Contexto actual
//      filAct := 1;
//      colAct := 1;
//      cEnt.arc := '';
//      nlin := 0;
    end else begin
      cEnt.SetPosXY(pc.col, pc.fil);  //posiciona al contexto
      cEnt.arc := pc.arc;
      cEnt.nlin := pc.nlin;
    end;
end;
//Manejo de tipos
procedure TCompiler.ClearTypes;  //Limpia los tipos
begin
  types.Clear;
end;
function TCompiler.CreateType(nom0: string; cat0: TtipDato; siz0: smallint): TType;
//Crea una nueva definición de tipo en el compilador. Devuelve referecnia al tipo recien creado
var r: TType;
  i: Integer;
begin
  //verifica nombre
  for i:=0 to types.Count-1 do begin
    if types[i].name = nom0 then begin
      PErr.GenError('Identificador de tipo duplicado.',PosAct);
      exit;
    end;
  end;
  //configura nuevo tipo
  r := TType.Create;
  r.name:=nom0;
  r.cat:=cat0;
  r.size:=siz0;
  r.idx:=types.Count;  //toma ubicación
//  r.amb:=;  //debe leer el bloque actual
  //agrega
  types.Add(r);
  Result:=r;   //devuelve índice al tipo
end;

procedure TCompiler.NuevoContexEntTxt(txt: string; arc0: String);
//Crea un Contexto a partir de una cadena.
//Fija el Contexto Actual "cEnt" como el Contexto creado.
begin
  cEnt := TContexto.Create; //crea Contexto
  cEnt.DefSyn(lex);     //asigna lexer
  ConsE.Add(cEnt);      //Registra Contexto
  cEnt.FijCad(txt);     //inicia con texto
  cEnt.arc := arc0;     {Se guarda el nombre del archivo actual, solo para poder procesar
                           las funciones $NOM_ACTUAL y $DIR_ACTUAL}
  cEnt.CurPosIni;       //posiciona al inicio
end;
procedure TCompiler.NuevoContexEntArc(arc0: String);
//Crea un Contexto a partir de un archivo.
//Fija el Contexto Actual "cEnt" como el Contexto creado.
begin
  If not FileExists(arc0)  Then  begin  //ve si existe
    PErr.GenError( 1, 'No se encuentra archivo: ' + arc0);
    Exit;
  end;
  cEnt := TContexto.Create; //crea Contexto
  cEnt.DefSyn(lex);     //asigna lexer
  ConsE.Add(cEnt);   //Registra Contexto
  cEnt.FijArc(arc0);     //inicia con archivo
  cEnt.CurPosIni;       //posiciona al inicio
end;
procedure TCompiler.NuevoContexEntArc(arc0: String; lins: Tstrings);
//Crea un Contexto a partir de un archivo.
//Fija el Contexto Actual "cEnt" como el Contexto creado.
begin
  cEnt := TContexto.Create; //crea Contexto
  cEnt.DefSyn(lex);     //asigna lexer
  ConsE.Add(cEnt);   //Registra Contexto
  cEnt.FijArc(arc0, lins);     //inicia con archivo
  cEnt.CurPosIni;       //posiciona al inicio
end;
procedure TCompiler.QuitaContexEnt;
//Elimina el contexto de entrada actual. Deja apuntando al anterior en la misma posición.
begin
  if ConsE.Count = 0 then exit;  //no se puede quitar más
  ConsE.Delete(ConsE.Count-1);
  if ConsE.Count = 0 then
    cEnt := nil
  else  //apunta al último
    CEnt := ConsE[ConsE.Count-1];
end;

function TCompiler.cogOperador: string;
//Coge el operador actual y pasa al siguiente token.
//Si no encuentra un operador, devuelve el operador Op_ninguno.
{
OPERADOR     JERARQUÍA	DESCRIPCIÓN
>>,<<,>+,>-	        1	Redirección
=	                2	Asignación
&&, ||,|! ,!	    3	Operadores Lógicos
==,<>,>,>=,<,<=,~   4	Operadores de comparación
+,-,|	            5	Suma, resta, concatenación
*,/,\, %,	        6	Multiplicación, División, Residuo
=>,=<	            7	Función Mayor, Función Menor
^,++,--,+=,-=,*=,/=	8	Potenciación, auto incremento, etc
}
begin
  cEnt.CapBlancos;
  if cEnt.tokType <> tkOperator then begin
    //no sigue un operador
    Result:='';
    exit;
  end;
  //Hay operador. Al menos así lo identifica el lexer.
  Result:=cEnt.tok;  //lee el texto
  cEnt.Next;  //coge el operador
end;
function TCompiler.CogOperando: TOperand;
  procedure TipDefecNumber(var Op: TOperand; toknum: string);
  //Devuelve el tipo de número entero o fltante más sencillo que le corresponde a un token
  var
    n: int64;   //para almacenar a los enteros
    f: extended;  //para almacenar a reales
    i: Integer;
  begin
    if pos('.',toknum) <> 0 then begin  //es flotante
      f := StrToFloat(toknum);
      Op.CatTyp := t_float;   //es flotante
    end else begin     //es entero
      //verificación de longitud de entero
      if length(toknum)>=19 then begin  //solo aquí puede haber problemas
        if toknum[1] = '-' then begin  //es negativo
          if length(toknum)>20 then begin
            pErr.GenError('Número muy grande. No se puede procesar. ', posAct);
            exit
          end else if (length(toknum) = 20) and  (toknum > '-9223372036854775808') then begin
            pErr.GenError('Número muy grande. No se puede procesar. ', posAct);
            exit
          end;
        end else begin  //es positivo
          if length(toknum)>19 then begin
            pErr.GenError('Número muy grande. No se puede procesar. ', posAct);
            exit
          end else if (length(toknum) = 19) and  (toknum > '9223372036854775807') then begin
            pErr.GenError('Número muy grande. No se puede procesar. ', posAct);
            exit
          end;
        end;
      end;
      //conversión. aquí ya no debe haber posibilidad de error
      n := StrToInt64(toknum);
      if (n>= -128) and  (n<=127) then
          Op.size := 1
      else if (n>= -32768) and (n<=32767) then
          Op.size := 2
      else if (n>= -2147483648) and (n<=2147483647) then
          Op.size := 4
      else if (n>= -9223372036854775808) and (n<=9223372036854775807) then
          Op.size := 8;
      Op.valInt := n;   //copia valor entero
      //busca si hay tipo numérico que soporte esta constante
      Op.typ:=nil;
      for i:=0 to types.Count-1 do begin
        { TODO : Se debería tener una lista adicional  TIntegerTypes, para acelerar la
        búsqueda}
        if (types[i].cat = t_integer) and (types[i].size=Op.size) then
          Op.typ:=types[i];  //encontró
      end;
    end;
  end;
//Parte de la funcion GAEE que genera codigo para leer un operando.
begin
  PErr.Limpiar;
  cEnt.CapBlancos;
  if cEnt.tokType = tkNumber then begin  //constantes numéricas
      Result.simple:=true;       //es simple
      Result.cat:=coConst;       //constante es Mono Operando
//      Result.cons:=true;       //es constante
      Result.txt:= cEnt.tok;     //toma el texto
      Result.CatTyp:= t_integer;  //es numérico
      TipDefecNumber(Result, cEnt.tok); //encuentra tipo de número, tamaño y valor
      if pErr.HayError then exit;  //verifica
      if Result.typ = nil then begin
        PErr.GenError('No hay tipo definido para albergar a esta constante numérica',PosAct);
        exit;
      end;
      cEnt.Next;    //Pasa al siguiente
    end
{    tkString: begin  //constantes de cadena

    end;
    tkSymbol: begin  //los únicos símbolos válidos son +,-, que son parte de un número

    end;
    tkIdentif: begin  //este es un caso común

    end;}
  else begin
    //No se reconoce el operador
    PErr.GenError('Se esperaba operando',PosAct);
  end;
end;
function TCompiler.GAEE(jerar: Integer): TOperand;
//Generador de Algoritmos de Evaluacion de expresiones.
//Esta es la función más importante del compilador
var
  Op1, Op2: TOperand;   //Operandos
  opr, opr2: string;    //Operadores
  jer, jer2: byte;      //jerarquías
  cadena: string;
  pos1,pos2: TPosCont;
  jerOpr: Integer;
begin
  Code('  ;expresión');
  Op1.CatTyp:=t_integer;    //asumir opcion por defecto
  Op2.CatTyp:=t_integer;   //asumir opcion por defecto
  pErr.Limpiar;
  //----------------coger primer operando------------------
  Op1 := CogOperando; if pErr.HayError then exit;
  //verifica si termina la expresion
  opr := cogOperador; if pErr.HayError then exit;
  if opr = '' then begin  //no sigue operador
    //Expresión de un solo operando. Lo carga por si se necesita
    Op1.Load;   //carga el operador para cumplir
    Result:=Op1;
    exit;  //termina ejecucion
  end;
  //------- sigue un operador ---------
  //verifica si el operador aplica al operando
  jer := Op1.typ.JerarOperator(opr);
  if jer=-1 then begin
    PErr.GenError('No está definido el operador: '+ opr + ' para tipo: '+Op1.typ.name, PosAct);
    exit;
  end;
  //¿Delimitada por jerarquía?
  If jer <= jerar Then begin  //es menor que la que sigue, expres.
    Result := Op1;  //solo devuelve el único operando que leyó
    Exit;
  End;

  while opr<>'' do begin
    pos1 := PosAct;    //Guarda por si lo necesita
    //--------------------coger operador ---------------------------
	//operadores unitarios ++ y -- (un solo operando).
    //Se evaluan como si fueran una mini-expresión o función
{	if opr.id = op_incremento then begin
      case Op1.CatTyp of
        t_integer: Cod_IncremOperanNumerico(Op1);
      else
        PErr.GenError('Operador ++ no es soportado en este tipo de dato.',PosAct);
        exit;
      end;
      opr := cogOperador; if pErr.HayError then exit;
      if opr.id = Op_ninguno then begin  //no sigue operador
        Result:=Op1; exit;  //termina ejecucion
      end;
    end else if opr.id = op_decremento then begin
      case Op1.CatTyp of
        t_integer: Cod_DecremOperanNumerico(Op1);
      else
        PErr.GenError('Operador -- no es soportado en este tipo de dato.',PosAct);
        exit;
      end;
      opr := cogOperador; if pErr.HayError then exit;
      if opr.id = Op_ninguno then begin  //no sigue operador
        Result:=Op1; exit;  //termina ejecucion
      end;
    end;}
    //--------------------prepara para evaluar---------------------
//    Cod_SalvarOperanNumerico(Op1);
    //--------------------coger segundo operando--------------------
	Op2 := CogOperando; if pErr.HayError then exit;
    pos2 := PosAct;    //Guarda por si lo necesita
    opr2 := cogOperador;   //Toma el siguiente operador
    If opr2 <> '' Then begin  //Hay otro operador

    end;
    Op1 := Evaluar(Op1, opr, Op2);    //evalua resultado
    if PErr.HayError then exit;
    //prepara siguiente operación
    opr := opr2;
    jerOpr := jer;    //actualiza operador anterior
  end;  //hata que ya no siga un operador

end;

procedure TCompiler.int8OperationsprocSum(var Op1: TOperand; opr: TOperat; Op2: TOperand);
begin
  //carga el primer operando
  if Op1.cat = coConst then begin
    Code('  mov dl,'+ Op1.txt);  //8 bits en dl
  end else if Op1.cat = coVariable then begin
    Code('  mov dl,'+ Op1.txt);  //8 bits en dl
  end else begin  //expresión
    //ya debe estar cargada
  end;
  //opera
  if Op2.cat = coConst then begin
    Code('  add dl,'+ Op2.txt);  //8 bits en dl
  end else if Op2.cat = coVariable then begin
    Code('  add dl,'+ Op2.txt);  //8 bits en dl
  end else begin  //expresión
    //ya debe estar cargada
  end;

end;

procedure TCompiler.int8procLoad(var Op: TOperand);
begin
  Code('  mov dl,'+ Op.txt);  //8 bits en dl
end;

procedure TCompiler.Code(cod: string);
begin
  mem.Add(cod);
end;
procedure TCompiler.CompileVarDeclar;
//Compila la declaración de variables.
var
  varType: String;
  varName: String;
  varNames: array of string;  //nombre de variables
  n: Integer;
  tmp: String;
begin
  setlength(varNames,0);  //inicia arreglo
  //procesa variables a,b,c : int8;
  repeat
    cEnt.CapBlancos;
    //ahora debe haber un identificador de variable
    if cEnt.tokType <> tkIdentif then begin
      Perr.GenError('Se esperaba identificador de variable.', PosAct);
      exit;
    end;
    //hay un identificador
    varName := cEnt.tok;
    cEnt.Next;  //lo toma
    cEnt.CapBlancos;
    //sgrega nombre de variable
    n := high(varNames)+1;
    setlength(varNames,n+1);  //hace espacio
    varNames[n] := varName;  //agrega nombre
    if cEnt.tok <> ',' then break; //sale
    cEnt.Next;  //toma la coma
  until false;
  //usualmente debería seguir ":"
  if cEnt.tok = ':' then begin
    //debe venir el tipo de la variable
    cEnt.Next;  //lo toma
    cEnt.CapBlancos;
    if (cEnt.tokType <> tkType) then begin
      Perr.GenError('Se esperaba identificador de tipo.', PosAct);
      exit;
    end;
    varType := cEnt.tok;   //lee tipo
    cEnt.Next;
    //reserva espacio para las variables
    for tmp in varNames do
      Cod_DefinirVariable(tmp, lowerCase(varType));
  end else begin
    Perr.GenError('Se esperaba ":" o ",".', PosAct);
    exit;
  end;
  if not CapturaDelim then exit;
  cEnt.CapBlancos;
end;
procedure TCompiler.CompilarArc(NomArc: String; LinArc: Tstrings);
var
  con: TPosCont;
  lin: String;
  o: TOperand;
begin
  PErr.IniError;
  con := PosAct;   //Guarda posición y contenido actual
  NuevoContexEntArc(Trim(NomArc),LinArc);   //Crea nuevo contenido
  If PErr.HayError Then begin
//      PErr.MosError;
      Exit;
  end;
//  CompilarAct;
  Perr.Limpiar;
  if tokAct = 'program' then begin
    cEnt.Next;  //pasa al nombre
    cEnt.CapBlancos;
    if cEnt.Eof then begin
      Perr.GenError('Se esperaba nombre de programa.', PosAct);
      exit;
    end;
    cEnt.Next;  //Toma el nombre y pasa al siguiente
  end else begin
    Perr.GenError('Se esperaba: "program ... "', PosAct);
    exit;
  end;
  if not CapturaDelim then exit;
  if cEnt.Eof then begin
    Perr.GenError('Se esperaba "begin", "var", "type" o "const".', PosAct);
    exit;
  end;
  cEnt.CapBlancos;
  //empiezan las declaraciones
  Code('.MODEL TINY');
  Code('.DATA');
  Code('  HelloMesg  db     ''Hello,World'',10,13,''$''');
  if tokAct = 'var' then begin
    cEnt.Next;    //lo toma
    while (tokAct <>'begin') and (tokAct <>'const') and
          (tokAct <>'type') and (tokAct <>'var') do begin
      CompileVarDeclar;
      if pErr.HayError then exit;;
    end;
  end;
  if tokAct = 'begin' then begin
    Code('.CODE');   //inicia la sección de código
    Cod_StartProgram;

    cEnt.Next;   //coge "begin"
    //codifica el contenido
    while not cEnt.Eof do begin
  //      Application.MessageBox(PChar(cEnt.tok),'',0);
      //se espera una expresión
      GAEE(0);
      if perr.HayError then exit;   //aborta
      //se espera delimitador
      if cEnt.Eof then break;  //sale
      if not CapturaDelim then break;
      if tokAct = 'end' then begin  //verifica si termina el programa
        cEnt.Next;   //lo toma
        break;       //sale
      end;
    end;
  end else begin
    Perr.GenError('Se esperaba "begin", "var", "type" o "const".', PosAct);
    exit;
  end;
  PosAct := con;   //recupera el contenido actual
  Cod_EndProgram;
end;
procedure TCompiler.Compilar(NombArc: string; LinArc: Tstrings; lex0 : TSynFacilSyn);
//Compila el contenido de un archivo a ensamblador
begin
  Perr.IniError;
  Iniciar(lex0);
  mem.Clear;
  //compila
  CompilarArc(NombArc, LinArc);
  if PErr.HayError then exit;
//  PPro.GenArchivo(ArcSal);
end;
function TCompiler.HayError: boolean;
begin
  Result := PErr.HayError;
end;
function TCompiler.ArcError: string;
begin
  Result := PErr.ArcError;
end;
function TCompiler.Evaluar(Op1: TOperand; opr: string; Op2: TOperand): TOperand;
//Se debe ejecutar una operación con dos operandos y un operador
var i   : LongInt;
    expr: TOperand;
begin
    PErr.IniError;
    Evaluar.cat := coExpres;    //ahora es expresión por defecto
    case op1.CatTyp of
    t_integer: {Cod_Operacion(const Op: TOperand)};
    else
      Perr.GenError('No se puede evaluar expresión con tipo: ' + Op1.NombreTipo, PosAct);
      Exit;
    end;
{    Case opr.id of
    Op_suma: begin
        if (Op1.CatTyp = t_integer) and (Op2.CatTyp = t_integer) then begin
          Evaluar := Cod_SumaNumerica(Op1, Op2);
        end else begin
          Perr.GenError('No se puede sumar estos operandos.', PosAct)
        end;
      end;
    Op_resta: begin
        Evaluar.valInt := Op1.valInt - Op2.valInt;
      end;
    Op_producto: begin
        Evaluar.valInt := Op1.valInt * Op2.valInt;
      end;
    Op_division: begin
        If Op2.valInt = 0 Then
            Perr.GenError('No se puede dividir por cero.', PosAct)
        Else begin   //error
            Evaluar.valInt := Op1.valInt div Op2.valInt;
        End;
      end;
    Else
      Perr.GenError('No se reconoce operador: ' + opr.txt, PosAct);
      Exit;
    end;
    //Completa campos de evaluar
    Evaluar.txt := Op1.txt + opr.txt + Op2.txt;   //texto de la expresión
//    Evaluar.uop := opr;   //última operación ejecutada}
End;

procedure TCompiler.GenTemplCompiler;
//Genera una plantilla de código para implementar este mismo compilador
var
  i: Integer;
  txt: TStringList;
  nomProc: String;
begin
  txt := TSTringList.Create;
  txt.Add('{plantilla de código para realizar una implementación del compilador');
  txt.Add('en una arquitectura específica.}');
  txt.Add('');
  txt.Add('//Tipos definidos:');
  for i:=0 to Types.Count-1 do begin
    txt.Add('// '+Types[i].name+' ');
  end;
  txt.Add('');
  txt.Add('//Métodos a implementar:');
  for i:=0 to Types.Count-1 do begin
    nomProc:='Execute_'+Types[i].name;
    txt.Add('procedure '+nomProc+'(var Op1: TOperan; opr: TOperad; Op2: TOperan);');
    txt.Add('begin');
    txt.Add('');
    txt.Add('  Result.tipNum:=t_int8;  //de 8 bits');
    txt.Add('  Result.valInt:=(Op1.valInt + Op2.valInt) and $FF;  //resultado');
    txt.Add('  Cod_CargarOperNumerico(Op1);   //carga operando 1 en dl');

    txt.Add('  case Op2.cat of');
    txt.Add('  coConst: //operación con una constante');
    txt.Add('    if Op2. then begin');
    txt.Add('      AddAsm(''  add dx,''+ Op2.txt);  //deja en dx;');
    txt.Add('    end else begin');
    txt.Add('      PErr.GenError(''No se soporta esta operación.'', posAct);');
    txt.Add('      exit;');
    txt.Add('    end;');
    txt.Add('  coVariable: begin');
    txt.Add('');
    txt.Add('  end;');
    txt.Add('  coExpres: begin  //es expresión. No debería pasar');
    txt.Add('    PErr.GenError(''No se soporta esta operación.'', posAct);');
    txt.Add('    exit;');
    txt.Add('  end;');
    txt.Add('  end;');
    txt.Add('end;');
  end;
  txt.Add('//código de preparación');
  for i:=0 to Types.Count-1 do begin
    nomProc:='Execute_'+Types[i].name;
    txt.Add(' Types['+IntToStr(i)+'].ProcExecOperat:=@'+nomProc+';');
  end;

  txt.SaveToFile('CodeTemplateForCompiler.txt');
  Shell('notepad.exe CodeTemplateForCompiler.txt');
  txt.Free;
end;

////////////////Rutinas de generación de código para el compilador////////////
procedure TCompiler.Cod_DefinirVariable(varName, varType: string);
//Se debe reservar espacio para las variables indicadas. Los tipos siempre
//aparecen en minúscula.
begin
  if varType = 'int8' then begin
    Code('  '+varName+ ' DB ?');
  end else if varType = 'int16' then begin
    Code('  '+varName+ ' DW ?');
  end else begin
    Perr.GenError('No se soporta este tipo de dato.', PosAct);
    exit;
  end;
end;
procedure TCompiler.Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  Code('START:');
  Code('  mov dx,OFFSET HelloMesg  ; offset of the text string');
  Code('  mov ah,9              ; print string function number');
  Code('  int 21h               ; dos call');
end;
procedure TCompiler.Cod_EndProgram;
//Codifica la parte inicial del programa
begin
  Code('  ;system getch()');
  Code('  mov ah,01h');
  Code('  int 21h');
  Code('  ;system terminate');
  Code('  mov ah,4ch');
  Code('  int 21h');
  Code('END START');
  Code('END');
end;
procedure TCompiler.Cod_CargarConstNumerica(const Op: TOperand);
//Carga un operando que es constante numérica, en un regsitro acumulador.
begin
  case Op.sType of
    'int8': Code('  mov dl,'+ Op.txt);  //8 bits en dl
    'int16':Code('  mov dx,'+ Op.txt); //16 bits en dx
  else
    PErr.GenError('Solo se soportan enteros de 8 o 16 bits.', posAct);
    Exit;
  end;
end;
procedure TCompiler.Cod_CargarOperNumerico(const Op: TOperand);
//Carga un operando que es numérico, en un registro acumulador.
begin
  case Op.cat of
  coConst: begin     //conatante numérica
    Cod_CargarConstNumerica(Op);
  end;
  coVariable: begin   //variable

  end;
  coExpres: begin   //expresión
     //no se carga porque ya debe estar cargada
  end;
  else
    PErr.GenError('No se soporta esta categoría de operador.', posAct);
    Exit;
  end;
end;
procedure TCompiler.Cod_SalvarOperanNumerico(const Opdo: TOperand);
//Debe guardar un operando numérico, de modo que pueda usarse para ejecutar alguna
//operación posterior.
begin
  case Opdo.sType of
    'int8': Code('  push dl,'+ cEnt.tok);  //8 bits en dl
    'int16':Code('  push dx,'+ cEnt.tok); //16 bits en dx
  else
    PErr.GenError('Solo se soportan enteros de 8 o 16 bits.', posAct);
    Exit;
  end;
end;
procedure TCompiler.Cod_IncremOperanNumerico(const Opdo: TOperand);
//Se debe incrementar un operador numérico ya cargado.
begin
  case Opdo.sType of
    'int8': Code('  inc dl');  //8 bits en dl
    'int16':Code('  inc dx'); //16 bits en dx
  else
    PErr.GenError('Solo se soportan enteros de 8 o 16 bits.', posAct);
    Exit;
  end;
end;
procedure TCompiler.Cod_DecremOperanNumerico(const Opdo: TOperand);
//Se debe incrementar un operador numérico ya cargado.
begin
  case Opdo.sType of
    'int8': Code('  inc dl');  //8 bits en dl
    'int16':Code('  inc dx'); //16 bits en dx
  else
    PErr.GenError('Solo se soportan enteros de 8 o 16 bits.', posAct);
    Exit;
  end;
end;
function TCompiler.Cod_SumaNumerica(const Op1: TOperand; const Op2: TOperand): TOperand;
//Se debe codificar la suma de dos números. El resultado debe quedar en dl, dx
begin
  Result.CatTyp:=t_integer;   //devuelve un número
  if (Op1.sType = 'int8') and (Op2.sType = 'int8') then begin
    //suma de 8 bits, en 8 bits.
    Result.sType:='int8';  //de 8 bits
    Result.valInt:=(Op1.valInt + Op2.valInt) and $FF;  //resultado
    Cod_CargarOperNumerico(Op1);   //carga operando 1 en dl
    case Op2.cat of
    coConst: begin //es una constante
      Code('  add dl,'+ Op2.txt);  //deja en dl;
    end;
    coVariable: begin

    end;
    coExpres: begin  //es expresión. No debería pasar
      PErr.GenError('No se soporta esta operación.', posAct);
      exit;
    end;
    end;
  end else if (Op1.sType = 'int16') and (Op2.sType = 'int8') then begin
    //suma de 16 bits, en 8 bits.
    Result.sType:='int16';  //de 16 bits
    Result.valInt:=(Op1.valInt + Op2.valInt) and $FFFF;  //resultado
    Cod_CargarOperNumerico(Op1);   //carga operando 1 en dl
    case Op2.cat of
    coConst: begin //es una constante
      Code('  add dx,'+ Op2.txt);  //deja en dx;
    end;
    coVariable: begin

    end;
    coExpres: begin  //es expresión. No debería pasar
      PErr.GenError('No se soporta esta operación.', posAct);
      exit;
    end;
    end;
  end else if (Op1.sType = 'int8') and (Op2.sType = 'int16') then begin
    //suma de 16 bits, en 8 bits.
    Result.sType:='int16';  //de 16 bits
    Result.valInt:=(Op1.valInt + Op2.valInt) and $FFFF;  //resultado
    Cod_CargarOperNumerico(Op2);   //carga operando 1 en dl
    case Op1.cat of
    coConst: begin //es una constante
      Code('  add dx,'+ Op1.txt);  //deja en dx;
    end;
    coVariable: begin

    end;
    coExpres: begin  //es expresión. No debería pasar
      PErr.GenError('No se soporta esta operación.', posAct);
      exit;
    end;
    end;
  end;
end;

end.

