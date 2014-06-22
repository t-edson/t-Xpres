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
    t_integer,  //números enteros
    t_uinteger, //enteros sin signo
    t_float,    //es de coma flotante
    t_string,   //cadena
    t_boolean,  //booleano
    t_enum      //enumerado
  );

  //Categoría de Operando
  CatOperan = (
    coConst,     //mono-operando constante
    coConstExp,  //expresión constante
    coVariable,  //variable
    coExpres     //expresión
  );

  TType = class;
  TOperator = class;

  { TOperand }
  //Operando
  TOperand = object
    nom: string;
    simple: boolean;  //indica si el tipo de dato es simple o compuesto
    typ   : TType;    //referencia al tipo de dato
  	catTyp: tTipDato; //Categoría de Tipo de dato
    size:  integer;   //tamaño del operando en bytes
    catOp: CatOperan; //Categoría de operando
    estOp: integer;   //Estado del operando (Usado para la generec. de código)
  	addStr: integer;  //Direción física de inicio (necesario para compilar)
    txt: string;      //Texto del operador o expresión
    //valores del operando
    valFloat: extended; //Valor en caso de que sea un flotante
    valInt: Int64;     //valor en caso de que sea un entero
    valUInt: Int64;    //valor en caso de que sea un entero sin signo
    function NombreTipo: string;  //nombre de tipo
    procedure Load;   //carga el operador en registro o pila
    function FindOperator(const oper: string): TOperator; //devuelve el objeto operador
    function GetOperator: Toperator;
  end;

  TProcDefineVar = procedure(const varName, varInitVal: string);
  TProcLoadOperand = procedure(var Op: TOperand);
  TProcExecOperat = function(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;

  //Tipo operación
  TxOperation = class
    OperadType : TType; //tipo de operador
    proc       : TProcExecOperat;  //Procesamiento de la operación
    //Si "procOperat" es NIL, se deben usar estos campos para procesar la oepración
    CodForConst: string;  //código para ejecutar con Operando constante
    CodForVar  : string;    //código para ejecutar con Operando variable
    CodForExpr : string;    //código para ejecutar con Operando expresión
  end;

  TOperations = specialize TFPGObjectList<TxOperation>; //lista de bloques

  //Operador
  { TOperator }

  TOperator = class
    txt: string;    //cadena del operador '+', '-', '++', ...
    jer: byte;      //jerarquía
    nom: string;    //nombre de la operación (suma, resta)
    idx: integer;   //ubicación dentro de un arreglo
    Operations: TOperations;  //operaciones soportadas. Debería haber tantos como
                              //Num. Operadores * Num.Tipos compatibles.
    function CreateOperation(OperadType: Ttype; proc: TProcExecOperat): TxOperation;  //Crea operación
    function FindOperation(typ0: Ttype): TxOperation;  //Busca una operación para este operador
    constructor Create;
    destructor Destroy; override;
  end;

  TOperators = specialize TFPGObjectList<TOperator>; //lista de bloques

  //"Tipos de datos"
  { TType }

  TType = class
    name : string;      //nombre del tipo ("int8", "int16", ...)
    cat  : TtipDato;    //categoría del tipo (numérico. cadena, etc)
    size : smallint;    //tamaño en bytes del tipo
    idx  : smallint;    //ubicación dentro de la matriz de tipos
    amb  : TFaSynBlock; //ámbito de validez del tipo
    procDefine: TProcDefineVar;  //Procesamiento de definición de una variable
    procLoad: TProcLoadOperand;  //Procesamiento de carga
    codLoad: string;   //código de carga de operando. Se usa si "procLoad" es NIL.
    Operators: TOperators;      //operadores soportados
    procedure DefineLoadOperand(codLoad0: string);  //Define carga de un operando
//    procedure DefineLoadOperand(procLoad0: TProcExecOperat);  //Define carga de un operando
    function CreateOperator(txt0: string; jer0: byte; name0: string): TOperator; //Crea operador
    function FindOperator(const Opr: string): TOperator;  //indica si el operador está definido
    constructor Create;
    destructor Destroy; override;
  end;


  //Lista de tipos
  TTypes = specialize TFPGObjectList<TType>; //lista de bloques

  //regsitro para almacenar infromación de las variables
  Tvar = record
    nom : string;  //nombre de la variable
    typ : Ttype;   //tipo de la variable
    amb: string;   //ámbito o alcance de la variable
  end;

var //variables públicas del compilador
  PErr : TPError;   //Objeto de Error
  mem: TStringList;   //texto de salida del compilador

procedure StartSyntax(lex0: TSynFacilSyn);   //Prepara la secuencia de preprocesamiento
function LeePosContAct: TPosCont;
procedure FijPosContAct(pc: TPosCont);
property PosAct: TPosCont read LeePosContAct write FijPosContAct;
procedure Compilar(NombArc: string; LinArc: Tstrings; lex0: TSynFacilSyn);

procedure GenTemplCompiler;

implementation

var  //variables privadas del compilador
  lex : TSynFacilSyn;
//    tipo: TtipDato;   //indica el tipo de dato que se esta procesando
  types: TTypes;   //lista de tipos
//    oper: string;      //indica el operador actual
  //tipos de tokens
  vars  : array of Tvar;
  tkIdentif : TSynHighlighterAttributes;
  tkKeyword : TSynHighlighterAttributes;
  tkNumber  : TSynHighlighterAttributes;
  tkOperator: TSynHighlighterAttributes;
  tkDelimiter: TSynHighlighterAttributes;
  tkType    : TSynHighlighterAttributes;
  tkOthers  : TSynHighlighterAttributes;

  ConsE: TListaCont;   //Lista de contextos de entrada
  //Variables del Contexto actual
  cEnt : TContexto;    //referencia al contexto de entrada actual
  nullOper: TOperator; //Operador nulo. Usado como valor cero.
  ExprLevel: Integer;  //Nivel de anidamiento de la rutina de evaluación de expresiones

{ TOperator }

function TOperator.CreateOperation(OperadType: Ttype; proc: TProcExecOperat): TxOperation;
var
  r: TxOperation;
begin
  //agrega
  r := TxOperation.Create;
  r.OperadType:=OperadType;
//  r.CodForConst:=codCons;
//  r.CodForVar:=codVar;
//  r.CodForExpr:=codExp;
  r.proc:=proc;
  //agrega
  operations.Add(r);
  Result := r;
end;
function TOperator.FindOperation(typ0: Ttype): TxOperation;
{Busca, si encuentra definida, alguna operación, de este operador con el tipo indicado.
Si no lo encuentra devuelve NIL}
var
  r: TxOperation;
begin
  Result := nil;
  for r in Operations do begin
    if r.OperadType = typ0 then begin
      exit(r);
    end;
  end;
end;
constructor TOperator.Create;
begin
  Operations := TOperations.Create(true);
end;
destructor TOperator.Destroy;
begin
  Operations.Free;
  inherited Destroy;
end;

{ TType }

procedure TType.DefineLoadOperand(codLoad0: string);
begin

end;
function TType.CreateOperator(txt0: string; jer0: byte; name0: string): TOperator;
{Permite crear un nuevo ooperador soportado por este tipo de datos. Si hubo error,
devuelve NIL. En caso normal devuelve una referencia al operador creado}
var
  n: Integer;
  r: TOperator;  //operador
  i: Integer;
begin
  //verifica nombre
  if FindOperator(txt0)<>nullOper then begin
    Result := nil;  //indica que hubo error
    exit;
  end;
  //inicia
  r := TOperator.Create;
  r.txt:=txt0;
  r.jer:=jer0;
  r.nom:=name0;
  r.idx:=Operators.Count;
  //agrega
  Operators.Add(r);
  Result := r;
end;
function TType.FindOperator(const Opr: string): TOperator;
//Recibe la cadena de un operador y devuelve una referencia a un objeto Toperator, del
//tipo. Si no está definido el operador para este tipo, devuelve nullOper.
var
  i: Integer;
begin
  Result := nullOper;   //valor por defecto
  for i:=0 to Operators.Count-1 do begin
    if Operators[i].txt = Opr then begin
      exit(Operators[i]); //está definido
    end;
  end;
end;
constructor TType.Create;
begin
  Operators := TOperators.Create(true);  //crea contenedor de Contextos, con control de objetos.
end;
destructor TType.Destroy;
begin
  Operators.Free;
  inherited Destroy;
end;

{ TOperand }

function TOperand.NombreTipo: string;
begin
   case catTyp of
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
  typ.procLoad(self);  //llama al evento de carga
end;
function TOperand.FindOperator(const oper: string): TOperator;
//Recibe la cadena de un operador y devuelve una referencia a un objeto Toperator, del
//operando. Si no está definido el operador para este operando, devuelve nullOper.
begin
  Result := typ.FindOperator(oper);
end;
function TOperand.GetOperator: Toperator;
//Lee del contexto de entrada y toma un operador. Si no encuentra un operador, devuelve NIL.
//Si el operador encontrado no se aplica al operando, devuelve nullOper.
begin
  cEnt.CapBlancos;
  if cEnt.tokType <> tkOperator then exit(nil);
  Result := typ.FindOperator(cEnt.tok);
  cEnt.Next;   //toma el token
end;

{ Rutinas del compilador }

procedure Code(cod: string);
begin
  mem.Add(cod);
end;
function FindVar(varname:string): integer;
//Busca el nombre dado para ver si se trata de una variable definida
var
  tmp: String;
  i: Integer;
begin
  Result := -1;
  tmp := upCase(varName);
  for i:=0 to high(vars) do begin
    if Upcase(vars[i].nom)=tmp then begin
      exit(i);
    end;
  end;
end;
//Manejo de tipos
function CreateType(nom0: string; cat0: TtipDato; siz0: smallint): TType;
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
procedure ClearTypes;  //Limpia los tipos
begin
  types.Clear;
end;
procedure ClearVars;  //Limpia los tipos
begin
  setlength(vars,0);
end;

////////////////Rutinas de generación de código para el compilador////////////
function HayError: boolean;
begin
  Result := PErr.HayError;
end;
function CogExpresion(const jerar: Integer): TOperand; forward;
{$I codgen8086.pas}
function TokAct: string; inline;
//Devuelve el token actual, ignorando la caja.
begin
  Result:=lowercase(cEnt.tok);
end;
function CapturaDelim: boolean;
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
function LeePosContAct: TPosCont;
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
procedure FijPosContAct(pc: TPosCont);
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
      cEnt.SetPosXY(pc.fil, pc.col );  //posiciona al contexto
      cEnt.arc := pc.arc;
      cEnt.nlin := pc.nlin;
    end;
end;

procedure NuevoContexEntTxt(txt: string; arc0: String);
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
procedure NuevoContexEntArc(arc0: String);
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
procedure NuevoContexEntArc(arc0: String; lins: Tstrings);
//Crea un Contexto a partir de un archivo.
//Fija el Contexto Actual "cEnt" como el Contexto creado.
begin
  cEnt := TContexto.Create; //crea Contexto
  cEnt.DefSyn(lex);     //asigna lexer
  ConsE.Add(cEnt);   //Registra Contexto
  cEnt.FijArc(arc0, lins);     //inicia con archivo
  cEnt.CurPosIni;       //posiciona al inicio
end;
procedure QuitaContexEnt;
//Elimina el contexto de entrada actual. Deja apuntando al anterior en la misma posición.
begin
  if ConsE.Count = 0 then exit;  //no se puede quitar más
  ConsE.Delete(ConsE.Count-1);
  if ConsE.Count = 0 then
    cEnt := nil
  else  //apunta al último
    CEnt := ConsE[ConsE.Count-1];
end;

function CogOperando: TOperand;
  procedure TipDefecNumber(var Op: TOperand; toknum: string);
  //Devuelve el tipo de número entero o fltante más sencillo que le corresponde a un token
  var
    n: int64;   //para almacenar a los enteros
    f: extended;  //para almacenar a reales
    i: Integer;
  begin
    if pos('.',toknum) <> 0 then begin  //es flotante
      f := StrToFloat(toknum);
      Op.catTyp := t_float;   //es flotante
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
var
  ivar: Integer;
begin
  PErr.Limpiar;
  cEnt.CapBlancos;
  Result.estOp:=0;  //Este estado significa NO CARGADO en registros.
  if cEnt.tokType = tkNumber then begin  //constantes numéricas
     Result.simple:=true;       //es simple
     Result.catOp:=coConst;       //constante es Mono Operando
     Result.txt:= cEnt.tok;     //toma el texto
     Result.catTyp:= t_integer;  //es numérico
     TipDefecNumber(Result, cEnt.tok); //encuentra tipo de número, tamaño y valor
     if pErr.HayError then exit;  //verifica
     if Result.typ = nil then begin
        PErr.GenError('No hay tipo definido para albergar a esta constante numérica',PosAct);
        exit;
      end;
     cEnt.Next;    //Pasa al siguiente
  end else if cEnt.tokType = tkIdentif then begin  //puede ser variable
    ivar := FindVar(cEnt.tok);
    if ivar = -1 then begin
      PErr.GenError('Identificador desconocido: "' + cEnt.tok + '"',PosAct);
      exit;
    end;
    //es una variable
    Result.simple:=true;       //es simple
    Result.catOp:=coVariable;    //variable
    Result.txt:= cEnt.tok;     //toma el texto
    Result.catTyp:= vars[ivar].typ.cat;  //categoría
    Result.typ:=vars[ivar].typ;
    cEnt.Next;    //Pasa al siguiente

{    tkString: begin  //constantes de cadena

    end;
    tkSymbol: begin  //los únicos símbolos válidos son +,-, que son parte de un número

    end;}
  end else begin
    //No se reconoce el operador
    PErr.GenError('Se esperaba operando',PosAct);
  end;
end;
procedure DefinirVariable(varName, varType: string);
//Se debe reservar espacio para las variables indicadas. Los tipos siempre
//aparecen en minúscula.
var t: ttype;
  hay: Boolean;
  n: Integer;
  r : Tvar;
begin
  //Verifica el tipo
  hay := false;
  for t in types do begin
    if t.name=varType then begin
       hay:=true; break;
    end;
  end;
  if not hay then begin
    Perr.GenError('Tipo "' + varType + '" no defiido.', PosAct);
    exit;
  end;
  //verifica nombre
  if Findvar(varName)<>-1 then begin
    Perr.GenError('Identificador duplicado: "' + varName + '".', PosAct);
    exit;
  end;
  //registra variable en tabla
  r.nom:=varName;
  r.typ := t;
  n := high(vars)+1;
  setlength(vars, n+1);
  vars[n] := r;
  //Ya encontró tipo, llama a evento
  if t.procDefine<>nil then t.procDefine(varName, '');
end;
procedure CompileVarDeclar;
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
    for tmp in varNames do begin
      DefinirVariable(tmp, lowerCase(varType));
      if Perr.HayError then exit;
    end;
  end else begin
    Perr.GenError('Se esperaba ":" o ",".', PosAct);
    exit;
  end;
  if not CapturaDelim then exit;
  cEnt.CapBlancos;
end;
procedure CompilarArc(NomArc: String; LinArc: Tstrings);
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
  Cod_StartData;
  if tokAct = 'var' then begin
    cEnt.Next;    //lo toma
    while (tokAct <>'begin') and (tokAct <>'const') and
          (tokAct <>'type') and (tokAct <>'var') do begin
      CompileVarDeclar;
      if pErr.HayError then exit;;
    end;
  end;
  if tokAct = 'begin' then begin
    Cod_StartProgram;

    cEnt.Next;   //coge "begin"
    //codifica el contenido
    while not cEnt.Eof do begin
  //      Application.MessageBox(PChar(cEnt.tok),'',0);
      //se espera una expresión
      CogExpresion(0);
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
procedure Compilar(NombArc: string; LinArc: Tstrings; lex0 : TSynFacilSyn);
//Compila el contenido de un archivo a ensamblador
begin
  Perr.IniError;
  StartSyntax(lex0); //!!!!!Debería hacerse solo uan vez al inicio
  mem.Clear;       //limpia salida
  ConsE.Clear;     //elimina todos los Contextos de entrada
  ClearVars;       //limpia las variables
  ExprLevel := 0;  //inicia
  //compila
  CompilarArc(NombArc, LinArc);
  if PErr.HayError then exit;
//  PPro.GenArchivo(ArcSal);
end;
function Evaluar(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
//Ejecuta una operación con dos operandos y un operador. "opr" es el operador de Op1.
var expr: TOperand;
  o: TxOperation;
begin
    PErr.IniError;
    //Busca si hay una operación definida para: <tipo de Op1>-opr-<tipo de Op2>
    o := opr.FindOperation(Op2.typ);
    if o = nil then begin
      Perr.GenError('No se ha definido la operación: ' +
                    Op1.NombreTipo + opr.txt + Op2.NombreTipo, PosAct);
      Exit;
    end;
    //Llama al evento asociado
    Result := o.proc(Op1, opr, Op2);
    //Completa campos de evaluar
    Result.catOp := coExpres;    //ahora es expresión por defecto
    Result.txt := Op1.txt + opr.txt + Op2.txt;   //texto de la expresión
//    Evaluar.uop := opr;   //última operación ejecutada
End;
function GetExpressionCore(const jerar: Integer): TOperand; //inline;
//Generador de Algoritmos de Evaluacion de expresiones.
//Esta es la función más importante del compilador
var
  Op1, Op2  : TOperand;   //Operandos
  opr1, opr2: TOperator;  //Operadores
  pos1, pos2: TPosCont;   //posiciones de texto
begin
  Op1.catTyp:=t_integer;    //asumir opcion por defecto
  Op2.catTyp:=t_integer;   //asumir opcion por defecto
  pErr.Limpiar;
  //----------------coger primer operando------------------
  Op1 := CogOperando; if pErr.HayError then exit;
  //verifica si termina la expresion
  opr1 := Op1.GetOperator; if pErr.HayError then exit;
  if opr1 = nil then begin  //no sigue operador
    //Expresión de un solo operando. Lo carga por si se necesita
    Op1.Load;   //carga el operador para cumplir
    Result:=Op1;
    exit;  //termina ejecucion
  end;
  //------- sigue un operador ---------
  //verifica si el operador aplica al operando
  if opr1 = nullOper then begin
    PErr.GenError('No está definido el operador: '+ opr1.txt + ' para tipo: '+Op1.typ.name, PosAct);
    exit;
  end;
  //¿Delimitada por jerarquía?
  If opr1.jer <= jerar Then begin  //es menor que la que sigue, expres.
    Result := Op1;  //solo devuelve el único operando que leyó
    exit;
  End;

  while opr1<>nil do begin
    pos1 := PosAct;    //Guarda por si lo necesita
    //--------------------coger operador ---------------------------
	//operadores unitarios ++ y -- (un solo operando).
    //Se evaluan como si fueran una mini-expresión o función
{	if opr1.id = op_incremento then begin
      case Op1.catTyp of
        t_integer: Cod_IncremOperanNumerico(Op1);
      else
        PErr.GenError('Operador ++ no es soportado en este tipo de dato.',PosAct);
        exit;
      end;
      opr1 := cogOperador; if pErr.HayError then exit;
      if opr1.id = Op_ninguno then begin  //no sigue operador
        Result:=Op1; exit;  //termina ejecucion
      end;
    end else if opr1.id = op_decremento then begin
      case Op1.catTyp of
        t_integer: Cod_DecremOperanNumerico(Op1);
      else
        PErr.GenError('Operador -- no es soportado en este tipo de dato.',PosAct);
        exit;
      end;
      opr1 := cogOperador; if pErr.HayError then exit;
      if opr1.id = Op_ninguno then begin  //no sigue operador
        Result:=Op1; exit;  //termina ejecucion
      end;
    end;}
    //--------------------prepara para evaluar---------------------
//    Cod_SalvarOperanNumerico(Op1);
    //--------------------coger segundo operando--------------------
	Op2 := CogOperando; if pErr.HayError then exit;
    pos2 := PosAct;    //Guarda por si lo necesita
    opr2 := Op2.GetOperator;   //Toma el siguiente operador
    If opr2 <> nil Then begin  //Hay otro operador
      if opr2=nullOper then begin
        PErr.GenError('No está definido el operador: '+ opr2.txt + ' para tipo: '+Op2.typ.name, PosAct);
        exit;
      end;
      //¿Delimitado por jerarquía de operador?
      If opr2.jer <= jerar Then begin  //sigue uno de menor jerarquía, hay que salir
          PosAct := pos2;   //antes de coger el operador
          Result := Evaluar(Op1, opr1, Op2);
          Exit;
      End;
      If opr2.jer > opr1.jer Then begin   //y es de mayor jerarquía, retrocede
          PosAct := pos1;        //retrocede
          Op2 := CogExpresion(opr1.jer);  //evalua primero
          if pErr.HayError then exit;
          opr2 := Op2.GetOperator;       //actualiza el siguiente operador
      End;

    end;
    Op1 := Evaluar(Op1, opr1, Op2);    //evalua resultado
    if PErr.HayError then exit;
    //prepara siguiente operación
    opr1 := opr2;
  end;  //hasta que ya no siga un operador
  Result := Op1;  //aquí debe haber quedado el resultado
end;
function CogExpresion(const jerar: Integer): TOperand;
//Envoltura para GetExpressionCore().
{ TODO : Para optimizar debería existir solo CogExpresion() y no GetExpressionCore() }
begin
  Inc(ExprLevel);  //cuenta el anidamiento
  expr_start;  //llama a evento
  Result := GetExpressionCore(jerar);
  expr_end;    //llama al evento de salida
  Dec(ExprLevel);
end;
procedure GenTemplCompiler;
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


initialization
  mem := TStringList.Create;
  PErr.IniError;   //inicia motor de errores
  //Crea lista de Contextos
  ConsE := TListaCont.Create(true);  //crea contenedor de Contextos, con control de objetos.
  cEnt := nil;
  //Inicia lista de tipos
  types := TTypes.Create(true);
  //Inicia lista de variables
  setlength(vars,0);
  //crea el operador NULL
  nullOper := TOperator.Create;
finalization;
  nullOper.Free;
  types.Free;
  //Limpia lista de Contextos
  ConsE.Free;
  mem.Free;  //libera
end.

