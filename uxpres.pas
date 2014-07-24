unit uXpres;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, fgl, SynHighlighterFacil, Forms, LCLType,
  SynEditHighlighter, Dialogs,
  XpresBas, Globales, lclProc, FormOut;

type
  //categorías básicas de tipo de datos
  TtipDato=(
    t_integer,  //números enteros
    t_uinteger, //enteros sin signo
    t_float,    //es de coma flotante
    t_string,   //cadena de caracteres
    t_boolean,  //booleano
    t_enum      //enumerado
  );

  //Categoría de Operando
  CatOperan = (
    coConst,     //mono-operando constante
//    coConstExp,  //expresión constante
    coVariable,  //variable
    coExpres     //expresión
  );

  //tipo de identificador
  TIdentifType = (idtNone, idtVar, idtFunc, idtCons);

  TType = class;
  TOperator = class;

  //registro para almacenar información de las variables
  Tvar = record
    nom : string;   //nombre de la variable
    typ : Ttype;    //tipo de la variable
    amb : string;   //ámbito o alcance de la variable
    //direción física. Usado para implementar un compilador
    adrr: integer;
    //Campos usados para implementar el intérprete sin máquina virtual
    //valores de la variable.
    valFloat: extended; //Valor en caso de que sea un flotante
    valInt  : Int64;    //valor en caso de que sea un entero
    valUInt : Int64;    //valor en caso de que sea un entero sin signo
    valBool  : Boolean;  //valor  en caso de que sea un booleano
    valStr  : string;     //valor  en caso de que sea una cadena
  end;

  { TOperand }
  //Operando
  TOperand = object
  private
    cons: Tvar;        //valor en caso de que sea una constante
{    procedure cons_SetvalInt(AValue: Int64);
    procedure var_SetvalInt(AValue: Int64);
    procedure cons_SetvalFloat(AValue: extended);
    procedure var_SetvalFloat(AValue: extended);
    procedure cons_SetvalBool(AValue: boolean);
    procedure var_SetvalBool(AValue: boolean);
    procedure cons_SetvalStr(AValue: string);
    procedure var_SetvalStr(AValue: string);}
    function GetValBool: boolean;
//    procedure SetValBol(AValue: boolean);
    function GetValInt: int64;
//    procedure SetValInt(AValue: Int64);
    function GetValFloat: extended;
//    procedure SetValFloat(AValue: extended);
    function GetValStr: string;
//    procedure SetValStr(AValue: string);
  public
//    name : string;
    typ  : TType;     //referencia al tipo de dato
  	catTyp: tTipDato; //Categoría de Tipo de dato
    size : integer;   //tamaño del operando en bytes
    catOp: CatOperan; //Categoría de operando
    estOp: integer;   //Estado del operando (Usado para la generec. de código)
//used:   boolean;
    txt  : string;    //Texto del operando o expresión
    ivar : integer;   //índice a variables, en caso de que sea variable
    ifun : integer;   //índice a funciones, en caso de que sea función
    procedure Load;   //carga el operador en registro o pila
    function FindOperator(const oper: string): TOperator; //devuelve el objeto operador
    function GetOperator: Toperator;

    //metodos para facilitar la implementación del intérprete
    function expres: string;  //devuelve una cadena que expresa al operando
    property valInt: Int64 read GetValInt; // write SetvalInt;
    property valFloat: extended read GetValFloat; // write SetValFloat;
    property valStr: string read GetValStr; // write SetValStr;
    property valBool: boolean read GetValBool;// write SetValBol;
{    property cons_valInt: Int64 write cons_SetvalInt;
     property var_valInt: Int64 write var_SetvalInt;
     property cons_valFloat: extended write cons_SetvalFloat;
     property var_valFloat: extended write var_SetvalFloat;
     property cons_valBool: boolean write cons_SetvalBool;
     property var_valBool: boolean write var_SetvalBool;
     property cons_valStr: string write cons_SetvalStr;
     property var_valStr: string write var_SetvalStr;}
  end;

  TProcDefineVar = procedure(const varName, varInitVal: string);
  TProcLoadOperand = procedure(var Op: TOperand);
  TProcExecOperat = procedure;

  //registro para almacenar información de las funciones
  Tfunc = record
    name: string;   //nombre de la función
    typ : Ttype;    //tipo que devuelve
    pars: array of Ttype;  //parámetros de entrada
    amb : string;   //ámbito o alcance de la función
    //direción física. Usado para implementar un compilador
    adrr: integer;  //dirección física
    //Campos usados para implementar el intérprete sin máquina virtual
    proc: TProcExecOperat;  //referencia a la función que implementa
    posF: TPoint;    //posición donde empieza la función en el código
  end;

  //Tipo operación
  TxOperation = class
    OperatType : TType;   //tipo de Operando sobre el cual se aplica la operación.
    proc       : TProcExecOperat;  //Procesamiento de la operación
  end;

  TOperations = specialize TFPGObjectList<TxOperation>; //lista de bloques

  //Operador
  { TOperator }

  TOperator = class
    txt: string;    //cadena del operador '+', '-', '++', ...
    jer: byte;      //precedencia
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
    cat  : TtipDato;    //categoría del tipo (numérico, cadena, etc)
    size : smallint;    //tamaño en bytes del tipo
    idx  : smallint;    //ubicación dentro de la matriz de tipos
    amb  : TFaSynBlock; //ámbito de validez del tipo
    procDefine: TProcDefineVar;  //Procesamiento de definición de una variable
    procLoad: TProcLoadOperand;  //Procesamiento de carga
    codLoad: string;   //código de carga de operando. Se usa si "procLoad" es NIL.
    Operators: TOperators;      //operadores soportados
    procedure DefineLoadOperand(codLoad0: string);  //Define carga de un operando
    function CreateOperator(txt0: string; jer0: byte; name0: string): TOperator; //Crea operador
    function FindOperator(const Opr: string): TOperator;  //indica si el operador está definido
    constructor Create;
    destructor Destroy; override;
  end;


  //Lista de tipos
  TTypes = specialize TFPGObjectList<TType>; //lista de bloques


var //variables públicas del compilador
  PErr : TPError;    //Objeto de Error
  mem: TStringList;  //texto de salida del compilador
  p1, p2: TOperand;  //operandos de la operación actual
  res: TOperand;     //resultado de la evaluación de la última expresión.
  xLex : TSynFacilSyn;  //resaltador - lexer

function LeePosContAct: TPosCont;
procedure FijPosContAct(pc: TPosCont);
property PosAct: TPosCont read LeePosContAct write FijPosContAct;
procedure Compilar(NombArc: string; LinArc: Tstrings);

procedure GenTemplCompiler;

implementation
uses Graphics;

var  //variables privadas del compilador
  types: TTypes;   //lista de tipos
//    oper: string;      //indica el operador actual
  //tipos de tokens
  vars  : array of Tvar;  //lista de variables
  funcs : array of Tfunc; //lista de funciones
  cons  : array of Tvar;  //lista de constantes
  nIntVar : integer;   //número de variables internas
  nIntFun : integer;   //núemro de funciones internas
  nIntCon : integer;   //núemro de constantes internas

  tkIdentif : TSynHighlighterAttributes;
  tkKeyword : TSynHighlighterAttributes;
  tkNumber  : TSynHighlighterAttributes;
  tkString  : TSynHighlighterAttributes;
  tkOperator: TSynHighlighterAttributes;
  tkExpDelim:TSynHighlighterAttributes;
  tkBlkDelim: TSynHighlighterAttributes;
  tkType    : TSynHighlighterAttributes;
  tkBoolean : TSynHighlighterAttributes;
  tkStruct  : TSynHighlighterAttributes;
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
  r.OperatType:=OperadType;
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
    if r.OperatType = typ0 then begin
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
  r: TOperator;  //operador
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
    if Operators[i].txt = upCase(Opr) then begin
      exit(Operators[i]); //está definido
    end;
  end;
  //no encontró
  Result.txt := Opr;    //para que sepa el operador leído
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

{ Rutinas del compilador }
function CategName(cat: TtipDato): string;
begin
   case cat of
   t_integer: Result := 'Numérico';
   t_float: Result := 'Flotante';
   t_string: Result := 'Cadena';
   t_boolean: Result := 'Booleano';
   t_enum: Result := 'Enumerado';
   else Result := 'Desconocido';
   end;
end;
procedure Code(cod: string);
begin
  mem.Add(cod);
end;
function FindVar(varName:string): integer;
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
function FindFunc(funName:string): integer;
//Busca el nombre dado para ver si se trata de una función definida
var
  tmp: String;
  i: Integer;
begin
  Result := -1;
  tmp := upCase(funName);
  for i:=0 to high(funcs) do begin
    if Upcase(funcs[i].name)=tmp then begin
      exit(i);
    end;
  end;
end;
function FindCons(conName:string): integer;
//Busca el nombre dado para ver si se trata de una constante definida
var
  tmp: String;
  i: Integer;
begin
  Result := -1;
  tmp := upCase(conName);
  for i:=0 to high(cons) do begin
    if Upcase(cons[i].nom)=tmp then begin
      exit(i);
    end;
  end;
end;
function FindPredefName(name: string): TIdentifType;
//Busca un identificador e indica si ya existe el nombre, sea como variable,
//función o constante.
var i: integer;
begin
  //busca como variable
  i := FindVar(name);
  if i<>-1 then begin
     exit(idtVar);
  end;
  //busca como función
  i := FindFunc(name);
  if i<>-1 then begin
     exit(idtFunc);
  end;
  //busca como constante
  i := FindCons(name);
  if i<>-1 then begin
     exit(idtCons);
  end;
  //no lo encuentra
  exit(idtNone);
end;
//Manejo de tipos
function CreateType(nom0: string; cat0: TtipDato; siz0: smallint): TType;
//Crea una nueva definición de tipo en el compilador. Devuelve referencia al tipo recien creado
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
procedure ClearVars;
//Limpia todas las variables creadas por el usuario.
begin
  setlength(vars, nIntVar);  //deja las del sistema
end;
procedure ClearAllVars;
//Elimina todas las variables, incluyendo las predefinidas.
begin
  nIntVar := 0;
  setlength(vars,0);
end;
procedure ClearFuncs;
//Limpia todas las funciones creadas por el usuario.
begin
  setlength(funcs,nIntFun);  //deja las del sistema
end;
procedure ClearAllFuncs;
//Elimina todas las funciones, incluyendo las predefinidas.
begin
  nIntFun := 0;
  setlength(funcs,0);
end;
procedure ClearAllConst;
//Elimina todas las funciones, incluyendo las predefinidas.
begin
  nIntCon := 0;
  setlength(cons,0);
end;

//declaraciones adelantadas
function GetExpression(const prec: Integer; isParam: boolean = false): TOperand; forward;
function GetBoolExpression: TOperand; forward;
procedure CompileCurBlock; forward;

////////////////Rutinas de generación de código para el compilador////////////
function HayError: boolean;
begin
  Result := PErr.HayError;
end;
function CreateFunction(funName: string; typ: ttype; proc: TProcExecOperat): integer;
//Crea una nueva función y devuelve un índice a la función.
var
  r : Tfunc;
  n: Integer;
begin
  //verifica nombre
  if FindPredefName(funName) <> idtNone then begin
    Perr.GenError('Identificador duplicado: "' + funName + '".', PosAct);
    exit;
  end;
  //registra la función en la tabla
  r.name:= funName;
  r.typ := typ;
  r.proc:= proc;
  setlength(r.pars,0);  //inicia arreglo
  //agrega
  n := high(funcs)+1;
  setlength(funcs, n+1);
  funcs[n] := r;
  Result := n;
end;
function CreateSysFunction(funName: string; typ: ttype; proc: TProcExecOperat): integer;
//Crea una función del sistema o interna. Estas funciones estan siempre disponibles.
//Las funciones internas deben crearse todas al inicio.
begin
  Result := CreateFunction(funName, typ, proc);
  Inc(nIntFun);  //leva la cuenta
end;
procedure CreateFunction(funName, varType: string);
//Define una nueva función en memoria.
var t: ttype;
  hay: Boolean;
begin
  //Verifica el tipo
  hay := false;
  for t in types do begin
    if t.name=varType then begin
       hay:=true; break;
    end;
  end;
  if not hay then begin
    Perr.GenError('Tipo "' + varType + '" no definido.', PosAct);
    exit;
  end;
  CreateFunction(funName, t, nil);
  //Ya encontró tipo, llama a evento
//  if t.procDefine<>nil then t.procDefine(funName, '');
end;
procedure CreateParam(ifun: integer; name: string; typ: ttype);
//Crea un parámetro para una función
var
  n: Integer;
begin
  //agrega
  n := high(funcs[ifun].pars)+1;
  setlength(funcs[ifun].pars, n+1);
  funcs[ifun].pars[n] := typ;  //agrega referencia
end;
{$I interprete1.pas}
function TokAct: string; inline;
//Devuelve el token actual, ignorando la caja.
begin
  Result:=lowercase(cEnt.tok);
end;
function CapturaDelim: boolean;
//Verifica si sigue un delimitador de expresión. Si encuentra devuelve false.
begin
  cEnt.CapBlancos;
  if cEnt.tokType=tkExpDelim then begin //encontró
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
      //resún no hay Contexto definido
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
//    filAct := 1;
//    colAct := 1;
//    cEnt.arc := '';
//    nlin := 0;
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
  cEnt.DefSyn(xLex);     //asigna lexer
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
  cEnt.DefSyn(xLex);     //asigna lexer
  ConsE.Add(cEnt);   //Registra Contexto
  cEnt.FijArc(arc0);     //inicia con archivo
  cEnt.CurPosIni;       //posiciona al inicio
end;
procedure NuevoContexEntArc(arc0: String; lins: Tstrings);
//Crea un Contexto a partir de un Tstring, como si fuera un archivo.
//Fija el Contexto Actual "cEnt" como el Contexto creado.
begin
  cEnt := TContexto.Create; //crea Contexto
  cEnt.DefSyn(xLex);     //asigna lexer
  ConsE.Add(cEnt);   //Registra Contexto
  cEnt.FijArc(arc0, lins);   //inicia con archivo contenido en TStrings
  cEnt.CurPosIni;       //posiciona al inicio
end;
procedure QuitaContexEnt;
//Elimina el contexto de entrada actual. Deja apuntando al anterior en la misma posición.
begin
  if ConsE.Count = 0 then begin
    cEnt := nil;   //por si acaso
    exit;  //no se puede quitar más
  end;
  ConsE.Delete(ConsE.Count-1);
  if ConsE.Count = 0 then
    cEnt := nil
  else  //apunta al último
    CEnt := ConsE[ConsE.Count-1];
end;
procedure TipDefecNumber(var Op: TOperand; toknum: string);
//Devuelve el tipo de número entero o fltante más sencillo que le corresponde a un token
//que representa a una constante numérica.
//Su forma de trabajo es buscar el tipo numérico más pequeño que permita alojar a la
//constante numérica indicada.

var
  n: int64;   //para almacenar a los enteros
  f: extended;  //para almacenar a reales
  i: Integer;
  menor: Integer;
  imen: integer;
begin
  if pos('.',toknum) <> 0 then begin  //es flotante
    Op.catTyp := t_float;   //es flotante
    f := StrToFloat(toknum);  //carag con la mayor precisión posible
    //busca el tipo numérico más pequeño que pueda albergar a este número
    Op.size := 4;   //se asume que con 4 bytes bastará
    {Aquí se puede decidir el tamaño de acuerdo a la cantidad de decimales indicados}

    Op.cons.valFloat := f;  //debe devolver un extended
    menor := 1000;
    for i:=0 to types.Count-1 do begin
      { TODO : Se debería tener una lista adicional TFloatTypes, para acelerar la
      búsqueda}
      if (types[i].cat = t_float) and (types[i].size>=Op.size) then begin
        //guarda el menor
        if types[i].size < menor then  begin
           imen := i;   //guarda referencia
           menor := types[i].size;
        end;
      end;
    end;
    if menor = 1000 then  //no hubo tipo
      Op.typ := nil
    else  //encontró
      Op.typ:=types[imen];

  end else begin     //es entero
    Op.catTyp := t_integer;   //es entero
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
    Op.cons.valInt := n;   //copia valor de constante entera
    //busca si hay tipo numérico que soporte esta constante
{      Op.typ:=nil;
    for i:=0 to types.Count-1 do begin
      { TODO : Se debería tener una lista adicional  TIntegerTypes, para acelerar la
      búsqueda}
      if (types[i].cat = t_integer) and (types[i].size=Op.size) then
        Op.typ:=types[i];  //encontró
    end;}
    //busca el tipo numérico más pequeño que pueda albergar a este número
    menor := 1000;
    for i:=0 to types.Count-1 do begin
      { TODO : Se debería tener una lista adicional  TIntegerTypes, para acelerar la
      búsqueda}
      if (types[i].cat = t_integer) and (types[i].size>=Op.size) then begin
        //guarda el menor
        if types[i].size < menor then  begin
           imen := i;   //guarda referencia
           menor := types[i].size;
        end;
      end;
    end;
    if menor = 1000 then  //no hubo tipo
      Op.typ := nil
    else  //encontró
      Op.typ:=types[imen];
  end;
end;
procedure TipDefecString(var Op: TOperand; tokcad: string);
//Devuelve el tipo de cadena encontrado en un token
var
  i: Integer;
begin
  Op.catTyp := t_string;   //es flotante
  Op.size:=length(tokcad);
  //toma el texto
  Op.cons.valStr := copy(cEnt.tok,2, length(cEnt.tok)-2);   //quita comillas
  //////////// Verifica si hay tipos string definidos ////////////
  Op.typ:=nil;
  //Busca primero tipo string (longitud variable)
  for i:=0 to types.Count-1 do begin
    { TODO : Se debería tener una lista adicional  TStringTypes, para acelerar la
    búsqueda}
    if (types[i].cat = t_string) and (types[i].size=-1) then begin  //busca un char
      Op.typ:=types[i];  //encontró
      break;
    end;
  end;
  if Op.typ=nil then begin
    //no hubo "string", busca al menos "char", para generar ARRAY OF char
    for i:=0 to types.Count-1 do begin
      { TODO : Se debería tener una lista adicional  TStringTypes, para acelerar la
      búsqueda}
      if (types[i].cat = t_string) and (types[i].size=1) then begin  //busca un char
        Op.typ:=types[i];  //encontró
        break;
      end;
    end;
  end;
end;
procedure TipDefecBoolean(var Op: TOperand; tokcad: string);
//Devuelve el tipo de cadena encontrado en un token
var
  i: Integer;
begin
  Op.catTyp := t_boolean;   //es flotante
  Op.size:=1;   //se usará un byte
  //toma valor constante
  Op.cons.valBool:= (tokcad[1] in ['t','T']);
  //verifica si hay tipo boolean definido
  Op.typ:=nil;
  for i:=0 to types.Count-1 do begin
    { TODO : Se debería tener una lista adicional  TBooleanTypes, para acelerar la
    búsqueda}
    if (types[i].cat = t_boolean) then begin  //basta con que haya uno
      Op.typ:=types[i];  //encontró
      break;
    end;
  end;
end;
function GetOperand: TOperand;
//Parte de la funcion GAEE que genera codigo para leer un operando.
var
  ivar: Integer;
  ifun: Integer;
  i: Integer;
begin
  PErr.Limpiar;
  cEnt.CapBlancos;
  Result.estOp:=0;  //Este estado significa NO CARGADO en registros.
  if cEnt.tokType = tkNumber then begin  //constantes numéricas
    Result.estOp:=STORED_LIT;
    Result.catOp:=coConst;       //constante es Mono Operando
    Result.txt:= cEnt.tok;     //toma el texto
    TipDefecNumber(Result, cEnt.tok); //encuentra tipo de número, tamaño y valor
    if pErr.HayError then exit;  //verifica
    if Result.typ = nil then begin
        PErr.GenError('No hay tipo definido para albergar a esta constante numérica',PosAct);
        exit;
      end;
    cEnt.Next;    //Pasa al siguiente
  end else if cEnt.tokType = tkIdentif then begin  //puede ser variable, constante, función
    ivar := FindVar(cEnt.tok);
    if ivar <> -1 then begin
      //es una variable
      Result.ivar:=ivar;   //guarda referencia a la variable
      Result.catOp:=coVariable;    //variable
      Result.catTyp:= vars[ivar].typ.cat;  //categoría
      Result.typ:=vars[ivar].typ;
      Result.estOp:=STORED_VAR;
      Result.txt:= cEnt.tok;     //toma el texto
      cEnt.Next;    //Pasa al siguiente
    end else begin  //no es variable
      //busca como función
      ifun := FindFunc(cEnt.tok);
      if ifun <> -1 then begin
        //es una función
        Result.ifun:=ifun;   //guarda referencia a la función
        Result.catOp :=coExpres;    //expresión
        Result.txt:= cEnt.tok;     //toma el texto
        cEnt.Next;    //Pasa al siguiente
        //lee parámetros
        for i:=0 to High(funcs[ifun].pars) do begin
          GetExpression(0, true);  //captura parámetro
          if perr.HayError then exit;   //aborta
          if res.typ<>funcs[ifun].pars[i] then begin
            Perr.GenError('Parámetro de tipo erroneo.', PosAct);
            exit;
          end;
          if i <> High(funcs[ifun].pars) then begin
            if tokAct<> ',' then begin
              Perr.GenError('Se esperaba ","', PosAct);
              exit;
            end;
            cEnt.Next;  //toma el THEN
          end;
        end;
        Result.catTyp:= funcs[ifun].typ.cat;  //categoría
        Result.typ:=funcs[ifun].typ;
//        Result.estOp:=STORED_VAR;  el estado lo decidirá la función
        funcs[ifun].proc;  //llama al código de la función
        Result.estOp:=res.estOp;
      end else begin
        PErr.GenError('Identificador desconocido: "' + cEnt.tok + '"',PosAct);
        exit;
      end;
    end;
  end else if cEnt.tokType = tkBoolean then begin  //true o false
    Result.estOp:=STORED_LIT;
    Result.catOp:=coConst;       //constante es Mono Operando
    Result.txt:= cEnt.tok;     //toma el texto
    TipDefecBoolean(Result, cEnt.tok); //encuentra tipo de número, tamaño y valor
    if pErr.HayError then exit;  //verifica
    if Result.typ = nil then begin
       PErr.GenError('No hay tipo definido para albergar a esta constante booleana',PosAct);
       exit;
     end;
    cEnt.Next;    //Pasa al siguiente
  end else if (cEnt.tokType = tkOthers) and (cEnt.tok = '(') then begin  //"("
    cEnt.Next;
    Result := GetExpression(0);
    if PErr.HayError then exit;
    If cEnt.tok = ')' Then begin
       cEnt.Next;  //lo toma
    end Else begin
       PErr.GenError('Error en expresión. Se esperaba ")"', PosAct);
       Exit;       //error
    end;
  end else if (cEnt.tokType = tkString) then begin  //constante cadena
    Result.estOp:=STORED_LIT;
    Result.catOp:=coConst;     //constante es Mono Operando
//    Result.txt:= cEnt.tok;     //toma el texto
    TipDefecString(Result, cEnt.tok); //encuentra tipo de número, tamaño y valor
    if pErr.HayError then exit;  //verifica
    if Result.typ = nil then begin
       PErr.GenError('No hay tipo definido para albergar a esta constante cadena',PosAct);
       exit;
     end;
    cEnt.Next;    //Pasa al siguiente
{  end else if (cEnt.tokType = tkOperator then begin
   //los únicos símbolos válidos son +,-, que son parte de un número
    }
  end else begin
    //No se reconoce el operador
    PErr.GenError('Se esperaba operando',PosAct);
  end;
end;
procedure CreateVariable(varName, varType: string);
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
    Perr.GenError('Tipo "' + varType + '" no definido.', PosAct);
    exit;
  end;
  //verifica nombre
  if FindPredefName(varName) <> idtNone then begin
    Perr.GenError('Identificador duplicado: "' + varName + '".', PosAct);
    exit;
  end;
  //registra variable en la tabla
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
  //procesa variables res,b,c : int;
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
      CreateVariable(tmp, lowerCase(varType));
      if Perr.HayError then exit;
    end;
  end else begin
    Perr.GenError('Se esperaba ":" o ",".', PosAct);
    exit;
  end;
  if not CapturaDelim then exit;
  cEnt.CapBlancos;
end;
procedure ShowOperand(const Op: TOperand);
//muestra un operando por pantalla
var
  tmp: String;
begin
  tmp := 'Result ' + CategName(Op.typ.cat) + '(' + Op.typ.name + ') = ';
  case Op.Typ.cat of
  t_integer: frmOut.puts(tmp + IntToStr(Op.valInt));
  t_float :  frmOut.puts(tmp + FloatToStr(Op.valFloat));
  t_string:  frmOut.puts(tmp + Op.valStr);
  t_boolean: if Op.valBool then frmOut.puts(tmp + 'TRUE')
             else frmOut.puts(tmp + 'FALSE');
  end;
end;
procedure ShowResult;
//muestra el resultado de la última exprersión evaluada
begin
  case res.estOp of
  NO_STORED : frmOut.puts('Resultado no almacen.');
  STORED_LIT, STORED_VAR, STORED_ACU, STORED_ACUB : ShowOperand(res);
  else
    frmOut.puts('Estado descon.');
  end;
end;
procedure CompileCurBlock;
//Compila el bloque de código actual hasta encontrar un delimitador de bloque.
begin
  cEnt.CapBlancos;
  while (cEnt.tokType <> tkBlkDelim) and (not cEnt.Eof) do begin
    //se espera una expresión o estructura
    if cEnt.tokType = tkStruct then begin  //es una estructura
      if tokAct = 'if' then begin  //condicional
        cEnt.Next;  //toma IF
        GetBoolExpression; //evalua expresión
        if PErr.HayError then exit;
        if tokAct<> 'then' then begin
          Perr.GenError('Se esperaba "then".', PosAct);
          exit;
        end;
        cEnt.Next;  //toma el THEN
        //cuerpo del if
        CompileCurBlock;  //procesa bloque
//        Result := res;  //toma resultado
        if PErr.HayError then exit;
        while tokAct = 'elsif' do begin
          cEnt.Next;  //toma ELSIF
          GetBoolExpression; //evalua expresión
          if PErr.HayError then exit;
          if tokAct<> 'then' then begin
            Perr.GenError('Se esperaba "then".', PosAct);
            exit;
          end;
          cEnt.Next;  //toma el THEN
          //cuerpo del if
          CompileCurBlock;  //evalua expresión
//          Result := res;  //toma resultado
          if PErr.HayError then exit;
        end;
        if tokAct = 'else' then begin
          cEnt.Next;  //toma ELSE
          CompileCurBlock;  //evalua expresión
//          Result := res;  //toma resultado
          if PErr.HayError then exit;
        end;
        if tokAct<> 'end' then begin
          Perr.GenError('Se esperaba "end".', PosAct);
          exit;
        end;
      end else begin
        Perr.GenError('Error de diseño. Estructura no implementada.', PosAct);
        exit;
      end;
    end else begin  //debe ser una expresión
      GetExpression(0);
      if perr.HayError then exit;   //aborta
    end;
    //se espera delimitador
    if cEnt.Eof then break;  //sale por fin de archivo
    //busca delimitador
    cEnt.CapBlancos;
    if cEnt.tokType=tkExpDelim then begin //encontró delimitador de expresión
      cEnt.Next;   //lo toma
      cEnt.CapBlancos;  //quita espacios
    end else if cEnt.tokType = tkBlkDelim then begin  //hay delimitador de bloque
      exit;  //no lo toma
    end else begin  //hay otra cosa
      exit;  //debe ser un error
    end;
  end;
end;
procedure CompilarArc;
//Compila un programa en el contexto actual
var
  tmp: TOperand;
begin
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
    if not CapturaDelim then exit;
//  end else begin
//    Perr.GenError('Se esperaba: "program ... "', PosAct);
//    exit;
  end;
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
    CompileCurBlock;   //compila el cuerpo
    if Perr.HayError then exit;
    if cEnt.Eof then begin
      Perr.GenError('Inesperado fin de archivo. Se esperaba "end".', PosAct);
      exit;       //sale
    end;
    if tokAct <> 'end' then begin  //verifica si termina el programa
      Perr.GenError('Se esperaba "end".', PosAct);
      exit;       //sale
    end;
    cEnt.Next;   //coge "end"
  end else begin
    Perr.GenError('Se esperaba "begin", "var", "type" o "const".', PosAct);
    exit;
  end;
  Cod_EndProgram;
end;
procedure Compilar(NombArc: string; LinArc: Tstrings);
//Compila el contenido de un archivo a ensamblador
begin
  Perr.IniError;
  ClearVars;       //limpia las variables
  ClearFuncs;      //limpia las funciones
  mem.Clear;       //limpia salida
  ConsE.Clear;     //elimina todos los Contextos de entrada
  ExprLevel := 0;  //inicia
  //compila el archivo abierto

//  con := PosAct;   //Guarda posición y referencia a contenido actual
  NuevoContexEntArc(NombArc,LinArc);   //Crea nuevo contenido
  if PErr.HayError then exit;
  CompilarArc;     //puede dar error
  QuitaContexEnt;   //es necesario por dejar limpio
  if PErr.HayError then exit;   //sale
//  PosAct := con;   //recupera el contenido actual

//  PPro.GenArchivo(ArcSal);
  ShowResult;  //muestra el resultado
end;
function Evaluar(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
//Ejecuta una operación con dos operandos y un operador. "opr" es el operador de Op1.
var
  o: TxOperation;
begin
   debugln(space(ExprLevel)+' Eval('+Op1.txt + ',' + Op2.txt+')');
   PErr.IniError;
   //Busca si hay una operación definida para: <tipo de Op1>-opr-<tipo de Op2>
   o := opr.FindOperation(Op2.typ);
   if o = nil then begin
//      Perr.GenError('No se ha definido la operación: (' +
//                    Op1.typ.name + ') '+ opr.txt + ' ('+Op2.typ.name+')', PosAct);
      Perr.GenError('Operación no válida: (' +
                    Op1.typ.name + ') '+ opr.txt + ' ('+Op2.typ.name+')', PosAct);
      Exit;
    end;
   p1 := Op1;    //fija operando 1
   p2 := Op2;    //fija operando 2
   o.proc;      //Llama al evento asociado
   Result.typ := res.typ;    //lee tipo
   Result.catOp:=res.catOp;  //tipo de operando
   Result.estOp:=res.estOp;  //actualiza estado
   //Completa campos de evaluar
   Result.txt := Op1.txt + opr.txt + Op2.txt;   //texto de la expresión
//   Evaluar.uop := opr;   //última operación ejecutada
End;
function GetOperandP(pre: integer): TOperand;
//Toma un operando realizando hasta encontrar un operador de precedencia igual o menor
//a la indicada
var
  Op1: TOperand;
  Op2: TOperand;
  opr: TOperator;
  pos: TPosCont;
begin
  debugln(space(ExprLevel)+' CogOperando('+IntToStr(pre)+')');
  Op1 :=  GetOperand;  //toma el operador
  if pErr.HayError then exit;
  //verifica si termina la expresion
  pos := PosAct;    //Guarda por si lo necesita
  opr := Op1.GetOperator;
  if opr = nil then begin  //no sigue operador
    Result:=Op1;
  end else if opr=nullOper then begin  //hay operador pero, ..
    //no está definido el operador siguente para el Op1, (no se puede comaprar las
    //precedencias) asumimos que aquí termina el operando.
    PosAct := pos;   //antes de coger el operador
    Result:=Op1;
  end else begin  //si está definido el operador (opr) para Op1, vemos precedencias
    If opr.jer > pre Then begin  //¿Delimitado por precedencia de operador?
      //es de mayor precedencia, se debe evaluar antes.
      Op2 := GetOperandP(pre);  //toma el siguiente operando (puede ser recursivo)
      if pErr.HayError then exit;
      Result:=Evaluar(Op1, opr, Op2);
    End else begin  //la precedencia es menor o igual, debe salir
      PosAct := pos;   //antes de coger el operador
      Result:=Op1;
    end;
  end;
end;
function GetExpressionCore(const prec: Integer): TOperand; //inline;
//Generador de Algoritmos de Evaluacion de expresiones.
//Esta es la función más importante del compilador
var
  Op1, Op2  : TOperand;   //Operandos
  opr1: TOperator;  //Operadores
begin
  Op1.catTyp:=t_integer;    //asumir opcion por defecto
  Op2.catTyp:=t_integer;   //asumir opcion por defecto
  pErr.Limpiar;
  //----------------coger primer operando------------------
  Op1 := GetOperand; if pErr.HayError then exit;
  debugln(space(ExprLevel)+' Op1='+Op1.txt);
  //verifica si termina la expresion
  opr1 := Op1.GetOperator;
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
  //inicia secuencia de lectura: <Operador> <Operando>
  while opr1<>nil do begin
    //¿Delimitada por precedencia?
    If opr1.jer <= prec Then begin  //es menor que la que sigue, expres.
      Result := Op1;  //solo devuelve el único operando que leyó
      exit;
    End;
{    //--------------------coger operador ---------------------------
	//operadores unitarios ++ y -- (un solo operando).
    //Se evaluan como si fueran una mini-expresión o función
	if opr1.id = op_incremento then begin
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
    //--------------------coger segundo operando--------------------
    Op2 := GetOperandP(Opr1.jer);   //toma operando con precedencia
    debugln(space(ExprLevel)+' Op2='+Op2.txt);
    if pErr.HayError then exit;
    //prepara siguiente operación
    Op1 := Evaluar(Op1, opr1, Op2);    //evalua resultado
    if PErr.HayError then exit;
    opr1 := Op1.GetOperator;   {lo toma ahora con el tipo de la evaluación Op1 (opr1) Op2
                                porque puede que Op1 (opr1) Op2, haya cambiado de tipo}
  end;  //hasta que ya no siga un operador
  Result := Op1;  //aquí debe haber quedado el resultado
end;
function GetExpression(const prec: Integer; isParam: boolean = false
    //indep: boolean = false
    ): TOperand;
//Envoltura para GetExpressionCore(). Se coloca así porque GetExpressionCore()
//tiene diversos puntos de salida y Se necesita llamar siempre a expr_end() al
//terminar.
//"isParam" indica que la expresión evaluada es el parámetro de una función.
//"indep", indica que la expresión que se está evaluando es anidada pero es independiente
//de la expresion que la contiene, así que se puede liberar los registros o pila.
{ TODO : Para optimizar debería existir solo GetExpression() y no GetExpressionCore() }
begin
  Inc(ExprLevel);  //cuenta el anidamiento
  debugln(space(ExprLevel)+'>Inic.expr');
  expr_start;  //llama a evento
  Result := GetExpressionCore(prec);
  expr_end(isParam);    //llama al evento de salida
  debugln(space(ExprLevel)+'>Fin.expr');
  Dec(ExprLevel);
  if ExprLevel = 0 then debugln('');
end;
function GetBoolExpression: TOperand;
//Simplifica la evaluación de expresiones booleanas, validadno el tipo
begin
  Result := GetExpression(0);  //evalua expresión
  if PErr.HayError then exit;
  if Result.Typ.cat <> t_boolean then begin
    PErr.GenError('Se esperaba expresión booleana',PosAct);
  end;
end;
{function GetNullExpression(): TOperand;
//Simplifica la evaluación de expresiones sin dar error cuando encuentra algún delimitador
begin
  if
  Result := GetExpression(0);  //evalua expresión
  if PErr.HayError then exit;
end;}
procedure GenTemplCompiler;
//Genera una plantilla de código para implementar este mismo compilador
var
  txt: TStringList;
  i: Integer;
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

{ TOperand }

function TOperand.GetValBool: boolean;
begin
  if estOp = STORED_VAR then  //lee de la variable
    Result := vars[ivar].valBool
  else if estOp = STORED_ACU then  //expresiones están en res
    Result := a.valBool
  else if estOp = STORED_ACUb then  //expresiones están en res
    Result := b.valBool
  else  //debe ser constante o expresión
    Result := cons.valBool;
end;
{procedure TOperand.SetValBol(AValue: boolean);
begin
  if estOp = STORED_VAR then  //solo permite escribir en variable
    vars[ivar].valBol := AValue;
//  else  //es expresión
//    cons.valInt := AValue;
end;}
function TOperand.GetValInt: int64;
begin
  if estOp = STORED_VAR then  //lee de la variable
    Result := vars[ivar].valInt
  else if estOp = STORED_ACU then  //expresiones están en res
    Result := a.valInt
  else if estOp = STORED_ACUB then  //expresiones están en res
    Result := b.valInt
  else  //debe ser constante
    Result := cons.valInt;
end;
{procedure TOperand.SetValInt(AValue: Int64);
begin
  if estOp = STORED_VAR then  //solo permite escribir en variable
    vars[ivar].valInt := AValue;
//  else  //es expresión
//    cons.valInt := AValue;
end;}
function TOperand.GetValFloat: extended;
begin
  if estOp = STORED_VAR then  //lee de la variable
    Result := vars[ivar].valFloat
  else if estOp = STORED_ACU then  //expresiones están en res
    Result := a.valFloat
  else if estOp = STORED_ACUB then  //expresiones están en res
    Result := b.valFloat
  else  //debe ser constante o expresión
    Result := cons.valFloat;
end;
{procedure TOperand.SetValFloat(AValue: extended);
begin
  if estOp = STORED_VAR then  //solo permite escribir en variable
    vars[ivar].valFloat := AValue;
//  else  //es expresión
//    cons.valInt := AValue;
end;}
function TOperand.GetValStr: string;
begin
  if estOp = STORED_VAR then  //lee de la variable
    Result := vars[ivar].valStr
  else if estOp = STORED_ACU then  //expresiones están en A
    Result := a.valStr
  else if estOp = STORED_ACUB then  //expresiones están en B
    Result := b.valStr
  else  //debe ser constante literal
    Result := cons.valStr;
end;
{procedure TOperand.SetValStr(AValue: string);
begin
  if estOp = STORED_VAR then  //solo permite escribir en variable
    vars[ivar].valStr := AValue;
//  else  //es expresión
//    cons.valInt := AValue;
end;}
procedure TOperand.Load;
begin
  //llama al evento de carga
  if typ.procLoad <> nil then typ.procLoad(self);
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
  //hay un operador
  Result := typ.FindOperator(cEnt.tok);
  cEnt.Next;   //toma el token
end;

initialization
  mem := TStringList.Create;
  PErr.IniError;   //inicia motor de errores
  //Crea lista de Contextos
  ConsE := TListaCont.Create(true);  //crea contenedor de Contextos, con control de objetos.
  cEnt := nil;
  //Inicia lista de tipos
  types := TTypes.Create(true);
  //Inicia variables, funciones y constantes
  ClearAllVars;
  ClearAllFuncs;
  ClearAllConst;
  //crea el operador NULL
  nullOper := TOperator.Create;
  //inicia la sintaxis
  xLex := TSynFacilSyn.Create(nil);   //crea lexer
  StartSyntax; //!!!!!Debería hacerse solo una vez al inicio
finalization;
  xLex.Free;
  nullOper.Free;
  types.Free;
  //Limpia lista de Contextos
  ConsE.Free;
  mem.Free;  //libera
end.

