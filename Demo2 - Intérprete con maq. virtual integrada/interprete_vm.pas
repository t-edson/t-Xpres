{Implementación de un interprete sencillo para el lenguaje Xpres.
Este módulo no generará código sino que lo ejecutará directamente.
Este intérprete, solo reconoce tipos de datos enteros y de cadena.
Para los enteros solo implementa la asignación(:=), sumas(+) y restas(-) y
para las cadenas se implementa la asignación(:=) y la concatenación(+)
Se pueden declarar y asignar variables.


En este archivo, se pueden declarar tipos, variables, constantes,
procedimientos y funciones. Hay rutinas obligatorias que siempre se deben
implementar.

Solo se define una función puts(), que muestra un texto en pantalla.

Este intérprete, se implementa a modo de "máquina virtual integrada", que
tiene dos registros físicos A y B y una pila. Cada registro se modela
como un RECORD, con propiedades adicionales como "used".

* Todas las operaciones recibe sus dos parámetros en las variables p1 y p2.
* El resultado de cualquier expresión se debe dejar indicado en el objeto "res".
* Los valores enteros y enteros sin signo se cargan en valInt
* Los valores string se cargan en valStr
* Las variables están mapeadas en el arreglo vars[]
* Cada variable, de cualquier tipo, ocupa una celda de vars[]
* La pila se usa para pasar los parámetros de las funciones.

Los procedimientos de operaciones, deben actualizar en el acumulador:

* El tipo de resultado (para poder evaluar la expresión completa como si fuera un
operando nuevo)
* La categoría del operador (constante, expresión, etc), para poder optimizar la generación
de código.
* El estado del registro (usado o libre)

Por Tito Hinostroza  30/07/2014
}

const
  //////// Constantes para indicar el estado de los operadores //////////
  //Estos estados deben existir siempre.
  NO_STORED = 0;  //No cargado.
  STORED_LIT = 1; //El resultado es una constante. Está cargado directamente.
  STORED_VAR = 2; //Indica que el resultado está en memoria, en una variable.
  //Estos estados pueden variar de acuerdo a la arquitectura
  STORED_ACU = 5; //Indica que el resultado esta cargado en el registro.
  STORED_ACUB = 6; //Indica que el resultado esta cargado en el registro.
type
  //define un registro virtual para implementar un intérprete
  Tregister = record
    used    : boolean;  //indica si está usado
    typ     : Ttype;    //tipo de dato
    catOp   : CatOperan;  //categoría de operando
    //valores de la variable.
    valFloat: extended; //Valor en caso de que sea un flotante
    valInt  : Int64;    //valor en caso de que sea un entero
    valUInt : Int64;    //valor en caso de que sea un entero sin signo
    valBool  : Boolean;  //valor  en caso de que sea un booleano
    valStr  : string;     //valor  en caso de que sea una cadena
  end;

const STACK_SIZE = 32;
var
  /////// Tipos de datos del lenguaje ////////////
  tipInt  : TType;   //entero flotante
  tipStr : Ttype;
  // Registros virtuales
  a, b: Tregister;
  //pila virtual
  sp: integer;  //puntero de pila
  stack: array[0..STACK_SIZE-1] of Tregister;

function setRes(const typ: TType; const estRes: integer): boolean;
//Marca el tipo y estado del registro "res", para que quede como información para
//las rutinas del evaluador de expresiones.
//Si encuentra error, lo geenra y devuelve FALSE.
begin
  Result := true;
  res.typ := typ;
//  res.catOp:=cat;
  res.estOp:=estRes;   //indica el estado del resultado
  if estRes = STORED_ACU then  begin //si va a dejar el resultado en el acumulador
    a.used:=true;  //marca para saber que ahí está
  end;
  if estRes = STORED_ACUB then  begin //si va a dejar el resultado en el acumulador
    b.used:=true;  //marca para saber que ahí está
  end;
end;
procedure LoadAcumInt(val: int64; op: string);
//Carga en el acumulador(es) un valor entero, y genera t-code
begin
    setRes(tipInt, STORED_ACU);
    a.valInt:=val;
    Code('A<-' + p1.expres + op + p2.expres);
end;
procedure LoadAcumStr(val: string; op: string);
//Carga en el acumulador(es) un valor booleano, y genera t-code
begin
    setRes(tipStr, STORED_ACU);
    a.valStr:=val;
    Code('A<-' + p1.expres + op + p2.expres);
end;
procedure PushResult;
//Coloca el resultado de una expresión en la pila
begin
  if sp>=STACK_SIZE then begin
    Perr.GenError('Desborde de pila.', PosAct);
    exit;
  end;
  stack[sp].typ := res.typ;
  case res.Typ.cat of
  t_string:  stack[sp].valStr  := res.GetValStr;
  t_integer: stack[sp].valInt  := res.GetValInt;
  end;
  Inc(sp);
end;
procedure PopResult;
//Reduce el puntero de pila, de modo que queda apuntando al último dato agregado
begin
  if sp<=0 then begin
    Perr.GenError('Desborde de pila.', PosAct);
    exit;
  end;
  Dec(sp);
end;
////////////rutinas obligatorias
procedure Cod_StartData;
//Codifica la parte inicial de declaración de variables estáticas
begin
end;
procedure Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  sp := 0;  //inicia pila
end;
procedure Cod_EndProgram;
//Codifica la parte inicial del programa
begin
end;
procedure expr_start;
//Se ejecuta siempre al StartSyntax el procesamiento de una expresión
begin
  if exprLevel=1 then begin //es el primer nivel
    Code('  ;expres');
    a.used:=false;   //inicia con el registro libre
    b.used:=false;
    res.estOp:=NO_STORED;  //indica que no tiene nada almacenado
    res.typ := tipInt;   //le pone un tipo por defecto
  end else begin  //es recursivo

  end;
end;
procedure expr_end(isParam: boolean);
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
  if isParam then begin
    //Se terminó de evaluar un parámetro
    PushResult;   //pone parámetro en pila
    if Perr.HayError then exit;
    a.used:=false;  //se libera registro
    b.used:=false;  //se libera registro
  end;
  if exprLevel = 1 then begin  //el último nivel
    Code('  ;fin expres');
  end;
end;
///// Métodos de TOperand, que se deben implementar cuando se define un intérprete /////
function TOperand.expres: string;
//Devuelve una cadena con un texto que representa el valor del operador. Depende de los
//estados de los oepradores que se haya definido.
begin
  case estOp of
//  NO_STORED:
  STORED_LIT: Result := typ.name + '(' + txt + ')';
  STORED_VAR: Result := typ.name + '(vars[' + IntToStr(ivar) + '])';
  STORED_ACU: Result := typ.name + '(A)';
  STORED_ACUB: Result := typ.name + '(B)';
  else Result := '???'
  end;
end;
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

////////////operaciones con Enteros
procedure int_procLoad(var Op: TOperand);
begin
  //carga el operando en res
  setRes(tipInt,Op.estOp);
  if Op.estOp = STORED_LIT then res.cons.valInt := Op.GetValInt;
//  a.valInt:=Op.valInt;
///  Code('A<-' + Op.expres);
end;
procedure int_asig_int;
begin
  if p1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  //en la VM se puede mover directamente res memoria sin usar el registro res
  vars[p1.ivar].valInt:=p2.GetValInt;
//  res.used:=false;  //No hay obligación de que la asignación devuelva un valor.
  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;

procedure int_suma_int;
begin
  LoadAcumInt(p1.GetValInt+p2.GetValInt,'+');
end;
procedure int_resta_int;
begin
  LoadAcumInt(p1.GetValInt-p2.GetValInt,'-');
end;

////////////operaciones con string
procedure str_procLoad(var Op: TOperand);
begin
  //carga el operando en res
  setRes(tipStr,Op.estOp);
  if Op.estOp = STORED_LIT then res.cons.valStr := Op.GetValStr;
//  a.valInt:=Op.valInt;
///  Code('A<-' + Op.expres);
end;
procedure str_asig_str;
begin
  if p1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  //en la VM se puede mover directamente res memoria sin usar el registro res
  vars[p1.ivar].valStr:=p2.GetValStr;
//  res.used:=false;  //No hay obligación de que la expresión devuelva un valor.
  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;

procedure str_concat_str;
begin
  LoadAcumStr(p1.GetValStr+p2.GetValStr,'+');
end;

//funciones básicas
procedure fun_puts;
//envia un texto a consola
begin
  PopResult;  //saca parámetro 1
  if Perr.HayError then exit;
  frmOut.puts(stack[sp].valStr);  //sabemos que debe ser String
  //el tipo devuelto lo fijará el framework, al tipo definido
  res.estOp:=STORED_LIT;  //el valor devuelto no importa, pero debe devolver algo
end;

procedure StartSyntax;
//Se ejecuta solo una vez al inicio
var
  opr: TOperator;
  f: integer;  //índice para funciones
begin
  ///////////define la sintaxis del compilador
  //crea y guarda referencia a los atributos
  tkIdentif  := xLex.tkIdentif;
  tkKeyword  := xLex.tkKeyword;
  tkKeyword.Foreground:=clGreen;
  tkKeyword.Style := [fsBold];     //en negrita
  tkNumber   := xLex.tkNumber;
  tkString   := xLex.tkString;
  //personalizados
  tkOperator := xLex.NewTokType('Operador'); //personalizado
  tkExpDelim := xLex.NewTokType('ExpDelim');//delimitador de expresión ";"
  tkBlkDelim := xLex.NewTokType('BlkDelim'); //delimitador de bloque
  tkBlkDelim.Foreground:=clGreen;
  tkBlkDelim.Style := [fsBold];     //en negrita
  tkType     := xLex.NewTokType('Types');    //personalizado
  tkBoolean  := xLex.NewTokType('Boolean');  //personalizado
  tkStruct   := xLex.NewTokType('Struct');   //personalizado
  tkStruct.Foreground:=clGreen;
  tkStruct.Style := [fsBold];     //en negrita
  tkOthers   := xLex.NewTokType('Others');   //personalizado
  //inicia la configuración
  xLex.ClearMethodTables;           //limpìa tabla de métodos
  xLex.ClearSpecials;               //para empezar a definir tokens
  //crea tokens por contenido
  xLex.DefTokIdentif('[$A..Za..z_]', 'A..Za..z0..9_');
  xLex.DefTokContent('[0..9]', '0..9.', '', tkNumber);
  if xLex.Err<>'' then ShowMessage(xLex.Err);
  //define palabras claves
  xLex.AddIdentSpecList('var type program begin', tkKeyword);
  xLex.AddIdentSpecList('end else elsif', tkBlkDelim);
  xLex.AddIdentSpecList('true false', tkBoolean);
  xLex.AddIdentSpecList('int string', tkType);
  //símbolos especiales
  xLex.AddSymbSpec('+',  tkOperator);
  xLex.AddSymbSpec('-',  tkOperator);
  xLex.AddSymbSpec('*',  tkOperator);
  xLex.AddSymbSpec('/',  tkOperator);
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
  xLex.DefTokDelim('/*','*/', xLex.tkComment, tdMulLin);
  //define bloques de sintaxis
  xLex.AddBlock('{','}');
  xLex.Rebuild;   //es necesario para terminar la definición

  ///////////Crea tipos y operaciones
  ClearTypes;
  tipInt  :=CreateType('int',t_integer,4);   //de 4 bytes
  tipInt.procLoad:=@int_procLoad;
  //debe crearse siempre el tipo char o string para manejar cadenas
//  tipStr:=CreateType('char',t_string,1);   //de 1 byte
  tipStr:=CreateType('string',t_string,-1);   //de longitud variable
  tipStr.procLoad:=@str_procLoad;

  //////// Operaciones con String ////////////
  opr:=tipStr.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipStr,@str_asig_str);
  opr:=tipStr.CreateOperator('+',7,'concat');
  opr.CreateOperation(tipStr,@str_concat_str);

  //////// Operaciones con Int ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=tipInt.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipInt,@int_asig_int);

  opr:=tipInt.CreateOperator('+',7,'suma');
  opr.CreateOperation(tipInt,@int_suma_int);

  opr:=tipInt.CreateOperator('-',7,'resta');
  opr.CreateOperation(tipInt,@int_resta_int);

  //////// Funciones básicas ////////////
  f := CreateSysFunction('puts', tipInt, @fun_puts);
  CreateParam(f,'',tipStr);
end;

