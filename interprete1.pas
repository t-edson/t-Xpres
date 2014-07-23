{Implementación de un interprete sencillo para el lenguaje Xpres.
Implementado para probar la flexibilidad del compilador Xpres
Este módulo no generará código sino que lo ejecutará directamente.
}
const
  //////// Constantes para indicar el estado de los operadores //////////
  //Este estados debe existir siempre.
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
//  	catTyp  : tTipDato; //Categoría de Tipo de dato
    //valores de la variable.
    valFloat: extended; //Valor en caso de que sea un flotante
    valInt  : Int64;    //valor en caso de que sea un entero
    valUInt : Int64;    //valor en caso de que sea un entero sin signo
    valBool  : Boolean;  //valor  en caso de que sea un booleano
    valStr  : string;     //valor  en caso de que sea una cadena
  end;

var
  /////// Tipos de datos del lenguaje ////////////
  tipInt  : TType;   //entero flotante
  tipFloat: Ttype;
  tipBool : Ttype;
  tipStr : Ttype;
  ////////// Registros virtuales ////////////
  {la arquitectura definida aquí contempla dos únicos registros generales A y B. Cada
  registro se modela como un registro, con propiedades adicionales, "used".

  * Todas las oepraciones recibe sus dos paráemtros en las variables p1 y p2.
  * El resultado de cualquier expresión se debe dejar indicado en el objeto "res".
  * Los valores enteros y enteros sin signo se cargan en valInt
  * Los valores flotantes  se cargan en valFloat
  * Los valores booleanos se cargan en valBool
  * Los valores string se cargan en valStr
  * Las variables están mapeadas en el arreglo vars[]
  * Cada variable, de cualquier tipo, ocupa una celda de vars[]

  Los procedimientos de operaciones, deben actualizar en el acumulador:

  * El tipo de resultado (para poder evaluar la expresión completa como si fuera un
  operando nuevo)
  * La categoría del operador (constante, expresión, etc), para poder optimizar la generación
  de código.
  * El estado del registro (usado o libre)
   }
  a, b: Tregister;
  //banderas
//  ALused: Boolean;  //indica que el registro Al está siendo usado

function canUseA(const Op1: TOperand; const Op2: TOperand): boolean;
//Indica si se puede usar el acumulador A para guardar un resultado. Si se puede
//devuelve TRUE, si no se puede genera error y devuelve FALSE.
begin
  if not a.used or
     ((Op1.estOp = STORED_ACU) or (Op2.estOp = STORED_ACU)) then begin
    Result := true;  //se puede usar
  end else begin
//    Perr.GenError('No se puede compilar expresión.', PosAct);
    Result := false;
  end;
end;
function canUseB(const Op1: TOperand; const Op2: TOperand): boolean;
//Indica si se puede usar el acumulador A para guardar un resultado. Si se puede
//devuelve TRUE, si no se puede genera error y devuelve FALSE.
begin
  if not b.used or
     ((Op1.estOp = STORED_ACUB) or (Op2.estOp = STORED_ACUB)) then begin
    Result := true;  //se puede usar
  end else begin
//    Perr.GenError('No se puede compilar expresión.', PosAct);
    Result := false;
  end;
end;
function setRes(const typ: TType; const estRes: integer): boolean;
//Marca el tipo y estado del registro "res", para que quede como información para
//las rutinas del evaluador de expresiones.
//Si encuentra error, lo geenra y devuelve FALSE.
begin
  Result := true;
//  if res.used then begin
//    Perr.GenError('No se puede compilar expresión.', PosAct);
//    Result := false;
//    exit;
//  end;
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
  if canUseA(p1,p2) then  begin
    if not setRes(tipInt, STORED_ACU) then exit;
    a.valInt:=val;
    Code('A<-' + p1.expres + op + p2.expres);
  end else if canUseB(p1,p2) then begin  //no se puede usar A
    if not setRes(tipInt, STORED_ACUB) then exit;
    b.valInt:=val;
    Code('B<-' + p1.expres + op + p2.expres);
  end else begin
    Perr.GenError('Expresión muy compleja.', PosAct);
    exit;
  end;
end;
procedure LoadAcumFloat(val: extended; op: string);
//Carga en el acumulador(es) un valor float, y genera t-code
begin
  if canUseA(p1,p2) then  begin
    if not setRes(tipInt, STORED_ACU) then exit;
    a.valFloat:=val;
    Code('A<-' + p1.expres + op + p2.expres);
  end else if canUseB(p1,p2) then begin  //no se puede usar A
    if not setRes(tipInt, STORED_ACUB) then exit;
    b.valFloat:=val;
    Code('B<-' + p1.expres + op + p2.expres);
  end else begin
    Perr.GenError('Expresión muy compleja.', PosAct);
    exit;
  end;
end;
procedure LoadAcumBool(val: boolean; op: string);
//Carga en el acumulador(es) un valor booleano, y genera t-code
begin
  if canUseA(p1,p2) then  begin
    if not setRes(tipInt, STORED_ACU) then exit;
    a.valBool:=val;
    Code('A<-' + p1.expres + op + p2.expres);
  end else if canUseB(p1,p2) then begin  //no se puede usar A
    if not setRes(tipInt, STORED_ACUB) then exit;
    b.valBool:=val;
    Code('B<-' + p1.expres + op + p2.expres);
  end else begin
    Perr.GenError('Expresión muy compleja.', PosAct);
    exit;
  end;
end;
procedure LoadAcumStr(val: string; op: string);
//Carga en el acumulador(es) un valor booleano, y genera t-code
begin
  if canUseA(p1,p2) then  begin
    if not setRes(tipInt, STORED_ACU) then exit;
    a.valStr:=val;
    Code('A<-' + p1.expres + op + p2.expres);
  end else if canUseB(p1,p2) then begin  //no se puede usar A
    if not setRes(tipInt, STORED_ACUB) then exit;
    b.valStr:=val;
    Code('B<-' + p1.expres + op + p2.expres);
  end else begin
    Perr.GenError('Expresión muy compleja.', PosAct);
    exit;
  end;
end;
////////////rutinas obligatorias
procedure Cod_StartData;
//Codifica la parte inicial de declaración de variables estáticas
begin
  Code('.DATA');
end;
procedure Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  Code('.CODE');   //inicia la sección de código
end;
procedure Cod_EndProgram;
//Codifica la parte inicial del programa
begin
  Code('.END');   //inicia la sección de código
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
//    if ALused then  //hay un dato evaluado
//      Code('  push al');  //lo guarda
  end;
end;
procedure expr_end;
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
  if exprLevel = 1 then begin  //el último nivel
    Code('  ;fin expres');
  end;
end;
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

////////////operaciones con Enteros
procedure int_procLoad(var Op: TOperand);
begin
  //carga el operando en res
  if not setRes(tipInt,Op.estOp) then exit;
  if Op.estOp = STORED_LIT then res.cons.valInt := Op.valInt;
//  a.valInt:=Op.valInt;
///  Code('A<-' + Op.expres);
end;
procedure int_asig_int;
begin
  if p1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  //en la VM se puede mover directamente res memoria sin usar el registro res
  vars[p1.ivar].valInt:=p2.valInt;
//  res.used:=false;  //No hay obligación de que la asignación devuelva un valor.
  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;
procedure int_asign_float;
begin
  if p1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  //en la VM se puede mover directamente res memoria sin usar el registro res
  vars[p1.ivar].valInt:=Trunc(p2.valFloat);  //en la VM se puede mover directamente
//  res.used:=false;  //No hay obligación de que la asignación devuelva un valor.
  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;

procedure int_igual_int;
begin
  LoadAcumBool(p1.valInt=p2.valInt,'=');
end;
procedure int_difer_int;
begin
  LoadAcumBool(p1.valInt<>p2.valInt,'<>');
end;
procedure int_mayor_int;
begin
  LoadAcumBool(p1.valInt>p2.valInt,'>');
end;
procedure int_menor_int;
begin
  LoadAcumBool(p1.valInt<p2.valInt,'<');
end;
procedure int_mayori_int;
begin
  LoadAcumBool(p1.valInt>=p2.valInt,'>=');
end;
procedure int_menori_int;
begin
  LoadAcumBool(p1.valInt<=p2.valInt,'<=');
end;

procedure int_suma_int;
begin
  if (p1.catOp = coConst) and (p2.catOp = coConst) then begin
    //es una operación con constantes, se optimiza evaluando primero

  end;
  LoadAcumInt(p1.valInt+p2.valInt,'+');
end;
procedure int_suma_float;
begin
  LoadAcumFloat(p1.valInt+p2.valFloat,'+');
end;
procedure int_resta_int;
begin
  LoadAcumInt(p1.valInt-p2.valInt,'-');
end;
procedure int_resta_float;
begin
  LoadAcumFloat(p1.valInt-p2.valFloat,'-');
end;
procedure int_mult_int;
begin
  LoadAcumInt(p1.valInt*p2.valInt,'*');
end;
procedure int_mult_float;
begin
  LoadAcumFloat(p1.valInt*p2.valFloat,'*');
end;
procedure int_div_int;
begin
  LoadAcumFloat(p1.valInt/p2.valInt,'/');
end;
procedure int_div_float;
begin
  LoadAcumFloat(p1.valInt/p2.valFloat,'/');
end;
procedure int_idiv_int;
begin
  LoadAcumInt(p1.valInt div p2.valInt,'\');
end;
procedure int_resid_int;
begin
  LoadAcumInt(p1.valInt mod p2.valInt,'%');
end;

////////////operaciones con Flotantes
procedure float_procLoad(var Op: TOperand);
begin
  //carga el operando en res
  if not setRes(tipFloat,Op.estOp) then exit;
  if Op.estOp = STORED_LIT then res.cons.valFloat := Op.valFloat;
//  a.valInt:=Op.valInt;
///  Code('A<-' + Op.expres);
end;
procedure float_asig_int;
begin
  if p1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  //en la VM se puede mover directamente res memoria sin usar el registro res
  vars[p1.ivar].valFloat:=p2.valInt;
//  res.used:=false;  //No hay obligación de que la asignación devuelva un valor.
  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;
procedure float_asign_float;
begin
  if p1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  //en la VM se puede mover directamente res memoria sin usar el registro res
  vars[p1.ivar].valFloat:=p2.valFloat;
//  res.used:=false;  //No hay obligación de que la asignación devuelva un valor.
  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;

procedure float_igual_float;
begin
  LoadAcumBool(p1.valFloat=p2.valFloat,'=');
end;
procedure float_difer_float;
begin
  LoadAcumBool(p1.valFloat<>p2.valFloat,'<>');
end;
procedure float_mayor_float;
begin
  LoadAcumBool(p1.valFloat>p2.valFloat,'>');
end;
procedure float_menor_float;
begin
  LoadAcumBool(p1.valFloat<p2.valFloat,'<');
end;
procedure float_mayori_float;
begin
  LoadAcumBool(p1.valFloat>=p2.valFloat,'>=');
end;
procedure float_menori_float;
begin
  LoadAcumBool(p1.valFloat<=p2.valFloat,'<=');
end;

procedure float_suma_int;
begin
  LoadAcumFloat(p1.valFloat+p2.valInt,'+');;
end;
procedure float_suma_float;
begin
  LoadAcumFloat(p1.valFloat+p2.valFloat,'+');
end;
procedure float_resta_int;
begin
  LoadAcumFloat(p1.valFloat-p2.valInt,'-');
end;
procedure float_resta_float;
begin
  LoadAcumFloat(p1.valFloat-p2.valFloat,'-');
end;
procedure float_mult_int;
begin
  LoadAcumFloat(p1.valFloat*p2.valInt,'*');
end;
procedure float_mult_float;
begin
  LoadAcumFloat(p1.valFloat*p2.valFloat,'*');
end;
procedure float_div_int;
begin
  LoadAcumFloat(p1.valFloat / p2.valInt,'/');;
end;
procedure float_div_float;
begin
  LoadAcumFloat(p1.valFloat / p2.valFloat,'/');
end;

////////////operaciones con booleanos
procedure bool_procLoad(var Op: TOperand);
begin
  //carga el operando en res
  if not setRes(tipBool,Op.estOp) then exit;
  if Op.estOp = STORED_LIT then res.cons.valBool := Op.valBool;
//  a.valInt:=Op.valInt;
///  Code('A<-' + Op.expres);
end;
procedure bool_asig_bool;
begin
  if p1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  //en la VM se puede mover directamente res memoria sin usar el registro res
  vars[p1.ivar].valBool:=p2.valBool;
//  res.used:=false;  //No hay obligación de que la expresión devuelva un valor.
  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;

procedure bool_igual_bool;
begin
  LoadAcumBool(p1.valBool=p2.valBool,'=');
end;
procedure bool_difer_bool;
begin
  LoadAcumBool(p1.valBool<>p2.valBool,'<>');
end;
procedure bool_and_bool;
begin
  LoadAcumBool(p1.valBool and p2.valBool,'and');
end;
procedure bool_or_bool;
begin
  LoadAcumBool(p1.valBool or p2.valBool,'or');
end;
procedure bool_xor_bool;
begin
  LoadAcumBool(p1.valBool xor p2.valBool,'xor');
end;

////////////operaciones con string
procedure str_procLoad(var Op: TOperand);
begin
  //carga el operando en res
  if not setRes(tipStr,Op.estOp) then exit;
  if Op.estOp = STORED_LIT then res.cons.valStr := Op.valStr;
//  a.valInt:=Op.valInt;
///  Code('A<-' + Op.expres);
end;
procedure str_asig_str;
begin
  if p1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  //en la VM se puede mover directamente res memoria sin usar el registro res
  vars[p1.ivar].valStr:=p2.valStr;
//  res.used:=false;  //No hay obligación de que la expresión devuelva un valor.
  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;

procedure str_igual_str;
begin
  LoadAcumBool(p1.valStr=p2.valStr,'=');
end;
procedure str_difer_str;
begin
  LoadAcumBool(p1.valStr<>p2.valStr,'<>');
end;
procedure str_concat_str;
begin
  LoadAcumStr(p1.valStr+p2.valStr,'+');
end;

//funciones básicas
procedure fun_msgbox;
begin
  msgbox('eureka');
  res.estOp:=0;
end;
procedure fun_puts;
//envia un texto a consola
begin
  frmOut.puts('eureka');
  res.estOp:=0;
end;

procedure StartSyntax;
var
  opr: TOperator;
begin
  ///////////define la sintaxis del compilador
  //crea y guarda referencia a los atributos
  tkIdentif  := xLex.tkIdentif;
  tkKeyword  := xLex.tkKeyword;
  tkNumber   := xLex.tkNumber;
  tkString   := xLex.tkString;
  tkOperator := xLex.NewTokType('Operador');  //personalizado
  tkDelimiter:= xLex.NewTokType('Delimiter'); //personalizado
  tkType     := xLex.NewTokType('Types');    //personalizado
  tkBoolean  := xLex.NewTokType('Boolean');   //personalizado
  tkStruct   := xLex.NewTokType('Struct');   //personalizado
  tkOthers   := xLex.NewTokType('Others');   //personalizado
  //inicia la configuración
  xLex.ClearMethodTables;           //limpìa tabla de métodos
  xLex.ClearSpecials;               //para empezar a definir tokens
  //crea tokens por contenido
  xLex.DefTokIdentif('[$A..Za..z_]', 'A..Za..z0..9_');
  xLex.DefTokContent('[0..9]', '0..9.', '', tkNumber);
  if xLex.Err<>'' then ShowMessage(xLex.Err);
  //define palabras claves
  xLex.AddIdentSpecList('THEN ELSE ELSIF END var type', tkKeyword);
  xLex.AddIdentSpecList('program public private method const', tkKeyword);
  xLex.AddIdentSpecList('class create destroy sub do begin', tkKeyword);
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
  xLex.AddSymbSpec(';', tkDelimiter);
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
  tipFloat:=CreateType('float',t_float,4);   //de 4 bytes
  tipFloat.procLoad:=@float_procLoad;
  tipBool :=CreateType('bool',t_boolean,1);  //booleano
  tipBool.procLoad:=@bool_procLoad;
  //debe crearse siempre el tipo char o string para manejar cadenas
//  tipStr:=CreateType('char',t_string,1);   //de 1 byte
  tipStr:=CreateType('string',t_string,-1);   //de longitud variable
  tipStr.procLoad:=@str_procLoad;

  //////// Operaciones con String ////////////
  opr:=tipStr.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipStr,@str_asig_str);
  opr:=tipStr.CreateOperator('=',5,'igual');  //asignación
  opr.CreateOperation(tipStr,@str_igual_str);
  opr:=tipStr.CreateOperator('<>',5,'diferente');  //asignación
  opr.CreateOperation(tipStr,@str_difer_str);
  opr:=tipStr.CreateOperator('+',7,'concat');
  opr.CreateOperation(tipStr,@str_concat_str);

  //////// Operaciones con Int ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=tipInt.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipInt,@int_asig_int);
  opr.CreateOperation(tipFloat,@int_asign_float);

  opr:=tipInt.CreateOperator('=',5,'igual');  //asignación
  opr.CreateOperation(tipInt,@int_igual_int);
  opr:=tipInt.CreateOperator('<>',5,'diferente');  //asignación
  opr.CreateOperation(tipInt,@int_difer_int);
  opr:=tipInt.CreateOperator('>',5,'mayor');  //asignación
  opr.CreateOperation(tipInt,@int_mayor_int);
  opr:=tipInt.CreateOperator('<',5,'menor');  //asignación
  opr.CreateOperation(tipInt,@int_menor_int);
  opr:=tipInt.CreateOperator('>=',5,'mayor');  //asignación
  opr.CreateOperation(tipInt,@int_mayori_int);
  opr:=tipInt.CreateOperator('<=',5,'menor');  //asignación
  opr.CreateOperation(tipInt,@int_menori_int);

  opr:=tipInt.CreateOperator('+',7,'suma');
  opr.CreateOperation(tipInt,@int_suma_int);
  opr.CreateOperation(tipFloat,@int_suma_float);

  opr:=tipInt.CreateOperator('-',7,'resta');
  opr.CreateOperation(tipInt,@int_resta_int);
  opr.CreateOperation(tipFloat,@int_resta_float);

  opr:=tipInt.CreateOperator('*',8,'mult');
  opr.CreateOperation(tipInt,@int_mult_int);
  opr.CreateOperation(tipFloat,@int_mult_float);

  opr:=tipInt.CreateOperator('/',8,'div');
  opr.CreateOperation(tipInt,@int_div_int);
  opr.CreateOperation(tipFloat,@int_div_float);

  opr:=tipInt.CreateOperator('\',8,'idiv');
  opr.CreateOperation(tipInt,@int_idiv_int);
  opr:=tipInt.CreateOperator('%',8,'resid');
  opr.CreateOperation(tipInt,@int_resid_int);

  //////// Operaciones con Float ///////////////
  opr:=tipFloat.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipInt,@float_asig_int);
  opr.CreateOperation(tipFloat,@float_asign_float);

  opr:=tipFloat.CreateOperator('=',5,'igual');  //asignación
  opr.CreateOperation(tipFloat,@float_igual_float);
  opr:=tipFloat.CreateOperator('<>',5,'diferente');  //asignación
  opr.CreateOperation(tipFloat,@float_difer_float);
  opr:=tipFloat.CreateOperator('>',5,'mayor');  //asignación
  opr.CreateOperation(tipFloat,@float_mayor_float);
  opr:=tipFloat.CreateOperator('<',5,'menor');  //asignación
  opr.CreateOperation(tipFloat,@float_menor_float);
  opr:=tipFloat.CreateOperator('>=',5,'mayor');  //asignación
  opr.CreateOperation(tipFloat,@float_mayori_float);
  opr:=tipFloat.CreateOperator('<=',5,'menor');  //asignación
  opr.CreateOperation(tipFloat,@float_menori_float);

  opr:=tipFloat.CreateOperator('+',7,'suma');
  opr.CreateOperation(tipInt,@float_suma_int);
  opr.CreateOperation(tipFloat,@float_suma_float);

  opr:=tipFloat.CreateOperator('-',7,'resta');
  opr.CreateOperation(tipInt,@float_resta_int);
  opr.CreateOperation(tipFloat,@float_resta_float);

  opr:=tipFloat.CreateOperator('*',8,'mult');
  opr.CreateOperation(tipInt,@float_mult_int);
  opr.CreateOperation(tipFloat,@float_mult_float);

  opr:=tipFloat.CreateOperator('/',8,'idiv');
  opr.CreateOperation(tipInt,@float_div_int);
  opr.CreateOperation(tipFloat,@float_div_float);

  //////// Operaciones con Booelan ////////////
  opr:=tipBool.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipBool,@bool_asig_bool);
  opr:=tipBool.CreateOperator('=',5,'igual');  //asignación
  opr.CreateOperation(tipBool,@bool_igual_bool);
  opr:=tipBool.CreateOperator('<>',5,'diferente');  //asignación
  opr.CreateOperation(tipBool,@bool_difer_bool);
  opr:=tipBool.CreateOperator('AND',3,'and');  //asignación
  opr.CreateOperation(tipBool,@bool_and_bool);
  opr:=tipBool.CreateOperator('OR',3,'or');  //asignación
  opr.CreateOperation(tipBool,@bool_or_bool);
  opr:=tipBool.CreateOperator('XOR',3,'xor');  //asignación
  opr.CreateOperation(tipBool,@bool_xor_bool);
//  opr:=tipBool.CreateOperator('NOT',2,'not');  //asignación
//  opr.CreateOperation(tipBool,@bool_and_bool);

//////// Funciones básicas ////////////
  CreateSysFunction('MsgBox', tipInt, @fun_msgbox);
  CreateSysFunction('puts', tipInt, @fun_puts);
end;

