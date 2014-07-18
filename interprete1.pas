{Interprete del lenguaje Xpres.
Implementado para probar la flexibilidad del compilador Xpres
Este módulo no generará código sino que lo ejecutará directamente.
}
const
  //Constamtes para indicar el estado de los operadores.
  NOLOADED = 0;  //No cargado. Este estado debe existir siempre.
  LOADED = 1;    //Indica que esta cargado en registro. Listo para operar.

var
  //tipos
  tipInt  : TType;   //entero flotante
  tipFloat: Ttype;
  tipBool : Ttype;
  tipStr : Ttype;
  //banderas
  ALused: Boolean;  //indica que el registro Al está siendo usado

procedure Cod_StartData;
//Codifica la parte inicial de declaración de variables estáticas
begin
end;
procedure Cod_StartProgram;
//Codifica la parte inicial del programa
begin
end;
procedure Cod_EndProgram;
//Codifica la parte inicial del programa
begin
end;
procedure expr_start;
//Se ejecuta siempre al StartSyntax el procesamiento de una expresión
begin
//  Code('  ;expres');
end;
procedure expr_end;
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
//  Code('  ;fin expres');
end;
///////// Eventos para la generación de código de evaluación de expresiones ////////
//operaciones con Enteros
//operaciones con Enteros
function int_asig_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.typ:=tipInt;
  Result.cons_valInt:=Op2.valInt;  //para que la asignación devuelva un valor
  vars[Op1.ivar].valInt:=Op2.valInt;  //solo cambia valor de variable
end;
function int_asign_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.typ:=tipInt;
  Result.cons_valInt:=Op2.valInt;  //para que la asignación devuelva un valor
  vars[Op1.ivar].valInt:=Trunc(Op2.valFloat);  //escribe en variable
end;
function int_igual_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valInt=Op2.valInt;
end;
function int_difer_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valInt<>Op2.valInt;
end;
function int_mayor_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valInt>Op2.valInt;
end;
function int_menor_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valInt<Op2.valInt;
end;
function int_mayori_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valInt>=Op2.valInt;
end;
function int_menori_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valInt<=Op2.valInt;
end;

function int_suma_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipInt;
  Result.cons_valInt:=Op1.valInt+Op2.valInt;
end;
function int_suma_float(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat:=Op1.valInt+Op2.valFloat;
end;
function int_resta_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipInt;   //devuelve tipInt
  Result.cons_valInt:=Op1.valInt-Op2.valInt;
end;
function int_resta_float(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat:=Op1.valInt-Op2.valFloat;
end;
function int_mult_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipInt;   //devuelve tipInt (se pierde AH)
  Result.cons_valInt:=Op1.valInt*Op2.valInt;
end;
function int_mult_float(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat:=Op1.valInt*Op2.valFloat;
end;
function int_div_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat:=Op1.valInt/Op2.valInt;
end;
function int_div_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat:=Op1.valInt/Op2.valFloat;
end;
function int_idiv_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipInt;   //devuelve tipInt
  Result.cons_valInt:=Op1.valInt div Op2.valInt;
end;
function int_resid_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipInt;   //devuelve tipInt
  Result.cons_valInt:=Op1.valInt mod Op2.valInt;
end;

//operaciones con Flotantes
function float_asig_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.typ:=tipFloat;
  Result.cons_valFloat:=Op2.valInt;  //para que devuelva valor
  vars[Op1.ivar].valFloat:=Op2.valInt;
end;
function float_asign_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.typ:=tipFloat;
  Result.cons_valFloat:=Op2.valFloat;  //para que devuelva valor
  vars[Op1.ivar].valFloat:=Op2.valFloat;
end;
function float_igual_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valFloat=Op2.valFloat;
end;
function float_difer_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valFloat<>Op2.valFloat;
end;
function float_mayor_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valFloat>Op2.valFloat;
end;
function float_menor_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valFloat<Op2.valFloat;
end;
function float_mayori_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valFloat>=Op2.valFloat;
end;
function float_menori_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valFloat<=Op2.valFloat;
end;

function float_suma_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat := Op1.valFloat+Op2.valInt;
end;
function float_suma_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat := Op1.valFloat+Op2.valFloat;
end;
function float_resta_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat := Op1.valFloat-Op2.valInt;
end;
function float_resta_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat := Op1.valFloat-Op2.valFloat;
end;
function float_mult_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat := Op1.valFloat*Op2.valInt;
end;
function float_mult_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat := Op1.valFloat*Op2.valFloat;
end;
function float_div_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat := Op1.valFloat / Op2.valInt;
end;
function float_div_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipFloat;
  Result.cons_valFloat := Op1.valFloat / Op2.valFloat;
end;
//operaciones con booleanos
function bool_asig_bool(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.typ:=tipBool;
  Result.cons_valBool:=Op2.valBool;  //para que devuelva valor
  vars[Op1.ivar].valBool:=Op2.valBool;
end;
function bool_igual_bool(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valBool=Op2.valBool;
end;
function bool_difer_bool(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valBool<>Op2.valBool;
end;
function bool_and_bool(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valBool and Op2.valBool;
end;
function bool_or_bool(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valBool or Op2.valBool;
end;
function bool_xor_bool(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valBool xor Op2.valBool;
end;

//operaciones con string
function str_asig_str(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.typ:=tipStr;
  Result.cons_valStr:=Op2.valStr;  //para que devuelva valor
  vars[Op1.ivar].valStr:=Op2.valStr;
end;
function str_igual_str(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valStr=Op2.valStr;
end;
function str_difer_str(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipBool;
  Result.cons_valBool:=Op1.valStr<>Op2.valStr;
end;
function str_concat_str(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipStr;
  Result.cons_valStr:=Op1.valStr+Op2.valStr;
end;


procedure StartSyntax(lex0: TSynFacilSyn);
var
  opr: TOperator;
begin
  lex := lex0;    //asigna lexer
  //guarda referencia a los atributos
  tkIdentif := lex.tkIdentif;
  tkKeyword := lex.tkKeyword;
  tkNumber := lex.tkNumber;
  tkString   := lex.tkString;
  tkOperator := lex.GetAttribByName('Operator');  //se debe haber creado
  tkDelimiter:= lex.GetAttribByName('Delimiter'); //se debe haber creado
  tkType     := lex.GetAttribByName('Types');    //se debe haber creado
  tkBoolean  := lex.GetAttribByName('Boolean');   //se debe haber creado
  tkOthers   := lex.GetAttribByName('Others');   //se debe haber creado

  ///////////Crea tipos y operaciones
  ClearTypes;
  tipInt  :=CreateType('int',t_integer,4);   //de 4 bytes
  tipFloat:=CreateType('float',t_float,4);   //de 4 bytes
  tipBool :=CreateType('bool',t_boolean,1);  //booleano
  //debe crearse siempre el tipo char o string para manejar cadenas
//  tipStr:=CreateType('char',t_string,1);   //de 1 byte
  tipStr:=CreateType('string',t_string,-1);   //de longitud variable

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
end;

