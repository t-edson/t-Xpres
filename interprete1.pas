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
  tipChar : Ttype;
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
function int_asig_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;
  Op1.valInt:=Op2.valInt;  //asigna
end;
function int_asign_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;
  Op1.valInt:=Trunc(Op2.valFloat);
end;
function int_igual_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_boolean;
  Result.typ:=tipBool;
  Result.cons.valBol:=Op1.valInt=Op2.valInt;
end;

function int_suma_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;
  Result.cons.valInt:=Op1.valInt+Op2.valInt;
end;
function int_suma_float(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat:=Op1.valInt+Op2.valFloat;
end;
function int_resta_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;   //devuelve tipInt
  Result.cons.valInt:=Op1.valInt-Op2.valInt;
end;
function int_resta_float(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat:=Op1.valInt-Op2.valFloat;
end;
function int_mult_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;   //devuelve tipInt (se pierde AH)
  Result.cons.valInt:=Op1.valInt*Op2.valInt;
end;
function int_mult_float(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat:=Op1.valInt*Op2.valFloat;
end;
function int_div_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat:=Op1.valInt/Op2.valInt;
end;
function int_div_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat:=Op1.valInt/Op2.valFloat;
end;
function int_idiv_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;   //devuelve tipInt
  Result.cons.valInt:=Op1.valInt div Op2.valInt;
end;
function int_resid_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;   //devuelve tipInt
  Result.cons.valInt:=Op1.valInt mod Op2.valInt;
end;

//operaciones con Flotantes
function float_asig_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  vars[Op1.ivar].valFloat:=Op2.valInt;
end;
function float_asign_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  vars[Op1.ivar].valFloat:=Op2.valFloat;
end;

function float_suma_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat := Op1.valFloat+Op2.valInt;
end;
function float_suma_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat := Op1.valFloat+Op2.valFloat;
end;
function float_resta_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat := Op1.valFloat-Op2.valInt;
end;
function float_resta_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat := Op1.valFloat-Op2.valFloat;
end;
function float_mult_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat := Op1.valFloat*Op2.valInt;
end;
function float_mult_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat := Op1.valFloat*Op2.valFloat;
end;
function float_div_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat := Op1.valFloat / Op2.valInt;
end;
function float_div_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  Result.cons.valFloat := Op1.valFloat / Op2.valFloat;
end;

//operaciones con Char
function char_asig_char(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.catTyp:=t_string;
  Result.typ:=tipChar;
  if Op2.catOp = coVariable then Op2.valInt :=vars[Op2.ivar].valInt;
  vars[Op1.ivar].valFloat:=Op2.valInt;
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
  tipBool :=CreateType('bool',t_boolean,1);  //booleano
  tipInt  :=CreateType('int',t_integer,4);   //de 4 bytes
  tipFloat:=CreateType('float',t_float,4);   //de 4 bytes

  //debe crearse siempre el tipo char para manejar cadenas
  tipChar:=CreateType('char',t_string,1);   //de 1 byte

  //////// Operaciones con Char ////////////
  opr:=tipChar.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipChar,@char_asig_char);

  //////// Operaciones con Int ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=tipInt.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipInt,@int_asig_int);
  opr.CreateOperation(tipFloat,@int_asign_float);

  opr:=tipInt.CreateOperator('=',2,'igual');  //asignación
  opr.CreateOperation(tipInt,@int_igual_int);

  opr:=tipInt.CreateOperator('+',5,'suma');
  opr.CreateOperation(tipInt,@int_suma_int);
  opr.CreateOperation(tipFloat,@int_suma_float);

  opr:=tipInt.CreateOperator('-',5,'resta');
  opr.CreateOperation(tipInt,@int_resta_int);
  opr.CreateOperation(tipFloat,@int_resta_float);

  opr:=tipInt.CreateOperator('*',6,'mult');
  opr.CreateOperation(tipInt,@int_mult_int);
  opr.CreateOperation(tipFloat,@int_mult_float);

  opr:=tipInt.CreateOperator('/',6,'div');
  opr.CreateOperation(tipInt,@int_div_int);
  opr.CreateOperation(tipFloat,@int_div_float);

  opr:=tipInt.CreateOperator('\',6,'idiv');
  opr.CreateOperation(tipInt,@int_idiv_int);
  opr:=tipInt.CreateOperator('%',6,'resid');
  opr.CreateOperation(tipInt,@int_resid_int);

  //////// Operaciones con Float ///////////////
  opr:=tipFloat.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipInt,@float_asig_int);
  opr.CreateOperation(tipFloat,@float_asign_float);

  opr:=tipFloat.CreateOperator('+',5,'suma');
  opr.CreateOperation(tipInt,@float_suma_int);
  opr.CreateOperation(tipFloat,@float_suma_float);

  opr:=tipFloat.CreateOperator('-',5,'resta');
  opr.CreateOperation(tipInt,@float_resta_int);
  opr.CreateOperation(tipFloat,@float_resta_float);

  opr:=tipFloat.CreateOperator('*',6,'mult');
  opr.CreateOperation(tipInt,@float_mult_int);
  opr.CreateOperation(tipFloat,@float_mult_float);

  opr:=tipFloat.CreateOperator('/',6,'idiv');
  opr.CreateOperation(tipInt,@float_div_int);
  opr.CreateOperation(tipFloat,@float_div_float);

  //  tipInt:=CreateType('int16',t_integer,1);
end;

