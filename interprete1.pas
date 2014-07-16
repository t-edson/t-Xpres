{Interprete del lenguaje Xpres.
Implementado para probar la flexibilida del compilador Xpres
Este módulo no generará código sino que lo ejecutará directamente.
}
const
  //Constamtes para indicar el estado de los operadores.
  NOLOADED = 0;  //No cargado. Este estado debe existir siempre.
  LOADED = 1;    //Indica que esta cargado en registro. Listo para operar.

var
  //tipos
  tipInt: TType;   //entero flotante
  tipFloat: Ttype;
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
procedure int_procDefine(const varName, varInitVal: string);
//Se dispara cuando se declara una variable de este tipo.
begin
end;
procedure int_procLoad(var Op: TOperand);
begin
end;

function int_asig_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;
  if Op2.catOp = coVariable then Op2.valInt :=vars[Op2.ivar].valInt;
  vars[Op1.ivar].valInt:=Op2.valInt;  //asigna
end;
function int_asign_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;
  if Op2.catOp = coVariable then Op2.valFloat :=vars[Op2.ivar].valFloat;
  vars[Op1.ivar].valInt:=Trunc(Op2.valFloat);
end;

function int_suma_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;
  if Op1.catOp = coVariable then Op1.valInt:=vars[Op1.ivar].valInt;
  if Op2.catOp = coVariable then Op2.valInt:=vars[Op2.ivar].valInt;
  Result.valInt:=Op1.valInt+Op2.valInt;
end;
function int_suma_float(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valInt:=vars[Op1.ivar].valInt;
  if Op2.catOp = coVariable then Op2.valFloat:=vars[Op2.ivar].valFloat;
  Result.valFloat:=Op1.valInt+Op2.valFloat;
end;
function int_resta_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;   //devuelve tipInt
  if Op1.catOp = coVariable then Op1.valInt:=vars[Op1.ivar].valInt;
  if Op2.catOp = coVariable then Op2.valInt:=vars[Op2.ivar].valInt;
  Result.valInt:=Op1.valInt-Op2.valInt;
end;
function int_resta_float(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valInt:=vars[Op1.ivar].valInt;
  if Op2.catOp = coVariable then Op2.valFloat:=vars[Op2.ivar].valFloat;
  Result.valFloat:=Op1.valInt-Op2.valFloat;
end;
function int_mult_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;   //devuelve tipInt (se pierde AH)
  if Op1.catOp = coVariable then Op1.valInt:=vars[Op1.ivar].valInt;
  if Op2.catOp = coVariable then Op2.valInt:=vars[Op2.ivar].valInt;
  Result.valInt:=Op1.valInt*Op2.valInt;
end;
function int_mult_float(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valInt:=vars[Op1.ivar].valInt;
  if Op2.catOp = coVariable then Op2.valFloat:=vars[Op2.ivar].valFloat;
  Result.valFloat:=Op1.valInt*Op2.valFloat;
end;
function int_div_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valInt:=vars[Op1.ivar].valInt;
  if Op2.catOp = coVariable then Op2.valInt:=vars[Op2.ivar].valInt;
  Result.valFloat:=Op1.valInt/Op2.valInt;
end;
function int_div_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valInt:=vars[Op1.ivar].valInt;
  if Op2.catOp = coVariable then Op2.valFloat:=vars[Op2.ivar].valFloat;
  Result.valFloat:=Op1.valInt/Op2.valFloat;
end;
function int_idiv_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_integer;
  Result.typ:=tipInt;   //devuelve tipInt
  if Op1.catOp = coVariable then Op1.valInt:=vars[Op1.ivar].valInt;
  if Op2.catOp = coVariable then Op2.valInt:=vars[Op2.ivar].valInt;
  Result.valInt:=Op1.valInt div Op2.valInt;
end;

//operaciones con Flotantes
procedure float_procDefine(const varName, varInitVal: string);
begin

end;
procedure float_procLoad(var Op: TOperand);
begin

end;
function float_asig_int(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op2.catOp = coVariable then Op2.valInt :=vars[Op2.ivar].valInt;
  vars[Op1.ivar].valFloat:=Op2.valInt;
end;
function float_asign_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  if Op1.catOp <> coVariable then begin  //validación
    Perr.GenError('Solo se puede asignar a variable.', PosAct); exit;
  end;
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op2.catOp = coVariable then Op2.valFloat :=vars[Op2.ivar].valFloat;
  vars[Op1.ivar].valFloat:=Op2.valFloat;
end;

function float_suma_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valFloat:=vars[Op1.ivar].valFloat;
  if Op2.catOp = coVariable then Op2.valInt :=vars[Op2.ivar].valInt;
  Result.valFloat := Op1.valFloat+Op2.valInt;
end;
function float_suma_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valFloat:=vars[Op1.ivar].valFloat;
  if Op2.catOp = coVariable then Op2.valFloat:=vars[Op2.ivar].valFloat;
  Result.valFloat := Op1.valFloat+Op2.valFloat;
end;
function float_resta_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valFloat:=vars[Op1.ivar].valFloat;
  if Op2.catOp = coVariable then Op2.valInt :=vars[Op2.ivar].valInt;
  Result.valFloat := Op1.valFloat-Op2.valInt;
end;
function float_resta_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valFloat:=vars[Op1.ivar].valFloat;
  if Op2.catOp = coVariable then Op2.valFloat:=vars[Op2.ivar].valFloat;
  Result.valFloat := Op1.valFloat-Op2.valFloat;
end;
function float_mult_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valFloat:=vars[Op1.ivar].valFloat;
  if Op2.catOp = coVariable then Op2.valInt :=vars[Op2.ivar].valInt;
  Result.valFloat := Op1.valFloat*Op2.valInt;
end;
function float_mult_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valFloat:=vars[Op1.ivar].valFloat;
  if Op2.catOp = coVariable then Op2.valFloat:=vars[Op2.ivar].valFloat;
  Result.valFloat := Op1.valFloat*Op2.valFloat;
end;
function float_div_int(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valFloat:=vars[Op1.ivar].valFloat;
  if Op2.catOp = coVariable then Op2.valInt :=vars[Op2.ivar].valInt;
  Result.valFloat := Op1.valFloat / Op2.valInt;
end;
function float_div_float(var Op1: TOperand; opr: Toperator; var Op2: TOperand): TOperand;
begin
  Result.catTyp:=t_float;
  Result.typ:=tipFloat;
  if Op1.catOp = coVariable then Op1.valFloat:=vars[Op1.ivar].valFloat;
  if Op2.catOp = coVariable then Op2.valFloat:=vars[Op2.ivar].valFloat;
  Result.valFloat := Op1.valFloat / Op2.valFloat;
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
  tkOperator := lex.GetAttribByName('Operator');  //se debe haber creado
  tkDelimiter:= lex.GetAttribByName('Delimiter'); //se debe haber creado
  tkType     := lex.GetAttribByName('Types');    //se debe haber creado
  tkOthers   := lex.GetAttribByName('Others');   //se debe haber creado

  ///////////Crea tipos y operaciones
  ClearTypes;
  tipInt:=CreateType('int',t_integer,4);   //de 4 bytes
  tipInt.procDefine:=@int_procDefine;
  tipInt.procLoad:=@int_procLoad;

  tipFloat:=CreateType('float',t_float,4);   //de 4 bytes
  tipFloat.procDefine:=@float_procDefine;
  tipFloat.procLoad:=@float_procLoad;

  //////// Operaciones con Int ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=tipInt.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipInt,@int_asig_int);
  opr.CreateOperation(tipFloat,@int_asign_float);

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
//  opr.CreateOperation(tipFloat,@int_idiv_float);  no tiene sentido

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

