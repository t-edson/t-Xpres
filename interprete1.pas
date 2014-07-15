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
  tipInt: TType;   //tipo
  //banderas
  ALused: Boolean;  //indica que el registro Al está siendo usado

procedure Cod_StartData;
//Codifica la parte inicial de declaración de variables estáticas
begin
  Code('.MODEL TINY');
  Code('.DATA');
  Code('  HelloMesg  db     ''Hola mundo'',10,13,''$''');
end;
procedure Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  Code('.CODE');   //inicia la sección de código
  Code('START:');
  Code('  mov dx,OFFSET HelloMesg  ; offset of the text string');
  Code('  mov ah,9              ; print string function number');
  Code('  int 21h               ; dos call');
end;
procedure Cod_EndProgram;
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
procedure expr_start;
//Se ejecuta siempre al StartSyntax el procesamiento de una expresión
begin
//  Code('  ;expres');
  if exprLevel=1 then begin //es el primer nivel
    Code('  ;expres');
    ALused := false;   //inicia con AL libre
  end else begin  //es recursivo
//    if ALused then  //hay un dato evaluado
//      Code('  push al');  //lo guarda
  end;
end;
procedure expr_end;
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
//  Code('  ;fin expres');
  if exprLevel = 1 then begin  //el último nivel
    ALused := false;   //termina con AL libre
//    Code('  ;fin expres');
  end;
end;
///////// Eventos para la generación de código de evaluación de expresiones ////////
procedure int_procDefine(const varName, varInitVal: string);
//Se dispara cuando se declara una variable de este tipo.
begin
  //la variable ya se ingresa en vars[]
//  Code('  '+varName+ ' DB ?');
end;
procedure int_procLoad(var Op: TOperand);
begin
{  if Op.estOp = LOADED then exit;   //ya está cargado
  //verifica disponibilidad de registro
  if ALused then begin
    Perr.GenError('No se puede compilar expresión.', PosAct);exit;
  end;
  //carga el operando en
  if Op.catOp = coConst then begin
    Code('  mov al,'+ Op.txt);  //8 bits en dl
  end else if Op.catOp = coVariable then begin
    Code('  mov al,'+ Op.txt);  //8 bits en dl
  end else begin  //expresión
    //ya debe estar cargada
  end;}
end;
function int_suma_intproc(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipInt;     //devuelve tipInt
  Result.valInt:=Op1.valInt+Op2.valInt;
end;
function int_resta_intproc(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipInt;   //devuelve tipInt
  Result.valInt:=Op1.valInt-Op2.valInt;
end;
function int_mult_intproc(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipInt;   //devuelve tipInt (se pierde AH)
  Result.valInt:=Op1.valInt*Op2.valInt;
end;
function int_idiv_intproc(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  Result.typ:=tipInt;   //devuelve tipInt (se pierde AH)
  Result.valInt:=Op1.valInt div Op2.valInt;
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
  //////// tipo tipInt ////////////
  tipInt:=CreateType('int',t_integer,4);   //de 4 bytes
  tipInt.procDefine:=@int_procDefine;
  tipInt.procLoad:=@int_procLoad;

  opr:=tipInt.CreateOperator('+',5,'suma');
  opr.CreateOperation(tipInt,@int_suma_intproc);

  opr:=tipInt.CreateOperator('-',5,'resta');
  opr.CreateOperation(tipInt,@int_resta_intproc);

  opr:=tipInt.CreateOperator('*',6,'mult');
  opr.CreateOperation(tipInt,@int_mult_intproc);

  opr:=tipInt.CreateOperator('\',6,'idiv');
  opr.CreateOperation(tipInt,@int_idiv_intproc);

  //  tipInt:=CreateType('int16',t_integer,1);
end;

