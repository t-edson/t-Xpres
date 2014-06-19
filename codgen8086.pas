{Para el 8086, se definen las siguientes normas:
Los números de 8 bits se dejan en AL
Los números de 16 bits se dejan en AX
}
var
  result_in_AL: Boolean;

procedure Cod_StartData;
//Codifica la parte inicial de declaración de variables estáticas
begin
  Code('.MODEL TINY');
  Code('.DATA');
  Code('  HelloMesg  db     ''Hello,World'',10,13,''$''');
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
//Se ejecuta al inicial el procesamiento de una expresión
begin
  Code('  ;expres');
  result_in_AL := false;   //inicia con al libre
end;
procedure int8_procDefine(const varName, varInitVal: string);
begin
  Code('  '+varName+ ' DB ?');
end;
procedure int8_procLoad(var Op: TOperand);
begin
  //carga el primer operando
  if Op.cat = coConst then begin
    Code('  mov al,'+ Op.txt);  //8 bits en dl
  end else if Op.cat = coVariable then begin
    Code('  mov al,'+ Op.txt);  //8 bits en dl
  end else begin  //expresión
    //ya debe estar cargada
  end;
end;
procedure int8_suma_int8proc(var Op1: TOperand; opr: TOperator; Op2: TOperand);
begin
  //carga el primer operando
  if not result_in_AL then begin
    int8_procLoad(Op1)
  end;
  //opera
  if Op2.cat = coConst then begin
    Code('  add al,'+ Op2.txt);  //8 bits en dl
    result_in_AL := true;
  end else if Op2.cat = coVariable then begin
    Code('  add al,'+ Op2.txt);  //8 bits en dl
    result_in_AL := true;
  end else begin  //expresión
    //ya debe estar cargada!!!!!
  end;
end;
procedure int8_resta_int8proc(var Op1: TOperand; opr: TOperator; Op2: TOperand);
begin
  //carga el primer operando
  if not result_in_AL then begin
    int8_procLoad(Op1)
  end;
  //opera
  if Op2.cat = coConst then begin
    Code('  sub al,'+ Op2.txt);  //8 bits en dl
    result_in_AL := true;
  end else if Op2.cat = coVariable then begin
    Code('  sub al,'+ Op2.txt);  //8 bits en dl
    result_in_AL := true;
  end else begin  //expresión
    //ya debe estar cargada!!!!!
  end;
end;
procedure int8_mult_int8proc(var Op1: TOperand; opr: TOperator; Op2: TOperand);
begin
  //carga el primer operando
  if not result_in_AL then begin
    int8_procLoad(Op1)
  end;
  //opera
  if Op2.cat = coConst then begin
    Code('  mov bl,'+ Op2.txt);  //no se puede multiplicar directo por constante
    Code('  mul al,bl');
    result_in_AL := true;
  end else if Op2.cat = coVariable then begin
    Code('  mul al,'+ Op2.txt);
    result_in_AL := true;
  end else begin  //expresión
    //????
  end;
  //deja resultado en AX
end;

procedure Iniciar(lex0: TSynFacilSyn);
var
  int8: TType;
  int8_suma,int8_resta,
  int8_mult: TOperator;
  int8_suma_int8, int8_resta_int8,
  int8_mult_int8: TxOperation;
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

  ///////////Crea tipos y operaciones
  ClearTypes;
  ClearVars;
  //////// tipo int8 ////////////
  int8:=CreateType('int8',t_integer,1);
  int8.procDefine:=@int8_procDefine;
  int8.procLoad:=@int8_procLoad;

  int8_suma:=int8.CreateOperator('+',5,'suma');
  int8_suma_int8 := int8_suma.CreateOperation(int8,'','','');
  int8_suma_int8.proc:=@int8_suma_int8proc;

  int8_resta:=int8.CreateOperator('-',5,'resta');
  int8_resta_int8 := int8_resta.CreateOperation(int8,'','','');
  int8_resta_int8.proc:=@int8_resta_int8proc;

  int8_mult:=int8.CreateOperator('*',5,'mult');
  int8_mult_int8 := int8_mult.CreateOperation(int8,'','','');
  int8_mult_int8.proc:=@int8_mult_int8proc;

  //  int8:=CreateType('int16',t_integer,1);
end;

