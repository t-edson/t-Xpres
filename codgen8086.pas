var
  result_in_AL: Boolean;

procedure Cod_StartProgram;
//Codifica la parte inicial del programa
begin
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
  result_in_AL := false;   //inicia con al libre
end;
procedure int8procDefine(const varName, varInitVal: string);
begin
  Code('  '+varName+ ' DB ?');
end;
procedure expr_int8_load(var Op: TOperand);
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
procedure expr_int8_suma_int8(var Op1: TOperand; opr: TOperator; Op2: TOperand);
begin
  //carga el primer operando
  if not result_in_AL then begin
    expr_int8_load(Op1)
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
procedure expr_int8_resta_int8(var Op1: TOperand; opr: TOperator; Op2: TOperand);
begin
  //carga el primer operando
  if not result_in_AL then begin
    expr_int8_load(Op1)
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

procedure Iniciar(lex0: TSynFacilSyn);
var
  int8: TType;
  int8_suma,int8_resta: TOperator;
  iSum, iRes: Integer;
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
  int8.procDefine:=@int8procDefine;
  int8.procLoad:=@expr_int8_load;

  int8_suma:=int8.CreateOperator('+',5,'suma');
  iSum := int8_suma.CreateOperation(int8,'','','');
  int8_suma.Operations[iSum].procOperat:=@expr_int8_suma_int8;

  int8_resta:=int8.CreateOperator('-',5,'resta');
  iRes := int8_resta.CreateOperation(int8,'','','');
  int8_resta.Operations[iREs].procOperat:=@expr_int8_resta_int8;;
//  int8:=CreateType('int16',t_integer,1);
end;

