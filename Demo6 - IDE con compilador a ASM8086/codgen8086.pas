{Para el 8086, se definen las siguientes normas:
Los números de 8 bits se dejan en AL
Los números de 16 bits se dejan en AX
}
const
  //Constamtes para indicar el estado de los operadores.
  NOLOADED = 0;  //No cargado. Este estado debe existir siempre.
  LOADED = 1;    //Indica que esta cargado en registro. Listo para operar.

var
  //tipos
  int8: TType;   //tipo
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
procedure int8_procDefine(const varName, varInitVal: string);
begin
  Code('  '+varName+ ' DB ?');
end;
procedure int8_procLoad(var Op: TOperand);
begin
  if Op.estOp = LOADED then exit;   //ya está cargado
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
  end;
end;
function int8_suma_int8proc(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  //opera
  if Op2.catOp = coConst then begin
    int8_procLoad(Op1); if HayError then exit;  //carga operando
    Code('  add al,'+ Op2.txt);  //8 bits en AL
    ALused := true;
  end else if Op2.catOp = coVariable then begin
    int8_procLoad(Op1); if HayError then exit;  //carga operando
    Code('  add al,'+ Op2.txt);  //8 bits en AL
    ALused := true;
  end else begin  //expresión
    if (Op1.estOp = NOLOADED) and (Op2.estOp = LOADED) then begin
      Code('  mov bl,al');  //pasa expresión a BL
      ALused := false;   //ya está libre AL
      int8_procLoad(Op1); if HayError then exit;  //carga operando
      Code('  add al,bl');         //deja en AL
      ALused := true;
    end else begin
      Perr.GenError('No se puede compilar expresión.', PosAct);
    end;
  end;
  Result.typ:=int8;     //devuelve int8
  Result.estOp:=LOADED; //indica que está cargado en registro
end;
function int8_resta_int8proc(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  //opera
  if Op2.catOp = coConst then begin
    int8_procLoad(Op1); if HayError then exit;  //carga operando
    Code('  sub al,'+ Op2.txt);  //8 bits en AL
    ALused := true;
  end else if Op2.catOp = coVariable then begin
    int8_procLoad(Op1); if HayError then exit;  //carga operando
    Code('  sub al,'+ Op2.txt);  //8 bits en AL
    ALused := true;
  end else begin  //expresión
    if (Op1.estOp = NOLOADED) and (Op2.estOp = LOADED) then begin
      Code('  mov bl,al');  //pasa expresión a BL
      ALused := false;   //ya está libre AL
      int8_procLoad(Op1); if HayError then exit;  //carga operando
      Code('  sub al,bl');         //deja en AL
      ALused := true;
    end else begin
      Perr.GenError('No se puede compilar expresión.', PosAct);
    end;
  end;
  Result.typ:=int8;   //devuelve int8
  Result.estOp:=LOADED; //indica que está cargado en registro
end;
function int8_mult_int8proc(var Op1: TOperand; opr: TOperator; var Op2: TOperand): TOperand;
begin
  //opera
  if Op2.catOp = coConst then begin
    int8_procLoad(Op1); if HayError then exit;  //carga operando
    Code('  mov bl,'+ Op2.txt);  //no se puede multiplicar directo por constante
    Code('  imul al,bl');         //deja en AX
    ALused := true;
  end else if Op2.catOp = coVariable then begin
    int8_procLoad(Op1); if HayError then exit;  //carga operando
    Code('  imul al,'+ Op2.txt);  //deja en AX
    ALused := true;
  end else begin  //expresión
    if (Op1.estOp = NOLOADED) and (Op2.estOp = LOADED) then begin
      Code('  mov bl,al');  //pasa expresión a BL
      ALused := false;   //ya está libre AL
      int8_procLoad(Op1); if HayError then exit;  //carga operando
      Code('  imul al,bl');         //deja en AX
      ALused := true;
    end else begin
      Perr.GenError('No se puede compilar expresión.', PosAct);
    end;
  end;
  //deja resultado en AX
  Result.typ:=int8;   //devuelve int8 (se pierde AH)
  Result.estOp:=LOADED; //indica que está cargado en registro
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
  //////// tipo int8 ////////////
  int8:=CreateType('int8',t_integer,1);
  int8.procDefine:=@int8_procDefine;
  int8.procLoad:=@int8_procLoad;

  opr:=int8.CreateOperator('+',5,'suma');
  opr.CreateOperation(int8,@int8_suma_int8proc);

  opr:=int8.CreateOperator('-',5,'resta');
  opr.CreateOperation(int8,@int8_resta_int8proc);

  opr:=int8.CreateOperator('*',6,'mult');
  opr.CreateOperation(int8,@int8_mult_int8proc);

  //  int8:=CreateType('int16',t_integer,1);
end;

