{Implementación de un interprete sencillo para el lenguaje Xpres.
Implementado para probar la flexibilidad del compilador Xpres
Este módulo no generará código sino que lo ejecutará directamente.
}
type
//  TSatReg = (srFree, srUsed)
  //define un registro virtual para implementar un intérprete
  Tregister = record
    used    : boolean;  //indica si está usado
    typ     : Ttype;    //tipo de dato
    catOp   : TCatOperan;  //categoría de operando
    //valores de la variable.
{    valFloat: extended; //Valor en caso de que sea un flotante
    valInt  : Int64;     //valor en caso de que sea un entero
    valUInt : Int64;     //valor en caso de que sea un entero sin signo
    valBool  : Boolean;  //valor  en caso de que sea un booleano
    valStr  : string;    //valor  en caso de que sea una cadena}
  end;

var
  /////// Tipos de datos del lenguaje ////////////
  tipInt : TType;   //entero flotante
  tipStr : Ttype;   //cadena de caracteres
  tipChr : Ttype;   //un caracter
  ////////// Registros virtuales ////////////
  {la arquitectura definida aquí contempla:

  Un registro de trabajo AX, de 16 bits.
  Registro auxiliar BX, de 16 bits.
  Registro auxiliar DX, de 16 bits.

  El resultado de una expresión numérica se dejará siempre en el registro AX, menos que
  ya esté ocupado, en cuyo caso se pone primero en la pila.
  El resultado de una expresión char se dejará siempre en el registro AL, menos que
  ya esté ocupado, en cuyo caso se pone primero en la pila.
  El resultado de una expresión de cadena se dejará siempre en el espacio temporal
  "_tmpStr0" con el puntero DX apuntando a esta zona.
  ya esté ocupado, en cuyo caso se pone primero en la pila.

  * Todas las operaciones recibe sus dos parámetros en las variables p1 y p2.
  * El resultado de cualquier expresión se debe dejar indicado en el objeto "res".
  * Los valores enteros y enteros sin signo se cargan en valInt
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
const STACK_SIZE = 32;
var
  a: Tregister;

  ncc: integer;  //Conatdor. Número de cosntantes cadenas.
  //banderas
//  ALused: Boolean;  //indica que el registro Al está siendo usado

{procedure LoadAcumStr(val: string; op: string);
//Carga en el acumulador(es) un valor booleano, y genera t-code
begin
  if canUseA(p1,p2) then  begin
    setRes(tipStr, STORED_ACU);
    a.valStr:=val;
    Code('A<-' + p1.expres + op + p2.expres);
  end else if canUseB(p1,p2) then begin  //no se puede usar A
    setRes(tipStr, STORED_ACUB);
    b.valStr:=val;
    Code('B<-' + p1.expres + op + p2.expres);
  end else begin
    GenError('Expresión muy compleja.');
    exit;
  end;
end;}
////////////rutinas obligatorias
procedure Cod_StartData;
//Codifica la parte inicial de declaración de variables estáticas
begin
  Code('.MODEL TINY');
  Code('.DATA');
  Code('  _tmpStr0 DB 255 DUP(''#''),0');  //variable para cadena temporal
end;
procedure Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  Code('.CODE');   //inicia la sección de código
end;
procedure Cod_EndProgram;
//Codifica la parte inicial del programa
begin
  Code('END');   //inicia la sección de código
end;
procedure expr_start(const exprLevel: integer);
//Se ejecuta siempre al StartSyntax el procesamiento de una expresión
begin
  if exprLevel=1 then begin //es el primer nivel
    Code('  ;expres');
    a.used:=false;   //inicia con el registro libre
    res.typ := tipInt;   //le pone un tipo por defecto
  end else begin  //es recursivo
//    if ALused then  //hay un dato evaluado
//      Code('  push al');  //lo guarda
  end;
end;
procedure expr_end(const exprLevel: integer; isParam: boolean);
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
  if isParam then begin
    //Se terminó de evaluar un parámetro
    res.Push;   //pone parámetro en pila
    if HayError then exit;
    a.used:=false;  //se libera registro
  end;
  if exprLevel = 1 then begin  //el último nivel
    Code('  ;fin expres');
  end;
end;

////////////operaciones con Enteros
procedure int_procDefine(const varName, varInitVal: string);
//Se genera al declarar la variable
begin
  Code('  ' + varname + ' DW 0');
end;
procedure int_procLoad(const OpPtr: pointer);
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  //carga el operando en res
  case Op^.catOp of
  coConst : Code('  mov ax,'+Op^.txt);
  coVariab: Code('  mov ax,'+Op^.VarName);
  coExpres: ;  //ya está en registro
  end;
  res.typ := tipInt;  //indica que devuelve un entero
  res.catOp := coExpres;  {Un operando cargado, se considerará siempre como una
                          exprwsión, auqnue si fuera una constante, ...   }
  a.used:=true;  //marac registro como usado
end;
procedure int_OnPush(const OpPtr: pointer);
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.catOp of
  coConst : begin
    code('  mov ax, ' + p2.txt);  //carga literal
    code('  push ax');
  end;
  coVariab: code('  push '+ p1.VarName);
  coExpres: code('  push ax'); //ya está en AX
  else
    GenError('No soportado'); exit;
  end;
end;
procedure int_asig_int;
begin
  if p1.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2.catOp of
  coConst : begin
    Code('  mov '+ p1.VarName +', '+ p2.txt);
  end;
  coVariab: begin
    Code('  mov ax, ' + p2 .VarName);
    Code('  mov '+ p1.VarName +', ax');
  end;
  coExpres: begin  //ya está en AX
    Code('  mov '+ p1.VarName +', ax');
  end;
  else
    GenError('No soportado'); exit;
  end;
//  vars[p1.ivar].valInt:=p2.GetvalInt;
//  res.used:=false;  //No hay obligación de que la asignación devuelva un valor.
//  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;
procedure CodAsmInt(const inst: string);
//Genera un código estandar para operaciones con entero.
begin
  res.typ := tipInt;   //el rsultado será siempre entero
  res.catOp:=coExpres; //por defecto generará una expresión
  case p1.catOp of
  coConst : case p2.catOp of
            coConst: begin
              //Es una operación con constantes, No genera cósigo. Se optimiza evaluando primero
              res.catOp:=coConst;
              res.valInt:= p1.valInt+p2.valInt;
              res.txt:=IntToStr(res.valInt);  //completa con texto
            end;
            coVariab: begin
              code('  mov ax, '+p1.txt);
              code('  '+inst+ ' ax, '+p2.VarName);
            end;
            coExpres: begin
              code('  '+inst+ ' ax, '+p1.txt);
            end;
            end;
  coVariab: case p2.catOp of
            coConst: begin
              code('  mov ax, '+p2.txt);
              code('  '+inst+ ' ax, '+p1.VarName);
            end;
            coVariab: begin
              code('  mov ax, '+p1.VarName);
              code('  '+inst+ ' ax, '+p2.VarName);
            end;
            coExpres: begin
              code('  '+inst+ ' ax, '+p1.VarName);
            end;
            end;
  coExpres: case p2.catOp of
            coConst: begin
              code('  '+inst+ ' ax, '+p2.txt);
            end;
            coVariab: begin
              code('  '+inst+ ' ax, '+p1.VarName);
            end;
            coExpres: begin
              code('  pull bx');  //una expresión está en la pila
              code('  '+inst+ ' ax, bx');  //deja en ax
            end;
            end;
  end;
end;
procedure int_suma_int;
begin
  CodAsmInt('add');
end;
procedure int_resta_int;
begin
  CodAsmInt('sub');
end;
procedure int_mult_int;
begin
//  CodAsmInt('add');
end;
procedure int_idiv_int;
begin
//  LoadAcumInt(p1.GetValInt div p2.GetValInt,'\');
end;
procedure int_resid_int;
begin
//  LoadAcumInt(p1.GetValInt mod p2.GetValInt,'%');
end;
////////////operaciones con string
procedure str_procLoad(const OpPtr: pointer);
var
  nomStr: String;
  nomLab: String;
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.catOp of
  coConst : begin
    //genera nombre de variable cadena y etiqueta
    nomStr := 'cad' + IntToStr(ncc);
    nomLab := 'salt' + IntToStr(ncc);
    inc(ncc);
    //codifica
    code('  jmp ' + nomLab);   //deja expacio para cadena
    code('  ' + nomStr + ' DB "' + Op^.valStr + '"');
    code(nomLab+':');
    code('  mov dx, OFFSET '+nomStr);
  end;
  coExpres: ;  //ya está en registro
  coVariab: ;
  end;
end;
procedure str_asig_str;
begin
  if p1.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  //en la VM se puede mover directamente res memoria sin usar el registro res
//  vars[p1.ivar].valStr:=p2.GetValStr;
//  res.used:=false;  //No hay obligación de que la expresión devuelva un valor.
//  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;
procedure str_concat_str;
begin
//  LoadAcumStr(p1.GetValStr+p2.GetValStr,'+');
end;
////////////operaciones con CHAR
procedure chr_procDefine(const varName, varInitVal: string);
begin
  Code('  ' + varname + ' DB 0');
end;
procedure chr_procLoad(const OpPtr: pointer);
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  //carga el operando en res
  case Op^.catOp of
  coConst : Code('  mov al, '''+Op^.valStr+ '''');
  coExpres: ;  //ya está en registro
  coVariab: Code('  mov al,'+Op^.VarName);
  end;
  res.typ := tipChr;  //indica que devuelve un entero
  res.catOp := coExpres;  {Un operando cargado, se considerará siempre como una
                          exprwsión, auqnue si fuera una constante, ...   }
  a.used:=true;  //marac registro como usado
end;
procedure chr_OnPush(const OpPtr: pointer);
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.catOp of
  coConst : begin
    code('  mov al, ''' + p2.valStr + '''');  //carga literal
    code('  push ax');
  end;
  coVariab: code('  push '+ p1.VarName);
  coExpres: code('  push ax'); //ya está en AX
  else
    GenError('No soportado'); exit;
  end;
end;
procedure chr_asig_chr;
begin
  if p1.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2.catOp of
  coConst : begin
    Code('  mov '+ p1.VarName +', '''+ p2.valStr+'''');
  end;
  coVariab: begin
    Code('  mov al, ' + p2.VarName);
    Code('  mov '+ p1.VarName +', al');
  end;
  coExpres: begin  //ya está en AL
    Code('  mov '+ p1.VarName +', al');
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
/////////////funciones del sistema
procedure fun_putchar(fun :TxpFun);
begin
  //Esta es una fucnión INLINE
  code('  pop dx');  //necesita el byte en DL. El valor se empujó con 16 bits.
  code('  mov ah, 02h');
  code('  int 21h');
  //Esta función no devuelve un valor, por eso no nos preocupamos del tipo.
end;
procedure fun_puts(fun :TxpFun);
//envia un texto a consola
begin
  if HayError then exit;
  //se supone que los datos están en "_tmpStr0", indezado por "DX"
  code('  mov ah, 09h');
  code('  int 21h');
  //Esta función no devuelve un valor, por eso no nos preocupamos del tipo.
end;
procedure fun_putsI(fun :TxpFun);
//envia un texto a consola
begin
  if HayError then exit;
  //se supone que los datos están en "_tmpStr0", indezado por "DX"
  code('  call convierte_entero');
  code('  mov ah, 09h');
  code('  int 21h');
  //Esta función no devuelve un valor, por eso no nos preocupamos del tipo.
end;

procedure TCompiler.StartSyntax;
//Se ejecuta solo una vez al inicio
var
  opr: TOperator;
  f: TxpFun;  //índice para funciones
begin

  //Define métodos a usar
  OnExprStart := @expr_start;
  OnExprEnd := @expr_End;

  ///////////Crea tipos y operaciones
  ClearTypes;
  tipInt  :=CreateType('int',t_integer,2);   //de 2 bytes
  tipInt.OnLoad:=@int_procLoad;
  tipInt.OnGlobalDef:=@int_procDefine;
  tipInt.OnPush:=@int_OnPush;
  //debe crearse siempre el tipo char o string para manejar cadenas
  tipChr := CreateType('char',t_string,1);   //de 1 byte
  tipChr.OnLoad:=@chr_procLoad;
  tipChr.OnGlobalDef:=@chr_procDefine;
  tipChr.OnPush:=@chr_OnPush;
//  tipStr:=CreateType('char',t_string,1);   //de 1 byte
  tipStr:=CreateType('string',t_string,-1);   //de longitud variable
  tipStr.OnLoad:=@str_procLoad;

  //////// Operaciones con Chr ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=tipChr.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipChr,@chr_asig_chr);

  //////// Operaciones con String ////////////
  opr:=tipStr.CreateOperator(':=',2,'asig');  //asignación
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

  opr:=tipInt.CreateOperator('*',8,'mult');
  opr.CreateOperation(tipInt,@int_mult_int);

  opr:=tipInt.CreateOperator('\',8,'idiv');
  opr.CreateOperation(tipInt,@int_idiv_int);
  opr:=tipInt.CreateOperator('%',8,'resid');
  opr.CreateOperation(tipInt,@int_resid_int);

//////// Funciones básicas ////////////
  f := CreateSysFunction('putchar', tipInt, @fun_putchar);
  f.CreateParam('',tipChr);
  f := CreateSysFunction('puts', tipInt, @fun_puts);
  f.CreateParam('',tipStr);
  f := CreateSysFunction('puts', tipInt, @fun_putsI);
  f.CreateParam('',tipInt);
  //Inicia contadores
  ncc := 0;
end;

