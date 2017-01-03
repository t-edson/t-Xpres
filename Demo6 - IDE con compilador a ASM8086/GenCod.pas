{Implementación de un compialdor sencillo para un lenguaje similar a Pascal.
Implementado para probar la implementación de compiladores, con del framework Xpres.
Este módulo compialrá a código ensamblador, en lugar de código objeto.
}
unit GenCod;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, SynEditHighlighter, Graphics,
  SynFacilBasic, XpresParser, XpresTypes,  XpresElements;

type
//  TSatReg = (srFree, srUsed)
  //define un registro virtual para implementar un intérprete
  Tregister = record
    used    : boolean;  //indica si está usado
    typ     : TType;    //tipo de dato
    catOp   : TCatOperan;  //categoría de operando
    //valores de la variable.
{    valFloat: extended; //Valor en caso de que sea un flotante
    valInt  : Int64;     //valor en caso de que sea un entero
    valUInt : Int64;     //valor en caso de que sea un entero sin signo
    valBool  : Boolean;  //valor  en caso de que sea un booleano
    valStr  : string;    //valor  en caso de que sea una cadena}
  end;

  { TGenCod }

  TGenCod = class(TCompilerBase)

  private
    procedure chr_asig_chr;
    procedure chr_OnPush(const OpPtr: pointer);
    procedure chr_procDefine(const varName, varInitVal: string);
    procedure chr_procLoad;
    procedure CodAsmInt(const inst: string);
    procedure Code(cod: string);
    procedure expr_end(isParam: boolean);
    procedure expr_start;
    procedure fun_putchar(fun: TxpFun);
    procedure fun_puts(fun: TxpFun);
    procedure fun_putsI(fun: TxpFun);
    procedure int_asig_int;
    procedure int_idiv_int;
    procedure int_mult_int;
    procedure int_OnPush(const OpPtr: pointer);
    procedure int_procDefine(const varName, varInitVal: string);
    procedure int_procLoad;
    procedure int_resid_int;
    procedure int_resta_int;
    procedure int_suma_int;
    procedure str_asig_str;
    procedure str_concat_str;
    procedure str_procLoad;
  protected
    tkStruct   : TSynHighlighterAttributes;
    tkExpDelim : TSynHighlighterAttributes;
    tkBlkDelim : TSynHighlighterAttributes;
    tkOthers   : TSynHighlighterAttributes;
    procedure Cod_StartData;
    procedure Cod_StartProgram;
    procedure Cod_EndProgram;
    procedure StartSyntax;
  public
    mem   : TStringList;   //Para almacenar el código de salida del compilador
  end;

var
  /////// Tipos de datos del lenguaje ////////////
  tipInt : TType;   //entero flotante
  tipStr : TType;   //cadena de caracteres
  tipChr : TType;   //un caracter
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
implementation

procedure TGenCod.Code(cod: string);
begin
  mem.Add(cod);
end;
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
procedure TGenCod.Cod_StartData;
//Codifica la parte inicial de declaración de variables estáticas
begin
  Code('.MODEL TINY');
  Code('.DATA');
  Code('  _tmpStr0 DB 255 DUP(''#''),0');  //variable para cadena temporal
end;
procedure TGenCod.Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  Code('.CODE');   //inicia la sección de código
end;
procedure TGenCod.Cod_EndProgram;
//Codifica la parte inicial del programa
begin
  Code('END');   //inicia la sección de código
end;
procedure TGenCod.expr_start;
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
procedure TGenCod.expr_end(isParam: boolean);
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
procedure TGenCod.int_procDefine(const varName, varInitVal: string);
//Se genera al declarar la variable
begin
  Code('  ' + varname + ' DW 0');
end;
procedure TGenCod.int_procLoad;
begin
  //carga el operando en res
  case p1^.catOp of
  coConst : Code('  mov ax,'+p1^.txt);
  coVariab: Code('  mov ax,'+p1^.VarName);
  coExpres: ;  //ya está en registro
  end;
  res.typ := tipInt;  //indica que devuelve un entero
  res.catOp := coExpres;  {Un operando cargado, se considerará siempre como una
                          expresión, auqnue si fuera una constante, ...   }
  a.used:=true;  //marac registro como usado
end;
procedure TGenCod.int_OnPush(const OpPtr: pointer);
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.catOp of
  coConst : begin
    code('  mov ax, ' + p2^.txt);  //carga literal
    code('  push ax');
  end;
  coVariab: code('  push '+ p1^.VarName);
  coExpres: code('  push ax'); //ya está en AX
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.int_asig_int;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    Code('  mov '+ p1^.VarName +', '+ p2^.txt);
  end;
  coVariab: begin
    Code('  mov ax, ' + p2^.VarName);
    Code('  mov '+ p1^.VarName +', ax');
  end;
  coExpres: begin  //ya está en AX
    Code('  mov '+ p1^.VarName +', ax');
  end;
  else
    GenError('No soportado'); exit;
  end;
//  vars[p1.ivar].valInt:=p2.GetvalInt;
//  res.used:=false;  //No hay obligación de que la asignación devuelva un valor.
//  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;
procedure TGenCod.CodAsmInt(const inst: string);
//Genera un código estandar para operaciones con entero.
begin
  res.typ := tipInt;   //el rsultado será siempre entero
  res.catOp:=coExpres; //por defecto generará una expresión
  case p1^.catOp of
  coConst : case p2^.catOp of
            coConst: begin
              //Es una operación con constantes, No genera cósigo. Se optimiza evaluando primero
              res.catOp:=coConst;
              res.valInt:= p1^.valInt+p2^.valInt;
              res.txt:=IntToStr(res.valInt);  //completa con texto
            end;
            coVariab: begin
              code('  mov ax, '+p1^.txt);
              code('  '+inst+ ' ax, '+p2^.VarName);
            end;
            coExpres: begin
              code('  '+inst+ ' ax, '+p1^.txt);
            end;
            end;
  coVariab: case p2^.catOp of
            coConst: begin
              code('  mov ax, '+p2^.txt);
              code('  '+inst+ ' ax, '+p1^.VarName);
            end;
            coVariab: begin
              code('  mov ax, '+p1^.VarName);
              code('  '+inst+ ' ax, ' + p2^.VarName);
            end;
            coExpres: begin
              code('  '+inst+ ' ax, '+p1^.VarName);
            end;
            end;
  coExpres: case p2^.catOp of
            coConst: begin
              code('  '+inst+ ' ax, '+p2^.txt);
            end;
            coVariab: begin
              code('  '+inst+ ' ax, '+p1^.VarName);
            end;
            coExpres: begin
              code('  pull bx');  //una expresión está en la pila
              code('  '+inst+ ' ax, bx');  //deja en ax
            end;
            end;
  end;
end;
procedure TGenCod.int_suma_int;
begin
  CodAsmInt('add');
end;
procedure TGenCod.int_resta_int;
begin
  CodAsmInt('sub');
end;
procedure TGenCod.int_mult_int;
begin
//  CodAsmInt('add');
end;
procedure TGenCod.int_idiv_int;
begin
//  LoadAcumInt(p1.GetValInt div p2.GetValInt,'\');
end;
procedure TGenCod.int_resid_int;
begin
//  LoadAcumInt(p1.GetValInt mod p2.GetValInt,'%');
end;
////////////operaciones con string
procedure TGenCod.str_procLoad;
var
  nomStr: String;
  nomLab: String;
begin
  case p1^.catOp of
  coConst : begin
    //genera nombre de variable cadena y etiqueta
    nomStr := 'cad' + IntToStr(ncc);
    nomLab := 'salt' + IntToStr(ncc);
    inc(ncc);
    //codifica
    code('  jmp ' + nomLab);   //deja expacio para cadena
    code('  ' + nomStr + ' DB "' + p1^.valStr + '"');
    code(nomLab+':');
    code('  mov dx, OFFSET '+nomStr);
  end;
  coExpres: ;  //ya está en registro
  coVariab: ;
  end;
end;
procedure TGenCod.str_asig_str;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  //en la VM se puede mover directamente res memoria sin usar el registro res
//  vars[p1.ivar].valStr:=p2.GetValStr;
//  res.used:=false;  //No hay obligación de que la expresión devuelva un valor.
//  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;
procedure TGenCod.str_concat_str;
begin
//  LoadAcumStr(p1.GetValStr+p2.GetValStr,'+');
end;
////////////operaciones con CHAR
procedure TGenCod.chr_procDefine(const varName, varInitVal: string);
begin
  Code('  ' + varname + ' DB 0');
end;
procedure TGenCod.chr_procLoad;
begin
  //carga el operando en res
  case p1^.catOp of
  coConst : Code('  mov al, '''+p1^.valStr+ '''');
  coExpres: ;  //ya está en registro
  coVariab: Code('  mov al,'+p1^.VarName);
  end;
  res.typ := tipChr;  //indica que devuelve un entero
  res.catOp := coExpres;  {Un operando cargado, se considerará siempre como una
                          exprwsión, auqnue si fuera una constante, ...   }
  a.used:=true;  //marac registro como usado
end;
procedure TGenCod.chr_OnPush(const OpPtr: pointer);
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.catOp of
  coConst : begin
    code('  mov al, ''' + p2^.valStr + '''');  //carga literal
    code('  push ax');
  end;
  coVariab: code('  push '+ p1^.VarName);
  coExpres: code('  push ax'); //ya está en AX
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.chr_asig_chr;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    Code('  mov '+ p1^.VarName +', '''+ p2^.valStr+'''');
  end;
  coVariab: begin
    Code('  mov al, ' + p2^.VarName);
    Code('  mov '+ p1^.VarName +', al');
  end;
  coExpres: begin  //ya está en AL
    Code('  mov '+ p1^.VarName +', al');
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
/////////////funciones del sistema
procedure TGenCod.fun_putchar(fun :TxpFun);
begin
  //Esta es una fucnión INLINE
  code('  pop dx');  //necesita el byte en DL. El valor se empujó con 16 bits.
  code('  mov ah, 02h');
  code('  int 21h');
  //Esta función no devuelve un valor, por eso no nos preocupamos del tipo.
end;
procedure TGenCod.fun_puts(fun :TxpFun);
//envia un texto a consola
begin
  if HayError then exit;
  //se supone que los datos están en "_tmpStr0", indezado por "DX"
  code('  mov ah, 09h');
  code('  int 21h');
  //Esta función no devuelve un valor, por eso no nos preocupamos del tipo.
end;
procedure TGenCod.fun_putsI(fun :TxpFun);
//envia un texto a consola
begin
  if HayError then exit;
  //se supone que los datos están en "_tmpStr0", indezado por "DX"
  code('  call convierte_entero');
  code('  mov ah, 09h');
  code('  int 21h');
  //Esta función no devuelve un valor, por eso no nos preocupamos del tipo.
end;

procedure TGenCod.StartSyntax;
//Se ejecuta solo una vez al inicio
var
  opr: TxpOperator;
  f: TxpFun;  //índice para funciones
begin
  //tokens personalizados
  tkExpDelim := xLex.NewTokType('ExpDelim');//delimitador de expresión ";"
  tkBlkDelim := xLex.NewTokType('BlkDelim'); //delimitador de bloque
  tkStruct   := xLex.NewTokType('Struct');   //personalizado
  tkOthers   := xLex.NewTokType('Others');   //personalizado
  //Configura atributos
  tkKeyword.Style := [fsBold];     //en negrita
  tkBlkDelim.Foreground:=clGreen;
  tkBlkDelim.Style := [fsBold];     //en negrita
  tkStruct.Foreground:=clGreen;
  tkStruct.Style := [fsBold];     //en negrita
  //inicia la configuración
  xLex.ClearMethodTables;           //limpìa tabla de métodos
  xLex.ClearSpecials;               //para empezar a definir tokens
  //crea tokens por contenido
  xLex.DefTokIdentif('[$A-Za-z_]', '[A-Za-z0-9_]*');
  xLex.DefTokContent('[0-9]', '[0-9.]*', tkNumber);
  //define palabras claves
  xLex.AddIdentSpecList('THEN var type', tkKeyword);
  xLex.AddIdentSpecList('program public private method const', tkKeyword);
  xLex.AddIdentSpecList('class create destroy sub do begin', tkKeyword);
  xLex.AddIdentSpecList('END ELSE ELSIF', tkBlkDelim);
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
  xLex.AddSymbSpec(';', tkExpDelim);
  xLex.AddSymbSpec('(',  tkOthers);
  xLex.AddSymbSpec(')',  tkOthers);
  xLex.AddSymbSpec(':',  tkOthers);
  xLex.AddSymbSpec(',',  tkOthers);
  //crea tokens delimitados
  xLex.DefTokDelim('''','''', tkString);
  xLex.DefTokDelim('"','"', tkString);
  xLex.DefTokDelim('//','', xLex.tkComment);
  xLex.DefTokDelim('/\*','\*/', xLex.tkComment, tdMulLin);
  //define bloques de sintaxis
  xLex.AddBlock('{','}');
  xLex.Rebuild;   //es necesario para terminar la definición

  //Define métodos a usar
  OnExprStart := @expr_start;
  OnExprEnd := @expr_End;

  ///////////Crea tipos y operaciones
  ClearTypes;
  tipInt  :=CreateType('int',t_integer,2);   //de 2 bytes
  tipInt.OperationLoad:=@int_procLoad;
  tipInt.OnGlobalDef:=@int_procDefine;
  tipInt.OnPush:=@int_OnPush;
  //debe crearse siempre el tipo char o string para manejar cadenas
  tipChr := CreateType('char',t_string,1);   //de 1 byte
  tipChr.OperationLoad:=@chr_procLoad;
  tipChr.OnGlobalDef:=@chr_procDefine;
  tipChr.OnPush:=@chr_OnPush;
//  tipStr:=CreateType('char',t_string,1);   //de 1 byte
  tipStr:=CreateType('string',t_string,-1);   //de longitud variable
  tipStr.OperationLoad:=@str_procLoad;

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


end.

