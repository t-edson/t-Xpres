unit Interprete;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, SynEditHighlighter, Graphics,
  SynFacilBasic, XPresParser, XpresTypes, XpresElements;
type
  { TInterprete }
  TInterprete = class(TCompilerBase)
  const
    STACK_SIZE = 32;
  private
    /////// Tipos de datos del lenguaje ////////////
    tipInt : TType;   //entero
    tipStr : TType;
    //Pila virtual
    sp: integer;  //puntero de pila
    stack: array[0..STACK_SIZE-1] of TOperand;
    procedure fun_puts(fun: TxpEleFun);
    procedure fun_putsI(fun: TxpEleFun);
    procedure int_asig_int;
    procedure int_procLoad;
    procedure int_suma_int;
    procedure LoadResInt(val: int64);
    procedure LoadResStr(val: string);
    procedure PopResult;
    procedure PushResult;
    procedure str_asig_str;
    procedure str_concat_str;
    procedure str_procLoad;
  public
    //Referencias de tipos adicionales a los ya existentes en TCompilerBase
    //Depende de la implementación a usar.
    tnExpDelim : integer;
    tnBlkDelim : integer;
    tnOthers   : integer;
    tkExpDelim : TSynHighlighterAttributes;
    tkBlkDelim : TSynHighlighterAttributes;
    tkOthers   : TSynHighlighterAttributes;
    procedure Cod_StartData;
    procedure Cod_StartProgram;
    procedure Cod_EndProgram;
    procedure expr_start;
    procedure expr_End(isParam: boolean);
    procedure StartSyntax;
  end;

implementation
uses
  FormOut;
{ TInterprete }

//////////// Rutinas obligatorias //////////////
procedure TInterprete.Cod_StartData;
//Codifica la parte inicial de declaración de variables estáticas
begin
end;
procedure TInterprete.Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  sp := 0;  //inicia pila
end;
procedure TInterprete.Cod_EndProgram;
//Codifica la parte inicial del programa
begin
end;
procedure TInterprete.expr_start;
//Se ejecuta siempre al StartSyntax el procesamiento de una expresión
begin
  if exprLevel=1 then begin //es el primer nivel
    res.typ := tipInt;   //le pone un tipo por defecto
  end;
end;
procedure TInterprete.expr_End(isParam: boolean);
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
  if isParam then begin
    //Se terminó de evaluar un parámetro
    PushResult;   //pone parámetro en pila
    if HayError then exit;
  end;
end;
procedure TInterprete.StartSyntax;
//Se ejecuta solo una vez al inicio
var
  opr: TxpOperator;
  f: TxpEleFun;
begin
  /////////// Crea tipos personalizados
  tnExpDelim := xLex.NewTokType('ExpDelim', tkExpDelim);//delimitador de expresión ";"
  tnBlkDelim := xLex.NewTokType('BlkDelim', tkBlkDelim); //delimitador de bloque
  tnOthers   := xLex.NewTokType('Others', tkOthers);   //personalizado
  /////////// Define atributos de los tokens /////////
  tkKeyword.Foreground:=clGreen;
  tkBlkDelim.Foreground:=clGreen;
  tkBlkDelim.Style := [fsBold];     //en negrita
  //inicia la configuración
  xLex.ClearMethodTables;           //limpìa tabla de métodos
  xLex.ClearSpecials;               //para empezar a definir tokens
  //crea tokens por contenido
  xLex.DefTokIdentif('[$A-Za-z_]', '[A-Za-z0-9_]*');
  xLex.DefTokContent('[0-9]', '[0-9.]*', tnNumber);
  //define palabras claves
  xLex.AddIdentSpecList('var type program begin', tnKeyword);
  xLex.AddIdentSpecList('end else elsif', tnBlkDelim);
  xLex.AddIdentSpecList('true false', tnBoolean);
  xLex.AddIdentSpecList('int string', tnType);
  //símbolos especiales
  xLex.AddSymbSpec(';',  tnExpDelim);
  xLex.AddSymbSpec('+',  tnOperator);
  xLex.AddSymbSpec('-',  tnOperator);
  xLex.AddSymbSpec('*',  tnOperator);
  xLex.AddSymbSpec('/',  tnOperator);
  xLex.AddSymbSpec(':=', tnOperator);
  xLex.AddSymbSpec('(',  tnOthers);
  xLex.AddSymbSpec(')',  tnOthers);
  xLex.AddSymbSpec(':',  tnOthers);
  //crea tokens delimitados
  xLex.DefTokDelim('''','''', tnString);
  xLex.DefTokDelim('"','"', tnString);
  xLex.DefTokDelim('//','', tnComment);
  xLex.DefTokDelim('/\*','\*/', tnComment, tdMulLin);
  //define bloques de sintaxis
  xLex.AddBlock('{','}');
  xLex.Rebuild;   //es necesario para terminar la definición

  //Define métodos a usar
  OnExprStart:=@expr_start;
  OnExprEnd:=@expr_End;

  ///////////Crea tipos y operaciones
  ClearTypes;
  tipInt  :=CreateType('int',t_integer,4);   //de 4 bytes
  //debe crearse siempre el tipo char o string para manejar cadenas
//  tipStr:=CreateType('char',t_string,1);   //de 1 byte
  tipStr:=CreateType('string',t_string,-1);   //de longitud variable

  //////// Operaciones con String ////////////
  tipStr.OperationLoad:=@str_procLoad;
  opr:=tipStr.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipStr,@str_asig_str);
  opr:=tipStr.CreateBinaryOperator('+',7,'concat');
  opr.CreateOperation(tipStr,@str_concat_str);

  //////// Operaciones con Int ////////////
  {Los operadores deben crearse con su precedencia correcta}
  tipInt.OperationLoad:=@int_procLoad;
  opr:=tipInt.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipInt,@int_asig_int);

  opr:=tipInt.CreateBinaryOperator('+',7,'suma');
  opr.CreateOperation(tipInt,@int_suma_int);

  //////// Funciones básicas ////////////
  f := CreateSysFunction('puts', tipInt, @fun_puts);
  f.CreateParam('',tipStr);
  f := CreateSysFunction('puts', tipInt, @fun_putsI);
  f.CreateParam('',tipInt);
  if FindDuplicFunction then exit;  //error
end;
////////////// Manejo de pila ///////////////
procedure TInterprete.LoadResInt(val: int64);
//Carga en el resultado un valor entero
begin
    res.typ := tipInt;
    res.valInt:=val;
end;
procedure TInterprete.LoadResStr(val: string);
//Carga en el resultado un valor string
begin
    res.typ := tipStr;
    res.valStr:=val;
end;
procedure TInterprete.PushResult;
//Coloca el resultado de una expresión en la pila
begin
  if sp>=STACK_SIZE then begin
    GenError('Desborde de pila.');
    exit;
  end;
  stack[sp].typ := res.typ;
  case res.Typ.cat of
  t_string:  stack[sp].valStr  := res.ReadStr;
  t_integer: stack[sp].valInt  := res.ReadInt;
  end;
  Inc(sp);
end;
procedure TInterprete.PopResult;
//Reduce el puntero de pila, de modo que queda apuntando al último dato agregado
begin
  if sp<=0 then begin
    GenError('Desborde de pila.');
    exit;
  end;
  Dec(sp);
end;

////////////operaciones con Enteros
procedure TInterprete.int_procLoad;
begin
  //carga el operando en res
  res.typ := tipInt;
  res.valInt := p1^.ReadInt;
end;
procedure TInterprete.int_asig_int;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  //en la VM se puede mover directamente res memoria sin usar el registro res
  p1^.rVar.valInt:=p2^.ReadInt;
//  res.used:=false;  //No hay obligación de que la asignación devuelva un valor.
//  Code('['+IntToStr(p1^.ivar)+']<-' + p2^.expres);
end;

procedure TInterprete.int_suma_int;
begin
  LoadResInt(p1^.ReadInt+p2^.ReadInt);
end;
////////////operaciones con string
procedure TInterprete.str_procLoad;
begin
  //carga el operando en res
  res.typ := tipStr;
  res.valStr := p1^.ReadStr;
end;
procedure TInterprete.str_asig_str;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  //aquí se puede mover directamente res memoria sin usar el registro res
  p1^.rVar.valStr:=p2^.ReadStr;
//  res.used:=false;  //No hay obligación de que la expresión devuelva un valor.
//  Code('['+IntToStr(p1^.ivar)+']<-' + p2^.expres);
end;
procedure TInterprete.str_concat_str;
begin
  LoadResStr(p1^.ReadStr+p2^.ReadStr);
end;

//funciones básicas
procedure TInterprete.fun_puts(fun :TxpEleFun);
//envia un texto a consola
begin
  PopResult;  //saca parámetro 1
  if HayError then exit;
  frmOut.puts(stack[sp].valStr);  //sabemos que debe ser String
  //el tipo devuelto lo fijará el framework, al tipo definido
end;
procedure TInterprete.fun_putsI(fun :TxpEleFun);
//envia un texto a consola
begin
  PopResult;  //saca parámetro 1
  if HayError then exit;
  frmOut.puts(IntTOStr(stack[sp].valInt));  //sabemos que debe ser Integer
  //el tipo devuelto lo fijará el framework, al tipo definido
end;


end.

