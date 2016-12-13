{Implementación de un interprete sencillo.
Este módulo no generará código sino que lo ejecutará directamente.
Este intérprete, solo reconoce tipos de datos enteros y de cadena.
Para los enteros solo implementa la asignación(:=), sumas(+) y restas(-) y
para las cadenas se implementa la asignación(:=) y la concatenación(+)
Se pueden declarar y asignar variables.

En este archivo, se pueden declarar tipos, variables, constantes,
procedimientos y funciones. Hay rutinas obligatorias que siempre se deben
implementar.

Solo se define una función puts(), que muestra un texto en pantalla.

Este intérprete, maneja una pila.

* Todas las operaciones reciben sus dos parámetros en las variables p1 y p2.
* El resultado de cualquier expresión se debe dejar indicado en el objeto "res".
* Los valores enteros y enteros sin signo se cargan en valInt
* Los valores string se cargan en valStr
* Las variables están mapeadas en el arreglo vars[]
* Cada variable, de cualquier tipo, ocupa una celda de vars[]
* La pila se usa para pasar los parámetros de las funciones (en este caso solo para
  puts()).

Los procedimientos de operaciones, deben actualizar en el acumulador:

* El tipo de resultado (para poder evaluar la expresión completa como si fuera un
operando nuevo)
* La categoría del operador (constante, expresión, etc), para poder optimizar la generación
de código.

Por Tito Hinostroza  30/07/2014
}

const STACK_SIZE = 32;
var
  /////// Tipos de datos del lenguaje ////////////
  tipInt : TType;   //entero
  tipStr : Ttype;
  //pila virtual
  sp: integer;  //puntero de pila
  stack: array[0..STACK_SIZE-1] of TOperand;

{function setRes(const typ: TType; const estRes: integer): boolean;
//Marca el tipo y estado del registro "res", para que quede como información para
//las rutinas del evaluador de expresiones.
//Si encuentra error, lo geenra y devuelve FALSE.
begin
  Result := true;
  res.typ := typ;
//  res.estOp:=estRes;   //indica el estado del resultado
end;}
procedure LoadResInt(val: int64);
//Carga en el resultado un valor entero
begin
    res.typ := tipInt;
    res.valInt:=val;
end;
procedure LoadResStr(val: string);
//Carga en el resultado un valor string
begin
    res.typ := tipStr;
    res.valStr:=val;
end;
procedure PushResult;
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
procedure PopResult;
//Reduce el puntero de pila, de modo que queda apuntando al último dato agregado
begin
  if sp<=0 then begin
    GenError('Desborde de pila.');
    exit;
  end;
  Dec(sp);
end;
////////////rutinas obligatorias
procedure Cod_StartData;
//Codifica la parte inicial de declaración de variables estáticas
begin
end;
procedure Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  sp := 0;  //inicia pila
end;
procedure Cod_EndProgram;
//Codifica la parte inicial del programa
begin
end;
procedure expr_start(const exprLevel: integer);
//Se ejecuta siempre al StartSyntax el procesamiento de una expresión
begin
  if exprLevel=1 then begin //es el primer nivel
    res.typ := tipInt;   //le pone un tipo por defecto
  end;
end;
procedure expr_end(const exprLevel: integer; isParam: boolean);
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
  if isParam then begin
    //Se terminó de evaluar un parámetro
    PushResult;   //pone parámetro en pila
    if HayError then exit;
  end;
end;

////////////operaciones con Enteros
procedure int_procLoad(const OpPtr: pointer);
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  //carga el operando en res
  res.typ := tipInt;
  res.valInt := Op^.ReadInt;
end;
procedure int_asig_int;
begin
  if p1.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  //en la VM se puede mover directamente res memoria sin usar el registro res
  p1.rVar.valInt:=p2.ReadInt;
//  res.used:=false;  //No hay obligación de que la asignación devuelva un valor.
//  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;

procedure int_suma_int;
begin
  LoadResInt(p1.ReadInt+p2.ReadInt);
end;
////////////operaciones con string
procedure str_procLoad(const OpPtr: pointer);
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  //carga el operando en res
  res.typ := tipStr;
  res.valStr := Op^.ReadStr;
end;
procedure str_asig_str;
begin
  if p1.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  //aquí se puede mover directamente res memoria sin usar el registro res
  p1.rVar.valStr:=p2.ReadStr;
//  res.used:=false;  //No hay obligación de que la expresión devuelva un valor.
//  Code('['+IntToStr(p1.ivar)+']<-' + p2.expres);
end;

procedure str_concat_str;
begin
  LoadResStr(p1.ReadStr+p2.ReadStr);
end;

//funciones básicas
procedure fun_puts(fun :TxpFun);
//envia un texto a consola
begin
  PopResult;  //saca parámetro 1
  if HayError then exit;
  frmOut.puts(stack[sp].valStr);  //sabemos que debe ser String
  //el tipo devuelto lo fijará el framework, al tipo definido
end;
procedure fun_putsI(fun :TxpFun);
//envia un texto a consola
begin
  PopResult;  //saca parámetro 1
  if HayError then exit;
  frmOut.puts(IntTOStr(stack[sp].valInt));  //sabemos que debe ser Integer
  //el tipo devuelto lo fijará el framework, al tipo definido
end;

procedure TCompiler.StartSyntax;
//Se ejecuta solo una vez al inicio
var
  opr: TOperator;
  f: TxpFun;
begin
  ///////////define la sintaxis del compilador
  //crea y guarda referencia a los atributos
  tkEol      := xLex.tkEol;
  tkIdentif  := xLex.tkIdentif;
  tkKeyword  := xLex.tkKeyword;
  tkKeyword.Foreground:=clGreen;
  tkNumber   := xLex.tkNumber;
  tkString   := xLex.tkString;
  //personalizados
  tkOperator := xLex.NewTokType('Operador'); //personalizado
  tkBoolean  := xLex.NewTokType('Boolean');  //personalizado
  tkSysFunct := xLex.NewTokType('SysFunct'); //funciones del sistema
  tkExpDelim := xLex.NewTokType('ExpDelim');//delimitador de expresión ";"
  tkBlkDelim := xLex.NewTokType('BlkDelim'); //delimitador de bloque
  tkBlkDelim.Foreground:=clGreen;
  tkBlkDelim.Style := [fsBold];     //en negrita
  tkType     := xLex.NewTokType('Types');    //personalizado
  tkOthers   := xLex.NewTokType('Others');   //personalizado
  //inicia la configuración
  xLex.ClearMethodTables;           //limpìa tabla de métodos
  xLex.ClearSpecials;               //para empezar a definir tokens
  //crea tokens por contenido
  xLex.DefTokIdentif('[$A-Za-z_]', '[A-Za-z0-9_]*');
  xLex.DefTokContent('[0-9]', '[0-9.]*', tkNumber);
  //define palabras claves
  xLex.AddIdentSpecList('var type program begin', tkKeyword);
  xLex.AddIdentSpecList('end else elsif', tkBlkDelim);
  xLex.AddIdentSpecList('true false', tkBoolean);
  xLex.AddIdentSpecList('int string', tkType);
  //símbolos especiales
  xLex.AddSymbSpec(';',  tkExpDelim);
  xLex.AddSymbSpec('+',  tkOperator);
  xLex.AddSymbSpec('-',  tkOperator);
  xLex.AddSymbSpec('*',  tkOperator);
  xLex.AddSymbSpec('/',  tkOperator);
  xLex.AddSymbSpec(':=', tkOperator);
  xLex.AddSymbSpec('(',  tkOthers);
  xLex.AddSymbSpec(')',  tkOthers);
  xLex.AddSymbSpec(':',  tkOthers);
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
  tipInt  :=CreateType('int',t_integer,4);   //de 4 bytes
  tipInt.OnLoad:=@int_procLoad;
  //debe crearse siempre el tipo char o string para manejar cadenas
//  tipStr:=CreateType('char',t_string,1);   //de 1 byte
  tipStr:=CreateType('string',t_string,-1);   //de longitud variable
  tipStr.OnLoad:=@str_procLoad;

  //////// Operaciones con String ////////////
  opr:=tipStr.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipStr,@str_asig_str);
  opr:=tipStr.CreateOperator('+',7,'concat');
  opr.CreateOperation(tipStr,@str_concat_str);

  //////// Operaciones con Int ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=tipInt.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipInt,@int_asig_int);

  opr:=tipInt.CreateOperator('+',7,'suma');
  opr.CreateOperation(tipInt,@int_suma_int);

  //////// Funciones básicas ////////////
  f := CreateSysFunction('puts', tipInt, @fun_puts);
  f.CreateParam('',tipStr);
  f := CreateSysFunction('puts', tipInt, @fun_putsI);
  f.CreateParam('',tipInt);
  if FindDuplicFunction then exit;  //error
end;

