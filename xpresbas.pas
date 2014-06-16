unit XpresBas;
//Manejo de las rutinas de error y los contextos de entrada de Xpres
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils, fgl,
  Forms, LCLType,  //Para mostrar mensajes con Application.MessageBox()
  SynEditHighlighter, SynHighlighterFacil;

const
  //Tipos de contextos
  TC_ARC = 0 ;     //contexto de tipo archivo
  TC_TXT = 1 ;     //contexto de tipo texto

type

  TContexto = class;

  {Posición dentro de un contexto. A diferecnia de "Tcontexto", es un registro y siempre
   guardará una copia permanente. Además no guarda el texto del contexto}
  TPosCont = record
    arc   : String    ;     //Nombre de archivo
    fil   : LongInt   ;     //Fila
    col   : Integer   ;     //Columna
    nlin  : LongInt   ;     //Número de líneas
    fCon  : TContexto;      //Referencia al Contexto
  End;

  { TPError }
{Define al objeto TPError, el que se usa para tratar los errores del compilador. Solo se
 espera que haya uno de estos objetos, por eso se ha declarado como OBJECT}
  TPError = object
private
  numER : Integer;   //codigo de error
  cadER :  String;   //cadena de error
  arcER :  String;   //nombre de archivo que origino el error
  fil : Longint;     //número de línea del error
  col : Longint;     //número de columna del error
public
  NombPrograma: string;  //Usado para poner en el encabezado del mensaje
  procedure IniError;
  procedure Limpiar;
  procedure GenError(num: Integer; msje : String; archivo: String = '';  nlin: LongInt = -1);
  procedure Generror(msje: String; posCon: TPosCont);
  function TxtError: string;
  procedure MosError;
  function ArcError: string;
  function nLinError: longint;
  Function nColError: longint;
  function HayError: boolean;
end;

  { TContexto }
  {Estructura que define a un objeto contexto. Se usa tanto para leer la entrada como para
   escribir en la salida.}
  TContexto = class
  private
    function getCol: integer;
//    function getFil: integer;
  public
    tip      : integer;
    arc      : String;      //nombre de archivo
    nlin     : LongInt;     //Número de líneas del Contexto
    lin      : TStringList; {Líneas de texto. Se almacena en TStringList porque es rápida la
                            carga desde un archivo y porque es compatible con el almacenamiento
                            en el Control Editor.}
    fil      : integer;
    lex      : TSynFacilSyn;  //analizador léxico
    tokType  : TSynHighlighterAttributes;  //tipo de token actual
    tok      : string;        //token actual
    constructor Create;
    destructor Destroy; override;
    //Propiedad
    //Métodos de lectura
    Function IniCont:Boolean;
    Function Eof:Boolean;
    Function CapBlancos:Boolean;

    procedure Next;   //Pasa al siguiente token
    //posición del cursor actual
//    property fil: integer read getFil;
    property col: integer read getCol;
    //Control de la posición actual
    procedure SetPosXY(fil0, col0: integer);
    procedure CurPosIni;
//    procedure CurPosFin;
    //Métodos de llenado/lectura
    function LeeCad: string;          //Lee el contenido del contexto
    procedure DefSyn(lex0: TSynFacilSyn);  //Fija la sintaxis del lexer con un archivo
    procedure FijCad(cad : string);   //Fija el contenido del contexto con cadena
    procedure FijArc(arc0: string);   //Fija el contenido del contexto con archivo
    procedure FijArc(arc0: string;    //Fija el contenido del contexto con archivo
                     lins: Tstrings);  //pasándole TStringList
  End;

  //Define una lista de Contextos
  TListaCont = specialize TFPGObjectList<TContexto>;

implementation

{ TContexto }
constructor TContexto.Create;
begin
inherited;   //solo se pone por seguridad, ya que no es necesario.
  lin := TStringList.Create;    //crea lista de cadenas para almacenar el texto
  nlin := 0;
  CurPosIni;   //inicia fil y col
//  lex := TSynFacilSyn.Create(nil);  //crea lexer
end;
destructor TContexto.Destroy;
begin
//  lex.Free;     //libera lexer
  lin.Free;     //libera lista
  inherited Destroy;
end;
function TContexto.IniCont: Boolean;
//Devuelve verdadero si se está al inicio del Contexto (fila 1, columna 1)
var
  p: TPoint;
begin
  p :=lex.GetXY;
  Result := (p.x = 1) and (p.y = 1);
//    Result := (fil = 1) And (col = 1);
end;
function TContexto.Eof: Boolean;
//Devuelve verdadero si se ha llegado al final del Contexto.
begin
  //Protección a Contexto vacío
  If nlin = 0 Then begin
      Result := True;
      Exit;
  End;
  //Verifica optimizando verificando primero la condición más probable
  If fil < nlin Then
    Result := False
  Else If fil > nlin Then
    Result := True
  Else If fil = nlin Then begin  //estamos en la línea final.
    Result := lex.GetEol;   //si apunta al final
  End;
end;
function TContexto.CapBlancos: Boolean;
//Coge los blancos iniciales del contexto de entrada.
//Si no encuentra algun blanco al inicio, devuelve falso
begin
  while not Eof and ((lex.GetTokenAttribute = lex.tkSpace) or
                     (lex.GetTokenAttribute = lex.tkEol) ) do
    Next;
  //actualiza estado
  tok := lex.GetToken;    //lee el token
  tokType := lex.GetTokenAttribute;  //lee atributo
end;
function TContexto.getCol: integer;
begin
  Result:=lex.GetXY.x;
end;

procedure TContexto.Next;
begin
  if nlin = 0 then exit;  //protección
  if lex.GetEol then begin  //llegó al fin de línea
    fil := fil + 1;  //Pasa a siguiente fila.
    if fil <= nlin then begin //se puede leer
      lex.SetLine(lin[fil-1],fil);  //prepara exploración
      //actualiza estado
      tok := lex.GetToken;    //lee el token
      tokType := lex.GetTokenAttribute;  //lee atributo
    end;
  end else begin //está en medio de la línea
    lex.Next;        //pasa al siguiente token
    //actualiza estado
    tok := lex.GetToken;    //lee el token
    tokType := lex.GetTokenAttribute;  //lee atributo
  end;
end;
procedure TContexto.SetPosXY(fil0, col0: integer);
{Posiciona el cursor del contexto en la posición indicada.
Para que esté definido correctamente el estado del lexer, se debe haber
explorado primero las líneas anteriores, de otra forma se estaría poniendo
al contexto en una posición con estado desconocido.}
begin
  //Hay que considerar que se requiere explorar la línea desde el principio para obtener
  //el estado apropiado. Esto implica que no se debe abusar de este método.
  lex.SetHighlighterAtXY(Point(col0,fil0));
  fil := fil0;  //actualiza la nueva fila
end;
procedure TContexto.CurPosIni;
//Mueve la posición al inicio del contenido.
begin
  if lin.Count = 0 then begin
    //No hay líneas
    fil := 0;
  end else begin //hay al menos una línea
    fil := 1;
    lex.StartAtLineIndex(0);  //restaura el estado inicial del editor.
                              //legalmente debería hacer un lex.SetHighlighterAtXY()
    //actualiza estado
    tok := lex.GetToken;    //lee el token
    tokType := lex.GetTokenAttribute;  //lee atributo
  end;
end;
function TContexto.LeeCad: string;
//Devuelve el contenido del contexto en una cadena.
begin
  Result := lin.text;
end;

procedure TContexto.DefSyn(lex0: TSynFacilSyn);
//Define el lexer a usar en el contexto
begin
  lex := lex0;
//  lex.LoadFromFile(arc0);
end;

procedure TContexto.FijCad(cad: string);
//Fija el contenido del contexto con una cadena.
begin
  tip := TC_TXT;        //indica que contenido es Texto
  if cad='' then begin
    //cadena vacía, crea una línea vacía
    lin.Clear;
    lin.Add('');
    nlin := 1;    //actualiza número de líneas
  end else begin
    lin.Text := cad;
    nlin := lin.Count;    //actualiza número de líneas
  end;
  CurPosIni;      //actualiza posición de cursor
  arc := '';            //No se incluye información de archivo
end;
procedure TContexto.FijArc(arc0: string);
//Fija el contenido del contexto con un archivo
begin
  tip := TC_ARC;        //indica que contenido es Texto
  lin.LoadFromFile(arc0);
  nlin := lin.Count;    //actualiza número de líneas
  CurPosIni;   //actualiza posición de cursor
  arc := arc0;          //Toma nombe de archivo
end;
procedure TContexto.FijArc(arc0: string; lins: Tstrings);
//Fija el contenido del contexto con un archivo, recibido en una lista
begin
  tip := TC_ARC;        //indica que contenido es Texto
  lin.Clear;
//  lin.Delete(0);
  lin.AddStrings(lins); //carga líneas, de la lista
  nlin := lin.Count;    //actualiza número de líneas
  CurPosIni;            //actualiza posición de cursor
  arc := arc0;          //Toma nombe de archivo
end;

{ TPError }
procedure TPError.IniError;
begin
  numER := 0;
  cadER := '';
  arcER := '';
  fil := 0;
end;
procedure TPError.Limpiar;
//Limpia rápidamente el error actual
begin
  numEr := 0;
end;
procedure TPError.GenError(num: Integer; msje: String; archivo: String;
nlin: LongInt);
//Genera un error
begin
  numER := num;
  cadER := msje;
  arcER := archivo;
  fil := nlin;
end;
procedure TPError.Generror(msje: String; posCon: TPosCont);
//Genera un error en la posición indicada
begin
  numER := 1;
  cadER := msje;
  arcER := posCon.arc;
  fil := posCon.fil;
  col := posCon.col;
end;
function TPError.TxtError: string;
begin
  If arcER <> '' Then begin
    //Hay nombre de archivo de error
    If fil <> -1 Then       //Hay número de línea
  //      Result := Pchar('[' + arcER + ']: ' + cadER + ' Línea: ' + IntToStr(fil);
      //Se usa este formato porque incluye información sobre fila-columna y se puede usar como
      //parámetro de la línea de comnando /MSJE.
      Result := '('+ IntToStr(fil) + ',' + IntToStr(col) + ') ' + cadER
    Else          //No hay número de línea, sólo archivo
  //      Result := '[' + arcER + ']: ' + cadER;
      Result := cadER;
  end Else
    Result :=cadER;
end;
procedure TPError.MosError;
//Muestra un mensaje de error
begin
  Application.MessageBox(PChar(TxtError), PChar(NombPrograma), MB_ICONEXCLAMATION);
end;
function TPError.ArcError: string;
//Devuelve el nombre del archivo de error
begin
  ArcError := arcER;
end;
function TPError.nLinError: longint;
//Devuelve el número de línea del error
begin
  nLinError := fil;
end;
function TPError.nColError: longint;
//Devuelve el número de línea del error
begin
  nColError := col;
end;
function TPError.HayError: boolean;
begin
  Result := numER <> 0;
end;

end.

