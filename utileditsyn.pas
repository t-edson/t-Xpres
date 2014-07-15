{                                   UtilEditSyn 0.4
* Se cambia el nombre al evento OnArchivoCargado().
* Se crea la propiedad de solo lectur "Modified".
* Se agrega la propiedad "canCopy".
* Se agrega el evento OnSelectionChange(), como funcionalidad adicional.
* Se corrige un error de desbordamiento que se podía producir en VerTipoArchivo().

                                  Modif. Por Tito Hinostroza 12/07/2014
}
unit utilEditSyn; {$mode objfpc}{$H+}
interface
uses  Classes, SysUtils, SynEdit, SynEditMarkupHighAll, SynEditMarkupSpecialLine,
      lconvencoding, Graphics, FileUtil, Dialogs, Controls, Forms, LCLType, ComCtrls,
      SynEditKeyCmds, SynEditTypes, Menus, strUtils, IniFiles;
const
{English messages}
{  MSG_ROW = 'row=';
  MSG_COL = 'col=';
  MSG_SAVED = 'Saved';
  MSG_NO_SAVED = 'Modified';
  MSG_FILE_NOSAVED = 'File %s, has been modified.' +
                     #13#10 + '¿Save?';
  MSG_FILE_NOT_FOUND = 'File not found: ';
  MSG_ERROR_SAVING = 'Error saving file: ';
  MSG_OVERWRITE = 'File %s already exists.' + #13#10 +
                  'Overwrite?';
  MSG_RECENTS = '&Recents';
  MSG_NO_RECENTS = 'No files';
  MSG_EDIT_NO_INIT = 'Internal: Not initialized Editor.';}
{Mnnsajes en español}
  MSG_ROW = 'fil=';
  MSG_COL = 'col=';
  MSG_SAVED = 'Guardado';
  MSG_NO_SAVED = 'Sin Guardar';
  MSG_FILE_NOSAVED = 'El archivo %s ha sido modificado.' +  #13#10 +
                     '¿Deseas guardar los cambios?';
  MSG_FILE_NOT_FOUND = 'No se encuentra el archivo: ';
  MSG_ERROR_SAVING = 'Error guardando archivo: ';
  MSG_OVERWRITE = 'El archivo %s ya existe.' + #13#10 +
                  '¿Deseas sobreescribirlo?';
  MSG_RECENTS = '&Recientes';
  MSG_NO_RECENTS = 'No hay archivos';
  MSG_EDIT_NO_INIT = 'Error Interno: Editor no inicializado.';

type
  //Tipos de delimitador de línea de archivo.
  TDelArc = (TAR_DESC,    //Tipo desconocido
             TAR_DOS,     //Tipo Windows/DOS
             TAR_UNIX,    //Tipo Unix/Linux
             TAR_MAC      //Tipo Mac OS
             );

  { TObjEditor }
  TEventoArchivo = procedure of object;

  //Define las propiedades que debe tener un texto que se está editando
  TObjEditor = class
    procedure edMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    procedure edChange(Sender: TObject);
    procedure edCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure itemClick(Sender: TObject);
    procedure menRecentsClick(Sender: TObject);
  private
    ed      : TSynEdit;  //referencia al editor
    ArcRecientes: TStringList;  //Lista de archivos recientes
    menRecents: TMenuItem; //Menú de archivos recientes
    MaxRecents: integer;   //Máxima cantidad de archivos recientes
    procedure ActualMenusReciente;
    procedure AgregArcReciente(arch: string);
    procedure ChangeFileInform;
    procedure GuardarADisco(var arcINI: TIniFile; etiq: string='edit');
    procedure LeerDeDisco(var arcINI: TIniFile; etiq: string='edit');
    //Estado de modificación
    procedure SetModified(valor: boolean);
    function GetModified: boolean;
  public
    NomArc  : string;    //nombre del archivo
    DelArc  : TDelArc;   //Tipo de delimitador de fin de línea
    CodArc  : string;    //codificación de archivo
    linErr  : integer;   //línea de error. SOlo usada para marcar un error
    Error   : string;    //mensaje de error en alguna operación
    extDef  : string;    //extensión por defecto para los archivos (txt, xml, ...)
    nomDef  : string;    //nombre por defecto pàra nuevos archivos
    //eventos
    OnChangeEditorState:TEventoArchivo;  {Cuando cambia el estado de modificado, con opción
                          "Undo", con "Redo", con opción "Copiar", "Cortar", "Pegar"}
    OnChangeFileInform: TEventoArchivo;  {Cuando cambia información de nombre de archivo, tipo
                           de delimitador de línea o tipo de codificación}
    OnSelectionChange : TEventoArchivo; //Cuando cambia el área seleccionada
    OnFileOpened : TEventoArchivo; //Cuando se ha cargado un nuevo archivo
    OnEditChange : TNotifyEvent;   //Reflejo del evento OnChange() de TSynEdit;
    //paneles con información del estado del editor
    PanFileSaved : TStatusPanel;  //Panel para mensaje "Guardado"
    PanCursorPos : TStatusPanel;  //Panel para mostrar posición del cursor
    //paneles para información del archivo
    PanFileName  : TStatusPanel;  //Panel para mostrar el nombre de archivo
    PanForEndLin : TStatusPanel;  //Panel para mostrar el tipo de delimitador de línea
    PanCodifFile : TStatusPanel;  //Panel para mostrar la codificaión de archivo
    procedure InitEditor(ed0: TsynEdit; nomDef0, extDef0: string);
    procedure InitMenuRecents(menRecents0: TMenuItem; MaxRecents0: integer=5);
    procedure NewFile;
    procedure LoadFile(arc8: string);
    procedure SaveFile;
    function OpenDialog(OpenDialog1: TOpenDialog): boolean;
    function SaveAsDialog(SaveDialog1: TSaveDialog): boolean;
    function SaveQuery: boolean;
    procedure ChangeEndLineDelim(nueFor: TDelArc);
    procedure CambiaCodific(nueCod: string);
    //Espejo de funciones comunes del editor
    procedure Cut;
    procedure Copy;
    procedure Paste;
    procedure Undo;
    procedure Redo;
    //Lee estado
    function CanUndo: boolean;
    function CanRedo: boolean;
    function CanCopy: boolean;
    function CanPaste: boolean;

    property Modified: boolean read GetModified;
    constructor Create;
    destructor Destroy; override;
  end;

procedure InicEditorC1(ed: TSynEdit);
procedure StringToFile(const s: string; const FileName: string);
function StringFromFile(const FileName: string): string;
procedure VerTipoArchivo(archivo: string; var Formato: TDelArc; var Codificacion: string);
function Descrip_DelArc(DelArc: TDelArc): string;
function CargarArchivoLin(arc8: string; Lineas: TStrings;
                           var TipArc: TDelArc; var CodArc: string): string;
function GuardarArchivoLin(arc0: string; Lineas: TStrings;
                           var TipArc: TDelArc; var CodArc: string): string;

implementation
const szChar = SizeOf(Char);
procedure msgErr(msje: string);  //Rutina útil
//Mensaje de error
begin
  Application.MessageBox(PChar(msje), '', MB_ICONERROR);
end;
procedure InicEditorC1(ed: TSynEdit);
//Inicia un editor con una configuración especial para empezar a trabajar con el.
var
  SynMarkup: TSynEditMarkupHighlightAllCaret;  //para resaltar palabras iguales
begin
   //Inicia resaltado de palabras iguales
  SynMarkup := TSynEditMarkupHighlightAllCaret(ed.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
  SynMarkup.MarkupInfo.FrameColor := clSilver;
  SynMarkup.MarkupInfo.Background := TColor($FFF0B0);

  SynMarkup.WaitTime := 250; // millisec
  SynMarkup.Trim := True;     // no spaces, if using selection
  SynMarkup.FullWord := True; // only full words If "Foo" is under caret, do not mark it in "FooBar"
  SynMarkup.IgnoreKeywords := False;

  //  ed.Font.Name:='Courier New';
  //  ed.Font.Size:=10;
  ed.Options:=[eoHideRightMargin,eoBracketHighlight];  //quita la línea vertical
  ed.Options := ed.Options + [eoKeepCaretX];  //Limita posición X del cursor para que no escape de la línea
  ed.Options := ed.Options + [eoTabIndent];  //permite indentar con <Tab>
  ed.Options2 := ed.Options2 + [eoCaretSkipTab];  //trata a las tabulaciones como un caracter
end;
procedure StringToFile(const s: string; const FileName: string);
///   saves a string to a file
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    FileStream.WriteBuffer(Pointer(s)^, (Length(s) * szChar));
  finally
    FreeAndNil(FileStream);
  end; // try
end;
function StringFromFile(const FileName: string): string;
///   returns the content of the file as a string
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Result, (FileStream.Size div szChar));
    FileStream.ReadBuffer(Pointer(Result)^, FileStream.Size);
  finally
    FreeAndNil(FileStream);
  end; // try
end;
procedure VerTipoArchivo(archivo: string; var Formato: TDelArc; var Codificacion: string);
(*Obtiene el tipo de delimitador de línea (Line Ending) de un archivo de texto, explorando
 los primeros bytes de archivo. Solo explora los primeros 8K del archivo.
 Si no encuentra un salto de línea en ese tamaño, no podrá deetrminar de que tipo de
 archivo se trata. También explora el posible tipo de codificación usado.
 *)
const TAM_BOL = 8192;
var ar: file;
    bolsa : Array[0..TAM_BOL] of char;  //deja un byte más para el delimitador
    Pbolsa: PChar;  //variable Pchar para "bolsa"
    Pos13: Word;    //posición de caracter #13
    Pos10: Word;    //posición de caracter #10
    Leidos: Word;   //bytes leidos
begin
   //Lee bloque de datos
   AssignFile(ar,archivo);
   reset(ar,1);
   BlockRead(ar,Bolsa,TAM_BOL,Leidos);  //Lectura masiva
   CloseFile(ar);
   bolsa[Leidos] := #0; //agrega delimitador
   Pbolsa := @bolsa;    //cadena PChar
   //Explora en busca de delimitadores de línea
   Pos13 := Pos(#13, Pbolsa);
   Pos10 := Pos(#10, Pbolsa);
   if Pos13 = 0 then
      //solo hay separador #10 o ninguno
      if Pos10<>0 then
         Formato := TAR_UNIX     //solo hay #10
      else
         Formato := TAR_DESC  //no se encontró separador
   else if Pos10 = 0 then
      //solo hay separador #13 o ninguno
      if Pos13 <> 0 then
         Formato := TAR_MAC     //solo hay #13
      else
         Formato := TAR_DESC  //no se encontró separador
   else if Pos10 = Pos13 + 1 then
      Formato := TAR_DOS    //no se encontró #13#10
   else
      Formato := TAR_DESC;  //no se reconoce delimitadores
   //Analiza codifiación
   Codificacion := GuessEncoding(Pbolsa);  //analiza los primeros bytes
{ TODO : Ver por qué no detectó correctaente la carga de un archivo UTF-8 sin BOM }
end;
function Descrip_DelArc(DelArc: TDelArc): string;
//proporciona una descripción al tipo de delimitador
begin
  case DelArc of
    TAR_DOS : Result := 'DOS/Win';  //DOS/Windows
    TAR_UNIX: Result := 'UNIX/Linux';
    TAR_MAC : Result := 'MAC OS';
    TAR_DESC: Result := 'Unknown'; //'Desconoc.';
  end;
end;
function CargarArchivoLin(arc8: string; Lineas: TStrings;
                           var TipArc: TDelArc; var CodArc: string): string;
{Carga el contenido de un archivo en un "TStrings". Si la codificación es diferente de
 UTF-8, hace la conversión. Esta pensado para usarse en un SynEdit.
 Además actualiza el Tipo de Delimitador de línea y la Codificación.
 Devuelve una cadena que indica si hubo conversión }
var
  arc0: String;
begin
  CodArc := '';
  arc0 := UTF8ToSys(arc8);   //pone en modo ANSI
  VerTipoArchivo(arc0, TipArc, CodArc);  //actualiza tipo de archivo de trabajo
  //Carga archivo solicitado
  Lineas.LoadFromFile(arc8);
  //realiza las conversiones necesarias, ya que "ed", solo maneja UTF-8
  if CodArc = 'cp1252' then begin
    Lineas.Text := CP1252ToUTF8(Lineas.Text);
    Result := 'Convertido a UTF-8';
  end
  else if CodArc = 'utf8bom' then begin
    Lineas.Text := UTF8BOMToUTF8(Lineas.Text);
    Result := 'Convertido a UTF-8';
  end
  else if CodArc = 'ISO-8859-1' then begin
    Lineas.Text := ISO_8859_1ToUTF8(Lineas.Text);
    Result := 'Convertido a UTF-8';
  end else begin  //cualquier otra codificación se asume UTF-8 y no se cambia
    Result := 'utf8';
  end;
end;
function GuardarArchivoLin(arc0: string; Lineas: TStrings;
                           var TipArc: TDelArc; var CodArc: string): string;
{Guarda el contenido de un "TStrings" en un archivo. Si la codificación es diferente de
 UTF-8, hace la conversión. Esta pensado para usarse en un SynEdit.
 Además usa el Tipo de Delimitador de línea para guardar el archivo.
 Devuelve una cadena con un mensaje de error, si es que lo hubiera. }
begin
  Result := '';  //sin error por defecto
  //configura tipo de separador
//  case TipArc of
//  TAR_DOS: TSynEditLines(ed.Lines).FileWriteLineEndType := sfleCrLf;
//  TAR_UNIX: TSynEditLines(ed.Lines).FileWriteLineEndType := sfleLf;
//  TAR_MAC: TSynEditLines(ed.Lines).FileWriteLineEndType := sfleCr;
//  TAR_DESCON: TSynEditLines(ed.Lines).FileWriteLineEndType := sfleCrLf;
//  end;
  case TipArc of
  TAR_DOS:  Lineas.TextLineBreakStyle := tlbsCRLF;
  TAR_UNIX: Lineas.TextLineBreakStyle := tlbsLF;
  TAR_MAC:  Lineas.TextLineBreakStyle := tlbsCR;
  TAR_DESC: Lineas.TextLineBreakStyle := tlbsCRLF;
  end;

  if CodArc = 'utf8' then begin
    //opción sin conversión
    StringToFile(Lineas.Text,arc0);
  end
  else if CodArc = 'cp1252' then  begin
    StringToFile(UTF8ToCP1252(Lineas.Text),arc0);
  end
  else if CodArc = 'utf8bom' then begin
    StringToFile(UTF8ToUTF8BOM(Lineas.Text),arc0);
  end
  else if CodArc = 'ISO-8859-1' then begin
    StringToFile(UTF8ToISO_8859_1(Lineas.Text),arc0);
  end
  else begin //si es otra codificación, se guarda como UTF-8
    ShowMessage('¡Codificación de archivo desconocida!');   //muestra
    StringToFile(Lineas.Text,arc0);
  end;
end;

{ TObjEditor }

procedure TObjEditor.edMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if PanCursorPos <> nil then
    PanCursorPos.Text:= MSG_ROW + IntToStr(ed.CaretY) + ', '+ MSG_COL + IntToStr(ed.CaretX);
  linErr := 0;  //para que quite la marca de fondo del error.
                //Solo se notará cuando se refresque la línea en el editor.
end;
procedure TObjEditor.edStatusChange(Sender: TObject; Changes: TSynStatusChanges);
//Cambia el estado del editor
begin
  if scSelection in changes then begin   //cambios en la selección
    if OnSelectionChange<>nil then OnSelectionChange;  //dispara eventos
    if OnChangeEditorState<>nil then OnChangeEditorState;  //para iniciar controles
  end;
end;
procedure TObjEditor.edChange(Sender: TObject);
begin
  if PanFileSaved <> nil then begin
    if GetModified then PanFileSaved.Text:=MSG_NO_SAVED else PanFileSaved.Text:=MSG_SAVED;
  end;
  //Ha habido cambio de contenido
  if OnChangeEditorState<>nil then OnChangeEditorState;  //para iniciar controles
  if OnEditChange <> nil then OnEditChange(Sender);  //pasa el evento.
end;
procedure TObjEditor.edCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  if PanCursorPos <> nil then
    PanCursorPos.Text:= MSG_ROW + IntToStr(ed.CaretY) + ', '+ MSG_COL + IntToStr(ed.CaretX);
  linErr := 0;  //para que quite la marca de fondo del error.
                //Solo se notará cuando se refresque la línea en el editor.
end;

procedure TObjEditor.InitEditor(ed0: TsynEdit; nomDef0, extDef0: string);
//Inicia el objeto con la referencia al editor, y parámetros fijos.
begin
  ed := ed0;
  nomDef := nomDef0;
  extDef := extDef0;
  NewFile;   //Inicia editor con archivo vacío
  //refresca controles con el estado inicial
  edChange(nil);       //para iniciar controles
  if PanCursorPos <> nil then
   PanCursorPos.Text:= MSG_ROW + IntToStr(ed.CaretY) + ', '+ MSG_COL + IntToStr(ed.CaretX);
//  if OnChangeEditorState<>nil then OnChangeEditorState;  //para iniciar controles
  //intercepta eventos
  ed.OnChange:=@edChange;  //necesita interceptar los cambios
  ed.OnCommandProcessed:=@edCommandProcessed;  //necesita para actualizar el cursor
  ed.OnStatusChange:=@edStatusChange;
  ed.OnMouseDown:=@edMouseDown;
end;
//Manejo de archivos recientes
procedure TObjEditor.AgregArcReciente(arch: string);
//Agrega el nombre de un archivo reciente
var hay: integer; //bandera-índice
    i: integer;
begin
  //verifica si ya existe
  hay := -1;   //valor inicial
  for i:= 0 to ArcRecientes.Count-1 do
    if ArcRecientes[i] = arch then hay := i;
  if hay = -1 then  //no existe
    ArcRecientes.Insert(0,arch)  //agrega al inicio
  else begin //ya existe
    ArcRecientes.Delete(hay);     //lo elimina
    ArcRecientes.Insert(0,arch);  //lo agrega al inicio
  end;
  while ArcRecientes.Count>MaxRecents do  //mantiene tamaño máximo
    ArcRecientes.Delete(MaxRecents);
end;
procedure TObjEditor.itemClick(Sender: TObject);
//Se selecciona un archivo de la lista de recientes
begin
   LoadFile(MidStr(TMenuItem(Sender).Caption,4,150));
end;
procedure TObjEditor.menRecentsClick(Sender: TObject);
//Evento del menú de archivos recientes
begin
  ActualMenusReciente;  //carga la lista de archivos recientes
end;
procedure TObjEditor.InitMenuRecents(menRecents0: TMenuItem; MaxRecents0: integer=5);
//Configura un menú, con el historial de los archivos abiertos recientemente
//"nRecents", es el número de archivos recientes que se guardará
var item: TMenuItem;
  i: Integer;
begin
  menRecents := menRecents0;
  MaxRecents := MaxRecents0;
  //configura menú
  menRecents.Caption:= MSG_RECENTS;
  menRecents.OnClick:=@menRecentsClick;
  for i:= 1 to MaxRecents do begin
    item := TMenuItem.Create(nil);
    item.Caption:= '&'+IntToStr(i);  //nombre
    item.OnClick:=@itemClick;
    menRecents.Add(item);
  end;
end;
procedure TObjEditor.ActualMenusReciente;
{Actualiza el menú de archivos recientes con la lista de los archivos abiertos
recientemente. }
var
  i: Integer;
begin
  if menRecents = nil then exit;
  //proteciión
  if ArcRecientes.Count = 0 then begin
    menRecents[0].Caption:=MSG_NO_RECENTS;
    menRecents[0].Enabled:=false;
    for i:= 1 to menRecents.Count-1 do begin
      menRecents[i].Visible:=false;
    end;
    exit;
  end;
  //hace visible los ítems
  menRecents[0].Enabled:=true;
  for i:= 0 to menRecents.Count-1 do begin
    if i<ArcRecientes.Count then
      menRecents[i].Visible:=true
    else
      menRecents[i].Visible:=false;
  end;
  //pone etiquetas a los menús, incluyendo un atajo numérico
  for i:=0 to ArcRecientes.Count-1 do begin
    menRecents[i].Caption := '&'+IntToStr(i+1)+' '+ArcRecientes[i];
  end;
end;
procedure TObjEditor.LeerDeDisco(var arcINI: TIniFile; etiq: string = 'edit');
//Lee las propiedades de disco.
//El parámetro "etiq", se usa para cuando se quiere guardar varios editores
begin
  //Lee archivos recientes
  arcINI.ReadSection(etiq+'_Recientes',ArcRecientes);
end;
procedure TObjEditor.GuardarADisco(var arcINI: TIniFile;  etiq: string = 'edit');
//lee las propiedades de disco
//El parámetro "etiq", se usa para cuando se quiere guardar varios editores
var
  i : integer;
begin
  //Escribe archivos recientes
  arcINI.EraseSection(etiq+'_Recientes');
  for i:= 0 to ArcRecientes.Count-1 do
    arcINI.WriteString(etiq+'_Recientes',ArcRecientes[i],'');
end;

procedure TObjEditor.SetModified(valor: boolean);
//Cambia el valor del campo "Modified", del editor
begin
  if ed.Modified<> valor then begin
    //se ha cambiado el estado de "Modificado"
    ed.Modified := valor;    //Fija valor
    //dispara evento
    if PanFileSaved <> nil then begin
      if GetModified then PanFileSaved.Text:=MSG_NO_SAVED else PanFileSaved.Text:=MSG_SAVED;
    end;
    if OnChangeEditorState<>nil then OnChangeEditorState;
  end;
end;
function TObjEditor.GetModified: boolean;
//Lee el valor del campo "Modified", del editor.
begin
  Result := ed.Modified;
end;

procedure TObjEditor.ChangeFileInform;
//Se debe llamar siempre que puede cambiar la información de nombre de archivo, tipo de
//delimitador de línea o tipo de codificación del archivo.
begin
  //actualiza información en los paneles
  if PanFileName <> nil then begin
    PanFileName.Text := SysToUTF8(NomArc);
  end;
  if PanForEndLin <> nil then begin
    PanForEndLin.Text:=Descrip_DelArc(DelArc);
  end;
  if PanCodifFile <> nil then begin
    PanCodifFile.Text:=CodArc;
  end;
  //dispara evento
  if OnChangeFileInform<>nil then OnChangeFileInform;
end;
procedure TObjEditor.NewFile;
//Inicia al editor con un nuevo nombre de archivo
begin
  if SaveQuery then Exit;   //Verifica cambios
  if Error<>'' then exit;  //hubo error
  Error := '';    //limpia bandera de error
  if extDef<> '' then //genera nombre por defecto
    NomArc := nomDef + '.' + extDef
  else NomArc := nomDef;
  //verifica existencia
//  if FileExists(Arc) then   //ya existe
//     AbrirArchivo(Arc)  //lo abre
//  else begin   //no existe
//    mnArGuarClick(nil);  //Lo crea
  DelArc := TAR_DOS;  //inicia con Windows por defecto
  CodArc := 'cp1252'; //inicia en formato Windows
  ed.ClearAll;        //limpia editor
  ed.ClearUndo;       //limpia acciones "deshacer"
  SetModified(false);
  ChangeFileInform;   //actualiza
end;

procedure TObjEditor.LoadFile(arc8: string);
//Carga el contenido de un archivo en el editor, analizando la codificación.
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
var
  arc0: String;
begin
  Error := '';    //limpia bandera de error
  arc0 := UTF8ToSys(arc8);   //pone en modo ANSI
  //verifica existencia de archivo
  if not FileExists(arc0) then begin
    Error := MSG_FILE_NOT_FOUND + arc0;
    msgErr(Error);
    Exit;                    //sale
  end;
  //carga y lee formato
  CargarArchivoLin(arc8, ed.Lines, DelArc, CodArc);
//  StatusBar1.Panels[4].Text := CodArc;  //actualiza codificación
  NomArc := arc0;         //fija nombre de archivo de trabajo
  SetModified(false);  //Inicia estado
  linErr := 0;            //limpia línea marcada por si acaso
  ChangeFileInform;   //actualiza
  if OnFileOpened<>nil then OnFileOpened;  //dispara evento
  AgregArcReciente(arc8);  //agrega a lista de recientes
end;

procedure TObjEditor.SaveFile;
//Guarda el contenido del editor en su archivo correspondiente
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
begin
  Error := '';    //limpia bandera de error
  try
    GuardarArchivoLin(NomArc, ed.Lines, DelArc, CodArc);  //guarda en formato original
    SetModified(false);
    edChange(self);  //para que actualice el panel PanFileSaved
    //se actualiza por si acaso, se haya guardado con otro nombre
    ChangeFileInform;   //actualiza
  except
    Error := MSG_ERROR_SAVING + NomArc;
    msgErr(Error);
  end;
end;

function TObjEditor.OpenDialog(OpenDialog1: TOpenDialog): boolean;
//Muestra el cuadro de diálogo para abrir un archivo, teniend cuidado de
//pedir confirmación para grabar el contenido actual.
var arc0: string;
begin
  if SaveQuery then Exit;   //Verifica cambios
  if Error<>'' then exit;  //hubo error
  if not OpenDialog1.Execute then exit;    //se canceló
  arc0 := OpenDialog1.FileName;
  LoadFile(arc0);  //legalmente debería darle en UTF-8
end;

function TObjEditor.SaveAsDialog(SaveDialog1: TSaveDialog): boolean;
//Guarda el contenido del editor, permitiendo cambiar el nombre con un diálogo.
//Si se ignora la acción, devuelve "true".
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
var
  arc0: String;
  resp: TModalResult;
begin
  Result := false;
  if not SaveDialog1.Execute then begin  //se canceló
    Result := true;   //Sale con "true"
    exit;    //se canceló
  end;
  arc0 := SaveDialog1.FileName;
  if FileExists(arc0) then begin
    resp := MessageDlg('', Format(MSG_OVERWRITE,[arc0]),
                       mtConfirmation, [mbYes, mbNo, mbCancel],0);
    if (resp = mrCancel) or (resp = mrNo) then Exit;
  end;
  NomArc := UTF8ToSys(arc0);   //asigna nuevo nombre
  if ExtractFileExt(NomArc) = '' then NomArc += '.'+extDef;  //completa extensión
  SaveFile;   //lo guarda
end;

function TObjEditor.SaveQuery: boolean;
//Verifica si es necesario guardar el archivo antes de ejecutar alguna oepración con el editor.
//Si se ignora la acción, devuelve "true".
//Si ocurre algún error, muestra el mensaje en pantalla y actualiza "Error".
var resp: integer;
begin
  Result := false;
  if ed = nil then begin
    Error := MSG_EDIT_NO_INIT;
    msgErr(Error);
    exit;
  end;
  if ed.Modified then begin
    resp := MessageDlg('', format(MSG_FILE_NOSAVED,[ExtractFileName(NomArc)]),
                       mtConfirmation, [mbYes, mbNo, mbCancel],0);
    if resp = mrCancel then begin
      Result := true;   //Sale con "true"
      Exit;
    end;
    if resp = mrYes then begin  //guardar
//       if arc = 'SinNombre.sql' then
//          mnArGuarCClick(NIL)
//       else
      SaveFile;  //Actualizar "Error"
    end;
  end;
end;

procedure TObjEditor.ChangeEndLineDelim(nueFor: TDelArc);
//Cambia el formato de salto de línea del contenido
begin
  if DelArc <> nueFor then begin  //verifica si hay cambio
    DelArc := nueFor;
    SetModified(true); //para indicar que algo ha cambiado
    ChangeFileInform;   //actualiza
  end;
end;
procedure TObjEditor.CambiaCodific(nueCod: string);
//Cambia la codificación del archivo
begin
  if CodArc <> nueCod then begin
    CodArc := nueCod;
    SetModified(true); //para indicar que algo ha cambiado
    ChangeFileInform;   //actualiza
  end;
end;

procedure TObjEditor.Cut;
begin
  ed.CutToClipboard;
end;
procedure TObjEditor.Copy;
begin
  ed.CopyToClipboard;
end;
procedure TObjEditor.Paste;
begin
  ed.PasteFromClipboard;
end;
procedure TObjEditor.Undo;
//Deshace una acción en el editor
begin
  ed.Undo;
end;
procedure TObjEditor.Redo;
//Rehace una acción en el editor
begin
  ed.Redo;
end;

function TObjEditor.CanUndo: boolean;
//Indica si Hay Algo por deshacer
begin
  Result := ed.CanUndo;
end;
function TObjEditor.CanRedo: boolean;
//Indica si Hay Algo por rehacer
begin
  Result := ed.CanRedo;
end;
function TObjEditor.CanCopy: boolean;
//Indica si hay algo por copiar
begin
  Result := ed.SelAvail;
end;
function TObjEditor.CanPaste: boolean;
//Indica si Hay Algo por pegar
begin
  Result := ed.CanPaste;
end;

constructor TObjEditor.Create;
begin
  ArcRecientes := TStringList.Create;
  MaxRecents := 1;   //Inicia con 1
end;
destructor TObjEditor.Destroy;
begin
  ArcRecientes.Free;
  inherited Destroy;
end;

end.

