{                                   PropertyFrame 0.0
 Unidad para interceptar la clase TFrame y usar un TFrame personalizado que facilite la
 administración de propiedades. Incluye el manejo de entrada y salida a archivos INI.

                                                         Por Tito Hinostroza 10/07/2014
}
unit PropertyFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Spin, IniFiles, Dialogs, Graphics, Variants;

type
  //Tipos de asociaciones
  TTipPar = (
   tp_Int_TEdit     //entero asociado a TEdit
  ,tp_Int_TSpnEdit  //entero asociado a TSpinEdit
  ,tp_Str_TEdit     //string asociado a TEdit
  ,tp_Str_TCmbBox   //string asociado a TComboBox
  ,tp_Bol_TChkB     //booleano asociado a CheckBox
  ,tp_TCol_TColBut  //TColor asociado a TColorButton
  ,tp_Enum_TRadBut  //Enumerado asociado a TRadioButton
  ,tp_Int           //Entero sin asociación
  ,tp_Bol           //Boleano sin asociación
  ,tp_Str           //String sin asociación
  ,tp_StrList       //TStringList sin asociación
  );

  //Para variable, elemento
  TParElem = record
    pVar: pointer;     //referencia a la variable
    lVar: integer;     //tamaño de variable. (Cuando no sea conocido)
    pCtl: TComponent;  //referencia al control
    radButs: array of TRadioButton;  //referencia a controles TRadioButton (se usan en conjunto)
    tipPar: TTipPar;   //tipo de par agregado
    etiqVar: string;   //etiqueta usada para grabar la variable en archivo INI
    minEnt, maxEnt: integer;  //valores máximos y mínimos para variables enteras
    //valores por defecto
    defEnt: integer;   //valor entero por defecto al leer de archivo INI
    defStr: string;    //valor string por defecto al leer de archivo INI
    defBol: boolean;   //valor booleano por defecto al leer de archivo INI
    defCol: TColor;    //valor TColor por defecto al leer de archivo INI
  end;

  { TFrame }

  TFrame = class(Forms.Tframe)   //TFrame personalizado
//  TFrame = class(TcustomFrame)   //TFrame personalizado
  private
    listParElem : array of TParElem;
  protected
    valInt: integer;  //valor entero de salida
  public
    secINI: string;   //sección donde se guardaran los datos en un archivo INI
    MsjErr: string;   //mensaje de error
    OnUpdateChanges: procedure of object;
    procedure ShowPos(x, y: integer);
    function EditValidateInt(edit: TEdit; min: integer=MaxInt; max: integer=-MaxInt): boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure PropToWindow; virtual;
    procedure WindowToProp; virtual;
    procedure ReadFileToProp(var arcINI: TIniFile); virtual;
    procedure SavePropToFile(var arcINI: TIniFile); virtual;
    //métodos para agregar pares- variable-control
    procedure Asoc_Int_TEdit(ptrInt: pointer; edit: TEdit; etiq: string;
                             defVal, minVal, maxVal: integer);
    procedure Asoc_Int_TSpnEdi(ptrInt: pointer; spEdit: TSpinEdit; etiq: string;
                             defVal, minVal, maxVal: integer);
    procedure Asoc_Str_TEdit(ptrStr: pointer; edit: TEdit; etiq: string;
                             defVal: string);
    procedure Asoc_Str_TCmbBox(ptrStr: pointer; cmbBox: TComboBox; etiq: string;
                             defVal: string);
    procedure Asoc_Bol_TChkB(ptrBol: pointer; chk: TCheckBox; etiq: string;
                             defVal: boolean);
    procedure Asoc_Col_TColBut(ptrInt: pointer; colBut: TColorButton; etiq: string;
                             defVal: TColor);
    procedure Asoc_Enum_TRadBut(ptrEnum: pointer; EnumSize: integer;
                    radButs: array of TRadioButton; etiq: string; defVal: integer);
    //métodos para agregar valores sin asociación a controles
    procedure Asoc_Int(ptrInt: pointer; etiq: string; defVal: integer);
    procedure Asoc_Bol(ptrBol: pointer; etiq: string; defVal: boolean);
    procedure Asoc_Str(ptrStr: pointer; etiq: string; defVal: string);
    procedure Asoc_StrList(ptrStrList: pointer; etiq: string);
  end;

TlistFrames = array of Tframe;
  //utilidades para el formulario de configuración
  function IsFrameProperty(c: TComponent): boolean;
  function ListOfFrames(form: TForm): TlistFrames;
  function GetIniName(ext: string = 'ini'): string;

implementation

function IsFrameProperty(c: TComponent): boolean;
//Permite identificar si un componente es un Frame creado a partir de TFaame de
//esta unidad.
begin
  if (c.ClassParent.ClassName='TFrame') and
     (UpCase(c.ClassParent.UnitName) = UpCase('PropertyFrame')) then
     Result := true
  else
     Result := false;
end;
function ListOfFrames(form: TForm): Tlistframes;
//Devuelve la lista de frames del tipo TFrame declarado aquí
var
  i: Integer;
  n : integer;
  f: TFrame;
begin
  SetLength(Result,0);
  for i:= 0 to form.ComponentCount-1 do begin
    if IsFrameProperty(form.Components[i]) then begin
      f:=TFrame(form.Components[i]);  //obtiene referencia
      n := high(Result)+1;    //número de elementos
      setlength(Result, n+1);  //hace espacio
      Result[n] := f;          //agrega
    end;
  end;
end;
function GetIniName(ext: string = 'ini'): string;
//Devuelve el nombre del archivo INI, creándolo si no existiera
var F:textfile;
begin
  Result := ChangeFileExt(Application.ExeName,'.'+ext);
  if not FileExists(Result) then begin
    ShowMessage('No se encuentra archivo de configuración: '+Result);
    //crea uno vacío para leer las opciones por defecto
    AssignFile( F, Result);
    Rewrite( F );
    CloseFile( F );
  end;
end;

constructor TFrame.Create(TheOwner: TComponent);
begin
  inherited;
  setlength(listParElem, 0)
end;
destructor TFrame.Destroy;
begin

  inherited Destroy;
end;

procedure TFrame.PropToWindow;
//Muestra en los controles, las variables asociadas
var
  i:integer;
  r: TParElem;
  n: integer;
  b: boolean;
  s: string;
  c: TColor;
begin
  msjErr := '';
  for i:=0 to high(listParElem) do begin
    r := listParElem[i];
    case r.tipPar of
    tp_Int_TEdit:  begin  //entero en TEdit
          //carga entero
          n:= Integer(r.Pvar^);
          TEdit(r.pCtl).Text:=IntToStr(n);
       end;
    tp_Int_TSpnEdit: begin  //entero en TSpinEdit
          //carga entero
          n:= Integer(r.Pvar^);
          TSpinEdit(r.pCtl).Value:=n;
       end;
    tp_Str_TEdit:  begin  //cadena en TEdit
          //carga cadena
          s:= String(r.Pvar^);
          TEdit(r.pCtl).Text:=s;
       end;
    tp_Str_TCmbBox: begin  //cadena en TComboBox
          //carga cadena
          s:= String(r.Pvar^);
          TComboBox(r.pCtl).Text:=s;
       end;
    tp_Bol_TChkB: begin //boolean a TCheckBox
          b := boolean(r.Pvar^);
          TCheckBox(r.pCtl).Checked := b;
       end;
    tp_TCol_TColBut: begin //Tcolor a TColorButton
          c := Tcolor(r.Pvar^);
          TColorButton(r.pCtl).ButtonColor := c;
       end;
    tp_Enum_TRadBut: begin //Enumerado a TRadioButtons
          if r.lVar = 4 then begin  //enumerado de 4 bytes
            n:= Int32(r.Pvar^);  //convierte a entero
            if n<=High(r.radButs) then
              r.radButs[n].checked := true;  //lo activa
          end else begin  //tamño no implementado
            msjErr := 'Typo enumerado no manejable.';
            exit;
          end;
       end;
    tp_Int:; //no tiene control asociado
    tp_Bol:; //no tiene control asociado
    tp_Str:; //no tiene control asociado
    tp_StrList:; //no tiene control asociado
    else  //no se ha implementado bien
      msjErr := 'Error de diseño.';
      exit;
    end;
  end;
end;
procedure TFrame.WindowToProp;
//Lee en las variables asociadas, los valores de loc controles
var
  i,j: integer;
  spEd: TSpinEdit;
  r: TParElem;
begin
  msjErr := '';
  for i:=0 to high(listParElem) do begin
    r := listParElem[i];
    case r.tipPar of
    tp_Int_TEdit:  begin  //entero de TEdit
          if not EditValidateInt(TEdit(r.pCtl),r.minEnt, r.MaxEnt) then
            exit;   //hubo error. con mensaje en "msjErr"
          Integer(r.Pvar^) := valInt;  //guarda
       end;
    tp_Int_TSpnEdit: begin   //entero de TSpinEdit
          spEd := TSpinEdit(r.pCtl);
          if spEd.Value < r.minEnt then begin
            MsjErr:='El menor valor permitido es: '+IntToStr(r.minEnt);
            if spEd.visible and spEd.enabled then spEd.SetFocus;
            exit;
          end;
          if spEd.Value > r.maxEnt then begin
            MsjErr:='El mayor valor permitido es: '+IntToStr(r.maxEnt);
            if spEd.visible and spEd.enabled then spEd.SetFocus;
            exit;
          end;
          Integer(r.Pvar^) := spEd.Value;
       end;
    tp_Str_TEdit: begin  //cadena de TEdit
          String(r.Pvar^) := TEdit(r.pCtl).Text;
       end;
    tp_Str_TCmbBox: begin //cadena de TComboBox
          String(r.Pvar^) := TComboBox(r.pCtl).Text;
       end;
    tp_Bol_TChkB: begin  //boolean de  CheckBox
          boolean(r.Pvar^) := TCheckBox(r.pCtl).Checked;
       end;
    tp_TCol_TColBut: begin //TColor a TColorButton
          TColor(r.Pvar^) := TColorButton(r.pCtl).ButtonColor;
       end;
    tp_Enum_TRadBut: begin //TRadioButtons a Enumerado
          //busca el que está marcado
          for j:=0 to high(r.radButs) do begin
             if r.radButs[j].checked then begin
               //debe fijar el valor del enumerado
               if r.lVar = 4 then begin  //se puede manejar como entero
                 Int32(r.Pvar^) := j;  //guarda
                 break;
               end else begin  //tamaño no implementado
                 msjErr := 'Typo enumerado no manejable.';
                 exit;
               end;
             end;
          end;
       end;
    tp_Int:; //no tiene control asociado
    tp_Bol:; //no tiene control asociado
    tp_Str:; //no tiene control asociado
    tp_StrList:; //no tiene control asociado
    else  //no se ha implementado bien
      msjErr := 'Error de diseño.';
      exit;
    end;
  end;
  //Terminó con éxito. Actualiza los cambios
  if OnUpdateChanges<>nil then OnUpdateChanges;
end;
procedure TFrame.ReadFileToProp(var arcINI: TIniFile);
//Lee de disco las variables registradas
var
  i: integer;
  r: TParElem;
begin
  for i:=0 to high(listParElem) do begin
    r := listParElem[i];
    case r.tipPar of
    tp_Int_TEdit:  begin  //lee entero
         Integer(r.Pvar^) := arcINI.ReadInteger(secINI, r.etiqVar, r.defEnt);
       end;
    tp_Int_TSpnEdit: begin  //lee entero
         Integer(r.Pvar^) := arcINI.ReadInteger(secINI, r.etiqVar, r.defEnt);
       end;
    tp_Str_TEdit: begin  //lee cadena
         String(r.Pvar^) := arcINI.ReadString(secINI, r.etiqVar, r.defStr);
       end;
    tp_Str_TCmbBox: begin  //lee cadena
         String(r.Pvar^) := arcINI.ReadString(secINI, r.etiqVar, r.defStr);
       end;
    tp_Bol_TChkB: begin  //lee booleano
         boolean(r.Pvar^) := arcINI.ReadBool(secINI, r.etiqVar, r.defBol);
       end;
    tp_TCol_TColBut: begin  //lee TColor
         TColor(r.Pvar^) := arcINI.ReadInteger(secINI, r.etiqVar, r.defCol);
       end;
    tp_Enum_TRadBut: begin  //lee enumerado como entero
         if r.lVar = 4 then begin
           Int32(r.Pvar^) := arcINI.ReadInteger(secINI, r.etiqVar, r.defEnt);
         end else begin  //tamaño no implementado
           msjErr := 'Typo enumerado no manejable.';
           exit;
         end;
       end;
    tp_Int: begin  //lee entero
         Integer(r.Pvar^) := arcINI.ReadInteger(secINI, r.etiqVar, r.defEnt);
       end;
    tp_Bol: begin  //lee booleano
         boolean(r.Pvar^) := arcINI.ReadBool(secINI, r.etiqVar, r.defBol);
       end;
    tp_Str: begin  //lee cadena
         String(r.Pvar^) := arcINI.ReadString(secINI, r.etiqVar, r.defStr);
       end;
    tp_StrList: begin //lee TStringList
         arcINI.ReadSection(secINI+'_'+r.etiqVar,TStringList(r.Pvar^));
       end;
    else  //no se ha implementado bien
      msjErr := 'Error de diseño.';
      exit;
    end;
  end;
  //Terminó con éxito. Actualiza los cambios
  if OnUpdateChanges<>nil then OnUpdateChanges;
end;
procedure TFrame.SavePropToFile(var arcINI: TIniFile);
//Guarda en disco las variables registradas
var
  i,j: integer;
  r: TParElem;
  n: integer;
  b: boolean;
  s: string;
  c: TColor;
  strlst: TStringList;
begin
  for i:=0 to high(listParElem) do begin
    r := listParElem[i];
    case r.tipPar of
    tp_Int_TEdit:  begin  //escribe entero
         n := Integer(r.Pvar^);
         arcINI.WriteInteger(secINI, r.etiqVar, n);
       end;
    tp_Int_TSpnEdit: begin //escribe entero
         n := Integer(r.Pvar^);
         arcINI.WriteInteger(secINI, r.etiqVar, n);
       end;
    tp_Str_TEdit: begin //escribe cadena
         s := String(r.Pvar^);
         arcINI.WriteString(secINI, r.etiqVar, s);
       end;
    tp_Str_TCmbBox: begin //escribe cadena
         s := String(r.Pvar^);
         arcINI.WriteString(secINI, r.etiqVar, s);
       end;
    tp_Bol_TChkB: begin  //escribe booleano
         b := boolean(r.Pvar^);
         arcINI.WriteBool(secINI, r.etiqVar, b);
       end;
    tp_TCol_TColBut: begin  //escribe TColor
         c := Tcolor(r.Pvar^);
         arcINI.WriteInteger(secINI, r.etiqVar, c);
       end;
    tp_Enum_TRadBut: begin  //escribe enumerado
       if r.lVar = 4 then begin
         n := Int32(r.Pvar^);   //lo guarda como entero
         arcINI.WriteInteger(secINI, r.etiqVar, n);
       end else begin  //tamaño no implementado
         msjErr := 'Typo enumerado no manejable.';
         exit;
       end;
    end;
    tp_Int: begin //escribe entero
         n := Integer(r.Pvar^);
         arcINI.WriteInteger(secINI, r.etiqVar, n);
       end;
    tp_Bol: begin  //escribe booleano
         b := boolean(r.Pvar^);
         arcINI.WriteBool(secINI, r.etiqVar, b);
       end;
    tp_Str: begin //escribe cadena
         s := String(r.Pvar^);
         arcINI.WriteString(secINI, r.etiqVar, s);
       end;
    tp_StrList: begin
          strlst := TStringList(r.Pvar^);
          arcINI.EraseSection(secINI+'_'+r.etiqVar);
          for j:= 0 to strlst.Count-1 do
            arcINI.WriteString(secINI+'_'+r.etiqVar,strlst[j],'');
       end;
    else  //no se ha implementado bien
      msjErr := 'Error de diseño.';
      exit;
    end;
  end;
end;
procedure TFrame.Asoc_Int_TEdit(ptrInt: pointer; edit: TEdit; etiq: string;
  defVal, minVal, maxVal: integer);
//Agrega un para variable entera - Control TEdit
var n: integer;
  r: TParElem;
begin
  r.pVar   := ptrInt;  //toma referencia
  r.pCtl   := edit;    //toma referencia
  r.tipPar := tp_Int_TEdit;  //tipo de par
  r.etiqVar:= etiq;
  r.defEnt := defVal;
  r.minEnt := minVal;    //protección de rango
  r.maxEnt := maxVal;    //protección de rango
  //agrega
  n := high(listParElem)+1;    //número de elementos
  setlength(listParElem, n+1);  //hace espacio
  listParElem[n] := r;          //agrega
end;
procedure TFrame.Asoc_Int_TSpnEdi(ptrInt: pointer; spEdit: TSpinEdit;
  etiq: string; defVal, minVal, maxVal: integer);
//Agrega un para variable entera - Control TSpinEdit
var n: integer;
  r: TParElem;
begin
  r.pVar   := ptrInt;  //toma referencia
  r.pCtl   := spEdit;    //toma referencia
  r.tipPar := tp_Int_TSpnEdit;  //tipo de par
  r.etiqVar:= etiq;
  r.defEnt := defVal;
  r.minEnt := minVal;    //protección de rango
  r.maxEnt := maxVal;    //protección de rango
  //agrega
  n := high(listParElem)+1;    //número de elementos
  setlength(listParElem, n+1);  //hace espacio
  listParElem[n] := r;          //agrega
end;
procedure TFrame.Asoc_Str_TEdit(ptrStr: pointer; edit: TEdit; etiq: string;
  defVal: string);
//Agrega un par variable string - Control TEdit
var n: integer;
  r: TParElem;
begin
  r.pVar   := ptrStr;  //toma referencia
  r.pCtl   := edit;    //toma referencia
  r.tipPar := tp_Str_TEdit;  //tipo de par
  r.etiqVar:= etiq;
  r.defStr := defVal;
  //agrega
  n := high(listParElem)+1;    //número de elementos
  setlength(listParElem, n+1);  //hace espacio
  listParElem[n] := r;          //agrega
end;
procedure TFrame.Asoc_Str_TCmbBox(ptrStr: pointer; cmbBox: TComboBox; etiq: string;
  defVal: string);
//Agrega un par variable string - Control TEdit
var n: integer;
  r: TParElem;
begin
  r.pVar   := ptrStr;     //toma referencia
  r.pCtl   := cmbBox;   //toma referencia
  r.tipPar := tp_Str_TCmbBox;  //tipo de par
  r.etiqVar:= etiq;
  r.defStr := defVal;
  //agrega
  n := high(listParElem)+1;    //número de elementos
  setlength(listParElem, n+1);  //hace espacio
  listParElem[n] := r;          //agrega
end;
procedure TFrame.Asoc_Bol_TChkB(ptrBol: pointer; chk: TCheckBox; etiq: string;
  defVal: boolean);
//Agrega un para variable booleana - Control TCheckBox
var n: integer;
  r: TParElem;
begin
  r.pVar   := ptrBol;  //toma referencia
  r.pCtl   := chk;    //toma referencia
  r.tipPar := tp_Bol_TChkB;  //tipo de par
  r.etiqVar:= etiq;
  r.defBol := defVal;
  //agrega
  n := high(listParElem)+1;    //número de elementos
  setlength(listParElem, n+1);  //hace espacio
  listParElem[n] := r;          //agrega
end;
procedure TFrame.Asoc_Col_TColBut(ptrInt: pointer; colBut: TColorButton; etiq: string;
  defVal: TColor);
//Agrega un par variable TColor - Control TColorButton
var n: integer;
  r: TParElem;
begin
  r.pVar   := ptrInt;  //toma referencia
  r.pCtl   := colBut;    //toma referencia
  r.tipPar := tp_TCol_TColBut;  //tipo de par
  r.etiqVar:= etiq;
  r.defCol := defVal;
  //agrega
  n := high(listParElem)+1;    //número de elementos
  setlength(listParElem, n+1);  //hace espacio
  listParElem[n] := r;          //agrega
end;
procedure TFrame.Asoc_Enum_TRadBut(ptrEnum: pointer; EnumSize: integer;
  radButs: array of TRadioButton; etiq: string; defVal: integer);
//Agrega un par variable Enumerated - Controles TRadioButton
//Solo se permiten enumerados de hasta 32 bits de tamaño
var n: integer;
  r: TParElem;
  i: Integer;
begin
  r.pVar   := ptrEnum;  //toma referencia
  r.lVar   :=EnumSize;  //necesita el tamaño para modificarlo luego
//  r.pCtl   := ;    //toma referencia
  r.tipPar := tp_Enum_TRadBut;  //tipo de par
  r.etiqVar:= etiq;
  r.defEnt := defVal;   //se maneja como entero
  //guarda lista de controles
  setlength(r.radButs,high(radButs)+1);  //hace espacio
  for i:=0 to high(radButs) do
    r.radButs[i]:= radButs[i];

  //agrega
  n := high(listParElem)+1;    //número de elementos
  setlength(listParElem, n+1);  //hace espacio
  listParElem[n] := r;          //agrega
end;

procedure TFrame.Asoc_Int(ptrInt: pointer; etiq: string; defVal: integer);
//Agrega una variable Entera para guardarla en el archivo INI.
var n: integer;
  r: TParElem;
begin
  r.pVar   := ptrInt;  //toma referencia
//  r.pCtl   := colBut;    //toma referencia
  r.tipPar := tp_Int;  //tipo de par
  r.etiqVar:= etiq;
  r.defEnt := defVal;
  //agrega
  n := high(listParElem)+1;    //número de elementos
  setlength(listParElem, n+1);  //hace espacio
  listParElem[n] := r;          //agrega
end;
procedure TFrame.Asoc_Bol(ptrBol: pointer; etiq: string; defVal: boolean);
//Agrega una variable String para guardarla en el archivo INI.
var n: integer;
  r: TParElem;
begin
  r.pVar   := ptrBol;  //toma referencia
//  r.pCtl   := colBut;    //toma referencia
  r.tipPar := tp_Bol;  //tipo de par
  r.etiqVar:= etiq;
  r.defBol := defVal;
  //agrega
  n := high(listParElem)+1;    //número de elementos
  setlength(listParElem, n+1);  //hace espacio
  listParElem[n] := r;          //agrega
end;
procedure TFrame.Asoc_Str(ptrStr: pointer; etiq: string; defVal: string);
//Agrega una variable String para guardarla en el archivo INI.
var n: integer;
  r: TParElem;
begin
  r.pVar   := ptrStr;  //toma referencia
//  r.pCtl   := colBut;    //toma referencia
  r.tipPar := tp_Str;  //tipo de par
  r.etiqVar:= etiq;
  r.defStr := defVal;
  //agrega
  n := high(listParElem)+1;    //número de elementos
  setlength(listParElem, n+1);  //hace espacio
  listParElem[n] := r;          //agrega
end;
procedure TFrame.Asoc_StrList(ptrStrList: pointer; etiq: string);
//Agrega una variable TStringList para guardarla en el archivo INI. El StrinList, debe estar
//ya creado, sino dará error.
var n: integer;
  r: TParElem;
begin
  r.pVar   := ptrStrList;  //toma referencia
//  r.pCtl   := colBut;    //toma referencia
  r.tipPar := tp_StrList;  //tipo de par
  r.etiqVar:= etiq;
//  r.defCol := defVal;
  //agrega
  n := high(listParElem)+1;    //número de elementos
  setlength(listParElem, n+1);  //hace espacio
  listParElem[n] := r;          //agrega
end;

procedure TFrame.ShowPos(x, y: integer);
//Muestra el frame en la posición indicada
begin
  Self.left:= x;
  Self.Top := y;
  Self.Visible:=true;
end;
function TFrame.EditValidateInt(edit: TEdit; min: integer; max: integer): boolean;
//Velida el contenido de un TEdit, para ver si se peude convertir a un valor entero.
//Si no se puede convertir, devuelve FALSE, devuelve el mensaje de error en "MsjErr", y
//pone el TEdit con enfoque.
//Si se puede convertir, devuelve TRUE, y el valor convertido en "valEnt".
var
  tmp : string;
  c : char;
  v: int64;
  signo: string;
  larMaxInt: Integer;
  n: Int64;
begin
  Result := false;
  //validaciones previas
  larMaxInt := length(IntToStr(MaxInt));
  tmp := trim(edit.Text);
  if tmp = '' then begin
    MsjErr:='Campo debe contener un valor.';
    if edit.visible and edit.enabled then edit.SetFocus;
    exit;
  end;
  if tmp[1] = '-' then begin  //es negativo
    signo := '-';  //guarda signo
    tmp := copy(tmp, 2, length(tmp));   //quita signo
  end;
  for c in tmp do begin
    if not (c in ['0'..'9']) then begin
      MsjErr:='Solo se permiten valores numéricos.';
      if edit.visible and edit.enabled then edit.SetFocus;
      exit;
    end;
  end;
  if length(tmp) > larMaxInt then begin
    MsjErr:='Valor numérico muy grande.';
    if edit.visible and edit.enabled then edit.SetFocus;
    exit;
  end;
  //lo leemos en Int64 por seguridad y validamos
  n := StrToInt64(signo + tmp);
  if n>max then begin
    MsjErr:='El mayor valor permitido es: '+IntToStr(max);
    if edit.visible and edit.enabled then edit.SetFocus;
    exit;
  end;
  if n<min then begin
    MsjErr:='El menor valor permitido es: '+IntToStr(min);
    if edit.visible and edit.enabled then edit.SetFocus;
    exit;
  end;
  //pasó las validaciones
  valInt:=n;  //actualiza valor
  Result := true;   //tuvo éxito
end;

end.

