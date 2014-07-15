{Modelo de formulario de configuración que usa dos Frame de propiedades}
unit FormConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, iniFiles, SynEdit,
  FrameCfgEdit,  //deben incluirse todos los frames de propiedades a usar
  PropertyFrame;  //necesario para manejar los Frames de propiedades

type

  { TConfig }

  TConfig = class(TForm)
    BitAplicar: TBitBtn;
    BitCancel: TBitBtn;
    BitAceptar: TBitBtn;
    lstCateg: TListBox;
    procedure BitAceptarClick(Sender: TObject);
    procedure BitAplicarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstCategClick(Sender: TObject);
  private
    { private declarations }
  public
    msjError: string;    //para los mensajes de error
    arIni   : String;      //Archivo de configuración
    //************  Modificar Aquí ***************//
    //frames de configuración
    Edit: TfraCfgEdit;
//    Colores: TfraColores;
    procedure escribirArchivoIni;
    procedure leerArchivoIni;
    procedure LeerDeVentana;
    procedure MostEnVentana;
    procedure Iniciar(ed: TSynEdit);
    procedure Mostrar;
  end;

var
  Config: TConfig;

implementation

{$R *.lfm}

{ TConfig }

procedure TConfig.FormCreate(Sender: TObject);
begin
  //************  Modificar Aquí ***************//
  //Crea dinámicamente los frames de configuración
  Edit:= TfraCfgEdit.Create(Self);
  Edit.parent := self;
//  Colores:= TfraColores.Create(Self);
//  Colores.parent := self;

  /////// verifica archivo INI /////////////
  arIni := GetIniName;
end;

procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  bitAplicarClick(Self);
  self.Close;
end;
procedure TConfig.BitAplicarClick(Sender: TObject);
begin
  LeerDeVentana;       //Escribe propiedades de los frames
  if msjError<>'' then exit;
  escribirArchivoIni;   //guarda propiedades en disco
end;

procedure TConfig.Iniciar(ed: TSynEdit);
//Inicia el formulario de configuración. Debe llamarse antes de usar el formulario y
//después de haber cargado todos los frames.
begin
  //************  Modificar Aquí ***************//
  //inicia los Frames creados
  Edit.Iniciar('texto',ed);
//  Colores.Iniciar('colores',);

  LeerArchivoIni;  //lee parámetros del archivo de configuración.
end;

procedure TConfig.FormDestroy(Sender: TObject);
var
  f: TFrame;
begin
  //Libera los frames de configuración
  for f in ListOfFrames(self) do f.Free;
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  MostEnVentana;   //carga las propiedades en el frame
end;

procedure TConfig.lstCategClick(Sender: TObject);
var
  f: TFrame;
begin
  for f in ListOfFrames(self) do f.visible := false;  //oculta todos
  //************  Modificar Aquí ***************//
  if lstCateg.ItemIndex = 0 then Edit.ShowPos(120,0) ;
//  if lstCateg.ItemIndex = 1 then Colores.ShowPos(120,0);
end;

procedure TConfig.Mostrar;
//Muestra el formulario para configurarlo
begin
  lstCateg.ItemIndex:=0;   //define frame inicial
  lstCategClick(self);
  Showmodal;
end;

procedure TConfig.LeerDeVentana;
//Lee las propiedades de la ventana de configuración.
var f: TFrame;
begin
  msjError := '';
  //Fija propiedades de los controles
  for f in ListOfFrames(self) do begin
    f.WindowToProp;
    msjError := f.MsjErr;
    if msjError<>'' then exit;
  end;
end;
procedure TConfig.MostEnVentana;
//Muestra las propiedades en la ventana de configuración.
var f: TFrame;
begin
  //llama a MostEnVentana de todos los PropertyFrame.Frames
  for f in ListOfFrames(self) do begin
    f.PropToWindow;
    msjError := f.MsjErr;
    if msjError<>'' then exit;
  end;
end;
//funciones públicas
procedure TConfig.leerArchivoIni;
//Lee el archivo de configuración
var
   appINI : TIniFile;
   f: Tframe;
begin
  if not FileExists(arIni) then exit;  //para que no intente leer

  msjError := 'Error leyendo de archivo de configuración: ' + arIni;  //valor por defecto
  try
     appINI := TIniFile.Create(arIni);
     //lee propiedades de los Frame de configuración
     for f in ListOfFrames(self) do begin
       f.ReadFileToProp(appINI);
     end;

     MsjError:='';  //Limpia
  finally
     appIni.Free;                   //libera
  end;
end;
procedure TConfig.escribirArchivoIni;
//Escribe el archivo de configuración
var
   appINI : TIniFile;
   i: Integer;
   f: Tframe;
begin
  msjError := 'Error escribiendo en archivo de configuración: ' + arIni;  //valor por defecto
  try
    If FileExists(arIni)  Then  begin  //ve si existe
        If FileIsReadOnly(arIni) Then begin
            msjError := 'Error. Archivo de configuración es de solo lectura';
            Exit;
        End;
        //Crea copia de seguridad antes de modificar
  //        FileCopy arIni, arIni & ".bak"
    End;
    appINI := TIniFile.Create(arIni);
    //escribe propiedades de los Frame de configuración
    for f in ListOfFrames(self) do begin
      f.SavePropToFile(appINI);
    end;

    MsjError:='';  //Limpia
  finally
    appIni.Free;                   //libera
  end;
end;

end.

