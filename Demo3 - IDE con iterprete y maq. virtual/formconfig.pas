{Modelo de formulario de configuración que usa dos Frame de propiedades}
unit FormConfig;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, iniFiles, SynEdit, FrameCfgEdit,
  ConfigFrame;   //para interceptar TFrame
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
  public
    msjError: string;    //para los mensajes de error
    arIni   : String;      //Archivo de configuración
    //frames de configuración
    Edit: TfraCfgEdit;
    procedure escribirArchivoIni;
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

  /////// verifica archivo INI /////////////
  arIni := GetIniName;
end;

procedure TConfig.BitAceptarClick(Sender: TObject);
begin
  bitAplicarClick(Self);
  if msjError='' then self.Close;  //sale si no hay error
end;
procedure TConfig.BitAplicarClick(Sender: TObject);
begin
  msjError := WindowToProp_AllFrames(self);
  if msjError<>'' then begin
    showmessage(msjError);
    exit;
  end;
  escribirArchivoIni;   //guarda propiedades en disco
end;

procedure TConfig.Iniciar(ed: TSynEdit);
//Inicia el formulario de configuración. Debe llamarse antes de usar el formulario y
//después de haber cargado todos los frames.
begin
  //inicia los Frames creados
  Edit.Iniciar('texto',ed);

  msjError := ReadFileToProp_AllFrames(self, arINI);
end;

procedure TConfig.FormDestroy(Sender: TObject);
begin
  Free_AllConfigFrames(self);  //Libera los frames de configuración
end;

procedure TConfig.FormShow(Sender: TObject);
begin
  msjError := PropToWindow_AllFrames(self);
end;

procedure TConfig.lstCategClick(Sender: TObject);
begin
  Hide_AllConfigFrames(self);   //oculta todos
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

procedure TConfig.escribirArchivoIni;
//Escribe el archivo de configuración
begin
  msjError := SavePropToFile_AllFrames(self, arINI);
end;

end.

