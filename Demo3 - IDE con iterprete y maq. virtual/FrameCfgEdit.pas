{Unidad con frame para almacenar y configurar las propiedades de un editor
 SynEdit. Las propiedades que se manejan son con respecto al coloreado.
 El frame definido, está pensado para usarse en una ventana de configuración.
 También incluye una lista para almacenamiento de los archivos recientes
                               Por Tito Hinostroza  23/11/2013
}
unit FrameCfgEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Dialogs, Buttons,
  ExtCtrls, Spin, SynEdit, Graphics, SynEditMarkupHighAll, SynEditMarkup, Menus,
  ConfigFrame;   //para interceptar TFrame

type

  { TfraCfgEdit }

  TfraCfgEdit = class(TFrame)
    cbutFonPan: TColorButton;
    cbutTxtPan: TColorButton;
    chkResPalCur: TCheckBox;
    chkVerPanVer: TCheckBox;
    chkMarLinAct: TCheckBox;
    cbutLinAct: TColorButton;
    chkVerBarDes: TCheckBox;
    chkVerNumLin: TCheckBox;
    chkVerMarPle: TCheckBox;
    cmbTipoLetra: TComboBox;
    cbutFondo: TColorButton;
    cbutTexto: TColorButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    spTam: TSpinEdit;
    procedure chkMarLinActChange(Sender: TObject);
    procedure chkVerPanVerChange(Sender: TObject);
  private
    ed: TSynEdit;
    procedure ConfigEditor;
  public
    //configuración del editor
    TipLet     : string;    //tipo de letra
    TamLet     : integer;   //tamaño de letra
    MarLinAct  : boolean;   //marcar línea actual
    VerBarDes  : boolean;   //ver barras de desplazamiento
    ResPalCur  : boolean;   //resaltar palabra bajo el cursor
    cTxtNor    : TColor;    //color de texto normal
    cFonEdi    : TColor;    //Color de fondo del control de edición
    cFonSel    : TColor;    //color del fondo de la selección
    cTxtSel    : TColor;    //color del texto de la selección
    cLinAct    : TColor;    //color de la línea actual
    //panel vertical
    VerPanVer  : boolean;   //ver pánel vertical
    VerNumLin  : boolean;   //ver número de línea
    VerMarPle  : boolean;   //ver marcas de plegado
    cFonPan    : TColor;    //color de fondo del panel vertical
    cTxtPan    : TColor;    //color de texto del panel vertical
    ArcRecientes: TStringList;  //Lista de archivos recientes

    procedure PropToWindow; override;
    procedure Iniciar(secINI0: string; ed0: TSynEdit); //Inicia el frame
    //genera constructor y destructor
    constructor Create(AOwner: TComponent) ; override;
    destructor Destroy; override;
  end;

implementation
{$R *.lfm}

{ TfraCfgEdit }
procedure TfraCfgEdit.Iniciar(secINI0: string; ed0: TSynEdit);
begin
  secINI := secINI0;  //sección INI
  //asigna referencia necesarias
  ed := ed0;
//  ArcRecientes := ArcRecientes0;
  OnUpdateChanges := @ConfigEditor;  //manejador de cambios
  //crea las relaciones variable-control
  Asoc_Col_TColBut(@cTxtNor, cbutTexto, 'cTxtNor',$000000);
  Asoc_Col_TColBut(@cFonEdi, cbutFondo, 'cFonEdi',$FFFFFF);
  Asoc_Col_TColBut(@cLinAct, cbutLinAct, 'cLinAct',clYellow);

  Asoc_Bol_TChkB(@VerBarDes,chkVerBarDes,'VerBarDes',true);
  Asoc_Bol_TChkB(@ResPalCur,chkResPalCur,'ResPalCur',true);
  Asoc_Bol_TChkB(@MarLinAct,chkMarLinAct,'MarLinAct',false);

  Asoc_Bol_TChkB(@VerPanVer, chkVerPanVer, 'VerPanVer',true);
  Asoc_Bol_TChkB(@VerNumLin, chkVerNumLin, 'VerNumLin',false);
  Asoc_Bol_TChkB(@VerMarPle, chkVerMarPle, 'VerMarPle',true);
  Asoc_Col_TColBut(@cFonPan, cbutFonPan, 'cFonPan',clWhite);
  Asoc_Col_TColBut(@cTxtPan, cbutTxtPan, 'cTxtPan',clWhite);

  Asoc_Int_TSpnEdi(@TamLet, spTam, 'TamLet', 10, 5, 20);

  cmbTipoLetra.Items.Clear;
  cmbTipoLetra.Items.Add('Courier New');
  cmbTipoLetra.Items.Add('Fixedsys');
  cmbTipoLetra.Items.Add('Lucida Console');
  cmbTipoLetra.Items.Add('Consolas');
  cmbTipoLetra.Items.Add('Cambria');
  Asoc_Str_TCmbBox(@TipLet, cmbTipoLetra, 'TipLet', 'Courier New');

//  if ArcRecientes0<> nil then Asoc_StrList(@ArcRecientes0, 'recient');
//  if ArcRecientes<> nil then
    Asoc_StrList(@ArcRecientes, 'recient');
end;

procedure TfraCfgEdit.chkVerPanVerChange(Sender: TObject);
begin
  chkVerNumLin.Enabled:=chkVerPanVer.Checked;
  chkVerMarPle.Enabled:=chkVerPanVer.Checked;
  cbutFonPan.Enabled:=chkVerPanVer.Checked;
  cbutTxtPan.Enabled:=chkVerPanVer.Checked;
  label2.Enabled:=chkVerPanVer.Checked;
  label3.Enabled:=chkVerPanVer.Checked;
end;
procedure TfraCfgEdit.chkMarLinActChange(Sender: TObject);
begin
  label1.Enabled:=chkMarLinAct.Checked;
  cbutLinAct.Enabled:=chkMarLinAct.Checked;
end;

procedure TfraCfgEdit.PropToWindow;
begin
   inherited;
   chkMarLinActChange(self);  //para actualizar
   chkVerPanVerChange(self);  //para actualizar
end;
constructor TfraCfgEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ArcRecientes := TStringList.Create;  //crea lista
end;
destructor TfraCfgEdit.Destroy;
begin
  FreeAndNil(ArcRecientes);
  inherited Destroy;
end;
procedure TfraCfgEdit.ConfigEditor;
{Configura el editor con las propiedades almacenadas}
var
  marc: TSynEditMarkup;
begin
   if ed = nil then exit;  //protección
   //tipo de texto
   if TipLet <> '' then ed.Font.Name:=TipLet;
   if (TamLet > 6) and (TamLet < 32) then ed.Font.Size:=Round(TamLet);

   ed.Font.Color:=cTxtNor;      //color de texto normal
   ed.Color:=cFonEdi;           //color de fondo
   if MarLinAct then          //resaltado de línea actual
     ed.LineHighlightColor.Background:=cLinAct
   else
     ed.LineHighlightColor.Background:=clNone;
   //configura panel vertical
   ed.Gutter.Visible:=VerPanVer;  //muestra panel vertical
   ed.Gutter.Parts[1].Visible:=VerNumLin;  //Número de línea
   ed.Gutter.Parts[4].Visible:=VerMarPle;  //marcas de plegado
   ed.Gutter.Color:=cFonPan;   //color de fondo del panel
   ed.Gutter.Parts[1].MarkupInfo.Background:=cFonPan; //fondo del núemro de línea
   ed.Gutter.Parts[1].MarkupInfo.Foreground:=cTxtPan; //texto del núemro de línea

   if VerBarDes then  //barras de desplazamiento
     ed.ScrollBars:= ssBoth
   else
     ed.ScrollBars := ssNone;
   ////////Configura el resaltado de la palabra actual //////////
   marc := ed.MarkupByClass[TSynEditMarkupHighlightAllCaret];
   if marc<>nil then begin  //hay marcador
      marc.Enabled:=ResPalCur;  //configura
   end;
   ///////fija color de delimitadores () {} [] ///////////
   ed.BracketMatchColor.Foreground := clRed;
end;

end.

