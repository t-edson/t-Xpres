unit FormPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Menus, ActnList, StdActns, ExtCtrls, utilEditSyn,
  SynHighlighterFacil, XpresComplet, uXpres, Process, Globales, FormConfig;

type

  { TfrmPrincipal }

  TfrmPrincipal = class(TForm)
    acArcAbrir: TAction;
    acArcGuaCom: TAction;
    acArcGuardar: TAction;
    acArcNueVent: TAction;
    acArcNuevo: TAction;
    acArcSalir: TAction;
    acBusBuscar: TAction;
    acBusBusSig: TAction;
    acBusRem: TAction;
    acEdiCopy: TEditCopy;
    acEdiCut: TEditCut;
    acEdiModCol: TAction;
    acEdiPaste: TEditPaste;
    acEdiRedo: TAction;
    acEdiSelecAll: TAction;
    acEdiUndo: TAction;
    acHerConfig: TAction;
    acHerDetener: TAction;
    acHerEjecutar: TAction;
    acPArcAbrir: TAction;
    acPArcAbrNue: TAction;
    acPArcCamNom: TAction;
    acPArcElim: TAction;
    acPArcNueCar: TAction;
    acPArcNueCon: TAction;
    acPArcNueEnc: TAction;
    acPArcRefres: TAction;
    acHerComp: TAction;
    acEdiRecSyn: TAction;
    acHerGenTemCom: TAction;
    ActionList: TActionList;
    acVerPanArc: TAction;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    mnRecientes: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    edXpr: TSynEdit;
    edAsm: TSynEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    VerBarEst1: TAction;
    VerNumLin1: TAction;
    procedure acArcAbrirExecute(Sender: TObject);
    procedure acArcGuaComExecute(Sender: TObject);
    procedure acArcGuardarExecute(Sender: TObject);
    procedure acArcNuevoExecute(Sender: TObject);
    procedure acArcSalirExecute(Sender: TObject);
    procedure acEdiCopyExecute(Sender: TObject);
    procedure acEdiCutExecute(Sender: TObject);
    procedure acEdiPasteExecute(Sender: TObject);
    procedure acEdiRecSynExecute(Sender: TObject);
    procedure acEdiRedoExecute(Sender: TObject);
    procedure acEdiUndoExecute(Sender: TObject);
    procedure acHerCompExecute(Sender: TObject);
    procedure acHerConfigExecute(Sender: TObject);
    procedure acHerEjecutarExecute(Sender: TObject);
    procedure acHerGenTemComExecute(Sender: TObject);
    procedure eArchivoCargado;
    procedure eCambiaInfArchivo;
    procedure eCambiaEstArchivo;
    procedure edXprKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
  private
    procedure MarcarError(nLin, nCol: integer);
    procedure PreparaEditor;
    procedure VerificarError;
    { private declarations }
  public
    e : TObjEditor;
    { public declarations }
  end;

var
  frmPrincipal: TfrmPrincipal;

implementation
{$R *.lfm}
var
  hlXpr, hlAsm: TSynFacilSyn;

{ TfrmPrincipal }

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  e := TObjEditor.Create;
  edXpr.Align:=alLeft;
  splitter1.Align:=alLeft;
  edAsm.Align:=alClient;
  //crea el resaltador-lexer, para el editor y el compilador
  hlXpr := TSynFacilSyn.Create(self);
  hlXpr.LoadFromFile('lang_Xpres.xml');
//  if hlXpr.Err<>'' then msgerr(hlXpr.Err);
  PreparaEditor;
end;

procedure TfrmPrincipal.FormDestroy(Sender: TObject);
begin
  FinAyudaContext;   //Finaliza ayuda contextual
  e.Destroy;
end;

procedure TfrmPrincipal.PreparaEditor;
//Configura el editor para empezar a trabajar
begin
  //configura editor de Xpres
  edXpr.Highlighter:=hlXpr;

  //configura editor de ASM
  hlAsm := TSynFacilSyn.Create(self);
  hlAsm.LoadFromFile('lang_8086asm.xml');
  edAsm.Highlighter:=hlAsm;
  //configura paneles
  e.PanFileSaved := StatusBar1.Panels[0]; //panel para mensaje "Guardado"
  e.PanCursorPos := StatusBar1.Panels[1];  //panel para la posición del cursor

  e.OnChangeFileInform:=@eCambiaInfArchivo;
  e.OnChangeEditorState:=@eCambiaEstArchivo;
  e.OnFileOpened:=@eArchivoCargado;
  e.InitEditor(edXpr,'SinNombre', 'xpr');
  InicEditorC1(edXpr);     //inicia editor
//  edXpr.OnSpecialLineMarkup:=@edSpecialLineMarkup;
end;

procedure TfrmPrincipal.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if e.SaveQuery then CanClose := false;   //cancela
end;

procedure TfrmPrincipal.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  //Carga archivo arrastrados
  if e.SaveQuery then Exit;   //Verifica cambios
  e.LoadFile(FileNames[0]);
end;

procedure TfrmPrincipal.FormShow(Sender: TObject);
begin
  InicAyudaContext(edXpr, Self,'');
  Config.Iniciar(edXpr);
  //inicia el menú "Recientes". Se debe hacer después de iniciar "Config", para poder
  //acceder a "Config.Edit.ArcRecientes".
  e.InitMenuRecents(mnRecientes, Config.Edit.ArcRecientes);
end;

procedure TfrmPrincipal.eCambiaInfArchivo;
begin
  //actualiza barra de título
  Caption:= 'XpresI - ' + SysToUTF8(e.NomArc);
end;
procedure TfrmPrincipal.eCambiaEstArchivo;
begin
  acArcGuardar.Enabled:=e.Modified;
  acEdiUndo.Enabled:=e.CanUndo;
  acEdiRedo.Enabled:=e.CanRedo;
//  acEdiPaste.Enabled:=e.CanPaste;
end;

procedure TfrmPrincipal.edXprKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  AyudContextKeyUp(Key, Shift);
end;

procedure TfrmPrincipal.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  Config.escribirArchivoIni;  //guarda la configuración actual
end;

procedure TfrmPrincipal.eArchivoCargado;
begin
  //  if Panel1.Visible then UbicarArchivoArbol(arc8);  //ubica ruta
  //  frmConfig.fraCfgEdit1.AgregArcReciente(arc8);  //agrega a lista de recientes
  //  frmConfig.escribirArchivoIni;  //guarda registro
end;

procedure TfrmPrincipal.acArcNuevoExecute(Sender: TObject);
begin
  e.NewFile;
end;

procedure TfrmPrincipal.acArcAbrirExecute(Sender: TObject);
//Abre archivo
begin
  OpenDialog1.Filter:='Programa Xpres|*.xpr;*.txt|Todos los archivos|*.*';
  e.OpenDialog(OpenDialog1);
end;

procedure TfrmPrincipal.acArcGuaComExecute(Sender: TObject);
begin
  e.SaveAsDialog(SaveDialog1);
end;

procedure TfrmPrincipal.acArcGuardarExecute(Sender: TObject);
begin
  e.SaveFile;
end;
procedure TfrmPrincipal.acArcSalirExecute(Sender: TObject);
begin
  self.Close;
end;

procedure TfrmPrincipal.acEdiCopyExecute(Sender: TObject);
begin
  e.Copy;
end;
procedure TfrmPrincipal.acEdiCutExecute(Sender: TObject);
begin
  e.Cut;
end;
procedure TfrmPrincipal.acEdiPasteExecute(Sender: TObject);
begin
  e.Paste;
end;
procedure TfrmPrincipal.acEdiRecSynExecute(Sender: TObject);
//Recarga el archivo de sintaxis
begin
  hlXpr.LoadFromFile('lang_Xpres.xml');
  edXpr.Invalidate;
end;

procedure TfrmPrincipal.acEdiRedoExecute(Sender: TObject);
begin
  e.Redo;
end;
procedure TfrmPrincipal.acEdiUndoExecute(Sender: TObject);
begin
  e.Undo;
end;

procedure TfrmPrincipal.acHerCompExecute(Sender: TObject);
begin
  Compilar(e.NomArc, edXpr.Lines, hlXpr);
  if Perr.HayError then begin
    VerificarError;
//    MsgErr(c.PErr.TxtError);
    exit;
  end;
  edAsm.ClearAll;
  edAsm.Lines.AddStrings(mem);
end;

procedure TfrmPrincipal.acHerConfigExecute(Sender: TObject);
begin
  Config.Mostrar;
end;

procedure TfrmPrincipal.MarcarError(nLin, nCol: integer);
begin
  //posiciona curosr
  edXpr.CaretX := nCol;
  edXpr.CaretY := nLin;
  //define línea con error
  e.linErr := nLin;
  edXpr.Invalidate;  //refresca
end;
procedure TfrmPrincipal.VerificarError;
//Verifica si se ha producido algún error en el preprocesamiento y si lo hay
//Ve la mejor forma de msotrarlo
begin
    If not pErr.HayError Then exit;  //verificación
    //Selecciona posición de error en el Editor
    If PErr.ArcError <> '' Then begin
        //Se ha identificado el archivo con el error
        If e.NomArc = '' Then begin
            //Tenemos el editor libre para mostrar el archivo
            e.LoadFile(PErr.ArcError);
            //Ubicamos número de línea, si hay
            MarcarError(Perr.nLinError,Perr.nColError);
            {If MostrarError Then }Perr.MosError;
        end Else begin
            //Hay un archivo cargado
            If Perr.ArcError = e.NomArc Then begin
                //El error está en el mismo archivo, lo mostramos
                If Perr.nLinError <> 0 Then begin
                   MarcarError(Perr.nLinError,Perr.nColError);
                   edXpr.Invalidate;
                end;
                {If MostrarError Then }Perr.MosError;
            end Else begin
                //Es otro archivo. Lo abre en otra ventana
//               AbrirPreSQL(PErr.ArcError, PErr.TxtError);
            end;
        end;
    End else begin   //no hay archivo de error
      {If MostrarError Then }Perr.MosError;
    end;
End;
procedure TfrmPrincipal.acHerEjecutarExecute(Sender: TObject);
begin
  edAsm.Lines.SaveToFile('code.asm');  //guarda
  //compila
  Shell('.\pas32v25\BIN\PASS32.EXE code.asm');
  //ejecuta
  Shell('cmd /c code.exe');
end;

procedure TfrmPrincipal.acHerGenTemComExecute(Sender: TObject);
//Genera plantilla de código para el compilador, de acuerdo a los tipos definidos
begin
  GenTemplCompiler;
end;

end.

