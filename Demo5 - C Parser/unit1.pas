{This demo shows how to use Xpres to create a very basic C parser and use
TsynFacilCompletion to implement dynamic completion reading struct declarations}
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Menus, Dialogs,
  SynEdit, SynEditHighlighter, LCLType, Lazlogger,
  SynFacilCompletion, SynFacilBasic, XpresElements, XpresBas;

type

  { TForm1 }
  TForm1 = class(TForm)
    ed1: TSynEdit;
    procedure ed1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ed1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    hlt : TSynFacilComplet;  //highlighter - lexer
    doc : TContext;    //Context to scan
    ast: TXpTreeElements;
    oev: TFaOpenEvent;
    blkStruct, blkStructBody: TFaSynBlock;
    procedure ScanSource;
    procedure oevLoadItems(curEnv: TFaCursorEnviron; var Cancel: boolean);
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

{ TForm1 }

procedure TForm1.ScanSource;
{Scans and creates a simple AST}
var
  newType: TxpType;
  newVar: TxpVar;
begin
  ast.Clear;
  //Scan source code
  doc.SetSource(ed1.Lines);
  while not doc.Eof do begin
    if doc.Block = blkStruct then begin
      //Start of block "struct". It's a struct declaration
      doc.Next;  //space
      doc.SkipWhites;
      //Creates new node
      newType := TxpType.Create;
      newType.name:=doc.Token;  //struct name
      ast.AddElement(newType);
      debugln('struct: '+ doc.Token);
      //scan until enter a new block or leaving the current block.
      if not doc.NextBlock then exit; //Exit if unexpected End OF file
      if doc.Block = blkStructBody then begin
        //Entered to the body block
        while (doc.Block = blkStructBody) and not doc.Eof do begin
          //Here we must read the fileds of the struct

          doc.Next;  //struct identifier
        end;
      end;
      if doc.Block = blkStruct then begin
        //Declaration still continue. It must be of the form:
        //       struct mystruct {} variable;  or
	//       struct mystruct {} variable = {};
        doc.SkipWhites;
        newVar := TxpVar.Create;   //create variable
        newVar.name:=doc.Token;  //var name
        ast.AddElement(newVar);
      end;
      //Assure we leave the current struct block
      while doc.Block <> hlt.MainBlk do
        if not doc.NextBlock then exit;
    end;
    doc.Next;
  end;
end;

procedure TForm1.oevLoadItems(curEnv: TFaCursorEnviron; var Cancel: boolean);
{Fills the completion list, scanning the Syntax Tree}
var
  el : TxpElement;
begin
  ScanSource;  //No optimal, doing this every time we press a key
  oev.ClearAvails;
  //only scan global declaraction
  for el in ast.main.elements do begin
    //Could be types, variables
    oev.AddAvail(el.name);
  end;
  oev.AddAvail('alfa');
  Cancel := true;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  found: boolean;
begin
  doc := TContext.Create;
  ast := TXpTreeElements.Create;   //Syntax Tree
  //configure highlighters
  hlt := TSynFacilComplet.Create(self);  //my highlighter
  hlt.LoadFromFile('c.xml');
  hlt.SelectEditor(ed1);
  ed1.OnUTF8KeyPress:=@ed1UTF8KeyPress;
  ed1.Lines.LoadFromFile('c_sample.txt');
  blkStruct := hlt.SearchBlock('struct', found);
  if not found then Application.Terminate;
  blkStructBody := hlt.SearchBlock('structBody', found);
  if not found then Application.Terminate;

  doc.DefSyn(hlt);

  oev :=  hlt.OpenEvents[0];
  oev.OnLoadItems:=@oevLoadItems;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  hlt.UnSelectEditor;
  hlt.Free;
  ast.Destroy;
  doc.Destroy;
end;

procedure TForm1.ed1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  hlt.KeyUp(Sender, Key, Shift);
end;

procedure TForm1.ed1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  hlt.UTF8KeyPress(Sender, UTF8Key);
end;

end.

