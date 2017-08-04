unit Unit1;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, Forms, Controls, Dialogs,
  StdCtrls, FormOut, SynEditHighlighter, SynFacilHighlighter, SynFacilBasic, XpresBas;

type
  { TForm1 }
  TForm1 = class(TForm)
    Button1: TButton;
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure ShowCurrentTok;
  public
    xLex : TSynFacilSyn;
    cIn : TContexts;
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

procedure TForm1.ShowCurrentTok;
var
  blk: String;
  Token, nom: String;
begin
  Token := cIn.tok; //lee el token
  nom := cIn.tokAttrib.Name;
  if cIn.curCon.Block = nil then blk := 'nil    '
  else blk := cIn.curCon.Block.name;
  frmOut.puts( nom + space(12-length(nom))+ '('+ blk +'): ' +
               Token);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  frmOut.Show;  //show console
  frmOut.SynEdit1.Text:='';
  //Explore
  cIn.NewContextFromFile('', SynEdit1.Lines);
  while not cIn.Eof do begin
    if cIn.tok = 'xpres' then begin
      cIn.Next;   //pass the token
      cIn.NewContextFromFile('sub', SynEdit2.Lines);  //open sub-context
      //We can use this if we need "autoclose" when finishing the scan
      cIn.curCon.autoClose := true;
    end;
    ShowCurrentTok;
    //frmOut.puts(cIn.TokenType.Name + ':' + cIn.Token);
    cIn.Next;
//    if cIn.Eof and (cIn.curCon.arc='sub') then begin  //End of sub-context
//      cIn.RemoveContext;  //return to the same position
//    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  blk: TFaSynBlock;
begin
  xLex := TSynFacilSyn.Create(nil);   //crea lexer
  cIn := TContexts.Create(xLex);
  /////////// Define syntax for the lexer
  xLex.ClearMethodTables;           //limpìa tabla de métodos
  xLex.ClearSpecials;               //para empezar a definir tokens
  // Tokens by content
  xLex.DefTokIdentif('[$A-Za-z_]', '[A-Za-z0-9_]*');
  xLex.DefTokContent('[0-9]', '[0-9.]*', xLex.tnNumber);
  // Keywords
  xLex.AddIdentSpecList('begin end else elsif', xLex.tnKeyword);
  xLex.AddIdentSpecList('true false int string', xLex.tnKeyword);
  // Create delimited tokens
  xLex.DefTokDelim('''','''', xLex.tnString);
  xLex.DefTokDelim('"','"', xLex.tnString);
  xLex.DefTokDelim('//','', xLex.tnComment);
  xLex.DefTokDelim('/\*','*/', xLex.tnComment, tdMulLin);
  // Define syntax block
  blk := xLex.AddBlock('{','}');
  blk.name:='bLlaves';
  xLex.Rebuild;
end;
procedure TForm1.FormDestroy(Sender: TObject);
begin
  xLex.Destroy;
  cIn.Destroy;
end;

end.

