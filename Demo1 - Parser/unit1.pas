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
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure ShowCurrentTok;
  public
    xLex : TSynFacilSyn;
    xCon : TContext;
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
  Token := xCon.Token; //lee el token
  nom := xCon.TokenAttrib.Name;
  if xCon.Block = nil then blk := 'nil    '
  else blk := xCon.Block.name;
  frmOut.puts( nom + space(12-length(nom))+ '('+ blk +'): ' +
               Token);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  frmOut.Show;  //show console
  frmOut.SynEdit1.Text:='';
  //Explore
  xCon.DefSyn(xLex);
  xCon.SetSource(SynEdit1.Lines);
  while not xCon.Eof do begin
    ShowCurrentTok;
    //frmOut.puts(xCon.TokenType.Name + ':' + xCon.Token);
    xCon.Next;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  blk: TFaSynBlock;
begin
  xCon := TContext.Create;
  xLex := TSynFacilSyn.Create(nil);   //crea lexer
  ///////////define syntax for the lexer
  xLex.ClearMethodTables;           //limpìa tabla de métodos
  xLex.ClearSpecials;               //para empezar a definir tokens
  //crea tokens por contenido
  xLex.DefTokIdentif('[$A-Za-z_]', '[A-Za-z0-9_]*');
  xLex.DefTokContent('[0-9]', '[0-9.]*', xLex.tnNumber);
  //define keywords
  xLex.AddIdentSpecList('begin end else elsif', xLex.tnKeyword);
  xLex.AddIdentSpecList('true false int string', xLex.tnKeyword);
  //create delimited tokens
  xLex.DefTokDelim('''','''', xLex.tnString);
  xLex.DefTokDelim('"','"', xLex.tnString);
  xLex.DefTokDelim('//','', xLex.tnComment);
  xLex.DefTokDelim('/\*','*/', xLex.tnComment, tdMulLin);
  //define syntax block
  blk := xLex.AddBlock('{','}');
  blk.name:='bLlaves';
  xLex.Rebuild;
end;
procedure TForm1.FormDestroy(Sender: TObject);
begin
  xLex.Destroy;
  xCon.Destroy;
end;

end.

