{Unidad con funciones para el autocompletado usando el menú contextual de ayuda.
 Se ha separado todo el código de Ayuda contextual, en esta unidad  para evitra tener
 que agregar el componente tSynCOmpletion al formulario.
 Para usarlo, en un editor, se debe iniciar con:

 procedure TForm1.FormShow(Sender: TObject);
 begin
   InicAyudaContext(editor, Self,'');  //inicia ayuda contextual
 end;

 Luego se debe interceptar el evento KeyUp, del SynEdit:

 procedure TForm1.edKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
 begin
   AyudContextKeyUp(Key, Shift);
 end;

 Y se debe terminar correctamente:

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FinAyudaContext;   //Finaliza ayuda contextual
end;

Cuando se desea desaparecer la ventana de ayuda contextual por algún evento, se debe
llamar a CierraAyudaContext().

 Creado por Tito Hinostroza - 08/08/2013
}
unit XpresComplet; {$mode objfpc}{$H+}
interface
uses  Classes, SysUtils, Math, Menus, Graphics, SynEdit, SynEditKeyCmds,
  StrUtils, lclType, syncompletionQ, SynEditTypes, SynEditHighlighter,
  SynHighlighterFacil, dialogs, lclproc;

procedure InicAyudaContext(ed0: TSynEdit; Self0: TComponent; cad_con: String);
procedure FinAyudaContext;
procedure CierraAyudaContext;
procedure AyudContextKeyUp(Key: Word; Shift: TShiftState);

implementation
//Variables para ayuda contextual
var

//  ListandoTab : Boolean;
//  SqlListaTab : String;   //Archivo de lista de Tablas
//  ArcListaTab : String;   //Archivo de lista de Tablas
//  ListaTablas() : String; //Guarda los nombres de las tablas
  MenuContex: TSynCompletion;   //menú contextual
  IdentAyudC: TStringList;  //Lista de identificadores para ayuda contextual
  ed: TSynEdit;            //referencia interna al editor
  hl: TSynFacilSyn;        //referencia al resaltador
type

  { TMiObj }
  TMiObj = object
     //Objeto inútil. Solo se usa para tener procedimientos de tipo "of Object" (método)
     //para usarlos como manejador de evento.
     procedure OnTextModified(Sender: TObject; KeyChar: TUTF8Char; Shift: TShiftState);
  end;
var
miObjeto: TMiObj;

procedure AbreAyudContextual;
//Abre la ayuda contextual, en la posición del cursor.
var p:TPoint;
begin
    P := Point(ed.CaretXPix,ed.CaretYPix + ed.LineHeight);
    P.X:=Max(0,Min(P.X, ed.ClientWidth - MenuContex.Width));
    P := ed.ClientToScreen(p);
    //Abre menú contextual. Solo se mostrará si tiene elementos.
    miObjeto.OnTextModified(nil,'',[]);       //para llenar lista
    MenuContex.Execute('', p.x, p.y);
End;
procedure InicAyudaContext(ed0: TSynEdit; Self0: TComponent; cad_con: String);
//Inicia el motor de ayuda contextual
begin
   ed := ed0;    //guarda referencia
   if ed.Highlighter = nil then begin
     showmessage('ERROR: No se ha asignado resaltador al editor.');
     exit;
   end;
   if ed.Highlighter.ClassName<>'TSynFacilSyn' then begin
     showmessage('ERROR: Se requiere un resaltador de tipo TSynFacilSyn');
     ed := nil;   //para indicar que no es válido
     exit;
   end;
   hl := TSynFacilSyn(ed.Highlighter);  //
   MenuContex:=TSynCompletion.Create(Self0);   //crea menú
   MenuContex.Editor:=ed;     //asigna editor
   MenuContex.Width:=200;     //ancho inicial
   MenuContex.TheForm.hl := hl;  //pasa el resaltador
//    MenuContex.EndOfTokenChr:='-';
//    MenuContex.OnValidate:=;
   MenuContex.OnTextModified :=@miObjeto.OnTextModified;  //asigna evento
   IdentAyudC := TStringList.Create;  //crea lista
//    ListandoTab := False;
//    SqlListaTab := App.Path & "\listar.sql";
//    ArcListaTab := App.Path & "\lista.lst";
End;
procedure FinAyudaContext;
//Termina la ayuda contextual
begin
    IdentAyudC.Destroy;
    MenuContex.Destroy;
end;

procedure LlenaAyudContextual_IDEN(agregar: Boolean = False);
//Llena las palabras reservadas de Oracle
begin
    If not agregar Then IdentAyudC.Clear;

    IdentAyudC.add('begin');
    IdentAyudC.add('begin '#13#10'end;');
    IdentAyudC.add('ELSE ');
    IdentAyudC.add('ELSIF ');
    IdentAyudC.add('code');
    IdentAyudC.add('end;');

    IdentAyudC.add('IF ');
    IdentAyudC.add('IF THEN '#13#10'END;');
    IdentAyudC.add('IF THEN '#13#10'ELSE'#10#13'END;');
    IdentAyudC.add('IF THEN ELSIF END;');
    IdentAyudC.add('IF NOT ');

    IdentAyudC.add('program ');
    IdentAyudC.add('program NoName;');
    IdentAyudC.add('program NoName;'#10#13'begin'#10#13'end;');
    IdentAyudC.add('method');
    IdentAyudC.add('type');
    IdentAyudC.add('class');

    IdentAyudC.add('and');
    IdentAyudC.add('or');
    IdentAyudC.add('xor');
    IdentAyudC.add('not');

    IdentAyudC.add('true');
    IdentAyudC.add('false');
End;
procedure LlenaAyudContextual_TIPO(agregar: Boolean = False);
//Llena los tipos de datos de Oracle
type aaa = byte;
begin
    If not agregar Then IdentAyudC.Clear;
    IdentAyudC.add('int;');
    IdentAyudC.add('float;');

    IdentAyudC.add('string;');

    IdentAyudC.add('bool;');
End;
procedure LlenaAyudContextual_FUNC(agregar: Boolean = False);
//Llena las funciones de Oracle
begin
    If not agregar Then IdentAyudC.Clear;
    IdentAyudC.add('get');
    IdentAyudC.add('put');
    IdentAyudC.add('puts(');
    IdentAyudC.add('putchar(');
    IdentAyudC.add('gets(');
    IdentAyudC.add('getchar(');
End;
procedure CierraAyudaContext;
//Cierra la ventana del menú contextual
begin
  MenuContex.Deactivate;
end;

procedure AyudContextKeyUp(Key: Word; Shift: TShiftState);
{Verifica la tecla pulsada, para determinar si abrir o no el menú de ayuda contextual
 Debe llamarse después de que el editor ha procesado el evento, para tener
 el estado final del editor}
var IdentAct : string;         //IDentificador actual
    IdentAnt : string;         //IDentificador anterior
    BloqueAct: TFaSynBlock;
    tok: TFaTokInfo;
    atri: TSynHighlighterAttributes;
//    tokens: TATokInfo;
    curTok: integer;
begin
    //verificación principal
    if MenuContex.IsActive then Exit;   //ya está mostrado
    if ed = NIL then exit;     //no hay editor
    if ed.SelectionMode <> smNormal then exit;  //para no interferir en modo columna
    //filtra por tecla válida
    if not ( (key in [VK_A .. VK_Z, VK_SPACE, VK_OEM_PERIOD]) and (Shift <= [ssShift]) ) then
       Exit;
    //Analiza entorno de cursor
    MenuContex.theForm.MiraEntornoCursor;
    IdentAct := UpCase(MenuContex.TheForm.IdentAct);
    IdentAnt := UpCase(MenuContex.TheForm.IdentAnt);
    BloqueAct := MenuContex.TheForm.BloqueAct;
    //Verifica condiciones para mostrar ventana
    case MenuContex.TheForm.PosiCursor of
    pcEnMedioIdent:  begin //en medio de un identificador
        AbreAyudContextual;
      end;
    pcFinalDeIdent: begin  //caso típico. no se identifica situación especifica
        if (BloqueAct<> nil) and (BloqueAct.name = 'sec_SELECT') then begin
          //dentro de sección SELECT
          LlenaAyudContextual_FUNC;
          IdentAyudC.add('FROM');  //se espEra también esta palabra
          AbreAyudContextual;
        end else if (BloqueAct<> nil) and (BloqueAct.name = 'sec_FROM') then begin
          IdentAyudC.add('WHERE');  //se espera también esta palabra
          AbreAyudContextual;
        end else begin
          //no se identifica el contexto
          LlenaAyudContextual_IDEN;
          LlenaAyudContextual_TIPO(True);
          LlenaAyudContextual_FUNC(True);
  //        LlenaConAyudContextual2(True);
          AbreAyudContextual;
        end
      end;
    pcDespuesIdent: begin   //después de identificador
        If (IdentAnt = 'RETURN') Then begin
           LlenaAyudContextual_TIPO;
           AbreAyudContextual;
        end else If (IdentAnt = 'CONNECT') Then begin
//           LlenaConAyudContextual;
//           AbreAyudContextual;
        end
      end;
    else      //'pcDesconocido' no se identifica el contexto del código
      begin  end;
    end;
    {
        'Llena la lista para verificar
        lin = LeePrimerosCar(xIniId, curYt)   'lee línea a comparar
        ElseIf lin Like "*SELECT" Or lin Like "*IF" Or lin Like "*=" Or _
               lin Like "*," Or lin Like "*+" Or lin Like "*-" Or _
               lin Like "*/" Or lin Like "*>" Or lin Like "*<" Then
            Call LlenaFunAyudContextual
            Call LlenaIdenAyudContextual(True)
        ElseIf lin Like "*(" Then
            Call LlenaFunAyudContextual
            Call LlenaIdenAyudContextual(True)
        ElseIf lin Like "*'" Then
            Call LlenaForAyudContextual
}
//debugln(IdentAct0 + ',' + IdentAnt);
End;

procedure TMiObj.OnTextModified(Sender: TObject; KeyChar: TUTF8Char; Shift: TShiftState);
//Genera una lista de identificadores similares a un identificador
var i : Integer;
    IdentAct : string;      //IDentificador actual
    l: Integer;
begin
   IdentAct := UpCase(MenuContex.TheForm.IdentAct0);
   l := length(IdentAct);
//debug.OutPut('IdentAct: ' + IdentAct );
   //Genera la lista que coincide { TODO : Este proceso es lento si se actualizan muchas opciones en la lista }
   MenuContex.ItemList.Clear;
   for i:=0 to IdentAyudC.Count-1 do begin
      if IdentAct = upcase(copy(IdentAyudC[i],1,l)) then
          MenuContex.ItemList.Add(IdentAyudC[i]);
   end;
   if MenuContex.ItemList.Count = 0 then begin
     MenuContex.Deactivate;
   end;
End;

end.
