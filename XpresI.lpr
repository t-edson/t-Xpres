program XpresI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, synhighlighterfacil, utileditsyn, syncompletionq, FormPrincipal,
  uXpres, XpresComplet, Globales, XpresBas
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='XpresI';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.

