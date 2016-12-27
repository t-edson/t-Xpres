{
Rutinas y variables globales a la aplicación
}
unit Globales;

{$mode objfpc}{$H+}

interface
uses  Classes, SysUtils, Forms, Graphics, Dialogs, SynEdit, SynEditKeyCmds,
      SynEditTypes, StrUtils, lclType, syncompletion, Process, FileUtil,
      ShellApi;

const
  NOM_PROG = 'PreSQL 2.2b';   //nombre de programa

procedure MsgExc(msje: string);
procedure MsgErr(msje: string);
procedure MsgBox(txt: PChar);
procedure MsgBox(txt: String);

procedure Shell(cmd:string);

implementation

procedure MsgExc(msje: string);
//Mensaje de exclamación
begin
  Application.MessageBox(PChar(msje), '', MB_ICONEXCLAMATION);
end;
procedure MsgErr(msje: string);
//Mensaje de error
begin
  Application.MessageBox(PChar(msje), '', MB_ICONERROR);
end;

procedure MsgBox(txt: PChar);
begin
  Application.MessageBox(txt,'Caption',0);
end;

procedure MsgBox(txt: String);
begin
  Application.MessageBox(Pchar(txt),'Caption',0);
end;

procedure Shell(cmd: string);
//Ejecuta un programa externo
var
  p: TProcess;
begin
  //compila
  p := TProcess.Create(nil);
  p.CommandLine := cmd;
  p.Options := p.Options + [poWaitOnExit];
  p.Execute;
  p.Free;
end;

end.

