program EDDMail;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ulogin, ustructures, umenuroot, umenuusuario, ubandejaentrada, 
uenviarcorreo;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TFormLogin, FormLogin);
  Application.CreateForm(TFormMenuRoot, FormMenuRoot);
  Application.CreateForm(TFormMenuUsuario, FormMenuUsuario);
  Application.CreateForm(TFormBandejaEntrada, FormBandejaEntrada);
  Application.CreateForm(TFormEnviarCorreo, FormEnviarCorreo);
  Application.Run;
end.

