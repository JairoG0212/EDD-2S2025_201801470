unit ucorreosprogramados;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ustructures;

type

  { TFormCorreosProgramados }

  TFormCorreosProgramados = class(TForm)
    btnCerrar: TButton;
    btnEnviar: TButton;
    lblTitulo: TLabel;
    lstCorreosProgramados: TListBox;
  private

  public
    procedure CargarCorreosProgramados;
  end;

var
  FormCorreosProgramados: TFormCorreosProgramados;

implementation

{$R *.lfm}

procedure TFormCorreosProgramados.CargarCorreosProgramados;
var
  actual: PNodoCola;
  item: String;
begin
  lstCorreosProgramados.Clear;

  if (usuarioActual <> nil) and not usuarioActual^.usuario.colaCorreos^.Vacia then
  begin
    actual := usuarioActual^.usuario.colaCorreos^.ObtenerFrente;
    while actual <> nil do
    begin
      item := actual^.correo.asunto + ' | ' +
              actual^.correo.destinatario + ' | ' +
              actual^.correo.fecha;
      lstCorreosProgramados.Items.Add(item);
      actual := actual^.siguiente;
    end;
  end;

  if lstCorreosProgramados.Items.Count = 0 then
    lstCorreosProgramados.Items.Add('No hay correos programados');
end;

end.

