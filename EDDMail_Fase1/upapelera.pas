unit upapelera;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ustructures;

type

  { TFormPapelera }

  TFormPapelera = class(TForm)
    btnBuscar: TButton;
    btnCerrar: TButton;
    btnEliminar: TButton;
    lblTitulo: TLabel;
    lstCorreosEliminados: TListBox;
    procedure btnBuscarClick(Sender: TObject);
  private

  public
     procedure CargarCorreosEliminados;
  end;

var
  FormPapelera: TFormPapelera;

implementation

{$R *.lfm}

procedure TFormPapelera.btnBuscarClick(Sender: TObject);
var
  palabraClave: String;
  actual: PNodoPila;
  item: String;
  encontrado: Boolean;
begin
  palabraClave := InputBox('Buscar Correo', 'Ingrese palabra clave del asunto:', '');

  if palabraClave = '' then
    Exit;

  lstCorreosEliminados.Clear;
  encontrado := False;

  if (usuarioActual <> nil) and not usuarioActual^.usuario.papelera^.Vacia then
  begin
    actual := usuarioActual^.usuario.papelera^.ObtenerTope;
    while actual <> nil do
    begin
      if Pos(LowerCase(palabraClave), LowerCase(actual^.correo.asunto)) > 0 then
      begin
        item := actual^.correo.estado + ' | ' +
                actual^.correo.asunto + ' | ' +
                actual^.correo.remitente;
        lstCorreosEliminados.Items.Add(item);
        encontrado := True;
      end;
      actual := actual^.siguiente;
    end;
  end;

  if not encontrado then
    lstCorreosEliminados.Items.Add('No se encontraron correos con esa palabra clave');
end;

procedure TFormPapelera.CargarCorreosEliminados;
var
  actual: PNodoPila;
  item: String;
begin
  lstCorreosEliminados.Clear;

  if (usuarioActual <> nil) and not usuarioActual^.usuario.papelera^.Vacia then
  begin
    actual := usuarioActual^.usuario.papelera^.ObtenerTope;
    while actual <> nil do
    begin
      item := actual^.correo.estado + ' | ' +
              actual^.correo.asunto + ' | ' +
              actual^.correo.remitente;
      lstCorreosEliminados.Items.Add(item);
      actual := actual^.siguiente;
    end;
  end;

  if lstCorreosEliminados.Items.Count = 0 then
    lstCorreosEliminados.Items.Add('No hay correos en la papelera');
end;

end.

