unit ubandejaentrada;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ustructures;

type

  { TFormBandejaEntrada }

  TFormBandejaEntrada = class(TForm)
    btnCerrar: TButton;
    btnEliminar: TButton;
    lblTitulo: TLabel;
    lstCorreos: TListBox;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
  private

  public
    procedure CargarCorreos;
  end;

var
  FormBandejaEntrada: TFormBandejaEntrada;

implementation

{$R *.lfm}

{ TFormBandejaEntrada }

procedure TFormBandejaEntrada.btnCerrarClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormBandejaEntrada.btnEliminarClick(Sender: TObject);
var
  indice: Integer;
  correoEliminado: TCorreo;
begin
  indice := lstCorreos.ItemIndex;
  if indice < 0 then
  begin
    ShowMessage('Seleccione un correo para eliminar');
    Exit;
  end;

  if lstCorreos.Items[indice] = 'No hay correos en la bandeja de entrada' then
    Exit;

  if MessageDlg('¿Está seguro de mover este correo a la papelera?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Eliminar correo de la bandeja y obtenerlo
    correoEliminado := usuarioActual^.usuario.bandejaEntrada^.EliminarCorreo(indice);

    // Agregarlo a la papelera
    usuarioActual^.usuario.papelera^.Apilar(correoEliminado);

    // Recargar la lista
    CargarCorreos;

    ShowMessage('Correo movido a la papelera');
  end;
end;

procedure TFormBandejaEntrada.CargarCorreos;
var
  actual: PNodoCorreo;
  item: String;
begin
  lstCorreos.Clear;

  if (usuarioActual <> nil) and (usuarioActual^.usuario.bandejaEntrada <> nil) then
  begin
    actual := usuarioActual^.usuario.bandejaEntrada^.ObtenerPrimero;
    while actual <> nil do
    begin
      item := actual^.correo.estado + ' | ' +
              actual^.correo.asunto + ' | ' +
              actual^.correo.remitente;
      lstCorreos.Items.Add(item);
      actual := actual^.siguiente;
    end;
  end;

  if lstCorreos.Items.Count = 0 then
    lstCorreos.Items.Add('No hay correos en la bandeja de entrada');
end;

end.

