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

