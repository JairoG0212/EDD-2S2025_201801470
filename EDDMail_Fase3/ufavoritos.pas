unit ufavoritos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, ustructures;

type

  { TFormFavoritos }

  TFormFavoritos = class(TForm)
    btnEliminar: TButton;
    btnCerrar: TButton;
    lblMensaje: TLabel;
    lblFecha: TLabel;
    lblAsunto: TLabel;
    lblRemitente: TLabel;
    lblTotal: TLabel;
    lblTitulo: TLabel;
    lvFavoritos: TListView;
    memoMensaje: TMemo;
    pnlDetalles: TPanel;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure lvFavoritosSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private

  public
    procedure CargarFavoritos;
  end;

var
  FormFavoritos: TFormFavoritos;

implementation

{$R *.lfm}

{ TFormFavoritos }

procedure TFormFavoritos.CargarFavoritos;
var
  lista: TList;
  i: Integer;
  pCorreo: ^TCorreo;
begin
  lvFavoritos.Clear;

  lista := arbolBFavoritos.ObtenerTodosFavoritos;
  try
    for i := 0 to lista.Count - 1 do
    begin
      pCorreo := lista[i];
      with lvFavoritos.Items.Add do
      begin
        Caption := IntToStr(pCorreo^.id);
        SubItems.Add(pCorreo^.asunto);
        SubItems.Add(pCorreo^.remitente);
      end;
    end;

    lblTotal.Caption := 'Total de favoritos: ' + IntToStr(lista.Count);

    if lvFavoritos.Items.Count = 0 then
    begin
      with lvFavoritos.Items.Add do
      begin
        Caption := '---';
        SubItems.Add('No hay correos favoritos');
        SubItems.Add('---');
      end;
      lblTotal.Caption := 'Total de favoritos: 0';
    end;

    for i := 0 to lista.Count - 1 do
    begin
      pCorreo := lista[i];
      Dispose(pCorreo);
    end;
  finally
    lista.Free;
  end;

  lblRemitente.Caption := 'Remitente: ';
  lblAsunto.Caption := 'Asunto: ';
  lblFecha.Caption := 'Fecha: ';
  memoMensaje.Clear;
end;

procedure TFormFavoritos.lvFavoritosSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  idCorreo: Integer;
  correo: TCorreo;
begin
  if not Selected then Exit;

  if Item.Caption = '---' then Exit;

  idCorreo := StrToIntDef(Item.Caption, 0);
  if idCorreo = 0 then Exit;

  correo := arbolBFavoritos.ObtenerCorreo(idCorreo);

  lblRemitente.Caption := 'Remitente: ' + correo.remitente;
  lblAsunto.Caption := 'Asunto: ' + correo.asunto;
  lblFecha.Caption := 'Fecha: ' + correo.fecha;
  memoMensaje.Text := correo.mensaje;
end;

procedure TFormFavoritos.btnEliminarClick(Sender: TObject);
begin
  ShowMessage('Funcionalidad de eliminar favoritos pendiente de implementar' + #13#10 +
              '(Requiere operacion de eliminacion en Arbol B)');
end;

procedure TFormFavoritos.btnCerrarClick(Sender: TObject);
begin
  Self.Close;
end;

end.

