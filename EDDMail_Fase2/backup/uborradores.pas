unit uborradores;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ustructures;

type

  { TFormBorradores }

  TFormBorradores = class(TForm)
    btnPreOrden: TButton;
    btnInOrden: TButton;
    btnPostOrden: TButton;
    btnModificar: TButton;
    btnEnviar: TButton;
    btnEliminar: TButton;
    btnCerrar: TButton;
    lblTitulo: TLabel;
    lvBorradores: TListView;
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure btnInOrdenClick(Sender: TObject);
    procedure btnModificarClick(Sender: TObject);
    procedure btnPostOrdenClick(Sender: TObject);
    procedure btnPreOrdenClick(Sender: TObject);
  private
    procedure CargarBorradores(tipoRecorrido: Integer);
  public

  end;

var
  FormBorradores: TFormBorradores;

implementation

{$R *.lfm}

{ TFormBorradores }

procedure TFormBorradores.CargarBorradores(tipoRecorrido: Integer);
var
  lista: TList;
  i: Integer;
  nodo: PNodoAVL;
begin
  lvBorradores.Clear;

  case tipoRecorrido of
    1: lista := arbolAVLBorradores.ObtenerBorradoresPreOrden;
    2: lista := arbolAVLBorradores.ObtenerBorradoresInOrden;
    3: lista := arbolAVLBorradores.ObtenerBorradoresPostOrden;
  else
    lista := arbolAVLBorradores.ObtenerBorradoresInOrden;
  end;

  try
    for i := 0 to lista.Count - 1 do
    begin
      nodo := PNodoAVL(lista[i]);
      with lvBorradores.Items.Add do
      begin
        Caption := IntToStr(nodo^.correo.id);
        SubItems.Add(nodo^.correo.asunto);
        SubItems.Add(nodo^.correo.destinatario);
      end;
    end;

    if lvBorradores.Items.Count = 0 then
      with lvBorradores.Items.Add do
      begin
        Caption := '---';
        SubItems.Add('No hay borradores');
        SubItems.Add('---');
      end;
  finally
    lista.Free;
  end;
end;

procedure TFormBorradores.btnPreOrdenClick(Sender: TObject);
begin
  CargarBorradores(1);
end;

procedure TFormBorradores.btnInOrdenClick(Sender: TObject);
begin
  CargarBorradores(2);
end;

procedure TFormBorradores.btnPostOrdenClick(Sender: TObject);
begin
  CargarBorradores(3);
end;

procedure TFormBorradores.btnModificarClick(Sender: TObject);
var
  indice: Integer;
  lista: TList;
  nodo: PNodoAVL;
  nuevoAsunto, nuevoMensaje: String;
begin
  indice := lvBorradores.ItemIndex;
  if indice < 0 then
  begin
    ShowMessage('Seleccione un borrador para modificar');
    Exit;
  end;

  if lvBorradores.Items[indice].Caption = '---' then
    Exit;

  lista := arbolAVLBorradores.ObtenerBorradoresInOrden;
  try
    if indice >= lista.Count then Exit;

    nodo := PNodoAVL(lista[indice]);

    nuevoAsunto := InputBox('Modificar Borrador', 'Asunto:', nodo^.correo.asunto);
    if Trim(nuevoAsunto) = '' then Exit;

    nuevoMensaje := InputBox('Modificar Borrador', 'Mensaje:', nodo^.correo.mensaje);
    if Trim(nuevoMensaje) = '' then Exit;

    nodo^.correo.asunto := nuevoAsunto;
    nodo^.correo.mensaje := nuevoMensaje;

    ShowMessage('Borrador modificado exitosamente');
    CargarBorradores(2);
  finally
    lista.Free;
  end;
end;

procedure TFormBorradores.btnEnviarClick(Sender: TObject);
var
  indice: Integer;
  lista: TList;
  nodo: PNodoAVL;
  nodoDestinatario: PNodoUsuario;
begin
  indice := lvBorradores.ItemIndex;
  if indice < 0 then
  begin
    ShowMessage('Seleccione un borrador para enviar');
    Exit;
  end;

  if lvBorradores.Items[indice].Caption = '---' then
    Exit;

  lista := arbolAVLBorradores.ObtenerBorradoresInOrden;
  try
    if indice >= lista.Count then Exit;

    nodo := PNodoAVL(lista[indice]);

    nodoDestinatario := listaUsuarios.Buscar(nodo^.correo.destinatario);
    if nodoDestinatario = nil then
    begin
      ShowMessage('Error: El destinatario ya no existe en el sistema');
      Exit;
    end;

    if MessageDlg('Confirmar', 'Enviar borrador a ' + nodo^.correo.destinatario + '?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      nodo^.correo.fecha := DateToStr(Now);
      nodo^.correo.estado := 'NL';
      nodoDestinatario^.usuario.bandejaEntrada^.AgregarCorreo(nodo^.correo);
      matrizRelaciones.IncrementarRelacion(nodo^.correo.remitente, nodo^.correo.destinatario);

      arbolAVLBorradores.Eliminar(nodo^.correo.id);

      ShowMessage('Correo enviado exitosamente');
      CargarBorradores(2);
    end;
  finally
    lista.Free;
  end;
end;

procedure TFormBorradores.btnEliminarClick(Sender: TObject);
var
  indice: Integer;
  lista: TList;
  nodo: PNodoAVL;
begin
  indice := lvBorradores.ItemIndex;
  if indice < 0 then
  begin
    ShowMessage('Seleccione un borrador para eliminar');
    Exit;
  end;

  if lvBorradores.Items[indice].Caption = '---' then
    Exit;

  lista := arbolAVLBorradores.ObtenerBorradoresInOrden;
  try
    if indice >= lista.Count then Exit;

    nodo := PNodoAVL(lista[indice]);

    if MessageDlg('Confirmar', 'Eliminar borrador permanentemente?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      arbolAVLBorradores.Eliminar(nodo^.correo.id);
      ShowMessage('Borrador eliminado');
      CargarBorradores(2);
    end;
  finally
    lista.Free;
  end;
end;

procedure TFormBorradores.btnCerrarClick(Sender: TObject);
begin
  Self.Close;
end;

end.

