unit ubandejaentrada;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ustructures;

type

  { TFormBandejaEntrada }

  TFormBandejaEntrada = class(TForm)
    btnCerrar: TButton;
    btnEliminar: TButton;
    btnOrdenar: TButton;
    btnAgregarFavorito: TButton;
    lblCantidadNoLeidos: TLabel;
    lblTitulo: TLabel;
    lvCorreos: TListView;
    procedure btnAgregarFavoritoClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnOrdenarClick(Sender: TObject);
    procedure lvCorreosDblClick(Sender: TObject);
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

procedure TFormBandejaEntrada.btnAgregarFavoritoClick(Sender: TObject);
var
  indice: Integer;
  actual: PNodoCorreo;
  contador: Integer;
begin
  indice := lvCorreos.ItemIndex;
  if indice < 0 then
  begin
    ShowMessage('Seleccione un correo para agregar a favoritos');
    Exit;
  end;

  if lvCorreos.Items[indice].Caption = '---' then
    Exit;

  actual := usuarioActual^.usuario.bandejaEntrada^.ObtenerPrimero;
  contador := 0;

  while (actual <> nil) and (contador < indice) do
  begin
    actual := actual^.siguiente;
    Inc(contador);
  end;

  if actual <> nil then
  begin
    arbolBFavoritos.Insertar(actual^.correo);
    ShowMessage('Correo agregado a favoritos');
  end;
end;

procedure TFormBandejaEntrada.btnEliminarClick(Sender: TObject);
var
  indice: Integer;
  correoEliminado: TCorreo;
begin
  indice := lvCorreos.ItemIndex;
  if indice < 0 then
  begin
    ShowMessage('Seleccione un correo para eliminar');
    Exit;
  end;

  if lvCorreos.Items[indice].Caption = 'No hay correos en la bandeja de entrada' then
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

procedure TFormBandejaEntrada.btnOrdenarClick(Sender: TObject);
var
  listaCorreos: array of TCorreo;
  actual: PNodoCorreo;
  i, j, contador: Integer;
  temp: TCorreo;
begin
  if (usuarioActual = nil) or (usuarioActual^.usuario.bandejaEntrada = nil) then
    Exit;

  if usuarioActual^.usuario.bandejaEntrada^.Vacia then
    Exit;

  // Contar correos
  contador := 0;
  actual := usuarioActual^.usuario.bandejaEntrada^.ObtenerPrimero;
  while actual <> nil do
  begin
    Inc(contador);
    actual := actual^.siguiente;
  end;

  if contador = 0 then Exit;

  // Copiar correos a array
  SetLength(listaCorreos, contador);
  actual := usuarioActual^.usuario.bandejaEntrada^.ObtenerPrimero;
  i := 0;
  while actual <> nil do
  begin
    listaCorreos[i] := actual^.correo;
    actual := actual^.siguiente;
    Inc(i);
  end;

  // Ordenamiento burbuja por asunto
  for i := 0 to contador - 2 do
  begin
    for j := 0 to contador - 2 - i do
    begin
      if CompareText(listaCorreos[j].asunto, listaCorreos[j + 1].asunto) > 0 then
      begin
        temp := listaCorreos[j];
        listaCorreos[j] := listaCorreos[j + 1];
        listaCorreos[j + 1] := temp;
      end;
    end;
  end;

  // Actualizar la lista enlazada con el orden
  actual := usuarioActual^.usuario.bandejaEntrada^.ObtenerPrimero;
  i := 0;
  while actual <> nil do
  begin
    actual^.correo := listaCorreos[i];
    actual := actual^.siguiente;
    Inc(i);
  end;

  // Recargar vista
  CargarCorreos;
  ShowMessage('Correos ordenados alfabéticamente por asunto');
end;

procedure TFormBandejaEntrada.lvCorreosDblClick(Sender: TObject);
var
  indice: Integer;
  actual: PNodoCorreo;
  contador: Integer;
begin
  indice := lvCorreos.ItemIndex;
  if indice < 0 then Exit;

  if lvCorreos.Items[indice].Caption = 'No hay correos en la bandeja de entrada' then
    Exit;

  // Buscar el correo en la posición indicada
  actual := usuarioActual^.usuario.bandejaEntrada^.ObtenerPrimero;
  contador := 0;

  while (actual <> nil) and (contador < indice) do
  begin
    actual := actual^.siguiente;
    Inc(contador);
  end;

  if actual <> nil then
  begin
    // Cambiar estado a leído si estaba no leído
    if actual^.correo.estado = 'NL' then
    begin
      actual^.correo.estado := 'L';
      lvCorreos.Items[indice].Caption := 'L'; // Actualizar vista
    end;

    // Mostrar correo completo
    ShowMessage('De: ' + actual^.correo.remitente + #13#10 +
                'Asunto: ' + actual^.correo.asunto + #13#10 +
                'Fecha: ' + actual^.correo.fecha + #13#10#13#10 +
                'Mensaje:' + #13#10 + actual^.correo.mensaje);
  end;
end;

procedure TFormBandejaEntrada.CargarCorreos;
var
  actual: PNodoCorreo;
  item: String;
  contadorNoLeidos: Integer;
begin
  lvCorreos.Clear;
  contadorNoLeidos := 0; // Inicializar contador

  if (usuarioActual <> nil) and (usuarioActual^.usuario.bandejaEntrada <> nil) then
  begin
    actual := usuarioActual^.usuario.bandejaEntrada^.ObtenerPrimero;
    while actual <> nil do
    begin
      with lvCorreos.Items.Add do
      begin
        Caption := actual^.correo.estado;
        SubItems.Add(actual^.correo.asunto);
        SubItems.Add(actual^.correo.remitente);
        SubItems.Add(actual^.correo.fecha);
      end;

      // Contar correos no leídos
      if actual^.correo.estado = 'NL' then
        Inc(contadorNoLeidos);

      actual := actual^.siguiente;
    end;
  end;

  if lvCorreos.Items.Count = 0 then
    with lvCorreos.Items.Add do
    begin
      Caption := '---';
      SubItems.Add('No hay correos en la bandeja de entrada');
      SubItems.Add('---');
      SubItems.Add('---');
    end;

  // Actualizar label del contador
  if contadorNoLeidos > 0 then
    lblCantidadNoLeidos.Caption := 'Correos no leídos: ' + IntToStr(contadorNoLeidos)
  else
    lblCantidadNoLeidos.Caption := 'Todos los correos leídos';
end;

end.

