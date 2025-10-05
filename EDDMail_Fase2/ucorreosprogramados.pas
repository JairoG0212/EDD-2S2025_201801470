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
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
  private

  public
    procedure CargarCorreosProgramados;
  end;

var
  FormCorreosProgramados: TFormCorreosProgramados;

implementation

{$R *.lfm}

procedure TFormCorreosProgramados.btnCerrarClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormCorreosProgramados.btnEnviarClick(Sender: TObject);
var
  correo: TCorreo;
  nodoDestinatario: PNodoUsuario;
  contadorEnviados: Integer;
begin
  if usuarioActual^.usuario.colaCorreos^.Vacia then
  begin
    ShowMessage('No hay correos programados para enviar');
    Exit;
  end;

  contadorEnviados := 0;

  // Procesar todos los correos de la cola (FIFO)
  while not usuarioActual^.usuario.colaCorreos^.Vacia do
  begin
    correo := usuarioActual^.usuario.colaCorreos^.Desencolar;

    // Buscar destinatario
    nodoDestinatario := listaUsuarios.Buscar(correo.destinatario);
    if nodoDestinatario <> nil then
    begin
      // Cambiar estado a no programado y agregar a bandeja del destinatario
      correo.programado := False;
      nodoDestinatario^.usuario.bandejaEntrada^.AgregarCorreo(correo);

      // Incrementar relación en matriz
      matrizRelaciones.IncrementarRelacion(correo.remitente, correo.destinatario);

      Inc(contadorEnviados);
    end;
  end;

  CargarCorreosProgramados; // Recargar lista (debería quedar vacía)
  ShowMessage('Se enviaron ' + IntToStr(contadorEnviados) + ' correos correctamente');
end;

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

