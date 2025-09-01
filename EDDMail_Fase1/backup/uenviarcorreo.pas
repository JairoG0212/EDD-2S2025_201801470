unit uenviarcorreo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ustructures;

type

  { TFormEnviarCorreo }

  TFormEnviarCorreo = class(TForm)
    btnCancelar: TButton;
    btnEnviar: TButton;
    edtAsunto: TEdit;
    edtDestinatario: TEdit;
    lblAsunto: TLabel;
    lblDestinatario: TLabel;
    lblMensaje: TLabel;
    lblTitulo: TLabel;
    memoMensaje: TMemo;
    procedure btnCancelarClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
  private

  public

  end;

var
  FormEnviarCorreo: TFormEnviarCorreo;

implementation

{$R *.lfm}

{ TFormEnviarCorreo }

procedure TFormEnviarCorreo.btnEnviarClick(Sender: TObject);
var
  destinatario, asunto, mensaje: String;
  nodoDestinatario: PNodoUsuario;
  correo: TCorreo;
begin
  destinatario := edtDestinatario.Text;
  asunto := edtAsunto.Text;
  mensaje := memoMensaje.Text;

  // Validar que los campos no estén vacíos
  if (destinatario = '') or (asunto = '') or (mensaje = '') then
  begin
    ShowMessage('Por favor complete todos los campos');
    Exit;
  end;

  // Buscar el usuario destinatario
  nodoDestinatario := listaUsuarios.Buscar(destinatario);
  if nodoDestinatario = nil then
  begin
    ShowMessage('El usuario destinatario no existe');
    Exit;
  end;

  // Verificar que el destinatario esté en la lista de contactos
  if not usuarioActual^.usuario.contactos^.BuscarContacto(destinatario) then
  begin
    ShowMessage('Error: Para enviar correos debe agregar primero al destinatario como contacto');
    Exit;
  end;

  // Crear el correo
  correo.id := Random(1000) + 1;
  correo.remitente := usuarioActual^.usuario.email;
  correo.destinatario := destinatario;
  correo.asunto := asunto;
  correo.mensaje := mensaje;
  correo.fecha := DateToStr(Now);
  correo.estado := 'NL'; // No leído
  correo.programado := False;

  // Agregar el correo a la bandeja del destinatario
  nodoDestinatario^.usuario.bandejaEntrada^.AgregarCorreo(correo);

  // Incrementar relación en la matriz dispersa
  matrizRelaciones.IncrementarRelacion(usuarioActual^.usuario.email, destinatario);

  ShowMessage('Correo enviado correctamente');

  //limpiar campos para proximo envio
  edtDestinatario.Text := '';
  edtAsunto.Text := '';
  memoMensaje.Text := '';

  Self.Close;
end;

procedure TFormEnviarCorreo.btnCancelarClick(Sender: TObject);
begin
  //limpiar campos para proximo envio
  edtDestinatario.Text := '';
  edtAsunto.Text := '';
  memoMensaje.Text := '';
  Self.Close;
end;

end.

