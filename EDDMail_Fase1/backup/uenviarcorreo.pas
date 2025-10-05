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
    chkProgramado: TCheckBox;
    edtFecha: TEdit;
    edtAsunto: TEdit;
    edtDestinatario: TEdit;
    lblFecha: TLabel;
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

  // Verificar si es correo programado
  if chkProgramado.Checked then
  begin
    if edtFecha.Text = '' then
    begin
      ShowMessage('Debe especificar fecha y hora para correos programados');
      Exit;
    end;

    correo.programado := True;
    correo.fecha := edtFecha.Text;

    // Agregar a la cola de correos programados del remitente
    usuarioActual^.usuario.colaCorreos^.Encolar(correo);

    ShowMessage('Correo programado exitosamente');
  end
  else
  begin
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
    edtFecha.Text := '';

    Self.Close;
  end;
end;

procedure TFormEnviarCorreo.btnCancelarClick(Sender: TObject);
begin
  //limpiar campos para proximo envio
  edtDestinatario.Text := '';
  edtAsunto.Text := '';
  memoMensaje.Text := '';
  edtFecha.Text := '';
  Self.Close;
end;

end.

