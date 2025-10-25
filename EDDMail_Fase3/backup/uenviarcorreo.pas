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
    chkBorrador: TCheckBox;
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

  if (destinatario = '') or (asunto = '') or (mensaje = '') then
  begin
    ShowMessage('Por favor complete todos los campos');
    Exit;
  end;

  nodoDestinatario := listaUsuarios.Buscar(destinatario);
  if nodoDestinatario = nil then
  begin
    ShowMessage('El usuario destinatario no existe');
    Exit;
  end;

  if not usuarioActual^.usuario.contactos^.BuscarContacto(destinatario) then
  begin
    ShowMessage('Error: Para enviar correos debe agregar primero al destinatario como contacto');
    Exit;
  end;

  correo.id := Random(10000) + 1;
  correo.remitente := usuarioActual^.usuario.email;
  correo.destinatario := destinatario;
  correo.asunto := asunto;
  correo.mensaje := mensaje;
  correo.fecha := DateToStr(Now);
  correo.estado := 'NL';
  correo.programado := False;

  if chkBorrador.Checked then
  begin
    arbolAVLBorradores.Insertar(correo);
    ShowMessage('Correo guardado como borrador exitosamente');

    edtDestinatario.Text := '';
    edtAsunto.Text := '';
    memoMensaje.Text := '';
    edtFecha.Text := '';
    chkProgramado.Checked := False;
    chkBorrador.Checked := False;

    Self.Close;
    Exit;
  end;

  if chkProgramado.Checked then
  begin
    if edtFecha.Text = '' then
    begin
      ShowMessage('Debe especificar fecha y hora para correos programados');
      Exit;
    end;

    correo.programado := True;
    correo.fecha := edtFecha.Text;
    usuarioActual^.usuario.colaCorreos^.Encolar(correo);
    ShowMessage('Correo programado exitosamente');

    edtDestinatario.Text := '';
    edtAsunto.Text := '';
    memoMensaje.Text := '';
    edtFecha.Text := '';
    chkProgramado.Checked := False;
    chkBorrador.Checked := False;

    Self.Close;
  end
  else
  begin
    correo.programado := False;
    nodoDestinatario^.usuario.bandejaEntrada^.AgregarCorreo(correo);
    matrizRelaciones.IncrementarRelacion(usuarioActual^.usuario.email, destinatario);
    ShowMessage('Correo enviado correctamente');

    edtDestinatario.Text := '';
    edtAsunto.Text := '';
    memoMensaje.Text := '';
    edtFecha.Text := '';

    Self.Close;
  end;
end;

procedure TFormEnviarCorreo.btnCancelarClick(Sender: TObject);
begin
  edtDestinatario.Text := '';
  edtAsunto.Text := '';
  memoMensaje.Text := '';
  edtFecha.Text := '';
  Self.Close;
end;

end.
