unit umenuusuario;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ubandejaentrada
  , uenviarcorreo, ustructures, ucontactos, upapelera;

type

  { TFormMenuUsuario }

  TFormMenuUsuario = class(TForm)
    btnBandejaEntrada: TButton;
    btnEnviarCorreo: TButton;
    btnPapelera: TButton;
    btnProgramarCorreo: TButton;
    btnCorreosProgramados: TButton;
    btnAgregarContacto: TButton;
    btnContactos: TButton;
    btnActualizarPerfil: TButton;
    btnGenerarReportes: TButton;
    btnCerrarSesionUsuario: TButton;
    lblHolaAlumno: TLabel;
    procedure btnAgregarContactoClick(Sender: TObject);
    procedure btnBandejaEntradaClick(Sender: TObject);
    procedure btnCerrarSesionUsuarioClick(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnEnviarCorreoClick(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
  private

  public

  end;

var
  FormMenuUsuario: TFormMenuUsuario;

implementation

{$R *.lfm}

{ TFormMenuUsuario }

procedure TFormMenuUsuario.btnCerrarSesionUsuarioClick(Sender: TObject);
begin
  // Cerrar este formulario
  Self.Close;
end;

procedure TFormMenuUsuario.btnContactosClick(Sender: TObject);
begin
  Self.Hide;
  FormContactos.CargarContactos;
  FormContactos.ShowModal;
  Self.Show;
end;

procedure TFormMenuUsuario.btnEnviarCorreoClick(Sender: TObject);
begin
  Self.Hide;
  FormEnviarCorreo.ShowModal;
  Self.Show;
end;

procedure TFormMenuUsuario.btnPapeleraClick(Sender: TObject);
begin
  Self.Hide;
  FormPapelera.CargarCorreosEliminados;
  FormPapelera.ShowModal;
  Self.Show;
end;

procedure TFormMenuUsuario.btnBandejaEntradaClick(Sender: TObject);
begin
  Self.Hide;
  FormBandejaEntrada.CargarCorreos;
  FormBandejaEntrada.ShowModal;
  Self.Show;
end;

procedure TFormMenuUsuario.btnAgregarContactoClick(Sender: TObject);
var
  email: String;
  nodoDestinatario: PNodoUsuario;
begin
  email := InputBox('Agregar Contacto', 'Ingrese el email del contacto:', '');

  if email = '' then
    Exit;

  // Verificar que el usuario existe
  nodoDestinatario := listaUsuarios.Buscar(email);
  if nodoDestinatario = nil then
  begin
    ShowMessage('El usuario no existe en el sistema');
    Exit;
  end;

  // Verificar que no sea él mismo
  if email = usuarioActual^.usuario.email then
  begin
    ShowMessage('No puedes agregarte a ti mismo como contacto');
    Exit;
  end;

  // Verificar que no esté ya agregado
  if usuarioActual^.usuario.contactos^.BuscarContacto(email) then
  begin
    ShowMessage('Este contacto ya está agregado');
    Exit;
  end;

  // Agregar el contacto
  usuarioActual^.usuario.contactos^.AgregarContacto(email);
  ShowMessage('Contacto agregado exitosamente');
end;

end.

