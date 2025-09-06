unit umenuusuario;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ubandejaentrada
  , uenviarcorreo, ustructures, ucontactos, upapelera, ucorreosprogramados;

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
    procedure btnActualizarPerfilClick(Sender: TObject);
    procedure btnAgregarContactoClick(Sender: TObject);
    procedure btnBandejaEntradaClick(Sender: TObject);
    procedure btnCerrarSesionUsuarioClick(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnCorreosProgramadosClick(Sender: TObject);
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

procedure TFormMenuUsuario.btnCorreosProgramadosClick(Sender: TObject);
begin
  Self.Hide;
  FormCorreosProgramados.CargarCorreosProgramados;
  FormCorreosProgramados.ShowModal;
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

procedure TFormMenuUsuario.btnActualizarPerfilClick(Sender: TObject);
var
  nuevoUsuario, nuevoTelefono: String;
  opcion, resultado: Integer;
begin
  opcion := MessageDlg('¿Qué desea actualizar?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);

  case opcion of
    mrYes: // Actualizar Usuario
    begin
      nuevoUsuario := InputBox('Actualizar Usuario', 'Ingrese el nuevo nombre de usuario:', usuarioActual^.usuario.usuario);

      resultado := listaUsuarios.ActualizarUsuario(usuarioActual^.usuario.email, nuevoUsuario);

      case resultado of
        0: ShowMessage('Usuario actualizado correctamente');
        1: ShowMessage('Error: El campo no puede estar vacío');
        2: ShowMessage('Error: Usuario no encontrado en el sistema');
        3: ShowMessage('Error: El nombre de usuario ya existe');
        4: ShowMessage('No se modificó nada: El nombre de usuario es el mismo');
      end;
    end;

    mrNo: // Actualizar Teléfono
    begin
      nuevoTelefono := InputBox('Actualizar Teléfono', 'Ingrese el nuevo teléfono:', usuarioActual^.usuario.telefono);

      resultado := listaUsuarios.ActualizarTelefono(usuarioActual^.usuario.email, nuevoTelefono);

      case resultado of
        0: ShowMessage('Teléfono actualizado correctamente');
        1: ShowMessage('Error: El campo no puede estar vacío');
        2: ShowMessage('Error: Usuario no encontrado en el sistema');
        3: ShowMessage('Error: El teléfono ya existe en otro usuario');
        4: ShowMessage('No se modificó nada: El teléfono es el mismo');
      end;
    end;

    mrCancel: // Cancelar
      Exit;
  end;
end;

end.

