unit umenuusuario;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ubandejaentrada
  , uenviarcorreo, ustructures, ucontactos, upapelera, ucorreosprogramados, uarbolbstcomunidades,
  uborradores, ufavoritos;

type

  { TFormMenuUsuario }

  TFormMenuUsuario = class(TForm)
    btnBandejaEntrada: TButton;
    btnEnviarCorreo: TButton;
    btnPapelera: TButton;
    btnCorreosProgramados: TButton;
    btnAgregarContacto: TButton;
    btnContactos: TButton;
    btnActualizarPerfil: TButton;
    btnGenerarReportes: TButton;
    btnCerrarSesionUsuario: TButton;
    Button1: TButton;
    btnVerBorradores: TButton;
    btnVerFavoritos: TButton;
    btnEliminarContacto: TButton;
    lblHolaAlumno: TLabel;
    procedure btnActualizarPerfilClick(Sender: TObject);
    procedure btnAgregarContactoClick(Sender: TObject);
    procedure btnBandejaEntradaClick(Sender: TObject);
    procedure btnCerrarSesionUsuarioClick(Sender: TObject);
    procedure btnContactosClick(Sender: TObject);
    procedure btnCorreosProgramadosClick(Sender: TObject);
    procedure btnEliminarContactoClick(Sender: TObject);
    procedure btnEnviarCorreoClick(Sender: TObject);
    procedure btnGenerarReportesClick(Sender: TObject);
    procedure btnPapeleraClick(Sender: TObject);
    procedure btnPublicarMensajeClick(Sender: Tobject);
    procedure btnVerBorradoresClick(Sender: TObject);
    procedure btnVerFavoritosClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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

procedure TFormMenuUsuario.btnEliminarContactoClick(Sender: TObject);
var
  email, nombreUsuario: String;
  nodoUsuario: PNodoUsuario;
  contactoActual: PNodoContacto;
  encontrado: Boolean;
begin
  nombreUsuario := InputBox('Eliminar Contacto', 'Ingrese el nombre de Usuario del contacto a eliminar:', '');

  if nombreUsuario = '' then
    Exit;

  email := '';
  encontrado := False;

  if not usuarioActual^.usuario.contactos^.Vacia then
    begin
      contactoActual := usuarioActual^.usuario.contactos^.ObtenerPrimero;
      repeat
        nodoUsuario := listaUsuarios.Buscar(contactoActual^.email);
        if (nodoUsuario <> nil) and (nodoUsuario^.usuario.usuario = nombreUsuario) then
          begin
            email := contactoActual^.email;
            encontrado := True;
            Break;
          end;
        contactoActual := contactoActual^.siguiente;
      until contactoActual = usuarioActual^.usuario.contactos^.ObtenerPrimero;
    end;

  if not encontrado then
    begin
      ShowMessage('El usuario "' + nombreUsuario + '" no existe en su lista de contactos');
      Exit;
    end;

  if MessageDlg('Confirmar Eliminar a "' + nombreUsuario+ '" de sus contactos',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
       begin
         if usuarioActual^.usuario.contactos^.EliminarContacto(email) then
           ShowMessage('Contacto eliminado');
         else
             ShowMessage('Error al eliminar contacto');
       end;
end;

procedure TFormMenuUsuario.btnEnviarCorreoClick(Sender: TObject);
begin
  Self.Hide;
  FormEnviarCorreo.ShowModal;
  Self.Show;
end;

procedure TFormMenuUsuario.btnGenerarReportesClick(Sender: TObject);
var
  carpetaReportes, nombreUsuario: String;
  rutaCompleta: String;
  reportesGenerados: Integer;
begin
  if usuarioActual = nil then
  begin
    ShowMessage('Error: No hay usuario logueado');
    Exit;
  end;

  reportesGenerados := 0;
  nombreUsuario := usuarioActual^.usuario.usuario;
  carpetaReportes := nombreUsuario + '-Reportes';

  try
    if not DirectoryExists(carpetaReportes) then
      CreateDir(carpetaReportes);

    // 1. Reporte de Correos Recibidos
    rutaCompleta := carpetaReportes + '/reporte_correos_recibidos.dot';
    if usuarioActual^.usuario.bandejaEntrada <> nil then
    begin
      usuarioActual^.usuario.bandejaEntrada^.GenerarReporteCorreosRecibidos(rutaCompleta, nombreUsuario);
      Inc(reportesGenerados);
    end;

    // 2. Reporte de Papelera
    rutaCompleta := carpetaReportes + '/reporte_papelera.dot';
    if usuarioActual^.usuario.papelera <> nil then
    begin
      usuarioActual^.usuario.papelera^.GenerarReportePapelera(rutaCompleta, nombreUsuario);
      Inc(reportesGenerados);
    end;

    // 3. Reporte de Correos Programados
    rutaCompleta := carpetaReportes + '/reporte_correos_programados.dot';
    if usuarioActual^.usuario.colaCorreos <> nil then
    begin
      usuarioActual^.usuario.colaCorreos^.GenerarReporteCorreosProgramados(rutaCompleta, nombreUsuario);
      Inc(reportesGenerados);
    end;

    // 4. Reporte de Contactos
    rutaCompleta := carpetaReportes + '/reporte_contactos.dot';
    if usuarioActual^.usuario.contactos <> nil then
    begin
      usuarioActual^.usuario.contactos^.GenerarReporteContactos(rutaCompleta, nombreUsuario);
      Inc(reportesGenerados);
    end;

    // 5. Reporte BST Comunidades
    rutaCompleta := carpetaReportes + '/reporte_comunidades_bst.dot';
    arbolBSTComunidades.SincronizarDesdeLista(listaComunidades);
    arbolBSTComunidades.GenerarReporteBST(rutaCompleta);
    Inc(reportesGenerados);

    // 6. Reporte AVL Borradores (NUEVO)
    rutaCompleta := carpetaReportes + '/reporte_borradores_avl.dot';
    arbolAVLBorradores.GenerarReporteAVL(rutaCompleta);
    Inc(reportesGenerados);

    ShowMessage('Reportes generados exitosamente (' + IntToStr(reportesGenerados) + ' reportes)' + #13#10 +
                'Ubicacion: ' + carpetaReportes + '/');

    // 7. Reporte Árbol B Favoritos (AGREGAR DESPUÉS DEL REPORTE AVL)
    rutaCompleta := carpetaReportes + '/reporte_favoritos_arbolb.dot';
    arbolBFavoritos.GenerarReporteArbolB(rutaCompleta);
    Inc(reportesGenerados);

  except
    on E: Exception do
      ShowMessage('Error al generar reportes: ' + E.Message);
  end;
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
  if email = usuarioActual^.usuario.usuario then
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
  resultado: Integer;
begin
  // Primer diálogo para elegir qué actualizar
  if MessageDlg('Actualizar Perfil', '¿Desea actualizar el nombre de usuario?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Actualizar Usuario
    nuevoUsuario := InputBox('Actualizar Usuario', 'Ingrese el nuevo nombre de usuario:', usuarioActual^.usuario.usuario);

    resultado := listaUsuarios.ActualizarUsuario(usuarioActual^.usuario.email, nuevoUsuario);

    case resultado of
      0: ShowMessage('Usuario actualizado correctamente');
      1: ShowMessage('Error: El campo no puede estar vacío');
      2: ShowMessage('Error: Usuario no encontrado en el sistema');
      3: ShowMessage('Error: El nombre de usuario ya existe');
      4: ShowMessage('No se modificó nada: El nombre de usuario es el mismo');
    end;
  end
  else if MessageDlg('Actualizar Perfil', '¿Desea actualizar el teléfono?',
                     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Actualizar Teléfono
    nuevoTelefono := InputBox('Actualizar Teléfono', 'Ingrese el nuevo teléfono:', usuarioActual^.usuario.telefono);

    resultado := listaUsuarios.ActualizarTelefono(usuarioActual^.usuario.email, nuevoTelefono);

    case resultado of
      0: ShowMessage('Teléfono actualizado correctamente');
      1: ShowMessage('Error: El campo no puede estar vacío');
      2: ShowMessage('Error: Usuario no encontrado en el sistema');
      3: ShowMessage('Error: El teléfono ya existe en otro usuario');
      4: ShowMessage('No se modificó nada: El teléfono es el mismo');
    end;
  end
  else
  begin
    ShowMessage('Operación cancelada');
  end;
end;

procedure TFormMenuUsuario.btnPublicarMensajeClick(Sender: TObject);
var
  nombreComunidad, mensaje: String;
  comunidad: PNodoComunidad;
  miembroActual: PNodoMiembro;
  esMiembro: Boolean;
begin
  if usuarioActual = nil then
  begin
    ShowMessage('Error: No hay usuario logueado');
    Exit;
  end;

  nombreComunidad := InputBox('Publicar Mensaje',
                              'Ingrese el nombre de la comunidad:', '');

  if Trim(nombreComunidad) = '' then
    Exit;

  arbolBSTComunidades.SincronizarDesdeLista(listaComunidades);
  comunidad := arbolBSTComunidades.Buscar(nombreComunidad);

  if comunidad = nil then
  begin
    ShowMessage('Error: La comunidad "' + nombreComunidad + '" no existe');
    Exit;
  end;

  esMiembro := False;
  miembroActual := comunidad^.miembros;
  while miembroActual <> nil do
  begin
    if miembroActual^.emailUsuario = usuarioActual^.usuario.email then
    begin
      esMiembro := True;
      Break;
    end;
    miembroActual := miembroActual^.siguiente;
  end;

  if not esMiembro then
  begin
    ShowMessage('Error: Debe ser miembro de la comunidad para publicar mensajes');
    Exit;
  end;

  mensaje := InputBox('Publicar Mensaje',
                      'Ingrese su mensaje:', '');

  if Trim(mensaje) = '' then
    Exit;

  listaComunidades.PublicarMensaje(nombreComunidad,
                                   usuarioActual^.usuario.email,
                                   mensaje);
end;

procedure TFormMenuUsuario.btnVerBorradoresClick(Sender: TObject);
begin
  Self.Hide;
  FormBorradores.CargarBorradores(2);
  FormBorradores.ShowModal;
  Self.Show;
end;

procedure TFormMenuUsuario.btnVerFavoritosClick(Sender: TObject);
begin
  Self.Hide;
  FormFavoritos.CargarFavoritos;
  FormFavoritos.ShowModal;
  Self.Show;
end;

procedure TFormMenuUsuario.Button1Click(Sender: TObject);
var
  nombreComunidad, mensaje: String;
  comunidad: PNodoComunidad;
  miembroActual: PNodoMiembro;
  esMiembro: Boolean;
begin
  if usuarioActual = nil then
  begin
    ShowMessage('Error: No hay usuario logueado');
    Exit;
  end;

  nombreComunidad := InputBox('Publicar Mensaje',
                              'Ingrese el nombre de la comunidad:', '');

  if Trim(nombreComunidad) = '' then
    Exit;

  arbolBSTComunidades.SincronizarDesdeLista(listaComunidades);
  comunidad := arbolBSTComunidades.Buscar(nombreComunidad);

  if comunidad = nil then
  begin
    ShowMessage('Error: La comunidad "' + nombreComunidad + '" no existe');
    Exit;
  end;

  esMiembro := False;
  miembroActual := comunidad^.miembros;
  while miembroActual <> nil do
  begin
    if miembroActual^.emailUsuario = usuarioActual^.usuario.email then
    begin
      esMiembro := True;
      Break;
    end;
    miembroActual := miembroActual^.siguiente;
  end;

  if not esMiembro then
  begin
    ShowMessage('Error: Debe ser miembro de la comunidad para publicar mensajes');
    Exit;
  end;

  mensaje := InputBox('Publicar Mensaje',
                      'Ingrese su mensaje:', '');

  if Trim(mensaje) = '' then
    Exit;

  listaComunidades.PublicarMensaje(nombreComunidad,
                                   usuarioActual^.usuario.email,
                                   mensaje);
end;

end.

