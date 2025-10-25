unit umenuroot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ustructures,
  uarbolbstcomunidades;

type

  { TFormMenuRoot }

  TFormMenuRoot = class(TForm)
    btnCargaMasiva: TButton;
    btnReporteDeUsuarios: TButton;
    btnReporteDeRelaciones: TButton;
    btnCerrarSesionRoot: TButton;
    btnCrearComunida: TButton;
    btnAgregarMiembro: TButton;
    btnReporteComunidades: TButton;
    lblTitulo: TLabel;
    procedure btnAgregarMiembroClick(Sender: TObject);
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnCerrarSesionRootClick(Sender: TObject);
    procedure btnCrearComunidaClick(Sender: TObject);
    procedure btnReporteComunidadesClick(Sender: TObject);
    procedure btnReporteDeRelacionesClick(Sender: TObject);
    procedure btnReporteDeUsuariosClick(Sender: TObject);
  private

  public

  end;

var
  FormMenuRoot: TFormMenuRoot;

implementation

{$R *.lfm}

{ TFormMenuRoot }

procedure TFormMenuRoot.btnCerrarSesionRootClick(Sender: TObject);
begin
  // Cerrar este formulario
  Self.Close;
end;

procedure TFormMenuRoot.btnCrearComunidaClick(Sender: TObject);
var
  nombreComunidad: String;
begin
  nombreComunidad := InputBox('Crear Comunidad', 'Ingrese el nombre de la comunidad:', '');

  if nombreComunidad = '' then
    Exit;

  if listaComunidades.CrearComunidad(nombreComunidad) then
    ShowMessage('Comunidad creada exitosamente')
  else
    ShowMessage('Error: La comunidad ya existe');
end;

procedure TFormMenuRoot.btnReporteComunidadesClick(Sender: TObject);
var
  nombreArchivo: String;
begin
  nombreArchivo := 'Root-Reportes/reporte_comunidades_bst.dot';

  try
    if not DirectoryExists('Root-Reportes') then
      CreateDir('Root-Reportes');

    arbolBSTComunidades.SincronizarDesdeLista(listaComunidades);
    arbolBSTComunidades.GenerarReporteBST(nombreArchivo);

    if FileExists('/usr/bin/dot') then
      ShowMessage('Reporte BST de comunidades generado: Root-Reportes/reporte_comunidades_bst.png')
    else
      ShowMessage('Reporte generado: ' + nombreArchivo);

  except
    on E: Exception do
      ShowMessage('Error al generar reporte: ' + E.Message);
  end;
end;

procedure TFormMenuRoot.btnReporteDeRelacionesClick(Sender: TObject);
var
  nombreArchivo, carpetaReportes: String;
begin
  try
    // Crear carpeta Root-Reportes si no existe
    carpetaReportes := 'Root-Reportes';
    if not DirectoryExists(carpetaReportes) then
      CreateDir(carpetaReportes);

    // Generar nombre del archivo
    nombreArchivo := carpetaReportes + '/reporte_relaciones.dot';

    // Generar el reporte
    matrizRelaciones.GenerarReporteRelaciones(nombreArchivo);

    ShowMessage('Reporte de relaciones generado exitosamente en: ' + nombreArchivo);

  except
    on E: Exception do
      ShowMessage('Error al generar reporte: ' + E.Message);
  end;
end;

procedure TFormMenuRoot.btnReporteDeUsuariosClick(Sender: TObject);
var
  nombreArchivo, carpetaReportes: String;
begin
  try
    // Crear carpeta Root-Reportes si no existe
    carpetaReportes := 'Root-Reportes';
    if not DirectoryExists(carpetaReportes) then
      CreateDir(carpetaReportes);

    // Generar nombre del archivo
    nombreArchivo := carpetaReportes + '/reporte_usuarios.dot';

    // Generar el reporte
    listaUsuarios.GenerarReporteUsuarios(nombreArchivo);

    ShowMessage('Reporte de usuarios generado exitosamente en: ' + nombreArchivo);

  except
    on E: Exception do
      ShowMessage('Error al generar reporte: ' + E.Message);
  end;
end;

procedure TFormMenuRoot.btnCargaMasivaClick(Sender: TObject);
var
  openDialog: TOpenDialog;
  extension: String;
begin
  openDialog := TOpenDialog.Create(Self);
  try
    openDialog.Title := 'Seleccionar archivo JSON';
    openDialog.Filter := 'Archivos JSON|*.json|Todos los archivos|*.*';
    openDialog.DefaultExt := 'json';

    if openDialog.Execute then
    begin
      if not FileExists(openDialog.FileName) then
      begin
        ShowMessage('Error: El archivo seleccionado no existe');
        Exit;
      end;

      if LowerCase(ExtractFileExt(openDialog.FileName)) <> '.json' then
      begin
        ShowMessage('Error: Por favor seleccione un archivo con extension .json');
        Exit;
      end;

      if Pos('usuario', LowerCase(ExtractFileName(openDialog.FileName))) > 0 then
      begin
        listaUsuarios.CargarDesdeJSON(openDialog.FileName);
      end
      else if Pos('correo', LowerCase(ExtractFileName(openDialog.FileName))) > 0 then
      begin
        listaUsuarios.CargarCorreosDesdeJSON(openDialog.FileName);
      end
      else
      begin
        if MessageDlg('Tipo de archivo',
                      'No se pudo determinar el tipo de archivo.' + #13#10 +
                      '¿Es un archivo de USUARIOS?',
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          listaUsuarios.CargarDesdeJSON(openDialog.FileName)
        else
          listaUsuarios.CargarCorreosDesdeJSON(openDialog.FileName);
      end;

      arbolBSTComunidades.SincronizarDesdeLista(listaComunidades);
    end;
  finally
    openDialog.Free;
  end;
end;

procedure TFormMenuRoot.btnAgregarMiembroClick(Sender: TObject);
var
  nombreComunidad, emailUsuario: String;
  resultado: Integer;
begin
  nombreComunidad := InputBox('Agregar Miembro', 'Ingrese el nombre de la comunidad:', '');
  if nombreComunidad = '' then Exit;

  emailUsuario := InputBox('Agregar Miembro', 'Ingrese el email del usuario:', '');
  if emailUsuario = '' then Exit;

  resultado := listaComunidades.AgregarMiembro(nombreComunidad, emailUsuario);

  case resultado of
    0: ShowMessage('Miembro agregado exitosamente');
    1: ShowMessage('Error: No existe la comunidad');
    2: ShowMessage('Error: El usuario no existe en el sistema');
    3: ShowMessage('Error: El usuario ya es miembro de esta comunidad');
  end;
end;

end.

