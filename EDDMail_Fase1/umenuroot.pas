unit umenuroot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ustructures;

type

  { TFormMenuRoot }

  TFormMenuRoot = class(TForm)
    btnCargaMasiva: TButton;
    btnReporteDeUsuarios: TButton;
    btnReporteDeRelaciones: TButton;
    btnCerrarSesionRoot: TButton;
    lblTitulo: TLabel;
    procedure btnCargaMasivaClick(Sender: TObject);
    procedure btnCerrarSesionRootClick(Sender: TObject);
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
begin
  openDialog := TOpenDialog.Create(Self);
  try
    openDialog.Title := 'Seleccionar archivo JSON';
    openDialog.Filter := 'Archivos JSON|*.json|Todos los archivos|*.*';
    openDialog.DefaultExt := 'json';

    if openDialog.Execute then
    begin
      try
        listaUsuarios.CargarDesdeJSON(openDialog.FileName);
        ShowMessage('Usuarios cargados exitosamente desde: ' + ExtractFileName(openDialog.FileName));
      except
        on E: Exception do
          ShowMessage('Error al cargar archivo: ' + E.Message);
      end;
    end;
  finally
    openDialog.Free;
  end;
end;

end.

