unit upapelera;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ustructures;

type

  { TFormPapelera }

  TFormPapelera = class(TForm)
    btnBuscar: TButton;
    btnCerrar: TButton;
    btnEliminar: TButton;
    lblTitulo: TLabel;
    lstCorreosEliminados: TListBox;
    procedure btnBuscarClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
  private

  public
     procedure CargarCorreosEliminados;
  end;

var
  FormPapelera: TFormPapelera;

implementation

{$R *.lfm}

procedure TFormPapelera.btnBuscarClick(Sender: TObject);
var
  palabraClave: String;
  actual: PNodoPila;
  item: String;
  encontrado: Boolean;
begin
  palabraClave := InputBox('Buscar Correo', 'Ingrese palabra clave del asunto:', '');

  if palabraClave = '' then
    Exit;

  lstCorreosEliminados.Clear;
  encontrado := False;

  if (usuarioActual <> nil) and not usuarioActual^.usuario.papelera^.Vacia then
  begin
    actual := usuarioActual^.usuario.papelera^.ObtenerTope;
    while actual <> nil do
    begin
      if Pos(LowerCase(palabraClave), LowerCase(actual^.correo.asunto)) > 0 then
      begin
        item := actual^.correo.estado + ' | ' +
                actual^.correo.asunto + ' | ' +
                actual^.correo.remitente;
        lstCorreosEliminados.Items.Add(item);
        encontrado := True;
      end;
      actual := actual^.siguiente;
    end;
  end;

  if not encontrado then
    lstCorreosEliminados.Items.Add('No se encontraron correos con esa palabra clave');
end;

procedure TFormPapelera.btnCerrarClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormPapelera.btnEliminarClick(Sender: TObject);
var
  indice: Integer;
  correosTemp: array of TCorreo;
  i, count: Integer;
begin
  indice := lstCorreosEliminados.ItemIndex;
  if indice < 0 then
  begin
    ShowMessage('Seleccione un correo para eliminar permanentemente');
    Exit;
  end;

  if MessageDlg('¿Está seguro de eliminar permanentemente este correo?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Desapilar todos los correos hasta llegar al seleccionado
    count := 0;
    SetLength(correosTemp, 100); // Tamaño máximo temporal

    // Desapilar correos anteriores y guardarlos temporalmente
    for i := 0 to indice - 1 do
    begin
      if not usuarioActual^.usuario.papelera^.Vacia then
      begin
        correosTemp[count] := usuarioActual^.usuario.papelera^.Desapilar;
        Inc(count);
      end;
    end;

    // Eliminar el correo seleccionado (desapilarlo sin guardarlo)
    if not usuarioActual^.usuario.papelera^.Vacia then
      usuarioActual^.usuario.papelera^.Desapilar;

    // Reinsertir los correos temporales
    for i := count - 1 downto 0 do
      usuarioActual^.usuario.papelera^.Apilar(correosTemp[i]);

    CargarCorreosEliminados;
    ShowMessage('Correo eliminado permanentemente');
  end;
end;

procedure TFormPapelera.CargarCorreosEliminados;
var
  actual: PNodoPila;
  item: String;
begin
  lstCorreosEliminados.Clear;

  if (usuarioActual <> nil) and not usuarioActual^.usuario.papelera^.Vacia then
  begin
    actual := usuarioActual^.usuario.papelera^.ObtenerTope;
    while actual <> nil do
    begin
      item := actual^.correo.estado + ' | ' +
              actual^.correo.asunto + ' | ' +
              actual^.correo.remitente;
      lstCorreosEliminados.Items.Add(item);
      actual := actual^.siguiente;
    end;
  end;

  if lstCorreosEliminados.Items.Count = 0 then
    lstCorreosEliminados.Items.Add('No hay correos en la papelera');
end;

end.

