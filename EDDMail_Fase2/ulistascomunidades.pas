unit ulistascomunidades;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, ustructures;

type
  // ===== Mensaje publicado en comunidad =====
  TMensajeComunidad = record
    Correo: String;
    Texto: String;
    FechaPublicacion: String;
  end;

  PNodoMensajeComunidad = ^TNodoMensajeComunidad;
  TNodoMensajeComunidad = record
    mensaje: TMensajeComunidad;
    siguiente: PNodoMensajeComunidad;
  end;

  // ===== Lista de mensajes (nueva estructura) =====
  TListaMensajesComunidad = class
  private
    cabeza: PNodoMensajeComunidad;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AgregarMensaje(correo, texto, fecha: String);
    function ObtenerPrimero: PNodoMensajeComunidad;
    function Vacia: Boolean;
    function ContarMensajes: Integer;
  end;

// Funciones globales que trabajan con tu TListaComunidades existente
procedure AgregarMensajeAComunidad(nombreComunidad, correo, texto: String);
function ObtenerComunidadesDeUsuario(emailUsuario: String): TStringList;

implementation

// ===== Implementación de TListaMensajesComunidad =====

constructor TListaMensajesComunidad.Create;
begin
  cabeza := nil;
end;

destructor TListaMensajesComunidad.Destroy;
var
  actual, temp: PNodoMensajeComunidad;
begin
  actual := cabeza;
  while actual <> nil do
  begin
    temp := actual;
    actual := actual^.siguiente;
    Dispose(temp);
  end;
  inherited Destroy;
end;

procedure TListaMensajesComunidad.AgregarMensaje(correo, texto, fecha: String);
var
  nuevo: PNodoMensajeComunidad;
begin
  New(nuevo);
  nuevo^.mensaje.Correo := correo;
  nuevo^.mensaje.Texto := texto;
  nuevo^.mensaje.FechaPublicacion := fecha;
  nuevo^.siguiente := cabeza;
  cabeza := nuevo;
end;

function TListaMensajesComunidad.ObtenerPrimero: PNodoMensajeComunidad;
begin
  Result := cabeza;
end;

function TListaMensajesComunidad.Vacia: Boolean;
begin
  Result := cabeza = nil;
end;

function TListaMensajesComunidad.ContarMensajes: Integer;
var
  actual: PNodoMensajeComunidad;
  contador: Integer;
begin
  contador := 0;
  actual := cabeza;
  while actual <> nil do
  begin
    Inc(contador);
    actual := actual^.siguiente;
  end;
  Result := contador;
end;

// ===== Funciones que trabajan con tu código existente =====

procedure AgregarMensajeAComunidad(nombreComunidad, correo, texto: String);
var
  comunidad: PNodoComunidad;
  fecha: String;
begin
  // Buscar comunidad en tu lista existente
  comunidad := listaComunidades.BuscarComunidad(nombreComunidad);

  if comunidad = nil then
  begin
    ShowMessage('❌ La comunidad "' + nombreComunidad + '" no existe.');
    Exit;
  end;

  // Generar fecha actual
  fecha := FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);

  // TODO: Aquí conectaremos con la lista de mensajes
  // Por ahora solo mostramos confirmación
  ShowMessage('✅ Mensaje agregado a "' + nombreComunidad + '"');
end;

function ObtenerComunidadesDeUsuario(emailUsuario: String): TStringList;
var
  comunidad: PNodoComunidad;
  miembro: PNodoMiembro;
  encontrado: Boolean;
begin
  Result := TStringList.Create;

  // Recorrer todas las comunidades
  comunidad := listaComunidades.BuscarComunidad(''); // Obtener primera

  // TODO: Implementar recorrido completo de comunidades
  // Por ahora retornamos lista vacía
end;

end.
