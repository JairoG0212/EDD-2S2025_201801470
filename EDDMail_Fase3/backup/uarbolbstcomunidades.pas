unit uarbolbstcomunidades;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ustructures, Process;

type
  PNodoBST = ^TNodoBST;
  TNodoBST = record
    clave: String;
    refComunidad: PNodoComunidad;
    izquierdo: PNodoBST;
    derecho: PNodoBST;
  end;

  TArbolBSTComunidades = class
  private
    raiz: PNodoBST;
    procedure InsertarNodoRecursivo(var nodo: PNodoBST; comunidad: PNodoComunidad);
    function BuscarNodoRecursivo(nodo: PNodoBST; nombre: String): PNodoComunidad;
    procedure RecolectarInOrden(nodo: PNodoBST; lista: TList);
    procedure LiberarNodo(var nodo: PNodoBST);
    procedure GenerarDotRecursivo(nodo: PNodoBST; var archivo: TextFile);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Insertar(comunidad: PNodoComunidad);
    function Buscar(nombre: String): PNodoComunidad;
    function ObtenerComunidadesOrdenadas: TStringList;
    procedure SincronizarDesdeLista(listaComunidades: TListaComunidades);
    procedure GenerarReporteBST(nombreArchivo: String);
  end;

var
  arbolBSTComunidades: TArbolBSTComunidades;

implementation

constructor TArbolBSTComunidades.Create;
begin
  raiz := nil;
end;

destructor TArbolBSTComunidades.Destroy;
begin
  LiberarNodo(raiz);
  inherited Destroy;
end;

procedure TArbolBSTComunidades.LiberarNodo(var nodo: PNodoBST);
begin
  if nodo = nil then Exit;

  LiberarNodo(nodo^.izquierdo);
  LiberarNodo(nodo^.derecho);
  Dispose(nodo);
  nodo := nil;
end;

procedure TArbolBSTComunidades.InsertarNodoRecursivo(var nodo: PNodoBST; comunidad: PNodoComunidad);
var
  comparacion: Integer;
begin
  if comunidad = nil then Exit;

  if nodo = nil then
  begin
    New(nodo);
    nodo^.clave := comunidad^.nombre;
    nodo^.refComunidad := comunidad;
    nodo^.izquierdo := nil;
    nodo^.derecho := nil;
    Exit;
  end;

  comparacion := CompareText(comunidad^.nombre, nodo^.clave);

  if comparacion < 0 then
    InsertarNodoRecursivo(nodo^.izquierdo, comunidad)
  else if comparacion > 0 then
    InsertarNodoRecursivo(nodo^.derecho, comunidad)
  else
    nodo^.refComunidad := comunidad;
end;

procedure TArbolBSTComunidades.Insertar(comunidad: PNodoComunidad);
begin
  InsertarNodoRecursivo(raiz, comunidad);
end;

function TArbolBSTComunidades.BuscarNodoRecursivo(nodo: PNodoBST; nombre: String): PNodoComunidad;
var
  comparacion: Integer;
begin
  if nodo = nil then
  begin
    Result := nil;
    Exit;
  end;

  comparacion := CompareText(nombre, nodo^.clave);

  if comparacion = 0 then
    Result := nodo^.refComunidad
  else if comparacion < 0 then
    Result := BuscarNodoRecursivo(nodo^.izquierdo, nombre)
  else
    Result := BuscarNodoRecursivo(nodo^.derecho, nombre);
end;

function TArbolBSTComunidades.Buscar(nombre: String): PNodoComunidad;
begin
  Result := BuscarNodoRecursivo(raiz, nombre);
end;

procedure TArbolBSTComunidades.RecolectarInOrden(nodo: PNodoBST; lista: TList);
begin
  if nodo = nil then Exit;

  RecolectarInOrden(nodo^.izquierdo, lista);
  lista.Add(nodo^.refComunidad);
  RecolectarInOrden(nodo^.derecho, lista);
end;

function TArbolBSTComunidades.ObtenerComunidadesOrdenadas: TStringList;
var
  referencias: TList;
  i: Integer;
  comunidad: PNodoComunidad;
  linea: String;
begin
  Result := TStringList.Create;
  referencias := TList.Create;
  try
    RecolectarInOrden(raiz, referencias);

    for i := 0 to referencias.Count - 1 do
    begin
      comunidad := PNodoComunidad(referencias[i]);
      if comunidad <> nil then
      begin
        linea := comunidad^.nombre +
                 ' | Fecha: ' + comunidad^.fechaCreacion +
                 ' | Mensajes: ' + IntToStr(comunidad^.numeroMensajes);
        Result.Add(linea);
      end;
    end;
  finally
    referencias.Free;
  end;
end;

procedure TArbolBSTComunidades.SincronizarDesdeLista(listaComunidades: TListaComunidades);
var
  actual: PNodoComunidad;
begin
  if listaComunidades = nil then Exit;

  LiberarNodo(raiz);
  raiz := nil;

  actual := listaComunidades.BuscarComunidad('');

  while actual <> nil do
  begin
    Insertar(actual);
    actual := actual^.siguiente;
  end;
end;

procedure TArbolBSTComunidades.GenerarDotRecursivo(nodo: PNodoBST; var archivo: TextFile);
var
  mensajeActual: PNodoMensajeComunidad;
  miembroActual: PNodoMiembro;
  contadorMiembros: Integer;
begin
  if nodo = nil then Exit;

  WriteLn(archivo, '  "' + nodo^.clave + '" [shape=record, label="{' +
          nodo^.clave + '|Fecha: ' + nodo^.refComunidad^.fechaCreacion +
          '|Mensajes: ' + IntToStr(nodo^.refComunidad^.numeroMensajes) + '}"];');

  if nodo^.izquierdo <> nil then
  begin
    WriteLn(archivo, '  "' + nodo^.clave + '" -> "' + nodo^.izquierdo^.clave + '";');
    GenerarDotRecursivo(nodo^.izquierdo, archivo);
  end;

  if nodo^.derecho <> nil then
  begin
    WriteLn(archivo, '  "' + nodo^.clave + '" -> "' + nodo^.derecho^.clave + '";');
    GenerarDotRecursivo(nodo^.derecho, archivo);
  end;
end;

procedure TArbolBSTComunidades.GenerarReporteBST(nombreArchivo: String);
var
  archivo: TextFile;
  nombrePNG: String;
begin
  AssignFile(archivo, nombreArchivo);
  Rewrite(archivo);

  try
    WriteLn(archivo, 'digraph BST_Comunidades {');
    WriteLn(archivo, '  node [shape=record];');
    WriteLn(archivo, '  rankdir=TB;');
    WriteLn(archivo, '');
    WriteLn(archivo, '  labelloc="t";');
    WriteLn(archivo, '  label="Arbol BST - Comunidades";');
    WriteLn(archivo, '  fontsize=16;');
    WriteLn(archivo, '');

    if raiz <> nil then
      GenerarDotRecursivo(raiz, archivo)
    else
      WriteLn(archivo, '  vacio [label="No hay comunidades"];');

    WriteLn(archivo, '}');
  finally
    CloseFile(archivo);
  end;

  nombrePNG := ChangeFileExt(nombreArchivo, '.png');
  if FileExists('/usr/bin/dot') then
    ExecuteProcess('/usr/bin/dot', ['-Tpng', nombreArchivo, '-o', nombrePNG]);
end;

initialization
  arbolBSTComunidades := TArbolBSTComunidades.Create;

finalization
  if Assigned(arbolBSTComunidades) then
    arbolBSTComunidades.Free;

end.
