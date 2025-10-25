unit uarbolavlborradores;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  TCorreo = record
    id: Integer;
    remitente: String;
    destinatario: String;
    asunto: String;
    mensaje: String;
    fecha: String;
    estado: String;
    programado: Boolean;
  end;

  PNodoAVL = ^TNodoAVL;
  TNodoAVL = record
    correo: TCorreo;
    altura: Integer;
    izquierdo: PNodoAVL;
    derecho: PNodoAVL;
  end;

  TArbolAVLBorradores = class
  private
    raiz: PNodoAVL;
    function ObtenerAltura(nodo: PNodoAVL): Integer;
    function ObtenerBalance(nodo: PNodoAVL): Integer;
    function Maximo(a, b: Integer): Integer;
    function RotarDerecha(y: PNodoAVL): PNodoAVL;
    function RotarIzquierda(x: PNodoAVL): PNodoAVL;
    function InsertarNodo(nodo: PNodoAVL; correo: TCorreo): PNodoAVL;
    function BuscarNodo(nodo: PNodoAVL; id: Integer): PNodoAVL;
    function EliminarNodo(nodo: PNodoAVL; id: Integer): PNodoAVL;
    function ObtenerMinimo(nodo: PNodoAVL): PNodoAVL;
    procedure LiberarNodo(var nodo: PNodoAVL);
    procedure RecorridoPreOrden(nodo: PNodoAVL; lista: TList);
    procedure RecorridoInOrden(nodo: PNodoAVL; lista: TList);
    procedure RecorridoPostOrden(nodo: PNodoAVL; lista: TList);
    procedure GenerarDotRecursivo(nodo: PNodoAVL; var archivo: TextFile);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Insertar(correo: TCorreo);
    function Buscar(id: Integer): PNodoAVL;
    function Eliminar(id: Integer): Boolean;
    function ObtenerBorradoresPreOrden: TList;
    function ObtenerBorradoresInOrden: TList;
    function ObtenerBorradoresPostOrden: TList;
    procedure GenerarReporteAVL(nombreArchivo: String);
    function Vacio: Boolean;
  end;

implementation

constructor TArbolAVLBorradores.Create;
begin
  raiz := nil;
end;

destructor TArbolAVLBorradores.Destroy;
begin
  LiberarNodo(raiz);
  inherited Destroy;
end;

procedure TArbolAVLBorradores.LiberarNodo(var nodo: PNodoAVL);
begin
  if nodo = nil then Exit;
  LiberarNodo(nodo^.izquierdo);
  LiberarNodo(nodo^.derecho);
  Dispose(nodo);
  nodo := nil;
end;

function TArbolAVLBorradores.Vacio: Boolean;
begin
  Result := raiz = nil;
end;

function TArbolAVLBorradores.ObtenerAltura(nodo: PNodoAVL): Integer;
begin
  if nodo = nil then
    Result := 0
  else
    Result := nodo^.altura;
end;

function TArbolAVLBorradores.ObtenerBalance(nodo: PNodoAVL): Integer;
begin
  if nodo = nil then
    Result := 0
  else
    Result := ObtenerAltura(nodo^.izquierdo) - ObtenerAltura(nodo^.derecho);
end;

function TArbolAVLBorradores.Maximo(a, b: Integer): Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function TArbolAVLBorradores.RotarDerecha(y: PNodoAVL): PNodoAVL;
var
  x, T2: PNodoAVL;
begin
  x := y^.izquierdo;
  T2 := x^.derecho;

  x^.derecho := y;
  y^.izquierdo := T2;

  y^.altura := Maximo(ObtenerAltura(y^.izquierdo), ObtenerAltura(y^.derecho)) + 1;
  x^.altura := Maximo(ObtenerAltura(x^.izquierdo), ObtenerAltura(x^.derecho)) + 1;

  Result := x;
end;

function TArbolAVLBorradores.RotarIzquierda(x: PNodoAVL): PNodoAVL;
var
  y, T2: PNodoAVL;
begin
  y := x^.derecho;
  T2 := y^.izquierdo;

  y^.izquierdo := x;
  x^.derecho := T2;

  x^.altura := Maximo(ObtenerAltura(x^.izquierdo), ObtenerAltura(x^.derecho)) + 1;
  y^.altura := Maximo(ObtenerAltura(y^.izquierdo), ObtenerAltura(y^.derecho)) + 1;

  Result := y;
end;

function TArbolAVLBorradores.InsertarNodo(nodo: PNodoAVL; correo: TCorreo): PNodoAVL;
var
  balance: Integer;
begin
  if nodo = nil then
  begin
    New(nodo);
    nodo^.correo := correo;
    nodo^.altura := 1;
    nodo^.izquierdo := nil;
    nodo^.derecho := nil;
    Result := nodo;
    Exit;
  end;

  if correo.id < nodo^.correo.id then
    nodo^.izquierdo := InsertarNodo(nodo^.izquierdo, correo)
  else if correo.id > nodo^.correo.id then
    nodo^.derecho := InsertarNodo(nodo^.derecho, correo)
  else
  begin
    Result := nodo;
    Exit;
  end;

  nodo^.altura := 1 + Maximo(ObtenerAltura(nodo^.izquierdo), ObtenerAltura(nodo^.derecho));

  balance := ObtenerBalance(nodo);

  if (balance > 1) and (correo.id < nodo^.izquierdo^.correo.id) then
  begin
    Result := RotarDerecha(nodo);
    Exit;
  end;

  if (balance < -1) and (correo.id > nodo^.derecho^.correo.id) then
  begin
    Result := RotarIzquierda(nodo);
    Exit;
  end;

  if (balance > 1) and (correo.id > nodo^.izquierdo^.correo.id) then
  begin
    nodo^.izquierdo := RotarIzquierda(nodo^.izquierdo);
    Result := RotarDerecha(nodo);
    Exit;
  end;

  if (balance < -1) and (correo.id < nodo^.derecho^.correo.id) then
  begin
    nodo^.derecho := RotarDerecha(nodo^.derecho);
    Result := RotarIzquierda(nodo);
    Exit;
  end;

  Result := nodo;
end;

procedure TArbolAVLBorradores.Insertar(correo: TCorreo);
begin
  raiz := InsertarNodo(raiz, correo);
end;

function TArbolAVLBorradores.BuscarNodo(nodo: PNodoAVL; id: Integer): PNodoAVL;
begin
  if nodo = nil then
  begin
    Result := nil;
    Exit;
  end;

  if id = nodo^.correo.id then
    Result := nodo
  else if id < nodo^.correo.id then
    Result := BuscarNodo(nodo^.izquierdo, id)
  else
    Result := BuscarNodo(nodo^.derecho, id);
end;

function TArbolAVLBorradores.Buscar(id: Integer): PNodoAVL;
begin
  Result := BuscarNodo(raiz, id);
end;

function TArbolAVLBorradores.ObtenerMinimo(nodo: PNodoAVL): PNodoAVL;
begin
  if nodo = nil then
    Result := nil
  else if nodo^.izquierdo = nil then
    Result := nodo
  else
    Result := ObtenerMinimo(nodo^.izquierdo);
end;

function TArbolAVLBorradores.EliminarNodo(nodo: PNodoAVL; id: Integer): PNodoAVL;
var
  temp: PNodoAVL;
  balance: Integer;
begin
  if nodo = nil then
  begin
    Result := nodo;
    Exit;
  end;

  if id < nodo^.correo.id then
    nodo^.izquierdo := EliminarNodo(nodo^.izquierdo, id)
  else if id > nodo^.correo.id then
    nodo^.derecho := EliminarNodo(nodo^.derecho, id)
  else
  begin
    if (nodo^.izquierdo = nil) or (nodo^.derecho = nil) then
    begin
      if nodo^.izquierdo <> nil then
        temp := nodo^.izquierdo
      else
        temp := nodo^.derecho;

      if temp = nil then
      begin
        temp := nodo;
        nodo := nil;
      end
      else
        nodo^ := temp^;

      Dispose(temp);
    end
    else
    begin
      temp := ObtenerMinimo(nodo^.derecho);
      nodo^.correo := temp^.correo;
      nodo^.derecho := EliminarNodo(nodo^.derecho, temp^.correo.id);
    end;
  end;

  if nodo = nil then
  begin
    Result := nodo;
    Exit;
  end;

  nodo^.altura := 1 + Maximo(ObtenerAltura(nodo^.izquierdo), ObtenerAltura(nodo^.derecho));

  balance := ObtenerBalance(nodo);

  if (balance > 1) and (ObtenerBalance(nodo^.izquierdo) >= 0) then
  begin
    Result := RotarDerecha(nodo);
    Exit;
  end;

  if (balance > 1) and (ObtenerBalance(nodo^.izquierdo) < 0) then
  begin
    nodo^.izquierdo := RotarIzquierda(nodo^.izquierdo);
    Result := RotarDerecha(nodo);
    Exit;
  end;

  if (balance < -1) and (ObtenerBalance(nodo^.derecho) <= 0) then
  begin
    Result := RotarIzquierda(nodo);
    Exit;
  end;

  if (balance < -1) and (ObtenerBalance(nodo^.derecho) > 0) then
  begin
    nodo^.derecho := RotarDerecha(nodo^.derecho);
    Result := RotarIzquierda(nodo);
    Exit;
  end;

  Result := nodo;
end;

function TArbolAVLBorradores.Eliminar(id: Integer): Boolean;
var
  nodoAntes: PNodoAVL;
begin
  nodoAntes := raiz;
  raiz := EliminarNodo(raiz, id);
  Result := nodoAntes <> raiz;
end;

procedure TArbolAVLBorradores.RecorridoPreOrden(nodo: PNodoAVL; lista: TList);
begin
  if nodo = nil then Exit;
  lista.Add(nodo);
  RecorridoPreOrden(nodo^.izquierdo, lista);
  RecorridoPreOrden(nodo^.derecho, lista);
end;

procedure TArbolAVLBorradores.RecorridoInOrden(nodo: PNodoAVL; lista: TList);
begin
  if nodo = nil then Exit;
  RecorridoInOrden(nodo^.izquierdo, lista);
  lista.Add(nodo);
  RecorridoInOrden(nodo^.derecho, lista);
end;

procedure TArbolAVLBorradores.RecorridoPostOrden(nodo: PNodoAVL; lista: TList);
begin
  if nodo = nil then Exit;
  RecorridoPostOrden(nodo^.izquierdo, lista);
  RecorridoPostOrden(nodo^.derecho, lista);
  lista.Add(nodo);
end;

function TArbolAVLBorradores.ObtenerBorradoresPreOrden: TList;
begin
  Result := TList.Create;
  RecorridoPreOrden(raiz, Result);
end;

function TArbolAVLBorradores.ObtenerBorradoresInOrden: TList;
begin
  Result := TList.Create;
  RecorridoInOrden(raiz, Result);
end;

function TArbolAVLBorradores.ObtenerBorradoresPostOrden: TList;
begin
  Result := TList.Create;
  RecorridoPostOrden(raiz, Result);
end;

procedure TArbolAVLBorradores.GenerarDotRecursivo(nodo: PNodoAVL; var archivo: TextFile);
var
  balance: Integer;
begin
  if nodo = nil then Exit;

  balance := ObtenerBalance(nodo);

  WriteLn(archivo, '  nodo' + IntToStr(nodo^.correo.id) +
          ' [label="{ID: ' + IntToStr(nodo^.correo.id) +
          '|Asunto: ' + nodo^.correo.asunto +
          '|Balance: ' + IntToStr(balance) + '}"];');

  if nodo^.izquierdo <> nil then
  begin
    WriteLn(archivo, '  nodo' + IntToStr(nodo^.correo.id) +
            ' -> nodo' + IntToStr(nodo^.izquierdo^.correo.id) + ';');
    GenerarDotRecursivo(nodo^.izquierdo, archivo);
  end;

  if nodo^.derecho <> nil then
  begin
    WriteLn(archivo, '  nodo' + IntToStr(nodo^.correo.id) +
            ' -> nodo' + IntToStr(nodo^.derecho^.correo.id) + ';');
    GenerarDotRecursivo(nodo^.derecho, archivo);
  end;
end;

procedure TArbolAVLBorradores.GenerarReporteAVL(nombreArchivo: String);
var
  archivo: TextFile;
  nombrePNG: String;
begin
  AssignFile(archivo, nombreArchivo);
  Rewrite(archivo);

  try
    WriteLn(archivo, 'digraph AVL_Borradores {');
    WriteLn(archivo, '  node [shape=record];');
    WriteLn(archivo, '  rankdir=TB;');
    WriteLn(archivo, '');
    WriteLn(archivo, '  labelloc="t";');
    WriteLn(archivo, '  label="Arbol AVL - Borradores";');
    WriteLn(archivo, '  fontsize=16;');
    WriteLn(archivo, '');

    if raiz <> nil then
      GenerarDotRecursivo(raiz, archivo)
    else
      WriteLn(archivo, '  vacio [label="No hay borradores"];');

    WriteLn(archivo, '}');
  finally
    CloseFile(archivo);
  end;

  nombrePNG := ChangeFileExt(nombreArchivo, '.png');
  if FileExists('/usr/bin/dot') then
    ExecuteProcess('/usr/bin/dot', ['-Tpng', nombreArchivo, '-o', nombrePNG]);
end;

end.
