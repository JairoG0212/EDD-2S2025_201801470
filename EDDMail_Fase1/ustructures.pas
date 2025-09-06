unit ustructures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
  // Registro para Usuario
  TUsuario = record
    id: Integer;
    nombre: String;
    usuario: String;
    email: String;
    telefono: String;
    password: String;
    bandejaEntrada: ^TListaCorreos; //puntero para mi lista doble enlazada
    contactos: ^TListaContactos; //puntero para los contactos de la lista
    papelera: ^TPapelera;// puntero para la papelera de cada usuario
    colaCorreos : ^TColaCorreos; // puntero para cola de correos programados
  end;

  // Registro para Correo
  TCorreo = record
    id: Integer;
    remitente: String;
    destinatario: String;
    asunto: String;
    mensaje: String;
    fecha: String;
    estado: String; // 'L' = Leído, 'NL' = No Leído
    programado: Boolean;
  end;

  // Nodo para lista simple de usuarios
  PNodoUsuario = ^TNodoUsuario;
  TNodoUsuario = record
    usuario: TUsuario;
    siguiente: PNodoUsuario;
  end;

  // Nodo para matriz dispersa (relaciones)
  PNodoMatriz = ^TNodoMatriz;
  TNodoMatriz = record
    fila: Integer;        // índice del remitente
    columna: Integer;     // índice del destinatario
    cantidad: Integer;    // cantidad de correos enviados
    siguiente: PNodoMatriz;
    abajo: PNodoMatriz;
  end;

  //Nodo para la lista doblemente enlazada bandeja de entrada
  PNodoCorreo = ^TNodoCorreo;
  TNodoCorreo = record
    correo: TCorreo;
    anterior: PNodoCorreo;
    siguiente: PNodoCorreo;
  end;

  //Nodo para la lista circular de contactos
  PNodoContacto = ^TNodoContacto;
  TNodoContacto = record
    email: String;
    siguiente: PNodoContacto;
  end;

  // Nodo para cola (correos programados)
  PNodoCola = ^TNodoCola;
  TNodoCola = record
    correo: TCorreo;
    siguiente: PNodoCola;
  end;

  // Nodo para pila (papelera)
  PNodoPila = ^TNodoPila;
  TNodoPila = record
    correo: TCorreo;
    siguiente: PNodoPila;
  end;

  // Nodo para miembros de comunidad
  PNodoMiembro = ^TNodoMiembro;
  TNodoMiembro = record
    emailUsuario: String;
    siguiente: PNodoMiembro;
  end;

  // Nodo para comunidades
  PNodoComunidad = ^TNodoComunidad;
  TNodoComunidad = record
    nombre: String;
    miembros: PNodoMiembro;
    siguiente: PNodoComunidad;
  end;

  // Pila para papelera
  TPapelera = class
  private
    tope: PNodoPila;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Apilar(correo: TCorreo);
    function Desapilar: TCorreo;
    function VerTope: TCorreo;
    function Vacia: Boolean;
    function ObtenerTope: PNodoPila;
  end;

  // Lista simple de usuarios
  TListaUsuarios = class
  private
    cabeza: PNodoUsuario;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CargarDesdeJSON(nombreArchivo: String);
    procedure Insertar(nuevoUsuario: TUsuario);
    function Buscar(email: String): PNodoUsuario;
    function Vacia: Boolean;
    procedure GenerarReporteUsuarios(nombreArchivo: String);
    function ActualizarUsuario(emailActual: String; nuevoUsuario: String): Integer;
    function ActualizarTelefono(emailActual: String; nuevoTelefono: String): Integer;
    function ExisteUsuario(nombreUsuario: String): Boolean;
  end;

  // Matriz dispersa para relaciones
  TMatrizDispersa = class
  private
    cabeza: PNodoMatriz;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncrementarRelacion(remitenteEmail, destinatarioEmail: String);
    procedure GenerarReporteRelaciones(nombreArchivo: String);
  end;

  //Lista doblemente enlazada para bandeja de entrada
  TListaCorreos = class
    private
      cabeza: PNodoCorreo;
      cola: PNodoCorreo;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AgregarCorreo(nuevoCorreo: TCorreo);
      function Vacia: Boolean;
      function ObtenerPrimero: PNodoCorreo;
      function EliminarCorreo(indice: Integer): TCorreo;
  end;

  //Lista circular para contactos
  TListaContactos = class
  private
    ultimo: PNodoContacto; // Apunta al último nodo, el siguiente es el primero
  public
    constructor Create;
    destructor Destroy; override;
    procedure AgregarContacto(email: String);
    function BuscarContacto(email: String): Boolean;
    function ObtenerPrimero: PnodoContacto;
    function Vacia: Boolean;
  end;

  // Clase para manejar comunidades
  TListaComunidades = class
  private
    cabeza: PNodoComunidad;
  public
    constructor Create;
    destructor Destroy; override;
    function CrearComunidad(nombre: String): Boolean;
    function AgregarMiembro(nombreComunidad, email: String): Integer;
    function BuscarComunidad(nombre: String): PNodoComunidad;
    function Vacia: Boolean;
    procedure GenerarReporteComunidades(nombreArchivo: String);
  end;

  // Cola para correos programados
  TColaCorreos = class
  private
    frente: PNodoCola;
    final: PNodoCola;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Encolar(correo: TCorreo);
    function Desencolar: TCorreo;
    function Vacia: Boolean;
    function ObtenerFrente: PNodoCola;
  end;

var
  listaUsuarios: TListaUsuarios;
  matrizRelaciones: TMatrizDispersa;
  usuarioActual: PNodoUsuario;
  listaComunidades: TListaComunidades;

implementation

// Implementación de TListaUsuarios
constructor TListaUsuarios.Create;
begin
  cabeza := nil;
end;

destructor TListaUsuarios.Destroy;
var
  actual, temp: PNodoUsuario;
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

procedure TListaUsuarios.Insertar(nuevoUsuario: TUsuario);
var
  nuevoNodo: PNodoUsuario;
begin
  New(nuevoNodo);
  nuevoNodo^.usuario := nuevoUsuario;
  New(nuevoNodo^.usuario.bandejaEntrada);
  nuevoNodo^.usuario.bandejaEntrada^ := TListaCorreos.Create;
  New(nuevoNodo^.usuario.contactos);
  nuevoNodo^.usuario.contactos^ := TListaContactos.Create;
  New(nuevoNodo^.usuario.papelera);
  nuevoNodo^.usuario.papelera^ := TPapelera.Create;
  New(nuevoNodo^.usuario.colaCorreos);
  nuevoNodo^.usuario.colaCorreos^ := TColaCorreos.Create;
  nuevoNodo^.siguiente := cabeza;
  cabeza := nuevoNodo;
end;

function TListaUsuarios.Buscar(email: String): PNodoUsuario;
var
  actual: PNodoUsuario;
begin
  actual := cabeza;
  while actual <> nil do
  begin
    if actual^.usuario.email = email then
    begin
      Result := actual;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
  Result := nil;
end;

function TListaUsuarios.Vacia: Boolean;
begin
  Result := cabeza = nil;
end;

function TListaCorreos.ObtenerPrimero: PNodoCorreo;
begin
  Result := cabeza;
end;

procedure TListaUsuarios.CargarDesdeJSON(nombreArchivo: String);
  var
    jsonString: String;
    jsonData: TJSONData;
    jsonArray: TJSONArray;
    jsonObject: TJSONObject;
    usuario: TUsuario;
    i: Integer;
    fileStream: TFileStream;
    stringStream: TStringStream;
  begin
    try
      // Leer archivo JSON
      fileStream := TFileStream.Create(nombreArchivo, fmOpenRead);
      stringStream := TStringStream.Create('');
      try
        stringStream.CopyFrom(fileStream, fileStream.Size);
        jsonString := stringStream.DataString;
      finally
        fileStream.Free;
        stringStream.Free;
      end;

      // Parsear JSON
      jsonData := GetJSON(jsonString);
      try
        if jsonData.JSONType = jtObject then
        begin
          jsonObject := TJSONObject(jsonData);
          jsonArray := TJSONArray(jsonObject.Arrays['usuarios']);

          // Procesar cada usuario
          for i := 0 to jsonArray.Count - 1 do
          begin
            jsonObject := TJSONObject(jsonArray[i]);

            usuario.id := jsonObject.Integers['id'];
            usuario.nombre := jsonObject.Strings['nombre'];
            usuario.usuario := jsonObject.Strings['usuario'];
            usuario.email := jsonObject.Strings['email'];
            usuario.telefono := jsonObject.Strings['telefono'];
            usuario.password := jsonObject.Strings['password'];
            usuario.bandejaEntrada := nil; //iniciar bandeja como nil
            usuario.contactos := nil; //iniciar contactos como nil
            usuario.papelera := nil; //iniciar papelera como nil
            usuario.colaCorreos := nil; //iniciar cola correos como nil
            Insertar(usuario);
          end;
        end;
      finally
        jsonData.Free;
      end;

    except
      on E: Exception do
        raise Exception.Create('Error al cargar archivo JSON: ' + E.Message);
    end;
end;

procedure TListaUsuarios.GenerarReporteUsuarios(nombreArchivo: String);
var
  archivo: TextFile;
  actual: PNodoUsuario;
  contador, totalNodos: Integer;
  nodos: array of PNodoUsuario;
  i: Integer;
begin
  AssignFile(archivo, nombreArchivo);
  Rewrite(archivo);

  try
    // Encabezado del archivo DOT para Graphviz
    WriteLn(archivo, 'digraph ListaUsuarios {');
    WriteLn(archivo, '  rankdir=LR;');
    WriteLn(archivo, '  node [shape=record];');
    WriteLn(archivo, '');

    // Usar label del grafo como título centrado arriba
    WriteLn(archivo, '  labelloc="t";'); // "t" = top
    WriteLn(archivo, '  label="Lista Enlazada";');
    WriteLn(archivo, '  fontsize=20;');
    WriteLn(archivo, '  fontname="Arial Bold";');
    WriteLn(archivo, '');

    // Contar nodos y almacenarlos en orden correcto
    actual := cabeza;
    totalNodos := 0;
    while actual <> nil do
    begin
      Inc(totalNodos);
      actual := actual^.siguiente;
    end;

    // Crear array para almacenar nodos en orden correcto
    SetLength(nodos, totalNodos);
    actual := cabeza;
    contador := totalNodos - 1;

    // Llenar array en orden inverso (para mostrar del ID menor al mayor)
    while actual <> nil do
    begin
      nodos[contador] := actual;
      actual := actual^.siguiente;
      Dec(contador);
    end;

    // Generar nodos en orden correcto
    for i := 0 to totalNodos - 1 do
    begin
      WriteLn(archivo, '  nodo' + IntToStr(i) + ' [label="ID: ' +
              IntToStr(nodos[i]^.usuario.id) + '\n' +
              'Nombre: ' + nodos[i]^.usuario.nombre + '\n' +
              'Usuario: ' + nodos[i]^.usuario.usuario + '\n' +
              'Password: ' +nodos[i]^.usuario.password + '\n' +
              'Email: ' + nodos[i]^.usuario.email + '\n' +
              'Teléfono: ' + nodos[i]^.usuario.telefono + '"];');
    end;

    WriteLn(archivo, '');

    // Generar conexiones entre nodos
    for i := 0 to totalNodos - 2 do
    begin
      WriteLn(archivo, '  nodo' + IntToStr(i) + ' -> nodo' + IntToStr(i + 1) + ';');
    end;

    WriteLn(archivo, '}');

  finally
    CloseFile(archivo);
  end;
end;

function TListaUsuarios.ExisteUsuario(nombreUsuario: String): Boolean;
var
  actual: PNodoUsuario;
begin
  Result := False;
  actual := cabeza;
  while actual <> nil do
  begin
    if actual^.usuario.usuario = nombreUsuario then
    begin
      Result := True;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
end;

function TListaUsuarios.ActualizarUsuario(emailActual: String; nuevoUsuario: String): Integer;
var
  nodoUsuario: PNodoUsuario;
begin
  // 0 = éxito, 1 = campo vacío, 2 = usuario no existe, 3 = nombre usuario ya existe, 4 = mismo valor

  if Trim(nuevoUsuario) = '' then
  begin
    Result := 1;
    Exit;
  end;

  nodoUsuario := Buscar(emailActual);
  if nodoUsuario = nil then
  begin
    Result := 2;
    Exit;
  end;

  if nodoUsuario^.usuario.usuario = nuevoUsuario then
  begin
    Result := 4;
    Exit;
  end;

  if ExisteUsuario(nuevoUsuario) then
  begin
    Result := 3;
    Exit;
  end;

  nodoUsuario^.usuario.usuario := nuevoUsuario;
  Result := 0;
end;

function TListaUsuarios.ActualizarTelefono(emailActual: String; nuevoTelefono: String): Integer;
var
  nodoUsuario, actual: PNodoUsuario;
begin
  // 0 = éxito, 1 = campo vacío, 2 = usuario no existe, 3 = teléfono ya existe, 4 = mismo valor

  if Trim(nuevoTelefono) = '' then
  begin
    Result := 1;
    Exit;
  end;

  nodoUsuario := Buscar(emailActual);
  if nodoUsuario = nil then
  begin
    Result := 2;
    Exit;
  end;

  if nodoUsuario^.usuario.telefono = nuevoTelefono then
  begin
    Result := 4;
    Exit;
  end;

  // Verificar si el teléfono ya existe
  actual := cabeza;
  while actual <> nil do
  begin
    if actual^.usuario.telefono = nuevoTelefono then
    begin
      Result := 3;
      Exit;
    end;
    actual := actual^.siguiente;
  end;

  nodoUsuario^.usuario.telefono := nuevoTelefono;
  Result := 0;
end;

// Implementación de TMatrizDispersa
constructor TMatrizDispersa.Create;
begin
  cabeza := nil;
end;

destructor TMatrizDispersa.Destroy;
var
  actual, temp: PNodoMatriz;
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

procedure TMatrizDispersa.IncrementarRelacion(remitenteEmail, destinatarioEmail: String);
var
  nodoRemitente, nodoDestinatario: PNodoUsuario;
  filaIndice, columnaIndice: Integer;
  actual, nuevo: PNodoMatriz;
begin
  // Buscar índices de remitente y destinatario
  nodoRemitente := listaUsuarios.Buscar(remitenteEmail);
  nodoDestinatario := listaUsuarios.Buscar(destinatarioEmail);

  if (nodoRemitente = nil) or (nodoDestinatario = nil) then
    Exit;

  filaIndice := nodoRemitente^.usuario.id;
  columnaIndice := nodoDestinatario^.usuario.id;

  // Buscar si ya existe la relación
  actual := cabeza;
  while actual <> nil do
  begin
    if (actual^.fila = filaIndice) and (actual^.columna = columnaIndice) then
    begin
      Inc(actual^.cantidad);
      Exit;
    end;
    actual := actual^.siguiente;
  end;

  // Si no existe, crear nuevo nodo
  New(nuevo);
  nuevo^.fila := filaIndice;
  nuevo^.columna := columnaIndice;
  nuevo^.cantidad := 1;
  nuevo^.siguiente := cabeza;
  nuevo^.abajo := nil;
  cabeza := nuevo;
end;

procedure TMatrizDispersa.GenerarReporteRelaciones(nombreArchivo: String);
var
  archivo: TextFile;
  actual: PNodoMatriz;
  usuarioActualPtr: PNodoUsuario;
  emailRemitente, emailDestinatario: String;
begin
  AssignFile(archivo, nombreArchivo);
  Rewrite(archivo);

  try
    WriteLn(archivo, 'digraph MatrizDispersa {');
    WriteLn(archivo, '  rankdir=LR;');
    WriteLn(archivo, '  node [shape=box];');
    WriteLn(archivo, '');
    WriteLn(archivo, '  labelloc="t";');
    WriteLn(archivo, '  label="Matriz Dispersa - Reporte de Relaciones";');
    WriteLn(archivo, '  fontsize=16;');
    WriteLn(archivo, '');

    // Recorrer todas las relaciones
    actual := cabeza;
    while actual <> nil do
    begin
      // Buscar emails por ID
      usuarioActualPtr := listaUsuarios.cabeza;
      emailRemitente := '';
      emailDestinatario := '';

      while usuarioActualPtr <> nil do
      begin
        if usuarioActualPtr^.usuario.id = actual^.fila then
          emailRemitente := usuarioActualPtr^.usuario.email;
        if usuarioActualPtr^.usuario.id = actual^.columna then
          emailDestinatario := usuarioActualPtr^.usuario.email;
        usuarioActualPtr := usuarioActualPtr^.siguiente;
      end;

      if (emailRemitente <> '') and (emailDestinatario <> '') then
        WriteLn(archivo, '  "' + emailRemitente + '" -> "' + emailDestinatario + '" [label="' + IntToStr(actual^.cantidad) + '"];');

      actual := actual^.siguiente;
    end;

    WriteLn(archivo, '}');
  finally
    CloseFile(archivo);
  end;
end;

//Implementacion de TListaCorreos
constructor TListaCorreos.Create;
begin
  cabeza := nil;
  cola := nil
end;

destructor TListaCorreos.Destroy;
var
  actual, temp: PNodoCorreo;
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

procedure TListaCorreos.AgregarCorreo(nuevoCorreo: TCorreo);
var
  nuevoNodo: PNodoCorreo;
begin
  New(nuevoNodo);
  nuevoNodo^.correo := nuevoCorreo;
  nuevoNodo^.anterior := nil;
  nuevoNodo^.siguiente := nil;

  if cabeza = nil then
  begin
    cabeza := nuevoNodo;
    cola := nuevoNodo;
  end
  else
  begin
    cola^.siguiente := nuevoNodo;
    nuevoNodo^.anterior := cola;
    cola := nuevoNodo;
  end;
end;

function TListaCorreos.Vacia: Boolean;
begin
  Result := cabeza = nil;
end;

function TListaCorreos.EliminarCorreo(indice: Integer): TCorreo;
var
  actual: PNodoCorreo;
  anterior: PNodoCorreo;
  contador: Integer;
  correoEliminado: TCorreo;
begin
  if (cabeza = nil) or (indice < 0) then
    Exit;

  contador := 0;
  actual := cabeza;
  anterior := nil;

  // Encontrar el correo en la posición indicada
  while (actual <> nil) and (contador < indice) do
  begin
    anterior := actual;
    actual := actual^.siguiente;
    Inc(contador);
  end;

  if actual = nil then
    Exit;

  correoEliminado := actual^.correo;

  // Eliminar el nodo de la lista
  if anterior = nil then
  begin
    // Es el primer nodo
    cabeza := actual^.siguiente;
    if cabeza <> nil then
      cabeza^.anterior := nil;
  end
  else
  begin
    anterior^.siguiente := actual^.siguiente;
    if actual^.siguiente <> nil then
      actual^.siguiente^.anterior := anterior;
  end;

  if actual = cola then
    cola := anterior;

  Dispose(actual);
  Result := correoEliminado;
end;


//Implementacion de TListaContactos
constructor TListaContactos.Create;
begin
  ultimo := nil;
end;

destructor TListaContactos.Destroy;
var
  actual, temp: PNodoContacto;
begin
  if ultimo <> nil then
  begin
    actual := ultimo^.siguiente; // Empezar desde el primero
    repeat
      temp := actual;
      actual := actual^.siguiente;
      Dispose(temp);
    until actual = ultimo^.siguiente;
    Dispose(ultimo);
  end;
  inherited Destroy;
end;

procedure TListaContactos.AgregarContacto(email: String);
var
  nuevoNodo: PNodoContacto;
begin
  New(nuevoNodo);
  nuevoNodo^.email := email;

  if ultimo = nil then
  begin
    // Primera inserción
    ultimo := nuevoNodo;
    ultimo^.siguiente := ultimo; // Apunta a sí mismo
  end
  else
  begin
    // Insertar después del último
    nuevoNodo^.siguiente := ultimo^.siguiente;
    ultimo^.siguiente := nuevoNodo;
    ultimo := nuevoNodo;
  end;
end;

function TListaContactos.BuscarContacto(email: String): Boolean;
var
  actual: PNodoContacto;
begin
  Result := False;
  if ultimo = nil then Exit;

  actual := ultimo^.siguiente; // Empezar desde el primero
  repeat
    if actual^.email = email then
    begin
      Result := True;
      Exit;
    end;
    actual := actual^.siguiente;
  until actual = ultimo^.siguiente;
end;

function TListaContactos.ObtenerPrimero: PNodoContacto;
begin
  if ultimo = nil then
    Result := nil
  else
    Result := ultimo^.siguiente; // El siguiente del último es el primero
end;

function TListaContactos.Vacia: Boolean;
begin
  Result := ultimo = nil;
end;

// Implementación de TPapelera
constructor TPapelera.Create;
begin
  tope := nil;
end;

destructor TPapelera.Destroy;
var
  actual: PNodoPila;
begin
  while tope <> nil do
  begin
    actual := tope;
    tope := tope^.siguiente;
    Dispose(actual);
  end;
  inherited Destroy;
end;

procedure TPapelera.Apilar(correo: TCorreo);
var
  nuevoNodo: PNodoPila;
begin
  New(nuevoNodo);
  nuevoNodo^.correo := correo;
  nuevoNodo^.siguiente := tope;
  tope := nuevoNodo;
end;

function TPapelera.Desapilar: TCorreo;
var
  nodoAEliminar: PNodoPila;
begin
  if tope <> nil then
  begin
    Result := tope^.correo;
    nodoAEliminar := tope;
    tope := tope^.siguiente;
    Dispose(nodoAEliminar);
  end;
end;

function TPapelera.VerTope: TCorreo;
begin
  if tope <> nil then
    Result := tope^.correo;
end;

function TPapelera.Vacia: Boolean;
begin
  Result := tope = nil;
end;

function TPapelera.ObtenerTope: PNodoPila;
begin
  Result := tope;
end;

// Implementación de TColaCorreos
constructor TColaCorreos.Create;
begin
  frente := nil;
  final := nil;
end;

destructor TColaCorreos.Destroy;
var
  actual: PNodoCola;
begin
  while frente <> nil do
  begin
    actual := frente;
    frente := frente^.siguiente;
    Dispose(actual);
  end;
  inherited Destroy;
end;

procedure TColaCorreos.Encolar(correo: TCorreo);
var
  nuevoNodo: PNodoCola;
begin
  New(nuevoNodo);
  nuevoNodo^.correo := correo;
  nuevoNodo^.siguiente := nil;

  if final = nil then
  begin
    // Primera inserción
    frente := nuevoNodo;
    final := nuevoNodo;
  end
  else
  begin
    // Agregar al final
    final^.siguiente := nuevoNodo;
    final := nuevoNodo;
  end;
end;

function TColaCorreos.Desencolar: TCorreo;
var
  nodoAEliminar: PNodoCola;
begin
  if frente <> nil then
  begin
    Result := frente^.correo;
    nodoAEliminar := frente;
    frente := frente^.siguiente;

    if frente = nil then
      final := nil;

    Dispose(nodoAEliminar);
  end;
end;

function TColaCorreos.Vacia: Boolean;
begin
  Result := frente = nil;
end;

function TColaCorreos.ObtenerFrente: PNodoCola;
begin
  Result := frente;
end;

// Implementacion de TListaComunidades
constructor TListaComunidades.Create;
begin
  cabeza := nil;
end;

destructor TListaComunidades.Destroy;
var
  actualCom, tempCom: PNodoComunidad;
  actualMiem, tempMiem: PNodoMiembro;
begin
  actualCom := cabeza;
  while actualCom <> nil do
  begin
    // Liberar miembros
    actualMiem := actualCom^.miembros;
    while actualMiem <> nil do
    begin
      tempMiem := actualMiem;
      actualMiem := actualMiem^.siguiente;
      Dispose(tempMiem);
    end;

    tempCom := actualCom;
    actualCom := actualCom^.siguiente;
    Dispose(tempCom);
  end;
  inherited Destroy;
end;

function TListaComunidades.BuscarComunidad(nombre: String): PNodoComunidad;
var
  actual: PNodoComunidad;
begin
  actual := cabeza;
  while actual <> nil do
  begin
    if actual^.nombre = nombre then
    begin
      Result := actual;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
  Result := nil;
end;

function TListaComunidades.CrearComunidad(nombre: String): Boolean;
var
  nuevaComunidad: PNodoComunidad;
begin
  Result := False;

  // Verificar si ya existe
  if BuscarComunidad(nombre) <> nil then
    Exit;

  // Crear nueva comunidad
  New(nuevaComunidad);
  nuevaComunidad^.nombre := nombre;
  nuevaComunidad^.miembros := nil;
  nuevaComunidad^.siguiente := cabeza;
  cabeza := nuevaComunidad;

  Result := True;
end;

function TListaComunidades.AgregarMiembro(nombreComunidad, email: String): Integer;
var
  comunidad: PNodoComunidad;
  nuevoMiembro, actualMiembro: PNodoMiembro;
begin
  // 0 = éxito, 1 = comunidad no existe, 2 = usuario no existe, 3 = ya es miembro

  comunidad := BuscarComunidad(nombreComunidad);
  if comunidad = nil then
  begin
    Result := 1;
    Exit;
  end;

  if listaUsuarios.Buscar(email) = nil then
  begin
    Result := 2;
    Exit;
  end;

  // Verificar si ya es miembro
  actualMiembro := comunidad^.miembros;
  while actualMiembro <> nil do
  begin
    if actualMiembro^.emailUsuario = email then
    begin
      Result := 3;
      Exit;
    end;
    actualMiembro := actualMiembro^.siguiente;
  end;

  // Agregar nuevo miembro
  New(nuevoMiembro);
  nuevoMiembro^.emailUsuario := email;
  nuevoMiembro^.siguiente := comunidad^.miembros;
  comunidad^.miembros := nuevoMiembro;

  Result := 0;
end;

function TListaComunidades.Vacia: Boolean;
begin
  Result := cabeza = nil;
end;

procedure TListaComunidades.GenerarReporteComunidades(nombreArchivo: String);
var
  archivo: TextFile;
  actualCom: PNodoComunidad;
  actualMiem, anteriorMiem: PNodoMiembro;
  contadorCom, contadorMiem: Integer;
begin
  AssignFile(archivo, nombreArchivo);
  Rewrite(archivo);

  try
    WriteLn(archivo, 'digraph ReporteComunidades {');
    WriteLn(archivo, '  rankdir=TB;');
    WriteLn(archivo, '  node [shape=box];');
    WriteLn(archivo, '');
    WriteLn(archivo, '  labelloc="t";');
    WriteLn(archivo, '  label="Reporte de Comunidades";');
    WriteLn(archivo, '  fontsize=16;');
    WriteLn(archivo, '');

    // Colocar todas las comunidades en el mismo nivel horizontal
    Write(archivo, '  {rank=same; ');
    contadorCom := 1;
    actualCom := cabeza;
    while actualCom <> nil do
    begin
      Write(archivo, 'com' + IntToStr(contadorCom));
      if actualCom^.siguiente <> nil then
        Write(archivo, '; ');
      actualCom := actualCom^.siguiente;
      Inc(contadorCom);
    end;
    WriteLn(archivo, '}');
    WriteLn(archivo, '');

    // Definir nodos de comunidades
    contadorCom := 1;
    actualCom := cabeza;
    while actualCom <> nil do
    begin
      WriteLn(archivo, '  com' + IntToStr(contadorCom) + ' [label="' + actualCom^.nombre + '", style=filled, fillcolor=lightblue];');
      actualCom := actualCom^.siguiente;
      Inc(contadorCom);
    end;
    WriteLn(archivo, '');

    // Conexiones horizontales entre comunidades (lista de comunidades)
    contadorCom := 1;
    actualCom := cabeza;
    while (actualCom <> nil) and (actualCom^.siguiente <> nil) do
    begin
      WriteLn(archivo, '  com' + IntToStr(contadorCom) + ' -> com' + IntToStr(contadorCom + 1) + ' [color=blue];');
      actualCom := actualCom^.siguiente;
      Inc(contadorCom);
    end;
    WriteLn(archivo, '');

    // Generar miembros verticalmente debajo de cada comunidad
    contadorCom := 1;
    actualCom := cabeza;
    while actualCom <> nil do
    begin
      actualMiem := actualCom^.miembros;
      anteriorMiem := nil;
      contadorMiem := 1;

      while actualMiem <> nil do
      begin
        // Crear nodo del miembro
        WriteLn(archivo, '  "miem' + IntToStr(contadorCom) + '_' + IntToStr(contadorMiem) + '" [label="' + actualMiem^.emailUsuario + '"];');

        if anteriorMiem = nil then
        begin
          // Primer miembro: conectar desde la comunidad
          WriteLn(archivo, '  com' + IntToStr(contadorCom) + ' -> "miem' + IntToStr(contadorCom) + '_' + IntToStr(contadorMiem) + '" [color=red];');
        end
        else
        begin
          // Miembros siguientes: conectar desde el anterior (lista vertical)
          WriteLn(archivo, '  "miem' + IntToStr(contadorCom) + '_' + IntToStr(contadorMiem - 1) + '" -> "miem' + IntToStr(contadorCom) + '_' + IntToStr(contadorMiem) + '" [color=green];');
        end;

        anteriorMiem := actualMiem;
        actualMiem := actualMiem^.siguiente;
        Inc(contadorMiem);
      end;

      actualCom := actualCom^.siguiente;
      Inc(contadorCom);
    end;

    WriteLn(archivo, '}');
  finally
    CloseFile(archivo);
  end;
end;


initialization
  // Crear la lista de usuarios
  listaUsuarios := TListaUsuarios.Create;
  matrizRelaciones := TMatrizDispersa.Create;
  listaComunidades := TListaComunidades.Create;

finalization
  // Liberar memoria
  if Assigned(listaUsuarios) then
     listaUsuarios.Free;
  if Assigned(matrizRelaciones) then
     matrizRelaciones.Free;
  if Assigned(listaComunidades) then
     listaComunidades.Free;
end.

