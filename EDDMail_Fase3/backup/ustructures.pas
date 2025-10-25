unit ustructures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, Process, Dialogs, uarbolavlborradores;

type

  // Registro para Usuario
  TUsuario = record
    id: Integer;
    nombre: String;
    usuario: String;
    email: String;
    telefono: String;
    password: String;
    edad: Integer;
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

  // Nodo para mensajes en comunidades
  PNodoMensajeComunidad = ^TNodoMensajeComunidad;
  TNodoMensajeComunidad = record
    correo: String;
    texto: String;
    fechaPublicacion: String;
    siguiente: PNodoMensajeComunidad;
  end;

  // Nodo para comunidades
  PNodoComunidad = ^TNodoComunidad;
  TNodoComunidad = record
    nombre: String;
    fechaCreacion: String;
    numeroMensajes: Integer;
    miembros: PNodoMiembro;
    mensajes: PNodoMensajeComunidad;
    siguiente: PNodoComunidad;
  end;


  // Nodo para arbol avl
  PNodoAVL = ^TNodoAVL;
  TNodoAVL = record
    correo: TCorreo;
    altura: Integer;
    izquierdo: PNodoAVL;
    derecho: PNodoAVL;
  end;

  // Nodo para arbol b orden 5
  PNodoB = ^TNodoB;
  TNodoB = record
    numClaves: Integer;
    claves: array[0..3] of TCorreo;
    hijos: array[0..4] of PNodoB;
    esHoja: Boolean;
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
    procedure GenerarReportePapelera(nombreArchivo: String; nombreUsuario: String);
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
    function ExisteID(id: Integer): Boolean;
    function ExisteEmail(email: String): Boolean;
    function ExisteTelefono(telefono: String): Boolean;
    function ValidarUsuarioUnico(usuario: TUsuario): Integer;
    procedure CargarCorreosDesdeJSON(nombreArchivo: String);
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
      procedure GenerarReporteCorreosRecibidos(nombreArchivo: String; nombreUsuario: String);
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
    procedure GenerarReporteContactos(nombreArchivo: String; nombreUsuario: String);
    function EliminarContacto(email: String): Boolean;
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
    procedure PublicarMensaje(nombreComunidad, correo, texto: String); // NUEVO
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
    procedure GenerarReporteCorreosProgramados(nombreArchivo: String; nombreUsuario: String);
  end;

  //clase para arbol avl
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

  //clase para arbol b
  TArbolBFavoritos = class
  private
    raiz: PNodoB;
    orden: Integer;
    function BuscarEnNodo(nodo: PNodoB; id: Integer): PNodoB;
    procedure DividirHijo(padre: PNodoB; indice: Integer);
    procedure InsertarNoLleno(nodo: PNodoB; correo: TCorreo);
    procedure LiberarNodo(nodo: PNodoB);
    procedure RecolectarCorreos(nodo: PNodoB; lista: TList);
    procedure GenerarDotRecursivo(nodo: PNodoB; var archivo: TextFile; var contador: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Insertar(correo: TCorreo);
    function Buscar(id: Integer): PNodoB;
    function ObtenerCorreo(id: Integer): TCorreo;
    function ObtenerTodosFavoritos: TList;
    procedure GenerarReporteArbolB(nombreArchivo: String);
    function Vacio: Boolean;
  end;

var
  listaUsuarios: TListaUsuarios;
  matrizRelaciones: TMatrizDispersa;
  usuarioActual: PNodoUsuario;
  listaComunidades: TListaComunidades;
  arbolAVLBorradores: TArbolAVLBorradores;
  arbolBFavoritos: TArbolBFavoritos;
  ultimoIDCorreo: Integer;

function GenerarIDCorreoUnico: Integer;
procedure ActualizarUltimoIDCorreo(id: Integer);

implementation

function GenerarIDCorreoUnico: Integer;
begin
  Inc(ultimoIDCorreo);
  Result := ultimoIDCorreo;
end;

procedure ActualizarUltimoIDCorreo(id: Integer);
begin
  if id > ultimoIDCorreo then
    ultimoIDCorreo := id;
end;

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
    if actual^.usuario.usuario = email then
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
    i, validacion: Integer;
    fileStream: TFileStream;
    stringStream: TStringStream;
    usuariosAgregados, usuariosRechazados: Integer;
    mensajeResultado: string;
  begin
    usuariosAgregados := 0;
    usuariosRechazados := 0;
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
            usuario.edad := jsonObject.Integers['edad'];
            usuario.bandejaEntrada := nil; //iniciar bandeja como nil
            usuario.contactos := nil; //iniciar contactos como nil
            usuario.papelera := nil; //iniciar papelera como nil
            usuario.colaCorreos := nil; //iniciar cola correos como nil

            //validar que el usuario sea unico
            validacion := ValidarUsuarioUnico(usuario);

            if validacion = 0 then
            begin
              Insertar(usuario);
              Inc(usuariosAgregados);
            end
            else
            begin
              Inc(usuariosRechazados);
            end;
          end;
        end
        else
        begin
          ShowMessage('Error: El archivo JSON no tiene el formato correcto');
          Exit;
        end;

        //Mensaje de resultado despues de la carga masiva
        mensajeResultado := 'Carga masiva completada:' + #13#10 +
                       'Usuarios agregados: ' + IntToStr(usuariosAgregados) + #13#10 +
                       'Usuarios rechazados (duplicados/inválidos): ' + IntToStr(usuariosRechazados);
        ShowMessage(mensajeResultado);

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
  nombrePNG: String;
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

  //Generar png automatico
  nombrePNG := ChangeFileExt(nombreArchivo, '.png');
  if FileExists('/usr/bin/dot') then
  begin
    ExecuteProcess('/usr/bin/dot', ['-Tpng', nombreArchivo, '-o', nombrePNG]);
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

function TListaUsuarios.ExisteID(id: Integer): Boolean;
var
  actual: PNodoUsuario;
begin
  Result := False;
  actual := cabeza;
  while actual <> nil do
  begin
    if actual^.usuario.id = id then
    begin
      Result := True;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
end;

function TListaUsuarios.ExisteEmail(email: String): Boolean;
var
  actual: PNodoUsuario;
begin
  Result := False;
  actual := cabeza;
  while actual <> nil do
  begin
    if actual^.usuario.email = email then
    begin
      Result := True;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
end;

function TListaUsuarios.ExisteTelefono(telefono: String): Boolean;
var
  actual: PNodoUsuario;
begin
  Result := False;
  actual := cabeza;
  while actual <> nil do
  begin
    if actual^.usuario.telefono = telefono then
    begin
      Result := True;
      Exit;
    end;
    actual := actual^.siguiente;
  end;
end;

function TListaUsuarios.ValidarUsuarioUnico(usuario: TUsuario): Integer;
begin
  // 0 = único, 1 = ID existe, 2 = email existe, 3 = usuario existe, 4 = teléfono existe

  if ExisteID(usuario.id) then
  begin
    Result := 1;
    Exit;
  end;

  if ExisteEmail(usuario.email) then
  begin
    Result := 2;
    Exit;
  end;

  if ExisteUsuario(usuario.usuario) then
  begin
    Result := 3;
    Exit;
  end;

  if ExisteTelefono(usuario.telefono) then
  begin
    Result := 4;
    Exit;
  end;

  Result := 0;
end;

procedure TListaUsuarios.CargarCorreosDesdeJSON(nombreArchivo: String);
var
  jsonString: String;
  jsonData: TJSONData;
  jsonArray: TJSONArray;
  jsonObject: TJSONObject;
  correo: TCorreo;
  i: Integer;
  fileStream: TFileStream;
  stringStream: TStringStream;
  nodoDestinatario: PNodoUsuario;
  correosEnviados, correosRechazados: Integer;
  mensajeResultado: String;
begin
  correosEnviados := 0;
  correosRechazados := 0;

  try
    fileStream := TFileStream.Create(nombreArchivo, fmOpenRead);
    stringStream := TStringStream.Create('');
    try
      stringStream.CopyFrom(fileStream, fileStream.Size);
      jsonString := stringStream.DataString;
    finally
      fileStream.Free;
      stringStream.Free;
    end;

    jsonData := GetJSON(jsonString);
    try
      if jsonData.JSONType = jtObject then
      begin
        jsonObject := TJSONObject(jsonData);
        jsonArray := TJSONArray(jsonObject.Arrays['correos']);

        for i := 0 to jsonArray.Count - 1 do
        begin
          jsonObject := TJSONObject(jsonArray[i]);

          correo.id := jsonObject.Integers['id'];
          correo.remitente := jsonObject.Strings['remitente'];
          correo.destinatario := jsonObject.Strings['destinatario'];
          correo.estado := jsonObject.Strings['estado'];
          correo.asunto := jsonObject.Strings['asunto'];
          correo.mensaje := jsonObject.Strings['mensaje'];
          correo.fecha := jsonObject.Strings['fecha_envio'];
          correo.programado := False;

          ActualizarUltimoIDCorreo(correo.id);

          nodoDestinatario := Buscar(correo.destinatario);
          if nodoDestinatario <> nil then
          begin
            nodoDestinatario^.usuario.bandejaEntrada^.AgregarCorreo(correo);
            matrizRelaciones.IncrementarRelacion(correo.remitente, correo.destinatario);
            Inc(correosEnviados);
          end
          else
          begin
            Inc(correosRechazados);
          end;
        end;
      end
      else
      begin
        ShowMessage('Error: El archivo JSON no tiene el formato correcto');
        Exit;
      end;

      mensajeResultado := 'Carga de correos completada:' + #13#10 +
                         'Correos enviados: ' + IntToStr(correosEnviados) + #13#10 +
                         'Correos rechazados (destinatario no existe): ' + IntToStr(correosRechazados);
      ShowMessage(mensajeResultado);

    finally
      jsonData.Free;
    end;

  except
    on E: Exception do
      raise Exception.Create('Error al cargar archivo JSON de correos: ' + E.Message);
  end;
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
  nombrePNG: String;
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

  //Generar png automatico
  nombrePNG := ChangeFileExt(nombreArchivo, '.png');
  if FileExists('/usr/bin/dot') then
  begin
    ExecuteProcess('/usr/bin/dot', ['-Tpng', nombreArchivo, '-o', nombrePNG]);
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

procedure TListaCorreos.GenerarReporteCorreosRecibidos(nombreArchivo: String; nombreUsuario: String);
var
  archivo: TextFile;
  actual: PNodoCorreo;
  contador: Integer;
  nombrePNG: String;
begin
  AssignFile(archivo, nombreArchivo);
  Rewrite(archivo);

  try
    WriteLn(archivo, 'digraph CorreosRecibidos {');
    WriteLn(archivo, '  rankdir=TB;');
    WriteLn(archivo, '  node [shape=record];');
    WriteLn(archivo, '');
    WriteLn(archivo, '  labelloc="t";');
    WriteLn(archivo, '  label="Correos Recibidos - ' + nombreUsuario + '";');
    WriteLn(archivo, '  fontsize=16;');
    WriteLn(archivo, '');

    contador := 1;
    actual := cabeza;

    while actual <> nil do
    begin
      WriteLn(archivo, '  correo' + IntToStr(contador) + ' [label="ID: ' + IntToStr(actual^.correo.id) + '\n' +
              'De: ' + actual^.correo.remitente + '\n' +
              'Asunto: ' + actual^.correo.asunto + '\n' +
              'Estado: ' + actual^.correo.estado + '\n' +
              'Fecha: ' + actual^.correo.fecha + '"];');

      if actual^.siguiente <> nil then
        WriteLn(archivo, '  correo' + IntToStr(contador) + ' -> correo' + IntToStr(contador + 1) + ';');

      actual := actual^.siguiente;
      Inc(contador);
    end;

    if contador = 1 then
      WriteLn(archivo, '  vacio [label="No hay correos recibidos"];');

    WriteLn(archivo, '}');
  finally
    CloseFile(archivo);
  end;

  // Generar PNG automáticamente
  nombrePNG := ChangeFileExt(nombreArchivo, '.png');
  if FileExists('/usr/bin/dot') then
    ExecuteProcess('/usr/bin/dot', ['-Tpng', nombreArchivo, '-o', nombrePNG]);
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

function TListaContactos.EliminarContacto(email: String): Boolean;
var
  actual, anterior: PNodoContacto;
begin
  Result := False;

  if ultimo = nil then Exit;

  actual := ultimo^.siguiente;
  anterior := ultimo;

  repeat
    if actual^.email = email then
    begin
      if actual = ultimo then
      begin
        if actual^.siguiente = actual then
        begin
          Dispose(actual);
          ultimo := nil;
        end
        else
        begin
          anterior^.siguiente := actual^.siguiente;
          ultimo := anterior;
          Dispose(actual);
        end;
      end
      else
      begin
        anterior^.siguiente := actual^.siguiente;
        if actual = ultimo^.siguiente then
          ultimo^.siguiente := actual^.siguiente;
        Dispose(actual);
      end;

      Result := True;
      Exit;
    end;

    anterior := actual;
    actual := actual^.siguiente;
  until actual = ultimo^.siguiente;
end;

procedure TListaContactos.GenerarReporteContactos(nombreArchivo: String; nombreUsuario: String);
var
  archivo: TextFile;
  actual: PNodoContacto;
  contador: Integer;
  nombrePNG: String;
  nodoUsuario: PNodoUsuario;
begin
  AssignFile(archivo, nombreArchivo);
  Rewrite(archivo);

  try
    WriteLn(archivo, 'digraph Contactos {');
    WriteLn(archivo, '  rankdir=LR;');
    WriteLn(archivo, '  node [shape=record, style=filled, fillcolor=lightgreen];');
    WriteLn(archivo, '');
    WriteLn(archivo, '  labelloc="t";');
    WriteLn(archivo, '  label="Contactos - ' + nombreUsuario + '";');
    WriteLn(archivo, '  fontsize=16;');
    WriteLn(archivo, '');

    if ultimo <> nil then
    begin
      contador := 1;
      actual := ultimo^.siguiente; // Empezar desde el primero

      repeat
        // Buscar información completa del usuario
        nodoUsuario := listaUsuarios.Buscar(actual^.email);

        if nodoUsuario <> nil then
        begin
          WriteLn(archivo, '  contacto' + IntToStr(contador) + ' [label="' +
                  'Nombre: ' + nodoUsuario^.usuario.nombre + '\n' +
                  'Usuario: ' + nodoUsuario^.usuario.usuario + '\n' +
                  'Email: ' + nodoUsuario^.usuario.email + '\n' +
                  'Teléfono: ' + nodoUsuario^.usuario.telefono + '"];');
        end
        else
        begin
          WriteLn(archivo, '  contacto' + IntToStr(contador) + ' [label="Email: ' + actual^.email + '\n(Usuario no encontrado)"];');
        end;

        // Conectar con el siguiente (circular)
        if actual^.siguiente <> ultimo^.siguiente then
          WriteLn(archivo, '  contacto' + IntToStr(contador) + ' -> contacto' + IntToStr(contador + 1) + ';')
        else
          WriteLn(archivo, '  contacto' + IntToStr(contador) + ' -> contacto1 [style=dashed, color=red];'); // Conexión circular

        actual := actual^.siguiente;
        Inc(contador);
      until actual = ultimo^.siguiente;
    end
    else
    begin
      WriteLn(archivo, '  vacio [label="No hay contactos"];');
    end;

    WriteLn(archivo, '}');
  finally
    CloseFile(archivo);
  end;

  // Generar PNG automáticamente
  nombrePNG := ChangeFileExt(nombreArchivo, '.png');
  if FileExists('/usr/bin/dot') then
    ExecuteProcess('/usr/bin/dot', ['-Tpng', nombreArchivo, '-o', nombrePNG]);
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

procedure TPapelera.GenerarReportePapelera(nombreArchivo: String; nombreUsuario: String);
var
  archivo: TextFile;
  actual: PNodoPila;
  contador: Integer;
  nombrePNG: String;
begin
  AssignFile(archivo, nombreArchivo);
  Rewrite(archivo);

  try
    WriteLn(archivo, 'digraph Papelera {');
    WriteLn(archivo, '  rankdir=TB;');
    WriteLn(archivo, '  node [shape=record, style=filled, fillcolor=lightcoral];');
    WriteLn(archivo, '');
    WriteLn(archivo, '  labelloc="t";');
    WriteLn(archivo, '  label="Papelera - ' + nombreUsuario + '";');
    WriteLn(archivo, '  fontsize=16;');
    WriteLn(archivo, '');

    contador := 1;
    actual := tope;

    while actual <> nil do
    begin
      WriteLn(archivo, '  eliminado' + IntToStr(contador) + ' [label="ID: ' + IntToStr(actual^.correo.id) + '\n' +
              'De: ' + actual^.correo.remitente + '\n' +
              'Asunto: ' + actual^.correo.asunto + '\n' +
              'Estado: Eliminado\n' +
              'Fecha: ' + actual^.correo.fecha + '"];');

      if actual^.siguiente <> nil then
        WriteLn(archivo, '  eliminado' + IntToStr(contador) + ' -> eliminado' + IntToStr(contador + 1) + ';');

      actual := actual^.siguiente;
      Inc(contador);
    end;

    if contador = 1 then
      WriteLn(archivo, '  vacio [label="Papelera vacía"];');

    WriteLn(archivo, '}');
  finally
    CloseFile(archivo);
  end;

  // Generar PNG automáticamente
  nombrePNG := ChangeFileExt(nombreArchivo, '.png');
  if FileExists('/usr/bin/dot') then
    ExecuteProcess('/usr/bin/dot', ['-Tpng', nombreArchivo, '-o', nombrePNG]);
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

procedure TColaCorreos.GenerarReporteCorreosProgramados(nombreArchivo: String; nombreUsuario: String);
var
  archivo: TextFile;
  actual: PNodoCola;
  contador: Integer;
  nombrePNG: String;
begin
  AssignFile(archivo, nombreArchivo);
  Rewrite(archivo);

  try
    WriteLn(archivo, 'digraph CorreosProgramados {');
    WriteLn(archivo, '  rankdir=TB;');
    WriteLn(archivo, '  node [shape=record, style=filled, fillcolor=lightblue];');
    WriteLn(archivo, '');
    WriteLn(archivo, '  labelloc="t";');
    WriteLn(archivo, '  label="Correos Programados - ' + nombreUsuario + '";');
    WriteLn(archivo, '  fontsize=16;');
    WriteLn(archivo, '');

    contador := 1;
    actual := frente;

    while actual <> nil do
    begin
      WriteLn(archivo, '  programado' + IntToStr(contador) + ' [label="ID: ' + IntToStr(actual^.correo.id) + '\n' +
              'Para: ' + actual^.correo.destinatario + '\n' +
              'Asunto: ' + actual^.correo.asunto + '\n' +
              'Fecha programada: ' + actual^.correo.fecha + '"];');

      if actual^.siguiente <> nil then
        WriteLn(archivo, '  programado' + IntToStr(contador) + ' -> programado' + IntToStr(contador + 1) + ';');

      actual := actual^.siguiente;
      Inc(contador);
    end;

    if contador = 1 then
      WriteLn(archivo, '  vacio [label="No hay correos programados"];');

    WriteLn(archivo, '}');
  finally
    CloseFile(archivo);
  end;

  // Generar PNG automáticamente
  nombrePNG := ChangeFileExt(nombreArchivo, '.png');
  if FileExists('/usr/bin/dot') then
    ExecuteProcess('/usr/bin/dot', ['-Tpng', nombreArchivo, '-o', nombrePNG]);
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

  if BuscarComunidad(nombre) <> nil then
    Exit;

  New(nuevaComunidad);
  nuevaComunidad^.nombre := nombre;
  nuevaComunidad^.fechaCreacion := FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);
  nuevaComunidad^.numeroMensajes := 0;
  nuevaComunidad^.miembros := nil;
  nuevaComunidad^.mensajes := nil;
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

procedure TListaComunidades.PublicarMensaje(nombreComunidad, correo, texto: String);
var
  comunidad: PNodoComunidad;
  nuevoMensaje: PNodoMensajeComunidad;
  miembroActual: PNodoMiembro;
  esMiembro: Boolean;
begin
  comunidad := BuscarComunidad(nombreComunidad);
  if comunidad = nil then
  begin
    ShowMessage('Error: La comunidad no existe');
    Exit;
  end;

  esMiembro := False;
  miembroActual := comunidad^.miembros;
  while miembroActual <> nil do
  begin
    if miembroActual^.emailUsuario = correo then
    begin
      esMiembro := True;
      Break;
    end;
    miembroActual := miembroActual^.siguiente;
  end;

  if not esMiembro then
  begin
    ShowMessage('Error: Debe ser miembro de la comunidad para publicar');
    Exit;
  end;

  New(nuevoMensaje);
  nuevoMensaje^.correo := correo;
  nuevoMensaje^.texto := texto;
  nuevoMensaje^.fechaPublicacion := FormatDateTime('dd/mm/yyyy hh:nn:ss', Now);
  nuevoMensaje^.siguiente := comunidad^.mensajes;
  comunidad^.mensajes := nuevoMensaje;
  Inc(comunidad^.numeroMensajes);

  ShowMessage('Mensaje publicado exitosamente');
end;

// Implementacion arbol avl
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

// implementacion arbol b orden 5
constructor TArbolBFavoritos.Create;
begin
  raiz := nil;
  orden := 5;
end;

destructor TArbolBFavoritos.Destroy;
begin
  LiberarNodo(raiz);
  inherited Destroy;
end;

procedure TArbolBFavoritos.LiberarNodo(nodo: PNodoB);
var
  i: Integer;
begin
  if nodo = nil then Exit;

  if not nodo^.esHoja then
  begin
    for i := 0 to nodo^.numClaves do
      LiberarNodo(nodo^.hijos[i]);
  end;

  Dispose(nodo);
end;

function TArbolBFavoritos.Vacio: Boolean;
begin
  Result := raiz = nil;
end;

procedure TArbolBFavoritos.DividirHijo(padre: PNodoB; indice: Integer);
var
  hijoLleno, nuevoHijo: PNodoB;
  i: Integer;
begin
  hijoLleno := padre^.hijos[indice];
  New(nuevoHijo);

  nuevoHijo^.esHoja := hijoLleno^.esHoja;
  nuevoHijo^.numClaves := 1;

  nuevoHijo^.claves[0] := hijoLleno^.claves[2];

  if not hijoLleno^.esHoja then
  begin
    nuevoHijo^.hijos[0] := hijoLleno^.hijos[2];
    nuevoHijo^.hijos[1] := hijoLleno^.hijos[3];
  end;

  hijoLleno^.numClaves := 1;

  for i := padre^.numClaves downto indice + 1 do
    padre^.hijos[i + 1] := padre^.hijos[i];

  padre^.hijos[indice + 1] := nuevoHijo;

  for i := padre^.numClaves - 1 downto indice do
    padre^.claves[i + 1] := padre^.claves[i];

  padre^.claves[indice] := hijoLleno^.claves[1];
  Inc(padre^.numClaves);
end;

procedure TArbolBFavoritos.InsertarNoLleno(nodo: PNodoB; correo: TCorreo);
var
  i: Integer;
begin
  i := nodo^.numClaves - 1;

  if nodo^.esHoja then
  begin
    while (i >= 0) and (correo.id < nodo^.claves[i].id) do
    begin
      nodo^.claves[i + 1] := nodo^.claves[i];
      Dec(i);
    end;

    nodo^.claves[i + 1] := correo;
    Inc(nodo^.numClaves);
  end
  else
  begin
    while (i >= 0) and (correo.id < nodo^.claves[i].id) do
      Dec(i);

    Inc(i);

    if nodo^.hijos[i]^.numClaves = 3 then
    begin
      DividirHijo(nodo, i);
      if correo.id > nodo^.claves[i].id then
        Inc(i);
    end;

    InsertarNoLleno(nodo^.hijos[i], correo);
  end;
end;

procedure TArbolBFavoritos.Insertar(correo: TCorreo);
var
  nuevaRaiz: PNodoB;
begin
  if raiz = nil then
  begin
    New(raiz);
    raiz^.esHoja := True;
    raiz^.numClaves := 1;
    raiz^.claves[0] := correo;
    Exit;
  end;

  if raiz^.numClaves = 3 then
  begin
    New(nuevaRaiz);
    nuevaRaiz^.esHoja := False;
    nuevaRaiz^.numClaves := 0;
    nuevaRaiz^.hijos[0] := raiz;

    DividirHijo(nuevaRaiz, 0);
    raiz := nuevaRaiz;
  end;

  InsertarNoLleno(raiz, correo);
end;

function TArbolBFavoritos.BuscarEnNodo(nodo: PNodoB; id: Integer): PNodoB;
var
  i: Integer;
begin
  if nodo = nil then
  begin
    Result := nil;
    Exit;
  end;

  i := 0;
  while (i < nodo^.numClaves) and (id > nodo^.claves[i].id) do
    Inc(i);

  if (i < nodo^.numClaves) and (id = nodo^.claves[i].id) then
  begin
    Result := nodo;
    Exit;
  end;

  if nodo^.esHoja then
  begin
    Result := nil;
    Exit;
  end;

  Result := BuscarEnNodo(nodo^.hijos[i], id);
end;

function TArbolBFavoritos.Buscar(id: Integer): PNodoB;
begin
  Result := BuscarEnNodo(raiz, id);
end;

function TArbolBFavoritos.ObtenerCorreo(id: Integer): TCorreo;
var
  nodo: PNodoB;
  i: Integer;
begin
  nodo := Buscar(id);

  if nodo <> nil then
  begin
    for i := 0 to nodo^.numClaves - 1 do
    begin
      if nodo^.claves[i].id = id then
      begin
        Result := nodo^.claves[i];
        Exit;
      end;
    end;
  end;

  FillChar(Result, SizeOf(Result), 0);
end;

procedure TArbolBFavoritos.RecolectarCorreos(nodo: PNodoB; lista: TList);
var
  i: Integer;
  pCorreo: ^TCorreo;
begin
  if nodo = nil then Exit;

  for i := 0 to nodo^.numClaves - 1 do
  begin
    if not nodo^.esHoja then
      RecolectarCorreos(nodo^.hijos[i], lista);

    New(pCorreo);
    pCorreo^ := nodo^.claves[i];
    lista.Add(pCorreo);
  end;

  if not nodo^.esHoja then
    RecolectarCorreos(nodo^.hijos[nodo^.numClaves], lista);
end;

function TArbolBFavoritos.ObtenerTodosFavoritos: TList;
begin
  Result := TList.Create;
  RecolectarCorreos(raiz, Result);
end;

procedure TArbolBFavoritos.GenerarDotRecursivo(nodo: PNodoB; var archivo: TextFile; var contador: Integer);
var
  i: Integer;
  miId: Integer;
  etiqueta: String;
begin
  if nodo = nil then Exit;

  miId := contador;
  Inc(contador);

  etiqueta := '';
  for i := 0 to nodo^.numClaves - 1 do
  begin
    etiqueta := etiqueta + '<f' + IntToStr(i) + '> ID:' + IntToStr(nodo^.claves[i].id);
    if i < nodo^.numClaves - 1 then
      etiqueta := etiqueta + ' | ';
  end;

  WriteLn(archivo, '  nodo' + IntToStr(miId) + ' [label="' + etiqueta + '"];');

  if not nodo^.esHoja then
  begin
    for i := 0 to nodo^.numClaves do
    begin
      if nodo^.hijos[i] <> nil then
      begin
        WriteLn(archivo, '  nodo' + IntToStr(miId) + ' -> nodo' + IntToStr(contador) + ';');
        GenerarDotRecursivo(nodo^.hijos[i], archivo, contador);
      end;
    end;
  end;
end;

procedure TArbolBFavoritos.GenerarReporteArbolB(nombreArchivo: String);
var
  archivo: TextFile;
  nombrePNG: String;
  contador: Integer;
begin
  AssignFile(archivo, nombreArchivo);
  Rewrite(archivo);

  try
    WriteLn(archivo, 'digraph ArbolB {');
    WriteLn(archivo, '  node [shape=record];');
    WriteLn(archivo, '  rankdir=TB;');
    WriteLn(archivo, '');
    WriteLn(archivo, '  labelloc="t";');
    WriteLn(archivo, '  label="Arbol B Orden 5 - Favoritos";');
    WriteLn(archivo, '  fontsize=16;');
    WriteLn(archivo, '');

    contador := 0;
    if raiz <> nil then
      GenerarDotRecursivo(raiz, archivo, contador)
    else
      WriteLn(archivo, '  vacio [label="No hay favoritos"];');

    WriteLn(archivo, '}');
  finally
    CloseFile(archivo);
  end;

  nombrePNG := ChangeFileExt(nombreArchivo, '.png');
  if FileExists('/usr/bin/dot') then
    ExecuteProcess('/usr/bin/dot', ['-Tpng', nombreArchivo, '-o', nombrePNG]);
end;

initialization
  // Crear la lista de usuarios
  listaUsuarios := TListaUsuarios.Create;
  matrizRelaciones := TMatrizDispersa.Create;
  listaComunidades := TListaComunidades.Create;
  arbolAVLBorradores := TArbolAVLBorradores.Create;
  arbolBFavoritos := TArbolBFavoritos.Create;
  ultimoIDCorreo := 0;

finalization
  // Liberar memoria
  if Assigned(listaUsuarios) then
     listaUsuarios.Free;
  if Assigned(matrizRelaciones) then
     matrizRelaciones.Free;
  if Assigned(listaComunidades) then
     listaComunidades.Free;
  if Assigned(arbolAVLBorradores) then
     arbolAVLBorradores.Free;
  if Assigned(arbolBFavoritos) then
     arbolBFavoritos.Free;
end.

