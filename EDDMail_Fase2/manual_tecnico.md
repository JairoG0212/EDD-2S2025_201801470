# Manual Técnico - EDDMail
## Fase 1

**Desarrollado por:**
- Jairo Josue Gomez Reyes
- R.A. 201801470

---

## Tabla de Contenidos
1. [Introducción](#introducción)
2. [Arquitectura del Sistema](#arquitectura-del-sistema)
3. [Estructuras de Datos Implementadas](#estructuras-de-datos-implementadas)
4. [Módulos del Sistema](#módulos-del-sistema)
5. [Funcionalidades Principales](#funcionalidades-principales)
6. [Reportes y Visualización](#reportes-y-visualización)
7. [Consideraciones Técnicas](#consideraciones-técnicas)

---

## Introducción

EDDMail es un sistema de simulación de correos electrónicos desarrollado en Object Pascal usando la librería GTK. El proyecto implementa múltiples estructuras de datos dinámicas para gestionar usuarios, correos, contactos y relaciones entre usuarios, proporcionando una experiencia completa de manejo de correo electrónico.

### Tecnologías Utilizadas
- **Lenguaje:** Object Pascal (Free Pascal)
- **Interfaz Gráfica:** GTK (a través de Lazarus)
- **Generación de Reportes:** Graphviz
- **Formato de Datos:** JSON para carga masiva

---

## Arquitectura del Sistema

El sistema está organizado en módulos que manejan diferentes aspectos de la aplicación:

```
EDDMail/
├── ustructures.pas     // Estructuras de datos principales
├── ulogin.pas/.lfm     // Interfaz de inicio de sesión
├── umenuroot.pas/.lfm  // Menú del administrador
├── umenuusuario.pas/.lfm // Menú del usuario estándar
├── ubandejaentrada.pas/.lfm // Gestión de bandeja de entrada
├── uenviarcorreo.pas/.lfm // Envío de correos
├── upapelera.pas/.lfm  // Gestión de papelera
├── ucontactos.pas/.lfm // Manejo de contactos
└── ucorreosprogramados.pas/.lfm // Correos programados
```

### Componentes Clave

1. **Usuario Administrador (Root):**
   - Credenciales: `root@edd.com / root123`
   - Funciones: Carga masiva, reportes generales

2. **Usuarios Estándar:**
   - Gestión personal de correos
   - Manejo de contactos
   - Generación de reportes individuales

---

## Estructuras de Datos Implementadas

### 1. Lista Simple - Usuarios del Sistema

```pascal
type
  PNodoUsuario = ^TNodoUsuario;
  TNodoUsuario = record
    usuario: TUsuario;
    siguiente: PNodoUsuario;
  end;

  TListaUsuarios = class
    private
      cabeza: PNodoUsuario;
    public
      procedure Insertar(nuevoUsuario: TUsuario);
      function Buscar(email: String): PNodoUsuario;
      // ... más métodos
  end;
```

**Uso:** Almacena todos los usuarios registrados en el sistema de forma secuencial. La inserción se realiza al inicio de la lista para optimizar el tiempo de inserción.

### 2. Lista Doblemente Enlazada - Bandeja de Entrada

```pascal
type
  PNodoCorreo = ^TNodoCorreo;
  TNodoCorreo = record
    correo: TCorreo;
    anterior: PNodoCorreo;
    siguiente: PNodoCorreo;
  end;

  TListaCorreos = class
    private
      cabeza: PNodoCorreo;
      cola: PNodoCorreo;
    public
      procedure AgregarCorreo(nuevoCorreo: TCorreo);
      function EliminarCorreo(indice: Integer): TCorreo;
      // ... más métodos
  end;
```

**Ventaja:** Permite navegación bidireccional y eliminación eficiente desde cualquier posición. Ideal para la bandeja de entrada donde se requiere acceso aleatorio a los correos.

### 3. Lista Circular - Contactos

```pascal
type
  PNodoContacto = ^TNodoContacto;
  TNodoContacto = record
    email: String;
    siguiente: PNodoContacto;
  end;

  TListaContactos = class
    private
      ultimo: PNodoContacto; // Apunta al último nodo
    public
      procedure AgregarContacto(email: String);
      function BuscarContacto(email: String): Boolean;
      // ... más métodos
  end;
```

**Implementación:** El puntero `ultimo` apunta al último elemento insertado, y su `siguiente` apunta al primer elemento, creando la circularidad. Esto permite navegación infinita entre contactos.

### 4. Cola (FIFO) - Correos Programados

```pascal
type
  PNodoCola = ^TNodoCola;
  TNodoCola = record
    correo: TCorreo;
    siguiente: PNodoCola;
  end;

  TColaCorreos = class
    private
      frente: PNodoCola;
      final: PNodoCola;
    public
      procedure Encolar(correo: TCorreo);
      function Desencolar: TCorreo;
      // ... más métodos
  end;
```

**Propósito:** Mantiene el orden de envío de correos programados. El primer correo programado es el primero en enviarse (FIFO).

### 5. Pila (LIFO) - Papelera

```pascal
type
  PNodoPila = ^TNodoPila;
  TNodoPila = record
    correo: TCorreo;
    siguiente: PNodoPila;
  end;

  TPapelera = class
    private
      tope: PNodoPila;
    public
      procedure Apilar(correo: TCorreo);
      function Desapilar: TCorreo;
      // ... más métodos
  end;
```

**Comportamiento:** El último correo eliminado es el primero que se puede recuperar o eliminar permanentemente (LIFO).

### 6. Matriz Dispersa - Relaciones entre Usuarios

```pascal
type
  PNodoMatriz = ^TNodoMatriz;
  TNodoMatriz = record
    fila: Integer;        // Índice del remitente
    columna: Integer;     // Índice del destinatario
    cantidad: Integer;    // Cantidad de correos enviados
    siguiente: PNodoMatriz;
    abajo: PNodoMatriz;
  end;
```

**Optimización:** Solo almacena las relaciones que existen (nodos no nulos), ahorrando memoria significativamente comparado con una matriz completa.

### 7. Lista de Listas - Comunidades

```pascal
type
  PNodoMiembro = ^TNodoMiembro;
  TNodoMiembro = record
    emailUsuario: String;
    siguiente: PNodoMiembro;
  end;

  PNodoComunidad = ^TNodoComunidad;
  TNodoComunidad = record
    nombre: String;
    miembros: PNodoMiembro;  // Lista de miembros
    siguiente: PNodoComunidad;
  end;
```

**Estructura:** Cada comunidad contiene una lista simple de sus miembros, y las comunidades forman otra lista simple.

---

## Módulos del Sistema

### Módulo de Autenticación (ulogin.pas)

- **Función:** Maneja el inicio de sesión tanto para usuarios normales como para root
- **Validaciones:**
  - Verificación de credenciales
  - Redirección según tipo de usuario
  - Limpieza de campos tras cada operación

### Módulo de Estructuras (ustructures.pas)

Este es el corazón del sistema, contiene:

- Definición de todos los tipos de datos
- Implementación de las clases para cada estructura
- Métodos de manipulación de datos
- Funciones de generación de reportes
- Validaciones de integridad de datos

**Características destacadas:**

```pascal
// Validación de usuario único durante carga masiva
function TListaUsuarios.ValidarUsuarioUnico(usuario: TUsuario): Integer;
begin
  if ExisteID(usuario.id) then Result := 1
  else if ExisteEmail(usuario.email) then Result := 2
  else if ExisteUsuario(usuario.usuario) then Result := 3
  else if ExisteTelefono(usuario.telefono) then Result := 4
  else Result := 0; // Usuario único
end;
```

### Módulo de Bandeja de Entrada (ubandejaentrada.pas)

**Funcionalidades clave:**

1. **Ordenamiento por Asunto:**
   ```pascal
   // Implementa ordenamiento burbuja
   for i := 0 to contador - 2 do
     for j := 0 to contador - 2 - i do
       if CompareText(listaCorreos[j].asunto, listaCorreos[j + 1].asunto) > 0 then
         // Intercambia elementos
   ```

2. **Gestión de Estados:**
   - 'NL': No Leído
   - 'L': Leído
   - Cambio automático al abrir un correo

### Módulo de Contactos (ucontactos.pas)

**Navegación circular:**
```pascal
// Botón siguiente
contactoActual := contactoActual^.siguiente;

// Botón anterior (requiere recorrer toda la lista)
actual := contactoActual;
while actual^.siguiente <> contactoActual do
  actual := actual^.siguiente;
contactoActual := actual;
```

---

## Funcionalidades Principales

### Carga Masiva de Usuarios

**Proceso:**
1. Lectura del archivo JSON
2. Validación de formato
3. Verificación de usuarios únicos
4. Inserción en la estructura
5. Reporte de resultados

```pascal
procedure TListaUsuarios.CargarDesdeJSON(nombreArchivo: String);
var
  // Variables para manejo JSON
  jsonString: String;
  jsonData: TJSONData;
  // Contadores para reporte
  usuariosAgregados, usuariosRechazados: Integer;
begin
  // Proceso de carga con manejo de errores
  try
    // Lectura y parsing del JSON
    // Validación de cada usuario
    // Inserción si es único
  except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;
```

### Sistema de Correos Programados

**Flujo:**
1. Usuario crea correo con fecha futura
2. Correo se encola en `TColaCorreos`
3. Administrador o usuario puede "enviar" todos los correos programados
4. Los correos se desencolan y se entregan a destinatarios

### Gestión de Papelera

**Características:**
- Eliminación lógica (mover a papelera)
- Eliminación física (eliminar permanentemente)
- Búsqueda por palabra clave en asunto
- Estructura LIFO para recuperación

---

## Reportes y Visualización

### Generación con Graphviz

Todos los reportes se generan en formato DOT y se convierten a PNG:

```pascal
procedure GenerarReporte(nombreArchivo: String);
begin
  // Crear archivo DOT
  WriteLn(archivo, 'digraph MiEstructura {');
  WriteLn(archivo, '  rankdir=LR;');
  WriteLn(archivo, '  node [shape=record];');
  
  // Generar nodos y conexiones
  // ...
  
  WriteLn(archivo, '}');
  
  // Convertir a PNG si Graphviz está disponible
  if FileExists('/usr/bin/dot') then
    ExecuteProcess('/usr/bin/dot', ['-Tpng', nombreArchivo, '-o', nombrePNG]);
end;
```

### Tipos de Reportes

1. **Lista de Usuarios:** Muestra todos los usuarios del sistema
2. **Matriz de Relaciones:** Visualiza quién envía correos a quién y con qué frecuencia
3. **Bandeja de Entrada:** Correos recibidos por usuario
4. **Contactos:** Lista circular de contactos
5. **Papelera:** Correos eliminados (estructura de pila)
6. **Correos Programados:** Cola de correos pendientes
7. **Comunidades:** Estructura de lista de listas

---

## Consideraciones Técnicas

### Gestión de Memoria

- Uso extensivo de punteros para estructuras dinámicas
- Liberación adecuada de memoria en destructores
- Verificación de punteros nulos antes de acceso

### Validaciones Implementadas

1. **Usuarios únicos:** ID, email, usuario y teléfono
2. **Contactos válidos:** Solo usuarios existentes en el sistema
3. **Correos:** Solo a contactos agregados
4. **Integridad referencial:** Verificación de existencia antes de operaciones

### Optimizaciones

1. **Búsquedas:** Uso de email como clave primaria
2. **Matriz dispersa:** Solo nodos necesarios
3. **Carga masiva:** Validación batch con reporte consolidado
4. **Reportes:** Generación bajo demanda

### Manejo de Errores

- Try-catch para operaciones críticas
- Validación de entrada de usuario
- Mensajes informativos para el usuario
- Recuperación graceful ante errores

### Escalabilidad

- Estructuras dinámicas que crecen según necesidad
- Algoritmos eficientes para operaciones comunes
- Separación de responsabilidades entre módulos
- Diseño modular para fácil extensión

---

## Conclusiones

El sistema EDDMail implementa de manera eficiente múltiples estructuras de datos, cada una optimizada para su propósito específico. La arquitectura modular permite mantenimiento sencillo y extensibilidad futura. El uso de Object Pascal y GTK proporciona una base sólida para el desarrollo de aplicaciones de escritorio con interfaces gráficas nativas.

La implementación demuestra conceptos fundamentales de estructuras de datos como listas enlazadas, pilas, colas y matrices dispersas, aplicados en un contexto práctico y funcional.

---

*Manual Técnico desarrollado por Jairo Josue Gomez Reyes - R.A. 201801470*
*Curso: Estructuras de Datos - Universidad San Carlos de Guatemala*