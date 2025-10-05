unit ucontactos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ustructures;

type

  { TFormContactos }

  TFormContactos = class(TForm)
    btnAnterior: TButton;
    btnCerrar: TButton;
    btnSiguiente: TButton;
    lblEmail: TLabel;
    lblTitulo: TLabel;
    procedure btnAnteriorClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    procedure btnSiguienteClick(Sender: TObject);
  private
    contactoActual: PNodoContacto;
  public
    procedure CargarContactos;
  end;

var
  FormContactos: TFormContactos;

implementation

{$R *.lfm}

procedure TFormContactos.btnSiguienteClick(Sender: TObject);
var
  nodoUsuario: PNodoUsuario;
begin
  if (contactoActual <> nil) then
  begin
    contactoActual := contactoActual^.siguiente;

    // Buscar información completa del usuario
    nodoUsuario := listaUsuarios.Buscar(contactoActual^.email);

    if nodoUsuario <> nil then
    begin
      lblEmail.Caption := 'Nombre: ' + nodoUsuario^.usuario.nombre + #13#10 +
                         'Usuario: ' + nodoUsuario^.usuario.usuario + #13#10 +
                         'Email: ' + nodoUsuario^.usuario.email + #13#10 +
                         'Teléfono: ' + nodoUsuario^.usuario.telefono;
    end
    else
    begin
      lblEmail.Caption := 'Email: ' + contactoActual^.email + #13#10 +
                         '(Información no disponible)';
    end;
  end;
end;

procedure TFormContactos.btnAnteriorClick(Sender: TObject);
var
  actual: PNodoContacto;
  nodoUsuario: PNodoUsuario;
begin
  if (contactoActual <> nil) then
  begin
    // Para ir hacia atrás en una lista circular, necesitamos encontrar el nodo anterior
    actual := contactoActual;
    while actual^.siguiente <> contactoActual do
      actual := actual^.siguiente;

    contactoActual := actual;

    // Buscar información completa del usuario
    nodoUsuario := listaUsuarios.Buscar(contactoActual^.email);

    if nodoUsuario <> nil then
    begin
      lblEmail.Caption := 'Nombre: ' + nodoUsuario^.usuario.nombre + #13#10 +
                         'Usuario: ' + nodoUsuario^.usuario.usuario + #13#10 +
                         'Email: ' + nodoUsuario^.usuario.email + #13#10 +
                         'Teléfono: ' + nodoUsuario^.usuario.telefono;
    end
    else
    begin
      lblEmail.Caption := 'Email: ' + contactoActual^.email + #13#10 +
                         '(Información no disponible)';
    end;
  end;
end;

procedure TFormContactos.btnCerrarClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormContactos.CargarContactos;
var
  nodoUsuario: PNodoUsuario;
begin
  if (usuarioActual <> nil) and not usuarioActual^.usuario.contactos^.Vacia then
  begin
    contactoActual := usuarioActual^.usuario.contactos^.ObtenerPrimero; // Primero de la lista circular

    // Buscar información completa del usuario
    nodoUsuario := listaUsuarios.Buscar(contactoActual^.email);

    if nodoUsuario <> nil then
    begin
      lblEmail.Caption := 'Nombre: ' + nodoUsuario^.usuario.nombre + #13#10 +
                         'Usuario: ' + nodoUsuario^.usuario.usuario + #13#10 +
                         'Email: ' + nodoUsuario^.usuario.email + #13#10 +
                         'Teléfono: ' + nodoUsuario^.usuario.telefono;
    end
    else
    begin
      lblEmail.Caption := 'Email: ' + contactoActual^.email + #13#10 +
                         '(Información no disponible)';
    end;
  end
  else
  begin
    contactoActual := nil;
    lblEmail.Caption := 'Sin contactos';
  end;
end;

end.

