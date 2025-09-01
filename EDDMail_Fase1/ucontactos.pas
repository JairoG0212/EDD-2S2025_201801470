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
begin
  if (contactoActual <> nil) then
  begin
    contactoActual := contactoActual^.siguiente;
    lblEmail.Caption := contactoActual^.email;
  end;
end;

procedure TFormContactos.btnAnteriorClick(Sender: TObject);
var
  actual: PNodoContacto;
begin
  if (contactoActual <> nil) then
  begin
    // Para ir hacia atrás en una lista circular, necesitamos encontrar el nodo anterior
    actual := contactoActual;
    while actual^.siguiente <> contactoActual do
      actual := actual^.siguiente;

    contactoActual := actual;
    lblEmail.Caption := contactoActual^.email;
  end;
end;

procedure TFormContactos.btnCerrarClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormContactos.CargarContactos;
begin
  if (usuarioActual <> nil) and not usuarioActual^.usuario.contactos^.Vacia then
  begin
    contactoActual := usuarioActual^.usuario.contactos^.ObtenerPrimero; // Primero de la lista circular
    lblEmail.Caption := contactoActual^.email;
  end
  else
  begin
    contactoActual := nil;
    lblEmail.Caption := 'Sin contactos';
  end;
end;

end.

