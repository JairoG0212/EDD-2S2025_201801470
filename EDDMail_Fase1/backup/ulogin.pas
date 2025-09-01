unit ulogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ustructures, StdCtrls,
  umenuroot, umenuusuario;

type

  { TFormLogin }

  TFormLogin = class(TForm)
    btnLogin: TButton;
    btnCreateAcc: TButton;
    edtPassword: TEdit;
    edtEmail: TEdit;
    lblPassword: TLabel;
    lblEmail: TLabel;
    lblTitulo: TLabel;
    procedure btnCreateAccClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
  private

  public

  end;

var
  FormLogin: TFormLogin;

implementation

{$R *.lfm}

{ TFormLogin }

procedure TFormLogin.btnLoginClick(Sender: TObject);
var
  email, password: String;
  nodoUsuario: PNodoUsuario;
begin
  email := edtEmail.Text;
  password := edtPassword.Text;

  // Validar que los campos no estén vacíos
  if (email = '') or (password = '') then
  begin
    ShowMessage('Por favor complete todos los campos');
    Exit;
  end;

  // Verificar usuario root
  if (email = 'root@edd.com') and (password = 'root123') then
  begin
    ShowMessage('Bienvenido Administrador');
    //ocultar menu login
    Self.Hide;
    //mostrar el menu root
    FormMenuRoot.ShowModal;
    //cuando regrese, mostramos login nuevamente
    Self.Show;
    //limpiar campos
    edtEmail.Text:='';
    edtPassword.Text:='';
  end
  else
    begin
    // Buscar usuario normal en la lista
    nodoUsuario := listaUsuarios.Buscar(email);
    if (nodoUsuario <> nil) and (nodoUsuario^.usuario.password = password) then
    begin
      ShowMessage('Bienvenido ' + nodoUsuario^.usuario.nombre);
      //Ocultamos menu login
      Self.Hide;
      FormMenuUsuario.lblHolaAlumno.Caption:= 'Hola: ' + nodoUsuario^.usuario.usuario;
      //Mostramos menu usuario
      FormMenuUsuario.ShowModal;
      // cuando regrese hacemos que muestre menu login de nuevo
      Self.Show;
      //limpiamos campos de ingreso
      edtEmail.Text := '';
      edtPassword.Text := '';
    end
    else
    begin
      ShowMessage('Usuario o contraseña incorrectos');
    end;
  end;
end;

procedure TFormLogin.btnCreateAccClick(Sender: TObject);
var
  email, password, nombre, usuario, telefono: String;
  nuevoUsuario: TUsuario;
begin
  // Por ahora, solo mostraremos un mensaje
  ShowMessage('Funcionalidad de crear cuenta próximamente');

  // Limpiar campos
  edtEmail.Text := '';
  edtPassword.Text := '';
end;

end.

