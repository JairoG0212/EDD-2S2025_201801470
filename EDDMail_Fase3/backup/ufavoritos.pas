unit ufavoritos;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, ustructures;

type

  { TFormFavoritos }

  TFormFavoritos = class(TForm)
    btnEliminar: TButton;
    btnCerrar: TButton;
    lblMensaje: TLabel;
    lblFecha: TLabel;
    lblAsunto: TLabel;
    lblRemitente: TLabel;
    lblTotal: TLabel;
    lblTitulo: TLabel;
    lvFavoritos: TListView;
    memoMensaje: TMemo;
    pnlDetalles: TPanel;

  private

  public

  end;

var
  FormFavoritos: TFormFavoritos;

implementation

{$R *.lfm}

{ TFormFavoritos }


end.

