unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FontSelector, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    { Private declarations }
    FontSelector : TFontSelector;

    procedure ChangeFont(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Create the Font Selector component
  // It has an issue with locking up when reaching a heavy font, such as 'Surfing Capital'
  FontSelector := TFontSelector.Create(Self);
  FontSelector.Parent := Self;
  FontSelector.Position.Point := PointF(16,200);
  FontSelector.Width := 250;
  FontSelector.Height := 25;
  FontSelector.ListHeight := 360;
  FontSelector.OnChange := ChangeFont;

  Caption := Caption + ' : ' + IntToStr(FontSelector.Count) + ' fonts found';
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FontSelector.ControlExit(Self);
end;

procedure TForm1.ChangeFont(Sender: TObject);
begin
  Label1.TextSettings.Font.Family := FontSelector.Font;
end;

end.
