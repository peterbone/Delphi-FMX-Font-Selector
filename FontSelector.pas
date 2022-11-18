unit FontSelector;

interface

uses
{$IFDEF MACOS}
MacApi.Appkit,Macapi.CoreFoundation, Macapi.Foundation,
{$ENDIF}
{$IFDEF MSWINDOWS}
Winapi.Messages, Winapi.Windows,
{$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.StdCtrls, FMX.Objects, FMX.Edit, FMX.ListBox;

type

  TFontSelector = class(TEdit)
  private
    FList : TListBox;
    FDropButton : TImage;
    FRect : TRectangle;
    FSymbolFonts: TStringList;
    FLastKey : Word;
    FFirstDrop : Boolean;

    FOnChange : TNotifyEvent;

    procedure DropButtonClick(Sender : TObject);
    procedure AutoComplete;
    procedure DoKeyDown(Sender: TObject; var Key: Word;
                      var KeyChar: Char; Shift: TShiftState);
    procedure Typing(Sender: TObject);
    procedure ListChange(Sender: TObject);
    function GetFont : String;
    procedure SetFont(AFontStr : String);
    function GetCount : Integer;
    procedure SetListHeight(AHeight : Integer);
    procedure ButtonMouseEnter(Sender: TObject);
    procedure ButtonMouseLeave(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    procedure SetParent(const AParent: TFmxObject); override;
    procedure ControlExit(Sender: TObject);
    function GetFontIndex(AIndex : Integer) : String;
    function IndexOf(AFontStr : String) : Integer;

    property Font : String read GetFont write SetFont;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property Count : Integer read GetCount;
    property ListHeight : Integer write SetListHeight;
  end;

procedure CollectFonts(FontList: TStringList);

const
  DefaultFont : String = 'Arial';
  TopFont : String = 'Arial'; // Font shown at the top of the list

implementation

uses System.Math, FMX.Forms;

constructor TFontSelector.Create(AOwner: TComponent);
var
  LFamList : TStringList;
  i : Integer;
  LFontStr : String;
  Item: TListBoxItem;
begin
  inherited Create(AOwner);

  KillFocusByReturn := True;
  FFirstDrop := True;

  OnKeyDown := DoKeyDown;
  OnTyping := Typing;
  OnExit := ControlExit;

  FList := TListBox.Create(AOwner);
  FList.ItemHeight := 22;
  FList.Visible := False;
  FList.OnChange := ListChange;
  FList.OnExit := ControlExit;

  FDropButton := TImage.Create(AOwner);
  FDropButton.OnClick := DropButtonClick;
  FDropButton.OnMouseEnter := ButtonMouseEnter;
  FDropButton.OnMouseLeave := ButtonMouseLeave;

  FRect := TRectangle.Create(AOwner);
  FRect.Stroke.Thickness := 1;
  FRect.Stroke.Color := $FFADADAD;
  FRect.Fill.Color := $FFE1E1E1;
  FRect.HitTest := False;

  // Symbol fonts in this list will not be shown in their own font
  FSymbolFonts := TStringList.Create;
  FSymbolFonts.Append('HoloLens MDL2 Assets');
  FSymbolFonts.Append('Marlett');
  FSymbolFonts.Append('Segoe MDL2 Assets');
  FSymbolFonts.Append('Symbol');
  FSymbolFonts.Append('Webdings');
  FSymbolFonts.Append('Wingdings');

  LFamList := TStringList.Create;
  CollectFonts(LFamList);

  if LFamList.Count <= 0 then LFamList.Append(DefaultFont); // incase CollectFonts fails
  LFamList.Sort;

  // Place TopFont at the top if it exists
  i := LFamList.IndexOf(TopFont);
  if i > 0 then LFamList.Move(i, 0);

  FList.BeginUpdate;

  for i := 0 to LFamList.Count - 1 do begin
    LFontStr := LFamList[i];

    //if LFontStr = 'Surfing Capital' then Continue; // heavy font, causes lag

    Item := TListBoxItem.Create(nil);
    Item.Parent := FList;
    Item.Text := LFontStr;

    // Set font of text in list, if not a symbol font
    Item.StyledSettings := Item.StyledSettings - [TStyledSetting.Family];
    if FSymbolFonts.IndexOf(LFontStr) < 0 then
      Item.TextSettings.Font.Family := LFontStr;
  end;

  i := LFamList.IndexOf(DefaultFont);
  if i < 0 then i := 0;
  FList.ItemIndex := i;
  Text := FList.ListItems[FList.ItemIndex].Text;

  FList.EndUpdate;

  // Allow edit to have it's font family set
  StyledSettings := StyledSettings - [TStyledSetting.Family];
  TextSettings.Font.Family := Text;

  LFamList.Free;
end;

destructor TFontSelector.Destroy;
begin
  FList.Free;
  FDropButton.Free;
  FRect.Free;

  inherited Destroy;
end;

// Position and size controls
procedure TFontSelector.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  FDropButton.Height := Self.Height;
  FDropButton.Width := 20;
  FDropButton.Position.Point := PointF(X + AWidth, Y);
  FDropButton.Bitmap.SetSize(Round(FDropButton.Width), Round(FDropButton.Height));
  FDropButton.Bitmap.BitmapScale := 1;

  FRect.Width := FDropButton.Width;
  FRect.Height := FDropButton.Height;
  FRect.Position := FDropButton.Position;
  FRect.SendToBack;

  FList.Position.X := X;
  FList.Position.Y := Y + AHeight;
  FList.Width := AWidth + FDropButton.Width;

  // Draw dropdown button graphics
  with FDropButton.Bitmap.Canvas do if BeginScene then begin
    Clear(TAlphaColorRec.Null);
    Stroke.Thickness := 2;
    Stroke.Color := TAlphaColorRec.Gray;
    DrawLine(PointF(6,10), PointF(10,14), 1);
    DrawLine(PointF(10,14), PointF(14,10), 1);
    EndScene;
  end;
end;

procedure TFontSelector.SetParent(const AParent: TFmxObject);
begin
  inherited SetParent(AParent);
  if not Assigned(FList) then Exit;

  FList.Parent := AParent;//Self;
  FDropButton.Parent := AParent;//Self;
  FRect.Parent := AParent;

  // To prevent error when destroying ???
  // Not needed if setting to Self above, but then cursor is wrong
  BringToFront;
end;

// Return the Font at a given list index. Default font if out of bounds
function TFontSelector.GetFontIndex(AIndex : Integer) : String;
begin
  if (AIndex >= 0) and (AIndex < FList.Count) then
    Result := FList.ListItems[AIndex].Text
  else
    Result := DefaultFont;
end;

// Return the currently selected font
function TFontSelector.GetFont : String;
begin
  Result := GetFontIndex(FList.ItemIndex);
end;

// Set the selected font
procedure TFontSelector.SetFont(AFontStr : String);
var
  Idx : Integer;
begin
  Idx := FList.Items.IndexOf(AFontStr);
  if Idx < 0 then Idx := FList.Items.IndexOf(DefaultFont);
  if Idx < 0 then Idx := 0;

  FList.ItemIndex := Idx; // Triggers ListChange
end;

// Return the list index of a given Font string. -1 if not found
function TFontSelector.IndexOf(AFontStr : String) : Integer;
begin
  Result := FList.Items.IndexOf(AFontStr);
end;

// Set the height of the dropdown list box
procedure TFontSelector.SetListHeight(AHeight : Integer);
begin
  FList.Height := AHeight;
end;

// Return the number of Fonts in the list
function TFontSelector.GetCount : Integer;
begin
  Result := FList.Count;
end;

// Highlight dropdown button when cursor is over it
procedure TFontSelector.ButtonMouseEnter(Sender: TObject);
begin
  FRect.Stroke.Color := $FF0078D7;
  FRect.Fill.Color := $FFE5F1FB;
end;

// Unhighlight dropdown button when cursor leaves
procedure TFontSelector.ButtonMouseLeave(Sender: TObject);
begin
  FRect.Stroke.Color := $FFADADAD;
  FRect.Fill.Color := $FFE1E1E1;
end;

// Show / hide the list box when clicking the drop down button
procedure TFontSelector.DropButtonClick(Sender : TObject);
begin
  FList.Visible := not FList.Visible;
  if not FList.Visible then ControlExit(Self);

  // Scroll to the selected font in the list
  // Doesn't work first time without ProcessMessages, because ContentLayout is nil
  if FList.Visible then begin
    if FFirstDrop then Application.ProcessMessages;
    FFirstDrop := False;
    FList.ScrollToItem(FList.ListItems[FList.ItemIndex]);
  end;
end;

// Hide the list box when another control has focus
procedure TFontSelector.ControlExit(Sender: TObject);
begin
  FList.Visible := False;
  
  // Check that text in the edit is valid
  if FList.Items.IndexOf(Text) < 0 then Text := GetFont;
end;

// Update the current selected font and hide the list box
procedure TFontSelector.ListChange(Sender: TObject);
begin
  if FList.ItemIndex < 0 then Exit;
  Text := FList.ListItems[FList.ItemIndex].Text;

  if FSymbolFonts.IndexOf(Text) < 0 then
    TextSettings.Font.Family := Text
  else
    TextSettings.Font.Family := DefaultFont;

  FList.Visible := False;

  // Call the on change event if set
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TFontSelector.DoKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  FLastKey := Key;
end;

procedure TFontSelector.Typing(Sender: TObject);
begin
  AutoComplete;
end;

procedure TFontSelector.AutoComplete;
var
  LFound : Boolean;
  LSrch, LText : String;
  i, LLen : Integer;
begin
  if (FLastKey = vkBack) or (FLastKey = vkDelete) or
     (FLastKey = vkLeft) or (FLastKey = vkRight) then begin
    FLastKey := 0;
    Exit;
  end;

  LSrch := LowerCase(Text);
  LLen := Length(LSrch);
  if LLen <= 0 then Exit;

  i := -1;
  Repeat
    Inc(i);
    LText := FList.ListItems[i].Text;
    LFound := LSrch = LowerCase(LText.Substring(0, Min(LLen, Length(LText))));
  Until LFound or (i = FList.Items.Count - 1);

  if LFound then begin
    FList.ItemIndex := -1; // Make sure ItemIndex updates
    FList.ItemIndex := i;
    //FList.Visible := True; // ??? keep this or not
    Text := FList.Items[i];
    SelStart  := LLen;
    SelLength := Length(Text) - LLen;
  end;
end;

{$IFDEF MSWINDOWS}
function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  S: TStrings;
  Temp: string;
begin
  S := TStrings(Data);
  Temp := LogFont.lfFaceName;

  if (Length(Temp) > 0) and (Temp[1] <> '@') and
     (AnsiCompareText(Temp, 'Modern') <> 0) and
     (AnsiCompareText(Temp, 'Roman' ) <> 0) and
     (AnsiCompareText(Temp, 'Script') <> 0) then // Old stroke fonts that don't work
    if LogFont.lfOutPrecision = OUT_STROKE_PRECIS then begin // Only TrueType fonts
      if (S.Count = 0) or (AnsiCompareText(S[S.Count-1], Temp) <> 0) then
        S.Add(Temp);
    end;
  Result := 1;
end;
{$ENDIF}

// Collect all installed true type fonts
procedure CollectFonts(FontList: TStringList);
var
{$IFDEF MACOS}
  fManager: NsFontManager;
  list:NSArray;
  lItem:NSString;
  i: Integer;
  Temp : String;
{$ENDIF}
{$IFDEF MSWINDOWS}
  DC: HDC;
  LFont: TLogFont;
{$ENDIF}
begin
  {$IFDEF MACOS}
    fManager := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
    list := fManager.availableFontFamilies;
    if (List <> nil) and (List.count > 0) then
    begin
      for i := 0 to List.Count-1 do
      begin
        lItem := TNSString.Wrap(List.objectAtIndex(i));
        Temp := String(lItem.UTF8String);
        if Temp[1] <> '@' then FontList.Add(Temp)
      end;
    end;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    DC := GetDC(0);
    FillChar(LFont, sizeof(LFont), 0);
    LFont.lfCharset := DEFAULT_CHARSET;
    EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, Winapi.Windows.LPARAM(FontList), 0);
    ReleaseDC(0, DC);
  {$ENDIF}
end;

end.
