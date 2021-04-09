unit ViBindings;

interface

uses
  Classes,
  ToolsAPI,
  Winapi.Windows;

type
  TDirection = (dForward, dBack);
  TBlockAction = (baDelete, baYank);

  TViRegister = record
    IsLine: Boolean;
    Text: String;
  end;

  TViAction = record
    ActionChar: Char;
    FInDelete, FInChange: Boolean;
    FEditCount, FCount: Integer;
    FInsertText: string;
  end;

  TViBindings = class(TObject)
  private
    FInsertMode: Boolean;
    FActive: Boolean;
    FParsingNumber: Boolean;
    FInDelete: Boolean;
    FInChange: Boolean;
    FInGo: Boolean;
    FInMark: Boolean;
    FInGotoMark: Boolean;
    FInRepeatChange: Boolean;
    FEditCount: Integer;
    FCount: Integer;
    FSelectedRegister: Integer;
    FPreviousAction: TViAction;
    FInsertText: String;
    FMarkArray: array [0 .. 255] of TOTAEditPos;
    FRegisterArray: array [0 .. 255] of TViRegister;
    FInYank: Boolean;
    FChar: Char;
    FShift: TShiftState;
    FEditPosition: IOTAEditPosition;
    FBuffer: IOTAEditBuffer;
    procedure ChangeIndentation(Direction: TDirection);
    function DeleteSelection: Boolean;
    function GetCount: Integer;
    function GetEditCount: Integer;
    procedure ResetCount;
    procedure UpdateCount;
    function GetPositionForMove(key: Char; count: Integer = 0): TOTAEditPos;
    procedure ProcessMovement;
    function IsMovementKey: Boolean;
    procedure MoveToMarkPosition;
    procedure Paste(const EditPosition: IOTAEditPosition; const Buffer: IOTAEditBuffer; Direction: TDirection);
    procedure SaveMarkPosition;
    procedure SetInsertMode(const Value: Boolean);
    function YankSelection: Boolean;
    procedure ApplyActionToBlock(Action: TBlockAction; IsLine: Boolean);
    procedure FindNextWordAtCursor(const count: Integer);
    procedure FindPreviousWordAtCursor;
    procedure FindWordAtCursor(const View: IOTAEditView; const count: Integer);
    procedure HandleChar(const c: Char);
    procedure ProcessChange;
    procedure ProcessDeletion;
    procedure ProcessAction;
    procedure ProcessLineDeletion;
    procedure ProcessLineYanking;
    procedure ProcessYanking;
    procedure SavePreviousAction;
    procedure SwitchToInsertModeOrDoPreviousAction;
  public
    constructor Create;
    procedure EditKeyDown(key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
    procedure EditChar(key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
    procedure ConfigureCursor;
    property count: Integer read GetCount;
    property InsertMode: Boolean read FInsertMode write SetInsertMode;
    property Active: Boolean read FActive write FActive;
  end;

implementation

uses
  System.SysUtils,
  System.Math;

function QuerySvcs(const Instance: IUnknown; const Intf: TGUID; out Inst): Boolean;
begin
  Result := (Instance <> nil) and Supports(Instance, Intf, Inst);
end;

function GetEditBuffer: IOTAEditBuffer;
var
  iEditorServices: IOTAEditorServices;
begin
  QuerySvcs(BorlandIDEServices, IOTAEditorServices, iEditorServices);
  if iEditorServices <> nil then
  begin
    Result := iEditorServices.GetTopBuffer;
    Exit;
  end;
  Result := nil;
end;

function GetEditPosition(Buffer: IOTAEditBuffer): IOTAEditPosition;
begin
  Result := nil;
  if Buffer <> nil then
    Result := Buffer.GetEditPosition;
end;

procedure TViBindings.ConfigureCursor;
var
  EditBuffer: IOTAEditBuffer;
begin
  EditBuffer := GetEditBuffer;
  if EditBuffer <> nil then
    EditBuffer.EditOptions.UseBriefCursorShapes := not FInsertMode;
end;

constructor TViBindings.Create;
begin
  Active := True;
  InsertMode := False;
end;

procedure TViBindings.EditChar(key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
begin
  if not Active then
    Exit;

  if InsertMode then
    Exit;

  FShift := Shift;
  HandleChar(Chr(key));
  Handled := True;
  (BorlandIDEServices As IOTAEditorServices).TopView.Paint;
end;

procedure TViBindings.EditKeyDown(key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);

  function GetTopMostEditView: IOTAEditView;
  var
    EditBuffer: IOTAEditBuffer;
  begin
    Result := nil;
    EditBuffer := GetEditBuffer;
    if EditBuffer <> nil then
      Exit(EditBuffer.GetTopView);
  end;

begin
  if not Active then
    Exit;

  if InsertMode then
  begin
    if (key = VK_ESCAPE) then
    begin
      GetTopMostEditView.Buffer.BufferOptions.InsertMode := True;
      InsertMode := False;
      Handled := True;
      Self.FPreviousAction.FInsertText := FInsertText;
      FInsertText := '';
    end;
  end
  else
  begin
    if (((key >= Ord('A')) and (key <= Ord('Z'))) or ((key >= Ord('0')) and (key <= Ord('9'))) or
      ((key >= 186) and (key <= 192)) or ((key >= 219) and (key <= 222))) and not((ssCtrl in Shift) or (ssAlt in Shift))
      and not InsertMode then
    begin
      // If the keydown is a standard keyboard press not altered with a ctrl or alt key
      // then create a WM_CHAR message so we can do all the locale mapping of the keyboard
      // and then handle the resulting key in TViBindings.EditChar
      // XXX can we switch to using ToAscii like we do for setting FInsertText
      TranslateMessage(Msg);
      Handled := True;
    end;
  end;
end;

function TViBindings.IsMovementKey: Boolean;
begin
  if (FChar = '0') and FParsingNumber then
    Exit(False);

  Result := CharInSet(FChar, ['0', '$', 'b', 'B', 'e', 'E', 'h', 'j', 'k', 'l', 'w', 'W']);
end;

procedure TViBindings.ResetCount;
begin
  FCount := 0;
  FParsingNumber := False;
end;

procedure TViBindings.UpdateCount;
begin
  FParsingNumber := True;
  if CharInSet(FChar, ['0' .. '9']) then
    FCount := 10 * FCount + (Ord(FChar) - Ord('0'));
end;

type
  TViCharClass = (viWhiteSpace, viWord, viSpecial);

procedure TViBindings.ApplyActionToBlock(Action: TBlockAction; IsLine: Boolean);
var
  count: Integer;
  Pos: TOTAEditPos;
  EditBlock: IOTAEditBlock;
begin
  count := GetCount * GetEditCount;
  ResetCount;
  Pos := GetPositionForMove(FChar, count);
  if CharInSet(FChar, ['e', 'E']) then
    Pos.Col := Pos.Col + 1;

  EditBlock := FBuffer.EditBlock;
  EditBlock.Reset;
  EditBlock.BeginBlock;
  EditBlock.Extend(Pos.Line, Pos.Col);
  FRegisterArray[FSelectedRegister].IsLine := IsLine;
  FRegisterArray[FSelectedRegister].Text := EditBlock.Text;

  case Action of
    baDelete:
      EditBlock.Delete;
    baYank:
      EditBlock.Reset;
  end;

  EditBlock.EndBlock;
end;

procedure TViBindings.ChangeIndentation(Direction: TDirection);
var
  EditBlock: IOTAEditBlock;
  StartedBlock: Boolean;
begin
  StartedBlock := False;
  EditBlock := FBuffer.EditBlock;
  EditBlock.Save;
  FEditPosition.Save;

  if EditBlock.Size = 0 then
  begin
    StartedBlock := True;
    FEditPosition.MoveBOL;
    EditBlock.Reset;
    EditBlock.BeginBlock;
    EditBlock.Extend(FEditPosition.Row, FEditPosition.Column + 1);
  end
  else
  begin
    // When selecting multiple lines, if the cursor is in the first column the last line doesn't get into the block
    // and the indent seems buggy, as the cursor is on the last line but it isn't indented, so we force
    // the selection of at least one char to correct this behavior
    EditBlock.ExtendRelative(0, 1);
  end;

  case Direction of
    dForward:
      EditBlock.Indent(FBuffer.EditOptions.BlockIndent);
    dBack:
      EditBlock.Indent(-FBuffer.EditOptions.BlockIndent);
  end;

  // If we don't call EndBlock, the selection gets buggy.
  if StartedBlock then
    EditBlock.EndBlock;

  FEditPosition.Restore;
  EditBlock.Restore;
end;

function TViBindings.DeleteSelection: Boolean;
var
  EditBlock: IOTAEditBlock;
begin
  EditBlock := FBuffer.EditBlock;
  if EditBlock.Size = 0 then
    Exit(False);

  FRegisterArray[FSelectedRegister].IsLine := False;
  FRegisterArray[FSelectedRegister].Text := EditBlock.Text;
  EditBlock.Delete;
  Result := True;
end;

procedure TViBindings.FindNextWordAtCursor(const count: Integer);
var
  EditBlock: IOTAEditBlock;
  I: Integer;
begin
  EditBlock := FBuffer.EditBlock;
  EditBlock.Reset;
  EditBlock.BeginBlock;
  EditBlock.ExtendRelative(0, Length(FEditPosition.SearchOptions.SearchText));
  if AnsiSameText(FEditPosition.SearchOptions.SearchText, EditBlock.Text) then
    FEditPosition.MoveRelative(0, Length(FEditPosition.SearchOptions.SearchText));
  EditBlock.EndBlock;

  FEditPosition.SearchOptions.Direction := sdForward;

  for I := 1 to count do
    FEditPosition.SearchAgain;

  FEditPosition.MoveRelative(0, -Length(FEditPosition.SearchOptions.SearchText));
end;

procedure TViBindings.FindPreviousWordAtCursor;
var
  I: Integer;
begin
  FEditPosition.SearchOptions.Direction := sdBackward;
  for I := 1 to count do
    FEditPosition.SearchAgain;
end;

procedure TViBindings.FindWordAtCursor(const View: IOTAEditView; const count: Integer);
var
  EditBlock: IOTAEditBlock;
  Pos: TOTAEditPos;
  I: Integer;
begin
  EditBlock := FBuffer.EditBlock;
  if FEditPosition.IsWordCharacter then
    FEditPosition.MoveCursor(mmSkipWord or mmSkipLeft)
  else
    FEditPosition.MoveCursor(mmSkipNonWord or mmSkipRight or mmSkipStream);

  Pos := GetPositionForMove('e', 1);

  EditBlock := FBuffer.EditBlock;
  EditBlock.Reset;
  EditBlock.BeginBlock;
  EditBlock.Extend(Pos.Line, Pos.Col + 1);
  FEditPosition.SearchOptions.SearchText := EditBlock.Text;
  EditBlock.EndBlock;

  // Move to one position after what we're searching for.
  FEditPosition.Move(Pos.Line, Pos.Col + 1);

  FEditPosition.SearchOptions.CaseSensitive := False;
  FEditPosition.SearchOptions.Direction := sdForward;
  FEditPosition.SearchOptions.FromCursor := True;
  FEditPosition.SearchOptions.RegularExpression := False;
  FEditPosition.SearchOptions.WholeFile := True;
  FEditPosition.SearchOptions.WordBoundary := True;

  for I := 1 to count do
    FEditPosition.SearchAgain;

  // Move back to the start of the text we searched for.
  FEditPosition.MoveRelative(0, -Length(FEditPosition.SearchOptions.SearchText));

  View.MoveViewToCursor;
end;

// Given a movement key and a count return the position in the buffer where that movement would take you.
// TOTAEditPos
function TViBindings.GetCount: Integer;
begin
  Result := IfThen(FCount <= 0, 1, FCount);
end;

function TViBindings.GetEditCount: Integer;
begin
  Result := IfThen(FEditCount > 0, FEditCount, 1);
end;

function TViBindings.GetPositionForMove(key: Char; count: Integer = 0): TOTAEditPos;
var
  Pos: TOTAEditPos;
  I: Integer;
  nextChar: TViCharClass;

  function CharAtRelativeLocation(Col: Integer): TViCharClass;
  begin
    FEditPosition.Save;
    FEditPosition.MoveRelative(0, Col);
    if FEditPosition.IsWhiteSpace or (FEditPosition.Character = #$D) then
    begin
      Result := viWhiteSpace
    end
    else if FEditPosition.IsWordCharacter then
    begin
      Result := viWord;
    end
    else
    begin
      Result := viSpecial;
    end;
    FEditPosition.Restore;
  end;

begin
  FEditPosition.Save;

  case key of
    '0':
      FEditPosition.MoveBOL;
    '$':
      begin
        FEditPosition.MoveEOL;
        // When moving, must stop at last char, not on line break.
        if (not FInDelete) and (not FInChange) and (not FInYank) then
          FEditPosition.MoveRelative(0, -1);
      end;
    'b':
      begin
        for I := 1 to count do
        begin
          nextChar := CharAtRelativeLocation(-1);
          if FEditPosition.IsWordCharacter and ((nextChar = viSpecial) or (nextChar = viWhiteSpace)) then
            FEditPosition.MoveRelative(0, -1);

          if FEditPosition.IsSpecialCharacter and ((nextChar = viWord) or (nextChar = viWhiteSpace)) then
            FEditPosition.MoveRelative(0, -1);

          if FEditPosition.IsWhiteSpace then
          begin
            FEditPosition.MoveCursor(mmSkipWhite or mmSkipLeft or mmSkipStream);
            FEditPosition.MoveRelative(0, -1);
          end;

          if FEditPosition.IsWordCharacter then
            FEditPosition.MoveCursor(mmSkipWord or mmSkipLeft) // Skip to first non word character.
          else if FEditPosition.IsSpecialCharacter then
            FEditPosition.MoveCursor(mmSkipSpecial or mmSkipLeft); // Skip to the first non special character
        end;
      end;
    'B':
      begin
        for I := 1 to count do
        begin
          FEditPosition.MoveCursor(mmSkipWhite or mmSkipLeft or mmSkipStream);
          FEditPosition.MoveCursor(mmSkipNonWhite or mmSkipLeft);
        end;
      end;
    'e':
      begin
        for I := 1 to count do
        begin
          nextChar := CharAtRelativeLocation(1);
          if (FEditPosition.IsWordCharacter and (nextChar = viWhiteSpace) or (nextChar = viSpecial)) then
            FEditPosition.MoveRelative(0, 1);

          if (FEditPosition.IsSpecialCharacter and (nextChar = viWhiteSpace) or (nextChar = viWord)) then
            FEditPosition.MoveRelative(0, 1);

          if FEditPosition.IsWhiteSpace then
            FEditPosition.MoveCursor(mmSkipWhite or mmSkipRight or mmSkipStream);

          if FEditPosition.IsSpecialCharacter then
            FEditPosition.MoveCursor(mmSkipSpecial or mmSkipRight);

          if FEditPosition.IsWordCharacter then
            FEditPosition.MoveCursor(mmSkipWord or mmSkipRight);

          FEditPosition.MoveRelative(0, -1);
        end;
      end;
    'E':
      begin
        for I := 1 to count do
        begin
          if (FEditPosition.IsWordCharacter or FEditPosition.IsSpecialCharacter) and
            (CharAtRelativeLocation(1) = viWhiteSpace) then
            FEditPosition.MoveRelative(0, 1);

          if FEditPosition.IsWhiteSpace then
            FEditPosition.MoveCursor(mmSkipWhite or mmSkipRight or mmSkipStream);

          FEditPosition.MoveCursor(mmSkipNonWhite or mmSkipRight);
          FEditPosition.MoveRelative(0, -1);
        end;
      end;
    'h':
      FEditPosition.MoveRelative(0, -count);
    'j':
      FEditPosition.MoveRelative(+count, 0);
    'k':
      FEditPosition.MoveRelative(-count, 0);
    'l':
      FEditPosition.MoveRelative(0, +count);
    'w':
      begin
        for I := 1 to count do
        begin
          if FEditPosition.IsWordCharacter then
            FEditPosition.MoveCursor(mmSkipWord or mmSkipRight) // Skip to first non word character.
          else if FEditPosition.IsSpecialCharacter then
            FEditPosition.MoveCursor(mmSkipSpecial or mmSkipRight or mmSkipStream);
          // Skip to the first non special character

          // If the character is whitespace or EOL then skip that whitespace
          if FEditPosition.IsWhiteSpace or (FEditPosition.Character = #$D) then
            FEditPosition.MoveCursor(mmSkipWhite or mmSkipRight or mmSkipStream);
        end;
      end;
    'W':
      begin
        for I := 1 to count do
        begin
          // Goto first white space after the end of the word.
          FEditPosition.MoveCursor(mmSkipNonWhite or mmSkipRight);
          // Now skip all the white space until we're at the start of a word again.
          FEditPosition.MoveCursor(mmSkipWhite or mmSkipRight or mmSkipStream);
        end;
      end;
  end;

  Pos.Col := FEditPosition.Column;
  Pos.Line := FEditPosition.Row;
  FEditPosition.Restore;

  Result := Pos;
end;

procedure TViBindings.HandleChar(const c: Char);
begin
  FChar := c;
  FBuffer := GetEditBuffer;
  FEditPosition := GetEditPosition(FBuffer);
  try
    if FInMark then
      SaveMarkPosition
    else if FInGotoMark then
      MoveToMarkPosition
    else if IsMovementKey then
      ProcessMovement
    else if CharInSet(FChar, ['0' .. '9']) then
      UpdateCount
    else if (FInDelete and (FChar = 'd')) then
      ProcessLineDeletion
    else if FInYank and (FChar = 'y') then
      ProcessLineYanking
    else
      ProcessAction;
  finally
    // Avoid dangling reference error when closing the IDE
    FBuffer := nil;
    FEditPosition := nil;
  end;
end;

procedure TViBindings.ProcessChange;
begin
  if FInRepeatChange then
  begin
    ApplyActionToBlock(baDelete, False);
    FEditPosition.InsertText(FPreviousAction.FInsertText)
  end
  else
  begin
    if (FChar = 'w') then
      FChar := 'e';
    if (FChar = 'W') then
      FChar := 'E';
    SavePreviousAction;
    ApplyActionToBlock(baDelete, False);
    InsertMode := True;
  end;
  FInChange := False;
end;

procedure TViBindings.ProcessDeletion;
begin
  if not FInRepeatChange then
    SavePreviousAction;

  ApplyActionToBlock(baDelete, False);
  FInDelete := False;
end;

procedure TViBindings.ProcessAction;
var
  View: IOTAEditView;
begin
  View := FBuffer.TopView;
  case FChar of
    'a':
      begin
        FEditPosition.MoveRelative(0, 1);
        SwitchToInsertModeOrDoPreviousAction;
      end;
    'A':
      begin
        FEditPosition.MoveEOL;
        SwitchToInsertModeOrDoPreviousAction;
      end;
    'c':
      begin
        if FInChange then
        begin
          FEditPosition.MoveBOL;
          HandleChar('$');
        end
        else
        begin
          if DeleteSelection then
            SwitchToInsertModeOrDoPreviousAction
          else
          begin
            FInChange := True;
            FEditCount := count;
          end
        end;
      end;
    'C':
      begin
        FInChange := True;
        FEditCount := count;
        HandleChar('$');
      end;
    'd':
      begin
        if not DeleteSelection then
        begin
          FInDelete := True;
          FEditCount := count;
        end;
      end;
    'D':
      begin
        FInDelete := True;
        HandleChar('$');
      end;
    'g':
      begin
        if FInGo then
        begin
          FEditPosition.Move(1, 1);
          FInGo := False;
        end
        else
        begin
          FInGo := True;
          FEditCount := count;
        end
      end;
    'G':
      begin
        if FParsingNumber then
          FEditPosition.GotoLine(count)
        else
          FEditPosition.MoveEOF;
      end;
    'H':
      begin
        FEditPosition.Move(FBuffer.TopView.TopRow, 0);
        FEditPosition.MoveBOL;
      end;
    'i':
      SwitchToInsertModeOrDoPreviousAction;
    'I':
      begin
        FEditPosition.MoveBOL;
        SwitchToInsertModeOrDoPreviousAction;
      end;
    'J':
      begin
        FEditPosition.MoveEOL;
        FEditPosition.Delete(1);
      end;
    'L':
      begin
        FEditPosition.Move(View.BottomRow - 1, 0);
        FEditPosition.MoveBOL;
      end;
    'm':
      FInMark := True;
    'M':
      begin
        FEditPosition.Move(View.TopRow + Trunc(((View.BottomRow - 1) - View.TopRow) / 2), 0);
        FEditPosition.MoveBOL;
      end;
    'n':
      FindNextWordAtCursor(count);
    'N':
      FindPreviousWordAtCursor;
    'o':
      begin
        FEditPosition.MoveEOL;
        FEditPosition.InsertText(#13#10);
        SwitchToInsertModeOrDoPreviousAction;
        (BorlandIDEServices As IOTAEditorServices).TopView.MoveViewToCursor;
      end;
    'O':
      begin
        FEditPosition.MoveBOL;
        FEditPosition.InsertText(#13#10);
        FEditPosition.MoveCursor(mmSkipWhite or mmSkipRight);
        FEditPosition.MoveRelative(-1, 0);
        SwitchToInsertModeOrDoPreviousAction;
        (BorlandIDEServices As IOTAEditorServices).TopView.MoveViewToCursor;
      end;
    'p':
      Paste(FEditPosition, FBuffer, dForward);
    'P':
      Paste(FEditPosition, FBuffer, dBack);
    'R':
      begin
        // XXX Fix me for '.' command
        FBuffer.BufferOptions.InsertMode := False;
        InsertMode := True;
      end;
    's':
      begin
        if not DeleteSelection then
          FEditPosition.Delete(1);
        SwitchToInsertModeOrDoPreviousAction;
      end;
    'S':
      begin
        FInChange := True;
        FEditPosition.MoveBOL;
        HandleChar('$');
      end;
    'u':
      FBuffer.Undo;
    'x':
      begin
        if not DeleteSelection then
        begin
          FInDelete := True;
          FEditCount := count - 1;
          HandleChar('l');
        end;
      end;
    'X':
      begin
        FInDelete := True;
        if DeleteSelection then
          HandleChar('d')
        else
        begin
          FEditCount := count - 1;
          HandleChar('h');
        end
      end;
    'y':
      begin
        FInYank := not YankSelection;
        if FInYank then
          FEditCount := count;
      end;
    'Y':
      begin
        FInYank := True;
        FEditCount := count;
        HandleChar('y');
      end;
    '.':
      begin
        FInRepeatChange := True;
        FInDelete := FPreviousAction.FInDelete;
        FInChange := FPreviousAction.FInChange;
        FEditCount := FPreviousAction.FEditCount;
        FCount := FPreviousAction.FCount;
        HandleChar(FPreviousAction.ActionChar);
        FInRepeatChange := False;
      end;
    '*':
      FindWordAtCursor(View, count);
    '''':
      FInGotoMark := True;
    '^':
      begin
        FEditPosition.MoveBOL;
        FEditPosition.MoveCursor(mmSkipWhite);
      end;
    '>':
      begin
        SavePreviousAction;
        ChangeIndentation(dForward);
      end;
    '<':
      begin
        SavePreviousAction;
        ChangeIndentation(dBack);
      end;
  end;
  ResetCount;
end;

procedure TViBindings.ProcessLineDeletion;
begin
  if not FInRepeatChange then
    SavePreviousAction;

  FEditPosition.MoveBOL;
  FChar := 'j';
  ApplyActionToBlock(baDelete, True);
  FInDelete := False;
end;

procedure TViBindings.ProcessLineYanking;
begin
  FEditPosition.Save;
  FEditPosition.MoveBOL;
  FChar := 'j';
  ApplyActionToBlock(baYank, True);
  FEditPosition.Restore;
  FInYank := False;
end;

procedure TViBindings.ProcessMovement;
var
  Pos: TOTAEditPos;
begin
  if FInDelete then
    ProcessDeletion
  else if FInChange then
    ProcessChange
  else if FInYank then
    ProcessYanking
  else
  begin
    Pos := GetPositionForMove(FChar, GetCount);
    FEditPosition.Move(Pos.Line, Pos.Col);
    FInGo := False;
  end;
  ResetCount;
end;

procedure TViBindings.ProcessYanking;
begin
  FEditPosition.Save;
  ApplyActionToBlock(baYank, False);
  FEditPosition.Restore;
  FInYank := False;
end;

procedure TViBindings.MoveToMarkPosition;
begin
  FEditPosition.Move(FMarkArray[Ord(FChar)].Line, FMarkArray[Ord(FChar)].Col);
  FInGotoMark := False;
end;

procedure TViBindings.Paste(const EditPosition: IOTAEditPosition; const Buffer: IOTAEditBuffer; Direction: TDirection);
var
  AutoIdent, PastingInSelection: Boolean;
  EditBlock: IOTAEditBlock;
  Row, Col: Integer;

  function FixCursorPosition: Boolean;
  begin
    Result := (not PastingInSelection) and (Direction = dForward);
  end;

begin
  SavePreviousAction;
  PastingInSelection := False;
  AutoIdent := Buffer.BufferOptions.AutoIndent;

  EditBlock := Buffer.EditBlock;
  if EditBlock.Size > 0 then
  begin
    PastingInSelection := True;
    Row := EditBlock.StartingRow;
    Col := EditBlock.StartingColumn;
    EditBlock.Delete;
    EditPosition.Move(Row, Col);
  end;

  if (FRegisterArray[FSelectedRegister].IsLine) then
  begin
    Buffer.BufferOptions.AutoIndent := False;
    EditPosition.MoveBOL;

    if FixCursorPosition then
      EditPosition.MoveRelative(1, 0);

    EditPosition.Save;
    EditPosition.InsertText(FRegisterArray[FSelectedRegister].Text);
    EditPosition.Restore;
    Buffer.BufferOptions.AutoIndent := AutoIdent;
  end
  else
  begin
    if FixCursorPosition then
      EditPosition.MoveRelative(0, 1);

    EditPosition.InsertText(FRegisterArray[FSelectedRegister].Text);
  end;
end;

procedure TViBindings.SaveMarkPosition;
begin
  FMarkArray[Ord(FChar)].Col := FEditPosition.Column;
  FMarkArray[Ord(FChar)].Line := FEditPosition.Row;
  FInMark := False;
end;

procedure TViBindings.SavePreviousAction;
begin
  // TODO: Save the new actions
  FPreviousAction.ActionChar := FChar;
  FPreviousAction.FInDelete := FInDelete;
  FPreviousAction.FInChange := FInChange;
  FPreviousAction.FEditCount := FEditCount;
  FPreviousAction.FCount := FCount;
  // self.FPreviousAction.FInsertText := FInsertText;
end;

procedure TViBindings.SetInsertMode(const Value: Boolean);
begin
  FInsertMode := Value;
  ConfigureCursor;
end;

procedure TViBindings.SwitchToInsertModeOrDoPreviousAction;
begin
  if (FInRepeatChange) then
    FEditPosition.InsertText(FPreviousAction.FInsertText)
  else
  begin
    SavePreviousAction;
    InsertMode := True;
  end;
end;

function TViBindings.YankSelection: Boolean;
var
  EditBlock: IOTAEditBlock;
begin
  EditBlock := FBuffer.EditBlock;
  if EditBlock.Size = 0 then
    Exit(False);

  FRegisterArray[FSelectedRegister].IsLine := False;
  FRegisterArray[FSelectedRegister].Text := EditBlock.Text;
  EditBlock.Reset;
  Result := True;
end;

end.
