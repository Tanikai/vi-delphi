{
  This file contains the implementation of Vi keybinds in the Delphi IDE.

  Copyright (c) 2016 Peter Ross
  Copyright (C) 2021  Kai Anter

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.
}

unit ViEngine;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.Math,
  ToolsAPI,
  Winapi.Windows;

type
  TP_ModeChanged = reference to procedure(AMode: String);

  TViMode = (mInactive, mNormal, mInsert, mVisual);

  TViModeHelper = record helper for TViMode
  private const
    STRINGS: array [0 .. 3] of String = ('Vi: -- INACTIVE --', 'Vi: -- NORMAL --', 'Vi: -- INSERT --',
      'Vi: -- VISUAL --');
  public
    function ToString: string;
  end;

  TViEditMode = (emNone, emDelete, emYank, emChange);

  TDirection = (dForward, dBack);
  TBlockAction = (baDelete, baYank);
  TViCharClass = (viWhiteSpace, viWord, viSpecial);

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
    FEditPosition: IOTAEditPosition;
    FBuffer: IOTAEditBuffer;
    FCurrentMode: TViMode;

    { TODO : Change to enum }
    FInDelete: Boolean;
    FInChange: Boolean;
    FInYank: Boolean;

    FParsingNumber: Boolean;
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

    FChar: Char;
    FShift: TShiftState;
    FOnModeChanged: TP_ModeChanged;
    // This Dictionary contains procedure references to the keybinds
    FViKeybinds: TDictionary<Char, TProc>;

    { General }
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
    function YankSelection: Boolean;
    procedure ApplyActionToBlock(Action: TBlockAction; IsLine: Boolean);
    procedure FindNextWordAtCursor(const count: Integer);
    procedure ActionFindPreviousWordAtCursor;
    procedure FindWordAtCursor(const View: IOTAEditView; const count: Integer);
    procedure HandleChar(const c: Char);
    procedure ProcessChange;
    procedure ProcessDeletion;
    procedure ProcessLineDeletion;
    procedure ProcessLineYanking;
    procedure ProcessYanking;
    procedure SavePreviousAction;
    procedure SwitchToInsertModeOrDoPreviousAction;
    procedure SetMode(ANewMode: TViMode);
    procedure SetOnModeChanged(ANewProc: TP_ModeChanged);

    // Keybinds
    procedure ActionAppendEOL;
    procedure ActionChangeRestOfLine;
    procedure ActionDeleteRestOfLine;
    procedure ActionGoToLineNumber;
    procedure ActionMoveToScreenLine;
    procedure ActionInsertBeginningOfLine;
    procedure ActionJoinLines;
    procedure ActionMoveToBottomScreenLine;
    procedure ActionMoveToMiddleOfScreen;
    procedure ActionOpenLineAboveCurrent;
    procedure ActionPasteBeforeCursor;
    procedure ActionReplaceCharacters;
    procedure ActionChangeLines;
    procedure ActionYankLine;
    procedure ActionAppend;
    procedure ActionChange;
    procedure ActionDelete;
    procedure ActionJump;
    procedure ActionInsert;
    procedure ActionSetMark;
    procedure ActionRepeatLastScan;
    procedure ActionOpenLineBelowCurrent;
    procedure ActionPaste;
    procedure ActionChangeSingleChar;
    procedure ActionUndo;
    procedure ActionDeleteSingleChar;
    procedure ActionDeleteSingleCharBeforeCursor;
    procedure ActionYank;
    procedure ActionAsterisk;
    procedure ActionShiftLeft;
    procedure ActionShiftRight;
    procedure ActionFirstNonWhiteInLine;
    procedure ActionRepeatLastCommand;
    procedure ActionMark;
  public
    constructor Create;
    destructor Destroy; override;
    procedure EditKeyDown(key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
    procedure EditChar(key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
    procedure ConfigureCursor;
    property count: Integer read GetCount;
    property currentMode: TViMode read FCurrentMode write SetMode;
    property onModeChanged: TP_ModeChanged read FOnModeChanged write SetOnModeChanged;

    procedure ToggleActive();
    procedure FillViBindings();
  end;

implementation

{ TViModeHelper }

function TViModeHelper.ToString(): string;
begin
  result := STRINGS[ord(Self)];
end;

{ TViBindings }

function QuerySvcs(const Instance: IUnknown; const Intf: TGUID; out Inst): Boolean;
begin
  result := (Instance <> nil) and Supports(Instance, Intf, Inst);
end;

function GetEditBuffer: IOTAEditBuffer;
var
  iEditorServices: IOTAEditorServices;
begin
  QuerySvcs(BorlandIDEServices, IOTAEditorServices, iEditorServices);
  if iEditorServices <> nil then
  begin
    result := iEditorServices.GetTopBuffer;
    Exit;
  end;
  result := nil;
end;

function GetEditPosition(Buffer: IOTAEditBuffer): IOTAEditPosition;
begin
  result := nil;
  if Buffer <> nil then
    result := Buffer.GetEditPosition;
end;

procedure TViBindings.ConfigureCursor;
var
  EditBuffer: IOTAEditBuffer;
begin
  EditBuffer := GetEditBuffer;
  if EditBuffer <> nil then
    EditBuffer.EditOptions.UseBriefCursorShapes := (currentMode = mNormal) or (currentMode = mVisual);
end;

constructor TViBindings.Create;
begin
  currentMode := mNormal;
  FViKeybinds := TDictionary<Char, TProc>.Create;
end;

procedure TViBindings.EditChar(key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
begin
  if currentMode = mInactive then
    Exit;

  if currentMode = mInsert then
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
    result := nil;
    EditBuffer := GetEditBuffer;
    if EditBuffer <> nil then
      Exit(EditBuffer.GetTopView);
  end;

begin
  case (currentMode) of
    mInactive:
      Exit;
    mNormal:
      begin
        if (((key >= ord('A')) and (key <= ord('Z'))) or ((key >= ord('0')) and (key <= ord('9'))) or
          ((key >= 186) and (key <= 192)) or ((key >= 219) and (key <= 222))) and
          not((ssCtrl in Shift) or (ssAlt in Shift)) and not(currentMode = mInsert) then
        begin
          // If the keydown is a standard keyboard press not altered with a ctrl
          // or alt key then create a WM_CHAR message so we can do all the
          // locale mapping of the keyboard and then handle the resulting key in
          // TViBindings.EditChar.

          // XXX can we switch to using ToAscii like we do for setting FInsertText
          TranslateMessage(Msg);
          Handled := True;
        end;
      end;
  else // Insert or Visual mode
    begin
      if (key = VK_ESCAPE) then
      begin
        GetTopMostEditView.Buffer.BufferOptions.InsertMode := True;
        currentMode := mNormal; // Go from Insert back to Normal
        Handled := True;
        Self.FPreviousAction.FInsertText := FInsertText;
        FInsertText := '';
      end;
    end;
  end;

end;

function TViBindings.IsMovementKey: Boolean;
begin
  if (FChar = '0') and FParsingNumber then
    Exit(False);

  result := CharInSet(FChar, ['0', '$', 'b', 'B', 'e', 'E', 'h', 'j', 'k', 'l', 'w', 'W']);
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
    FCount := 10 * FCount + (ord(FChar) - ord('0'));
end;

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
  result := True;
end;

destructor TViBindings.Destroy;
begin
  FreeAndNil(FViKeybinds);
  inherited;
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
  result := IfThen(FCount <= 0, 1, FCount);
end;

function TViBindings.GetEditCount: Integer;
begin
  result := IfThen(FEditCount > 0, FEditCount, 1);
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
      result := viWhiteSpace
    end
    else if FEditPosition.IsWordCharacter then
    begin
      result := viWord;
    end
    else
    begin
      result := viSpecial;
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

  result := Pos;
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
    begin
      try
        FViKeybinds[c];
      finally
        if not FParsingNumber then // nicht am Zählen
          ResetCount;
      end;
    end;
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
    currentMode := mInsert;
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

procedure TViBindings.FillViBindings;
begin
  // FViKeybinds.Add('''', ActionMark);
  // FViKeybinds.Add('*', ActionAsterisk);
  FViKeybinds.Add('.', ActionRepeatLastCommand);
  FViKeybinds.Add('<', ActionShiftLeft);
  FViKeybinds.Add('>', ActionShiftRight);
  FViKeybinds.Add('A', ActionAppendEOL);
  FViKeybinds.Add('C', ActionChangeRestOfLine);
  FViKeybinds.Add('D', ActionDeleteRestOfLine);
  FViKeybinds.Add('G', ActionGoToLineNumber);
  FViKeybinds.Add('H', ActionMoveToScreenLine);
  FViKeybinds.Add('I', ActionInsertBeginningOfLine);
  FViKeybinds.Add('J', ActionJoinLines);
  FViKeybinds.Add('L', ActionMoveToBottomScreenLine);
  FViKeybinds.Add('M', ActionMoveToMiddleOfScreen);
  FViKeybinds.Add('N', ActionFindPreviousWordAtCursor);
  FViKeybinds.Add('O', ActionOpenLineAboveCurrent);
  FViKeybinds.Add('P', ActionPasteBeforeCursor);
  FViKeybinds.Add('R', ActionReplaceCharacters);
  FViKeybinds.Add('S', ActionChangeLines);
  FViKeybinds.Add('X', ActionDeleteSingleCharBeforeCursor);
  FViKeybinds.Add('Y', ActionYankLine);
  FViKeybinds.Add('^', ActionFirstNonWhiteInLine);
  FViKeybinds.Add('a', ActionAppend);
  FViKeybinds.Add('c', ActionChange);
  FViKeybinds.Add('d', ActionDelete);
  FViKeybinds.Add('g', ActionJump);
  FViKeybinds.Add('i', ActionInsert);
  FViKeybinds.Add('m', ActionSetMark);
  FViKeybinds.Add('n', ActionRepeatLastScan);
  FViKeybinds.Add('o', ActionOpenLineBelowCurrent);
  FViKeybinds.Add('p', ActionPaste);
  FViKeybinds.Add('s', ActionChangeSingleChar);
  FViKeybinds.Add('u', ActionUndo);
  FViKeybinds.Add('x', ActionDeleteSingleChar);
  FViKeybinds.Add('y', ActionYank);
end;

{ --- BEGIN OF ACTION PROCEDURES --------------------------------------------- }

// '
procedure TViBindings.ActionMark;
begin
  { TODO : I have no idea what this is }
  FInGotoMark := True;
end;

// *
procedure TViBindings.ActionAsterisk;
begin
  { TODO : Look for asterisk in vi specification }
  FindWordAtCursor(FBuffer.TopView, count);
end;

// .
procedure TViBindings.ActionRepeatLastCommand;
begin
  FInRepeatChange := True;
  FInDelete := FPreviousAction.FInDelete;
  FInChange := FPreviousAction.FInChange;
  FEditCount := FPreviousAction.FEditCount;
  FCount := FPreviousAction.FCount;
  HandleChar(FPreviousAction.ActionChar);
  FInRepeatChange := False;
end;

// <
procedure TViBindings.ActionShiftLeft;
begin
  SavePreviousAction;
  ChangeIndentation(dBack);
end;

// >
procedure TViBindings.ActionShiftRight;
begin
  SavePreviousAction;
  ChangeIndentation(dForward);
end;

// A
procedure TViBindings.ActionAppendEOL;
begin
  FEditPosition.MoveEOL;
  SwitchToInsertModeOrDoPreviousAction;
end;

// C
procedure TViBindings.ActionChangeRestOfLine;
begin
  FInChange := True;
  FEditCount := count;
  HandleChar('$');
end;

// D
procedure TViBindings.ActionDeleteRestOfLine;
begin
  FInDelete := True;
  HandleChar('$');
end;

// G
procedure TViBindings.ActionGoToLineNumber;
begin
  if FParsingNumber then
    FEditPosition.GotoLine(count)
  else
    FEditPosition.MoveEOF;
end;

// H
procedure TViBindings.ActionMoveToScreenLine;
begin
  { TODO : Support for Count: Move cursor to count'th line displayed on screen }
  FEditPosition.Move(FBuffer.TopView.TopRow, 0);
  FEditPosition.MoveBOL;
end;

// I
procedure TViBindings.ActionInsertBeginningOfLine;
begin
  FEditPosition.MoveBOL;
  SwitchToInsertModeOrDoPreviousAction;
end;

// J
procedure TViBindings.ActionJoinLines;
begin
  { TODO : Support for Count: Join multiple lines }
  FEditPosition.MoveEOL;
  FEditPosition.Delete(1);
end;

// L
procedure TViBindings.ActionMoveToBottomScreenLine;
begin
  { TODO : Support for Count: ith a count, to the first non-white of the
    count'th line from the bottom. Operators affect whole lines when used
    with L(2.3). }
  FEditPosition.Move(FBuffer.TopView.BottomRow - 1, 0);
  FEditPosition.MoveBOL;
end;

// M
procedure TViBindings.ActionMoveToMiddleOfScreen;
var
  LView: IOTAEditView;
begin
  LView := FBuffer.TopView;
  FEditPosition.Move(LView.TopRow + Trunc(((LView.BottomRow - 1) - LView.TopRow) / 2), 0);
  FEditPosition.MoveBOL;
end;

// N
procedure TViBindings.ActionFindPreviousWordAtCursor;
var
  I: Integer;
begin
  FEditPosition.SearchOptions.Direction := sdBackward;
  for I := 1 to count do
    FEditPosition.SearchAgain;
end;

// O
procedure TViBindings.ActionOpenLineAboveCurrent;
begin
  FEditPosition.MoveBOL;
  FEditPosition.InsertText(#13#10);
  FEditPosition.MoveCursor(mmSkipWhite or mmSkipRight);
  FEditPosition.MoveRelative(-1, 0);
  SwitchToInsertModeOrDoPreviousAction;
  (BorlandIDEServices As IOTAEditorServices).TopView.MoveViewToCursor;
end;

// P
procedure TViBindings.ActionPasteBeforeCursor;
begin
  Paste(FEditPosition, FBuffer, dBack);
end;

// R
procedure TViBindings.ActionReplaceCharacters;
begin
  // XXX Fix me for '.' command
  FBuffer.BufferOptions.InsertMode := False;
  currentMode := mInsert;
end;

// S
procedure TViBindings.ActionChangeLines;
begin
  FInChange := True;
  FEditPosition.MoveBOL;
  HandleChar('$');
end;

// X
procedure TViBindings.ActionDeleteSingleCharBeforeCursor;
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

// Y
procedure TViBindings.ActionYankLine;
begin
  FInYank := True;
  FEditCount := count;
  HandleChar('y');
end;

// ^
procedure TViBindings.ActionFirstNonWhiteInLine;
begin
  FEditPosition.MoveBOL;
  FEditPosition.MoveCursor(mmSkipWhite);
end;

// a
procedure TViBindings.ActionAppend;
begin
  FEditPosition.MoveRelative(0, 1);
  SwitchToInsertModeOrDoPreviousAction;
end;

// c
procedure TViBindings.ActionChange;
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

// d
procedure TViBindings.ActionDelete;
begin
  if not DeleteSelection then
  begin
    FInDelete := True;
    FEditCount := count;
  end;
end;

// g
procedure TViBindings.ActionJump;
begin
  { TODO : Look for better name of this function }
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

// i
procedure TViBindings.ActionInsert;
begin
  SwitchToInsertModeOrDoPreviousAction;
end;

// m
procedure TViBindings.ActionSetMark;
begin
  FInMark := True;
end;

// n
procedure TViBindings.ActionRepeatLastScan;
begin
  { TODO : Look for better function name }
  FindNextWordAtCursor(count);
end;

// o
procedure TViBindings.ActionOpenLineBelowCurrent;
begin
  FEditPosition.MoveEOL;
  FEditPosition.InsertText(#13#10);
  SwitchToInsertModeOrDoPreviousAction;
  (BorlandIDEServices As IOTAEditorServices).TopView.MoveViewToCursor;
end;

// p
procedure TViBindings.ActionPaste;
begin
  Paste(FEditPosition, FBuffer, dForward);
end;

// s
procedure TViBindings.ActionChangeSingleChar;
begin
  if not DeleteSelection then
    FEditPosition.Delete(1);
  SwitchToInsertModeOrDoPreviousAction;
end;

// u
procedure TViBindings.ActionUndo;
begin
  FBuffer.Undo;
end;

// x
procedure TViBindings.ActionDeleteSingleChar;
begin
  if not DeleteSelection then
  begin
    FInDelete := True;
    FEditCount := count - 1;
    HandleChar('l');
  end;
end;

// y
procedure TViBindings.ActionYank;
begin
  FInYank := not YankSelection;
  if FInYank then
    FEditCount := count;
end;

{ ---------------------------------------------------------------------------- }

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
  FEditPosition.Move(FMarkArray[ord(FChar)].Line, FMarkArray[ord(FChar)].Col);
  FInGotoMark := False;
end;

procedure TViBindings.Paste(const EditPosition: IOTAEditPosition; const Buffer: IOTAEditBuffer; Direction: TDirection);
var
  AutoIdent, PastingInSelection: Boolean;
  EditBlock: IOTAEditBlock;
  Row, Col: Integer;

  function FixCursorPosition: Boolean;
  begin
    result := (not PastingInSelection) and (Direction = dForward);
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
  FMarkArray[ord(FChar)].Col := FEditPosition.Column;
  FMarkArray[ord(FChar)].Line := FEditPosition.Row;
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

procedure TViBindings.SetMode(ANewMode: TViMode);
var
  LText: string;
begin
  FCurrentMode := ANewMode;
  ConfigureCursor;
  if assigned(FOnModeChanged) then
  begin
    LText := ANewMode.ToString;
    FOnModeChanged(LText);
  end;
end;

procedure TViBindings.SetOnModeChanged(ANewProc: TP_ModeChanged);
begin
  FOnModeChanged := ANewProc;
  FOnModeChanged(currentMode.ToString); // call new procedure immediately
end;

procedure TViBindings.SwitchToInsertModeOrDoPreviousAction;
begin
  if (FInRepeatChange) then
    FEditPosition.InsertText(FPreviousAction.FInsertText)
  else
  begin
    SavePreviousAction;
    currentMode := mInsert;
  end;
end;

procedure TViBindings.ToggleActive;
begin
  if currentMode = mInactive then
    currentMode := mNormal
  else
    currentMode := mInactive;
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
  result := True;
end;

end.
