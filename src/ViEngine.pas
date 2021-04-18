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
    FCurrentEditMode: TViEditMode;
    FEditCount, FCount: Integer;
    FInsertText: string;
  end;

  TViBindings = class(TObject)
  private
    FEditPosition: IOTAEditPosition;
    FBuffer: IOTAEditBuffer;
    FCurrentViMode: TViMode;
    FCurrentEditMode: TViEditMode;

    FMovementCount: Integer;

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
    FViMoveKeybinds: TDictionary<Char, TProc>;

    { General }
    procedure ChangeIndentation(Direction: TDirection);
    function DeleteSelection: Boolean;
    function GetCount: Integer;
    function GetEditCount: Integer;
    procedure ResetCount;
    procedure ActionUpdateCount;
    function GetPositionForMove(AKey: Char; ACount: Integer = 0): TOTAEditPos;
    procedure ProcessMovement;
    procedure MoveToMarkPosition;
    procedure Paste(const AEditPosition: IOTAEditPosition; const ABuffer: IOTAEditBuffer; ADirection: TDirection);
    procedure SaveMarkPosition;
    function YankSelection: Boolean;
    procedure ApplyActionToBlock(Action: TBlockAction; IsLine: Boolean);
    procedure FindNextWordAtCursor(const count: Integer);
    procedure ActionFindPreviousWordAtCursor;
    procedure FindWordAtCursor(const View: IOTAEditView; const count: Integer);
    procedure HandleChar(const AChar: Char);
    procedure ProcessChange;
    procedure ProcessDeletion;
    procedure ProcessLineDeletion;
    procedure ProcessLineYanking;
    procedure ProcessYanking;
    procedure SavePreviousAction;
    procedure SwitchToInsertModeOrDoPreviousAction;
    procedure SetViMode(ANewMode: TViMode);
    procedure SetOnModeChanged(ANewProc: TP_ModeChanged);
    function GetHasCountInput: Boolean;

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
    function CharAtRelativeLocation(Col: Integer): TViCharClass;

    // Movement
    procedure ActionMoveBOLorCount;
    procedure ActionMoveEOL;
    procedure ActionMoveWordBack;
    procedure ActionMoveNonWhitespaceBack;
    procedure ActionMoveEndOfNextWord;
    procedure ActionMoveToEndOfWord;
    procedure ActionMoveLeft;
    procedure ActionMoveDown;
    procedure ActionMoveUp;
    procedure ActionMoveRight;
    procedure ActionMoveToBeginningOfNextWord;
    procedure ActionMoveToNextCharacterWord;

  public
    constructor Create;
    destructor Destroy; override;
    procedure EditKeyDown(key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
    procedure EditChar(key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
    procedure ConfigureCursor;
    property count: Integer read GetCount;
    property hasCountInput: Boolean read GetHasCountInput;
    property currentViMode: TViMode read FCurrentViMode write SetViMode;
    property currentEditMode: TViEditMode read FCurrentEditMode write FCurrentEditMode;
    property onModeChanged: TP_ModeChanged read FOnModeChanged write SetOnModeChanged;

    procedure ToggleActive();
    procedure FillViBindings();
  end;

implementation

{ General }

function QuerySvcs(const AInstance: IUnknown; const AIntf: TGUID; out AInst): Boolean;
begin
  result := (AInstance <> nil) and Supports(AInstance, AIntf, AInst);
end;

function GetEditBuffer: IOTAEditBuffer;
var
  LEditorServices: IOTAEditorServices;
begin
  QuerySvcs(BorlandIDEServices, IOTAEditorServices, LEditorServices);
  if LEditorServices <> nil then
  begin
    result := LEditorServices.GetTopBuffer;
    Exit;
  end;
  result := nil;
end;

function GetEditPosition(ABuffer: IOTAEditBuffer): IOTAEditPosition;
begin
  result := nil;
  if ABuffer <> nil then
    result := ABuffer.GetEditPosition;
end;

{ TViModeHelper }

function TViModeHelper.ToString(): string;
begin
  result := STRINGS[ord(Self)];
end;

{ TViBindings }

constructor TViBindings.Create;
begin
  currentViMode := mNormal;
  currentEditMode := emNone;
  FViKeybinds := TDictionary<Char, TProc>.Create;
  FViMoveKeybinds := TDictionary<Char, TProc>.Create;
  FillViBindings;
end;

destructor TViBindings.Destroy;
begin
  FreeAndNil(FViKeybinds);
  FreeAndNil(FViMoveKeybinds);
  inherited;
end;

procedure TViBindings.ConfigureCursor;
var
  LEditBuffer: IOTAEditBuffer;
begin
  LEditBuffer := GetEditBuffer;
  if LEditBuffer <> nil then
    LEditBuffer.EditOptions.UseBriefCursorShapes := (currentViMode = mNormal) or (currentViMode = mVisual);
end;

procedure TViBindings.EditChar(key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
begin
  if currentViMode = mInactive then
    Exit;

  if currentViMode = mInsert then
    Exit;

  FShift := Shift;
  HandleChar(Chr(key));
  Handled := True;
  (BorlandIDEServices As IOTAEditorServices).TopView.Paint;
end;

function TViBindings.CharAtRelativeLocation(Col: Integer): TViCharClass;
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
  case (currentViMode) of
    mInactive:
      Exit;
    mNormal:
      begin
        if (((key >= ord('A')) and (key <= ord('Z'))) or ((key >= ord('0')) and (key <= ord('9'))) or
          ((key >= 186) and (key <= 192)) or ((key >= 219) and (key <= 222))) and
          not((ssCtrl in Shift) or (ssAlt in Shift)) and not(currentViMode = mInsert) then
        begin
          // If the keydown is a standard keyboard press not altered with a ctrl
          // or alt key then create a WM_CHAR message so we can do all the
          // locale mapping of the keyboard and then handle the resulting key in
          // TViBindings.EditChar.

          // XXX can we switch to using ToAscii like we do for setting FInsertText
          TranslateMessage(Msg);
          Handled := True;
        end
        else if (key = VK_ESCAPE) then // cancel all current commands
        begin
          currentViMode := mNormal;
          currentEditMode := emNone;
          ResetCount;
          Handled := True;
        end;
      end;
  else // Insert or Visual mode
    begin
      if (key = VK_ESCAPE) then
      begin
        GetTopMostEditView.Buffer.BufferOptions.InsertMode := True;
        currentViMode := mNormal; // Go from Insert back to Normal
        Handled := True;

        // Save inserted text
        Self.FPreviousAction.FInsertText := FInsertText;
        FInsertText := '';
      end;
    end;
  end;
end;

procedure TViBindings.ResetCount;
begin
  FCount := 0;
end;

procedure TViBindings.ApplyActionToBlock(Action: TBlockAction; IsLine: Boolean);
var
  LCount: Integer;
  LPos: TOTAEditPos;
  LEditBlock: IOTAEditBlock;
begin
  LCount := GetCount * GetEditCount;
  ResetCount;
  LPos := GetPositionForMove(FChar, LCount);
  if CharInSet(FChar, ['e', 'E']) then
    LPos.Col := LPos.Col + 1;

  LEditBlock := FBuffer.EditBlock;
  LEditBlock.Reset;
  LEditBlock.BeginBlock;
  LEditBlock.Extend(LPos.Line, LPos.Col);
  FRegisterArray[FSelectedRegister].IsLine := IsLine;
  FRegisterArray[FSelectedRegister].Text := LEditBlock.Text;

  case Action of
    baDelete:
      LEditBlock.Delete;
    baYank:
      LEditBlock.Reset;
  end;

  LEditBlock.EndBlock;
end;

procedure TViBindings.ChangeIndentation(Direction: TDirection);
var
  LEditBlock: IOTAEditBlock;
  LStartedBlock: Boolean;
begin
  LStartedBlock := False;
  LEditBlock := FBuffer.EditBlock;
  LEditBlock.Save;
  FEditPosition.Save;

  if LEditBlock.Size = 0 then
  begin
    LStartedBlock := True;
    FEditPosition.MoveBOL;
    LEditBlock.Reset;
    LEditBlock.BeginBlock;
    LEditBlock.Extend(FEditPosition.Row, FEditPosition.Column + 1);
  end
  else
  begin
    // When selecting multiple lines, if the cursor is in the first column the last line doesn't get into the block
    // and the indent seems buggy, as the cursor is on the last line but it isn't indented, so we force
    // the selection of at least one char to correct this behavior
    LEditBlock.ExtendRelative(0, 1);
  end;

  case Direction of
    dForward:
      LEditBlock.Indent(FBuffer.EditOptions.BlockIndent);
    dBack:
      LEditBlock.Indent(-FBuffer.EditOptions.BlockIndent);
  end;

  // If we don't call EndBlock, the selection gets buggy.
  if LStartedBlock then
    LEditBlock.EndBlock;

  FEditPosition.Restore;
  LEditBlock.Restore;
end;

function TViBindings.DeleteSelection: Boolean;
var
  LEditBlock: IOTAEditBlock;
begin
  LEditBlock := FBuffer.EditBlock;
  if LEditBlock.Size = 0 then
    Exit(False);

  FRegisterArray[FSelectedRegister].IsLine := False;
  FRegisterArray[FSelectedRegister].Text := LEditBlock.Text;
  LEditBlock.Delete;
  result := True;
end;

procedure TViBindings.FindNextWordAtCursor(const count: Integer);
var
  LEditBlock: IOTAEditBlock;
  i: Integer;
begin
  LEditBlock := FBuffer.EditBlock;
  LEditBlock.Reset;
  LEditBlock.BeginBlock;
  LEditBlock.ExtendRelative(0, Length(FEditPosition.SearchOptions.SearchText));
  if AnsiSameText(FEditPosition.SearchOptions.SearchText, LEditBlock.Text) then
    FEditPosition.MoveRelative(0, Length(FEditPosition.SearchOptions.SearchText));
  LEditBlock.EndBlock;

  FEditPosition.SearchOptions.Direction := sdForward;

  for i := 1 to count do
    FEditPosition.SearchAgain;

  FEditPosition.MoveRelative(0, -Length(FEditPosition.SearchOptions.SearchText));
end;

procedure TViBindings.FindWordAtCursor(const View: IOTAEditView; const count: Integer);
var
  LEditBlock: IOTAEditBlock;
  LPos: TOTAEditPos;
  i: Integer;
begin
  LEditBlock := FBuffer.EditBlock;
  if FEditPosition.IsWordCharacter then
    FEditPosition.MoveCursor(mmSkipWord or mmSkipLeft)
  else
    FEditPosition.MoveCursor(mmSkipNonWord or mmSkipRight or mmSkipStream);

  LPos := GetPositionForMove('e', 1);

  LEditBlock := FBuffer.EditBlock;
  LEditBlock.Reset;
  LEditBlock.BeginBlock;
  LEditBlock.Extend(LPos.Line, LPos.Col + 1);
  FEditPosition.SearchOptions.SearchText := LEditBlock.Text;
  LEditBlock.EndBlock;

  // Move to one position after what we're searching for.
  FEditPosition.Move(LPos.Line, LPos.Col + 1);

  FEditPosition.SearchOptions.CaseSensitive := False;
  FEditPosition.SearchOptions.Direction := sdForward;
  FEditPosition.SearchOptions.FromCursor := True;
  FEditPosition.SearchOptions.RegularExpression := False;
  FEditPosition.SearchOptions.WholeFile := True;
  FEditPosition.SearchOptions.WordBoundary := True;

  for i := 1 to count do
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

function TViBindings.GetHasCountInput: Boolean;
begin
  result := (FCount > 0);
end;

function TViBindings.GetPositionForMove(AKey: Char; ACount: Integer = 0): TOTAEditPos;
var
  LPos: TOTAEditPos;
begin
  FMovementCount := ACount;
  FEditPosition.Save;

  if FViMoveKeybinds.ContainsKey(AKey) then
    FViMoveKeybinds[AKey]();

  LPos.Col := FEditPosition.Column;
  LPos.Line := FEditPosition.Row;
  FEditPosition.Restore;

  result := LPos;
end;

procedure TViBindings.HandleChar(const AChar: Char);
begin
  FChar := AChar;
  FBuffer := GetEditBuffer;
  FEditPosition := GetEditPosition(FBuffer);
  try
    if FInMark then
      SaveMarkPosition
    else if FInGotoMark then
      MoveToMarkPosition
    else if CharInSet(FChar, ['0' .. '9']) then
      ActionUpdateCount
    else if FViMoveKeybinds.ContainsKey(FChar) then
      ProcessMovement
    else if FViKeybinds.ContainsKey(FChar) then
    begin
      FViKeybinds[AChar]();
      ResetCount;
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
    currentViMode := mInsert;
  end;
  currentEditMode := emNone;
end;

procedure TViBindings.ProcessDeletion;
begin
  if not FInRepeatChange then
    SavePreviousAction;

  ApplyActionToBlock(baDelete, False);
  currentEditMode := emNone;
end;

procedure TViBindings.FillViBindings;
begin
  // FViKeybinds.Add('''', ActionMark);
  // FViKeybinds.Add('*', ActionAsterisk);
  FViMoveKeybinds.Add('$', ActionMoveEOL);
  FViKeybinds.Add('.', ActionRepeatLastCommand);
  FViMoveKeybinds.Add('0', ActionMoveBOLorCount);
  FViKeybinds.Add('<', ActionShiftLeft);
  FViKeybinds.Add('>', ActionShiftRight);
  FViKeybinds.Add('A', ActionAppendEOL);
  FViMoveKeybinds.Add('B', ActionMoveNonWhitespaceBack);
  FViKeybinds.Add('C', ActionChangeRestOfLine);
  FViKeybinds.Add('D', ActionDeleteRestOfLine);
  FViMoveKeybinds.Add('E', ActionMoveToEndOfWord);
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
  FViMoveKeybinds.Add('W', ActionMoveToNextCharacterWord);
  FViKeybinds.Add('X', ActionDeleteSingleCharBeforeCursor);
  FViKeybinds.Add('Y', ActionYankLine);
  FViKeybinds.Add('^', ActionFirstNonWhiteInLine);
  FViKeybinds.Add('a', ActionAppend);
  FViKeybinds.Add('b', ActionMoveWordBack);
  FViKeybinds.Add('c', ActionChange);
  FViKeybinds.Add('d', ActionDelete);
  FViMoveKeybinds.Add('e', ActionMoveEndOfNextWord);
  FViKeybinds.Add('g', ActionJump);
  FViMoveKeybinds.Add('h', ActionMoveLeft);
  FViKeybinds.Add('i', ActionInsert);
  FViMoveKeybinds.Add('j', ActionMoveDown);
  FViMoveKeybinds.Add('k', ActionMoveUp);
  FViMoveKeybinds.Add('l', ActionMoveRight);
  FViKeybinds.Add('m', ActionSetMark);
  FViKeybinds.Add('n', ActionRepeatLastScan);
  FViKeybinds.Add('o', ActionOpenLineBelowCurrent);
  FViKeybinds.Add('p', ActionPaste);
  FViKeybinds.Add('s', ActionChangeSingleChar);
  FViKeybinds.Add('u', ActionUndo);
  FViMoveKeybinds.Add('w', ActionMoveToBeginningOfNextWord);
  FViKeybinds.Add('x', ActionDeleteSingleChar);
  FViKeybinds.Add('y', ActionYank);
end;

procedure TViBindings.ProcessLineDeletion;
begin
  if not FInRepeatChange then
    SavePreviousAction;

  FEditPosition.MoveBOL;
  FChar := 'j';
  ApplyActionToBlock(baDelete, True);
  currentEditMode := emNone;
end;

procedure TViBindings.ProcessLineYanking;
begin
  FEditPosition.Save;
  FEditPosition.MoveBOL;
  FChar := 'j';
  ApplyActionToBlock(baYank, True);
  FEditPosition.Restore;
  currentEditMode := emNone;
end;

procedure TViBindings.ProcessMovement;
var
  Pos: TOTAEditPos;
begin
  case currentEditMode of
    emNone:
      begin
        Pos := GetPositionForMove(FChar, GetCount);
        FEditPosition.Move(Pos.Line, Pos.Col);
        FInGo := False;
      end;
    emDelete:
      ProcessDeletion;
    emYank:
      ProcessYanking;
    emChange:
      ProcessChange;
  end;

  ResetCount;
end;

procedure TViBindings.ProcessYanking;
begin
  FEditPosition.Save;
  ApplyActionToBlock(baYank, False);
  FEditPosition.Restore;
  currentEditMode := emNone;
end;

procedure TViBindings.MoveToMarkPosition;
begin
  FEditPosition.Move(FMarkArray[ord(FChar)].Line, FMarkArray[ord(FChar)].Col);
  FInGotoMark := False;
end;

procedure TViBindings.Paste(const AEditPosition: IOTAEditPosition; const ABuffer: IOTAEditBuffer;
  ADirection: TDirection);
var
  LAutoIndent, LPastingInSelection: Boolean;
  LEditBlock: IOTAEditBlock;
  LRow, LCol: Integer;

  function FixCursorPosition: Boolean;
  begin
    result := (not LPastingInSelection) and (ADirection = dForward);
  end;

begin
  SavePreviousAction;
  LPastingInSelection := False;
  LAutoIndent := ABuffer.BufferOptions.AutoIndent;

  LEditBlock := ABuffer.EditBlock;
  if LEditBlock.Size > 0 then
  begin
    LPastingInSelection := True;
    LRow := LEditBlock.StartingRow;
    LCol := LEditBlock.StartingColumn;
    LEditBlock.Delete;
    AEditPosition.Move(LRow, LCol);
  end;

  if (FRegisterArray[FSelectedRegister].IsLine) then
  begin
    ABuffer.BufferOptions.AutoIndent := False;
    AEditPosition.MoveBOL;

    if FixCursorPosition then
      AEditPosition.MoveRelative(1, 0);

    AEditPosition.Save;
    AEditPosition.InsertText(FRegisterArray[FSelectedRegister].Text);
    AEditPosition.Restore;
    ABuffer.BufferOptions.AutoIndent := LAutoIndent;
  end
  else
  begin
    if FixCursorPosition then
      AEditPosition.MoveRelative(0, 1);

    AEditPosition.InsertText(FRegisterArray[FSelectedRegister].Text);
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
  FPreviousAction.FCurrentEditMode := currentEditMode;
  FPreviousAction.FEditCount := FEditCount;
  FPreviousAction.FCount := FCount;
  // self.FPreviousAction.FInsertText := FInsertText;
end;

procedure TViBindings.SetViMode(ANewMode: TViMode);
var
  LText: string;
begin
  FCurrentViMode := ANewMode;
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
  FOnModeChanged(currentViMode.ToString); // call new procedure immediately
end;

procedure TViBindings.SwitchToInsertModeOrDoPreviousAction;
begin
  if (FInRepeatChange) then
    FEditPosition.InsertText(FPreviousAction.FInsertText)
  else
  begin
    SavePreviousAction;
    currentViMode := mInsert;
  end;
end;

procedure TViBindings.ToggleActive;
begin
  if currentViMode = mInactive then
    currentViMode := mNormal
  else
    currentViMode := mInactive;
end;

function TViBindings.YankSelection: Boolean;
var
  LEditBlock: IOTAEditBlock;
begin
  LEditBlock := FBuffer.EditBlock;
  if LEditBlock.Size = 0 then
    Exit(False);

  FRegisterArray[FSelectedRegister].IsLine := False;
  FRegisterArray[FSelectedRegister].Text := LEditBlock.Text;
  LEditBlock.Reset;
  result := True;
end;

{ --- BEGIN OF ACTION PROCEDURES --------------------------------------------- }

// '$'
procedure TViBindings.ActionMoveEOL;
begin
  FEditPosition.MoveEOL;
  // When moving, must stop at last char, not on line break.
  if currentEditMode = emNone then
    FEditPosition.MoveRelative(0, -1);
end;

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
  currentEditMode := FPreviousAction.FCurrentEditMode;
  FEditCount := FPreviousAction.FEditCount;
  FCount := FPreviousAction.FCount;
  HandleChar(FPreviousAction.ActionChar);
  FInRepeatChange := False;
end;

// 0 -> count handling in ActionUpdateCount
procedure TViBindings.ActionMoveBOLorCount;
begin
  FEditPosition.MoveBOL;
end;

// 1-9; 0 if no count input
procedure TViBindings.ActionUpdateCount;
begin
  if (FChar = '0') and (not hasCountInput) then
    ProcessMovement
  else
    FCount := 10 * FCount + (ord(FChar) - ord('0'));
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

// B
procedure TViBindings.ActionMoveNonWhitespaceBack;
var
  i: Integer;
begin
  for i := 1 to FMovementCount do
  begin
    FEditPosition.MoveCursor(mmSkipWhite or mmSkipLeft or mmSkipStream);
    FEditPosition.MoveCursor(mmSkipNonWhite or mmSkipLeft);
  end;
end;

// C
procedure TViBindings.ActionChangeRestOfLine;
begin
  currentEditMode := emChange;
  FEditCount := count;
  HandleChar('$');
end;

// D
procedure TViBindings.ActionDeleteRestOfLine;
begin
  currentEditMode := emDelete;
  HandleChar('$');
end;

// E
procedure TViBindings.ActionMoveToEndOfWord;
var
  i: Integer;
begin
  for i := 1 to FMovementCount do
  begin
    if (FEditPosition.IsWordCharacter or FEditPosition.IsSpecialCharacter) and (CharAtRelativeLocation(1) = viWhiteSpace)
    then
      FEditPosition.MoveRelative(0, 1);

    if FEditPosition.IsWhiteSpace then
      FEditPosition.MoveCursor(mmSkipWhite or mmSkipRight or mmSkipStream);

    FEditPosition.MoveCursor(mmSkipNonWhite or mmSkipRight);
    FEditPosition.MoveRelative(0, -1);
  end;
end;

// G
procedure TViBindings.ActionGoToLineNumber;
begin
  if hasCountInput then
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
  i: Integer;
begin
  FEditPosition.SearchOptions.Direction := sdBackward;
  for i := 1 to count do
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
  currentViMode := mInsert;
end;

// S
procedure TViBindings.ActionChangeLines;
begin
  currentEditMode := emChange;
  FEditPosition.MoveBOL;
  HandleChar('$');
end;

// W
procedure TViBindings.ActionMoveToNextCharacterWord;
var
  i: Integer;
begin
  for i := 1 to FMovementCount do
  begin
    // Goto first white space after the end of the word.
    FEditPosition.MoveCursor(mmSkipNonWhite or mmSkipRight);
    // Now skip all the white space until we're at the start of a word again.
    FEditPosition.MoveCursor(mmSkipWhite or mmSkipRight or mmSkipStream);
  end;
end;

// X
procedure TViBindings.ActionDeleteSingleCharBeforeCursor;
begin
  currentEditMode := emDelete;
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
  currentEditMode := emYank;
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

// b
procedure TViBindings.ActionMoveWordBack;
var
  i: Integer;
  LNextChar: TViCharClass;
begin
  for i := 1 to FMovementCount do
  begin
    LNextChar := CharAtRelativeLocation(-1);
    if FEditPosition.IsWordCharacter and ((LNextChar = viSpecial) or (LNextChar = viWhiteSpace)) then
      FEditPosition.MoveRelative(0, -1);

    if FEditPosition.IsSpecialCharacter and ((LNextChar = viWord) or (LNextChar = viWhiteSpace)) then
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

// c
procedure TViBindings.ActionChange;
begin
  if currentEditMode = emChange then
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
      currentEditMode := emChange;
      FEditCount := count;
    end
  end;
end;

// d
procedure TViBindings.ActionDelete;
begin
  if currentEditMode = emDelete then
  begin
    ProcessLineDeletion;
  end
  else if not DeleteSelection then
  begin
    currentEditMode := emDelete;
    FEditCount := count;
  end;
end;

// e
procedure TViBindings.ActionMoveEndOfNextWord;
var
  i: Integer;
  LNextChar: TViCharClass;
begin
  for i := 1 to FMovementCount do
  begin
    LNextChar := CharAtRelativeLocation(1);
    if (FEditPosition.IsWordCharacter and (LNextChar = viWhiteSpace) or (LNextChar = viSpecial)) then
      FEditPosition.MoveRelative(0, 1);

    if (FEditPosition.IsSpecialCharacter and (LNextChar = viWhiteSpace) or (LNextChar = viWord)) then
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

// h
procedure TViBindings.ActionMoveLeft;
begin
  FEditPosition.MoveRelative(0, -FMovementCount);
end;

// i
procedure TViBindings.ActionInsert;
begin
  SwitchToInsertModeOrDoPreviousAction;
end;

// j
procedure TViBindings.ActionMoveDown;
begin
  FEditPosition.MoveRelative(+FMovementCount, 0);
end;

// k
procedure TViBindings.ActionMoveUp;
begin
  FEditPosition.MoveRelative(-FMovementCount, 0);
end;

// l
procedure TViBindings.ActionMoveRight;
begin
  FEditPosition.MoveRelative(0, +FMovementCount);
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

// w
procedure TViBindings.ActionMoveToBeginningOfNextWord;
var
  i: Integer;
begin
  for i := 1 to FMovementCount do
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

// x
procedure TViBindings.ActionDeleteSingleChar;
begin
  if not DeleteSelection then
  begin
    currentEditMode := emDelete;
    FEditCount := count - 1;
    HandleChar('l');
  end;
end;

// y
procedure TViBindings.ActionYank;
begin
  if currentEditMode = emYank then
    ProcessLineYanking
  else
  begin
    if not YankSelection() then
      currentEditMode := emYank
    else
      currentEditMode := emNone;

    if currentEditMode = emYank then
      FEditCount := count;
  end;
end;

{ ---------------------------------------------------------------------------- }

end.
