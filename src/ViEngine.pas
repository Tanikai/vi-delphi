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
  TP_Movement = reference to procedure(ACount: Integer);

  TViMode = (mInactive, mNormal, mInsert, mVisual);

  TViModeHelper = record helper for TViMode
  private const
    STRINGS: array [0 .. 3] of String = ('Vi: -- INACTIVE --', 'Vi: -- NORMAL --', 'Vi: -- INSERT --',
      'Vi: -- VISUAL --');
  public
    function ToString: String;
  end;

  TViEditMode = (emNone, emDelete, emYank, emChange);
  TDirection = (dForward, dBack);
  TBlockAction = (baDelete, baYank);
  TViCharClass = (viWhiteSpace, viWord, viSpecial);

  TViRegister = record
    IsLine: Boolean;
    Text: String;
  end;

  /// <summary>
  /// Stores an action. Used for repeating actions.
  /// </summary>
  TViAction = record
    ActionChar: Char;
    FCurrentEditMode: TViEditMode;
    FEditCount, FCurrentCount: Integer;
    FInsertText: String;
  end;

  TViEngine = class(TObject)
  private
    { General }
    FCursorPosition: IOTAEditPosition;
    FBuffer: IOTAEditBuffer;
    FCurrentViMode: TViMode;
    FCurrentEditMode: TViEditMode;
    FCurrentChar: Char;
    FShiftState: TShiftState;
    FCurrentCount: Integer; // Most recent number input of user
    FEditCount: Integer; // Previous input of number when editing text
    FOnModeChanged: TP_ModeChanged; // called when Vi Mode is changed
    FViKeybinds: TDictionary<Char, TProc>; // Contains keybinds & procedures for Vi Actions
    FViMoveKeybinds: TDictionary<Char, TP_Movement>; // Contains keybinds & procedures for movement

    { Clipboard }
    FRegisterArray: array [0 .. 255] of TViRegister;
    FSelectedRegister: Integer;

    { History }
    FInRepeatChange: Boolean;
    FPreviousAction: TViAction;
    FInsertText: String;

    { Marks }
    FMarkArray: array [0 .. 255] of TOTAEditPos;
    FInGo: Boolean;
    FInMark: Boolean;
    FInGotoMark: Boolean;

    { --- Functions and Procedures --- }
    { General }
    procedure FillViBindings();
    procedure HandleChar(const AChar: Char);
    procedure SwitchToInsertModeOrDoPreviousAction;
    procedure ResetCount;

    { Text Navigation }
    function GetPositionForMove(AKey: Char; ACount: Integer = 0): TOTAEditPos;
    function CharAtRelativeLocation(ACol: Integer): TViCharClass;
    procedure FindNextWordAtCursor(const ACount: Integer);
    procedure FindWordAtCursor(const AView: IOTAEditView; const ACount: Integer);

    { Text Editing }
    procedure ChangeIndentation(ADirection: TDirection);
    procedure Paste(const AEditPosition: IOTAEditPosition; const ABuffer: IOTAEditBuffer; ADirection: TDirection);

    { Text Editing: Selection }
    procedure ProcessMovement;
    procedure ProcessChange;
    procedure ProcessDeletion;
    procedure ProcessLineDeletion;
    function DeleteSelection: Boolean;
    procedure ProcessYanking;
    procedure ProcessLineYanking;
    function YankSelection: Boolean;
    procedure ApplyActionToSelection(AAction: TBlockAction; AIsLine: Boolean);

    { History }
    procedure SavePreviousAction;

    { Marks }
    procedure SaveMarkPosition;
    procedure MoveToMarkPosition;

    { Vi Actions }
    procedure ActionUpdateCount;

    procedure ActionFindPreviousWordAtCursor;
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

    { Movement }
    procedure ActionMoveBOLorCount(ACount: Integer);
    procedure ActionMoveEOL(ACount: Integer);
    procedure ActionMoveWordBack(ACount: Integer);
    procedure ActionMoveNonWhitespaceBack(ACount: Integer);
    procedure ActionMoveEndOfNextWord(ACount: Integer);
    procedure ActionMoveToEndOfWord(ACount: Integer);
    procedure ActionMoveLeft(ACount: Integer);
    procedure ActionMoveDown(ACount: Integer);
    procedure ActionMoveUp(ACount: Integer);
    procedure ActionMoveRight(ACount: Integer);
    procedure ActionMoveToBeginningOfNextWord(ACount: Integer);
    procedure ActionMoveToNextCharacterWord(ACount: Integer);

    { Getter / Setter }
    function GetCurrentCount: Integer;
    function GetEditCount: Integer;
    function GetHasCountInput: Boolean;
    procedure SetViMode(ANewMode: TViMode);
    procedure SetOnModeChanged(ANewProc: TP_ModeChanged);

  public
    property currentCount: Integer read GetCurrentCount;
    property editCount: Integer read GetEditCount;
    property hasCountInput: Boolean read GetHasCountInput;
    property currentViMode: TViMode read FCurrentViMode write SetViMode;
    property currentEditMode: TViEditMode read FCurrentEditMode write FCurrentEditMode;
    property onModeChanged: TP_ModeChanged read FOnModeChanged write SetOnModeChanged;

    constructor Create;
    destructor Destroy; override;
    procedure EditKeyDown(AKey, AScanCode: Word; AShift: TShiftState; AMsg: TMsg; var AHandled: Boolean);
    procedure EditChar(AKey, AScanCode: Word; AShift: TShiftState; AMsg: TMsg; var AHandled: Boolean);
    procedure ConfigureCursor;
    procedure ToggleActive();

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

function TViModeHelper.ToString(): String;
begin
  result := STRINGS[ord(Self)];
end;

{ TViBindings }

constructor TViEngine.Create;
begin
  currentViMode := mNormal;
  currentEditMode := emNone;
  FViKeybinds := TDictionary<Char, TProc>.Create;
  FViMoveKeybinds := TDictionary<Char, TP_Movement>.Create;
  FillViBindings;
end;

destructor TViEngine.Destroy;
begin
  FreeAndNil(FViKeybinds);
  FreeAndNil(FViMoveKeybinds);
  inherited;
end;

procedure TViEngine.ConfigureCursor;
var
  LEditBuffer: IOTAEditBuffer;
begin
  LEditBuffer := GetEditBuffer;
  if LEditBuffer <> nil then
    LEditBuffer.EditOptions.UseBriefCursorShapes := (currentViMode = mNormal) or (currentViMode = mVisual);
end;

procedure TViEngine.EditChar(AKey, AScanCode: Word; AShift: TShiftState; AMsg: TMsg; var AHandled: Boolean);
begin
  if currentViMode = mInactive then
    Exit;

  if currentViMode = mInsert then
    Exit;

  FShiftState := AShift;
  HandleChar(Chr(AKey));
  AHandled := True;
  (BorlandIDEServices as IOTAEditorServices).TopView.Paint;
end;

function TViEngine.CharAtRelativeLocation(ACol: Integer): TViCharClass;
begin
  FCursorPosition.Save;
  FCursorPosition.MoveRelative(0, ACol);
  if FCursorPosition.IsWhiteSpace or (FCursorPosition.Character = #$D) then
  begin
    result := viWhiteSpace
  end
  else if FCursorPosition.IsWordCharacter then
  begin
    result := viWord;
  end
  else
  begin
    result := viSpecial;
  end;
  FCursorPosition.Restore;
end;

procedure TViEngine.EditKeyDown(AKey, AScanCode: Word; AShift: TShiftState; AMsg: TMsg; var AHandled: Boolean);
var
  LIsLetter, LIsSymbol: Boolean;

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
        if (ssCtrl in AShift) or (ssAlt in AShift) then
          Exit;

        LIsLetter := ((AKey >= ord('A')) and (AKey <= ord('Z'))) or ((AKey >= ord('0')) and (AKey <= ord('9')));
        LIsSymbol := ((AKey >= 186) and (AKey <= 192)) or ((AKey >= 219) and (AKey <= 222)) or (AKey = VK_SPACE);

        if LIsLetter or LIsSymbol then
        begin
          // If the keydown is a standard keyboard press not altered with a ctrl
          // or alt key then create a WM_CHAR message so we can do all the
          // locale mapping of the keyboard and then handle the resulting key in
          // TViBindings.EditChar.
          // XXX can we switch to using ToAscii like we do for setting FInsertText
          TranslateMessage(AMsg);
          AHandled := True;
        end
        else if (AKey = VK_ESCAPE) then // cancel all current commands
        begin
          currentViMode := mNormal;
          currentEditMode := emNone;
          ResetCount;
          AHandled := True;
        end;
      end;
  else // Insert or Visual mode
    begin
      if (AKey = VK_ESCAPE) then
      begin
        GetTopMostEditView.Buffer.BufferOptions.InsertMode := True;
        currentViMode := mNormal; // Go from Insert back to Normal
        AHandled := True;

        // Save inserted text
        Self.FPreviousAction.FInsertText := FInsertText;
        FInsertText := '';
      end;
    end;
  end;
end;

procedure TViEngine.ResetCount;
begin
  FCurrentCount := 0;
end;

procedure TViEngine.ApplyActionToSelection(AAction: TBlockAction; AIsLine: Boolean);
var
  LCount: Integer;
  LPos: TOTAEditPos;
  LSelection: IOTAEditBlock;
  LTemp: String;
begin
  LCount := GetCurrentCount * GetEditCount;
  ResetCount;
  LPos := GetPositionForMove(FCurrentChar, LCount);
  if CharInSet(FCurrentChar, ['e', 'E']) then
    LPos.Col := LPos.Col + 1;

  LSelection := FBuffer.EditBlock;
  LSelection.Reset;
  LSelection.BeginBlock;
  LSelection.Extend(LPos.Line, LPos.Col);
  FRegisterArray[FSelectedRegister].IsLine := AIsLine;
  LTemp := LSelection.Text;
  FRegisterArray[FSelectedRegister].Text := LTemp;

  case AAction of
    baDelete:
      LSelection.Delete;
    baYank:
      LSelection.Reset;
  end;

  LSelection.EndBlock;
end;

procedure TViEngine.ChangeIndentation(ADirection: TDirection);
var
  LSelection: IOTAEditBlock;
  LStartedSelection: Boolean;
begin
  LStartedSelection := False;
  LSelection := FBuffer.EditBlock;
  LSelection.Save;
  FCursorPosition.Save;

  if LSelection.Size = 0 then
  begin
    LStartedSelection := True;
    FCursorPosition.MoveBOL;
    LSelection.Reset;
    LSelection.BeginBlock;
    LSelection.Extend(FCursorPosition.Row, FCursorPosition.Column + 1);
  end
  else
  begin
    // When selecting multiple lines, if the cursor is in the first column the last line doesn't get into the block
    // and the indent seems buggy, as the cursor is on the last line but it isn't indented, so we force
    // the selection of at least one char to correct this behavior
    LSelection.ExtendRelative(0, 1);
  end;

  case ADirection of
    dForward:
      LSelection.Indent(FBuffer.EditOptions.BlockIndent);
    dBack:
      LSelection.Indent(-FBuffer.EditOptions.BlockIndent);
  end;

  // If we don't call EndBlock, the selection gets buggy.
  if LStartedSelection then
    LSelection.EndBlock;

  FCursorPosition.Restore;
  LSelection.Restore;
end;

function TViEngine.DeleteSelection: Boolean;
var
  LSelection: IOTAEditBlock;
begin
  LSelection := FBuffer.EditBlock;
  if LSelection.Size = 0 then
    Exit(False);

  FRegisterArray[FSelectedRegister].IsLine := False;
  FRegisterArray[FSelectedRegister].Text := LSelection.Text;
  LSelection.Delete;
  result := True;
end;

procedure TViEngine.FindNextWordAtCursor(const ACount: Integer);
var
  LSelection: IOTAEditBlock;
  i: Integer;
begin
  LSelection := FBuffer.EditBlock;
  LSelection.Reset;
  LSelection.BeginBlock;
  LSelection.ExtendRelative(0, Length(FCursorPosition.SearchOptions.SearchText));
  if AnsiSameText(FCursorPosition.SearchOptions.SearchText, LSelection.Text) then
    FCursorPosition.MoveRelative(0, Length(FCursorPosition.SearchOptions.SearchText));
  LSelection.EndBlock;

  FCursorPosition.SearchOptions.Direction := sdForward;

  for i := 1 to ACount do
    FCursorPosition.SearchAgain;

  FCursorPosition.MoveRelative(0, -Length(FCursorPosition.SearchOptions.SearchText));
end;

procedure TViEngine.FindWordAtCursor(const AView: IOTAEditView; const ACount: Integer);
var
  LSelection: IOTAEditBlock;
  LPos: TOTAEditPos;
  i: Integer;
begin
  LSelection := FBuffer.EditBlock;
  if FCursorPosition.IsWordCharacter then
    FCursorPosition.MoveCursor(mmSkipWord or mmSkipLeft)
  else
    FCursorPosition.MoveCursor(mmSkipNonWord or mmSkipRight or mmSkipStream);

  LPos := GetPositionForMove('e', 1);

  LSelection := FBuffer.EditBlock;
  LSelection.Reset;
  LSelection.BeginBlock;
  LSelection.Extend(LPos.Line, LPos.Col + 1);
  FCursorPosition.SearchOptions.SearchText := LSelection.Text;
  LSelection.EndBlock;

  // Move to one position after what we're searching for.
  FCursorPosition.Move(LPos.Line, LPos.Col + 1);

  FCursorPosition.SearchOptions.CaseSensitive := False;
  FCursorPosition.SearchOptions.Direction := sdForward;
  FCursorPosition.SearchOptions.FromCursor := True;
  FCursorPosition.SearchOptions.RegularExpression := False;
  FCursorPosition.SearchOptions.WholeFile := True;
  FCursorPosition.SearchOptions.WordBoundary := True;

  for i := 1 to ACount do
    FCursorPosition.SearchAgain;

  // Move back to the start of the text we searched for.
  FCursorPosition.MoveRelative(0, -Length(FCursorPosition.SearchOptions.SearchText));

  AView.MoveViewToCursor;
end;

function TViEngine.GetCurrentCount: Integer;
begin
  if FCurrentCount <= 0 then
    result := 1
  else
    result := FCurrentCount;
end;

function TViEngine.GetEditCount: Integer;
begin
  if FEditCount <= 0 then
    result := 1
  else
    result := FEditCount;
end;

function TViEngine.GetHasCountInput: Boolean;
begin
  result := (FCurrentCount > 0);
end;

// Given a movement key and a count return the position in the buffer where that
// movement would take you.
function TViEngine.GetPositionForMove(AKey: Char; ACount: Integer = 0): TOTAEditPos;
var
  LPos: TOTAEditPos;
begin
  FCursorPosition.Save;

  if FViMoveKeybinds.ContainsKey(AKey) then
    FViMoveKeybinds[AKey](ACount);

  LPos.Col := FCursorPosition.Column;
  LPos.Line := FCursorPosition.Row;
  FCursorPosition.Restore;

  result := LPos;
end;

procedure TViEngine.HandleChar(const AChar: Char);
begin
  FCurrentChar := AChar;
  FBuffer := GetEditBuffer;
  FCursorPosition := GetEditPosition(FBuffer);
  try
    if FInMark then
      SaveMarkPosition
    else if FInGotoMark then
      MoveToMarkPosition
    else if CharInSet(FCurrentChar, ['0' .. '9']) then
      ActionUpdateCount
    else if FViMoveKeybinds.ContainsKey(FCurrentChar) then
      ProcessMovement
    else if FViKeybinds.ContainsKey(FCurrentChar) then
    begin
      FViKeybinds[AChar]();
      ResetCount;
    end;

  finally
    // Avoid dangling reference error when closing the IDE
    FBuffer := nil;
    FCursorPosition := nil;
  end;
end;

procedure TViEngine.ProcessChange;
begin
  if FInRepeatChange then
  begin
    ApplyActionToSelection(baDelete, False);
    FCursorPosition.InsertText(FPreviousAction.FInsertText)
  end
  else
  begin
    if (FCurrentChar = 'w') then
      FCurrentChar := 'e';
    if (FCurrentChar = 'W') then
      FCurrentChar := 'E';
    SavePreviousAction;
    ApplyActionToSelection(baDelete, False);
    currentViMode := mInsert;
  end;
  currentEditMode := emNone;
end;

procedure TViEngine.ProcessDeletion;
begin
  if not FInRepeatChange then
    SavePreviousAction;

  ApplyActionToSelection(baDelete, False);
  currentEditMode := emNone;
end;

procedure TViEngine.FillViBindings;
begin
  // FViKeybinds.Add('''', ActionMark);
  // FViKeybinds.Add('*', ActionAsterisk);
  FViMoveKeybinds.Add(' ', ActionMoveRight);
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
  FViMoveKeybinds.Add('b', ActionMoveWordBack);
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

procedure TViEngine.ProcessLineDeletion;
begin
  if not FInRepeatChange then
    SavePreviousAction;

  FCursorPosition.MoveBOL;
  FCurrentChar := 'j';
  ApplyActionToSelection(baDelete, True);
  currentEditMode := emNone;
end;

procedure TViEngine.ProcessLineYanking;
begin
  FCursorPosition.Save;
  FCursorPosition.MoveBOL;
  FCurrentChar := 'j';
  ApplyActionToSelection(baYank, True);
  FCursorPosition.Restore;
  currentEditMode := emNone;
end;

procedure TViEngine.ProcessMovement;
var
  Pos: TOTAEditPos;
begin
  case currentEditMode of
    emNone:
      begin
        Pos := GetPositionForMove(FCurrentChar, GetCurrentCount);
        FCursorPosition.Move(Pos.Line, Pos.Col);
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

procedure TViEngine.ProcessYanking;
begin
  FCursorPosition.Save;
  ApplyActionToSelection(baYank, False);
  FCursorPosition.Restore;
  currentEditMode := emNone;
end;

procedure TViEngine.MoveToMarkPosition;
begin
  FCursorPosition.Move(FMarkArray[ord(FCurrentChar)].Line, FMarkArray[ord(FCurrentChar)].Col);
  FInGotoMark := False;
end;

procedure TViEngine.Paste(const AEditPosition: IOTAEditPosition; const ABuffer: IOTAEditBuffer; ADirection: TDirection);
var
  LAutoIndent, LPastingInSelection: Boolean;
  LSelection: IOTAEditBlock;
  LRow, LCol: Integer;

  function FixCursorPosition: Boolean;
  begin
    result := (not LPastingInSelection) and (ADirection = dForward);
  end;

begin
  SavePreviousAction;
  LPastingInSelection := False;
  LAutoIndent := ABuffer.BufferOptions.AutoIndent;

  LSelection := ABuffer.EditBlock;
  if LSelection.Size > 0 then
  begin
    LPastingInSelection := True;
    LRow := LSelection.StartingRow;
    LCol := LSelection.StartingColumn;
    LSelection.Delete;
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

procedure TViEngine.SaveMarkPosition;
begin
  FMarkArray[ord(FCurrentChar)].Col := FCursorPosition.Column;
  FMarkArray[ord(FCurrentChar)].Line := FCursorPosition.Row;
  FInMark := False;
end;

procedure TViEngine.SavePreviousAction;
begin
  // TODO: Save the new actions
  FPreviousAction.ActionChar := FCurrentChar;
  FPreviousAction.FCurrentEditMode := currentEditMode;
  FPreviousAction.FEditCount := FEditCount;
  FPreviousAction.FCurrentCount := FCurrentCount;
  // self.FPreviousAction.FInsertText := FInsertText;
end;

procedure TViEngine.SetViMode(ANewMode: TViMode);
var
  LText: String;
begin
  FCurrentViMode := ANewMode;
  ConfigureCursor;
  if assigned(FOnModeChanged) then
  begin
    LText := ANewMode.ToString;
    FOnModeChanged(LText);
  end;
end;

procedure TViEngine.SetOnModeChanged(ANewProc: TP_ModeChanged);
begin
  FOnModeChanged := ANewProc;
  FOnModeChanged(currentViMode.ToString); // call new procedure immediately
end;

procedure TViEngine.SwitchToInsertModeOrDoPreviousAction;
begin
  if (FInRepeatChange) then
    FCursorPosition.InsertText(FPreviousAction.FInsertText)
  else
  begin
    SavePreviousAction;
    currentViMode := mInsert;
  end;
end;

procedure TViEngine.ToggleActive;
begin
  if currentViMode = mInactive then
    currentViMode := mNormal
  else
    currentViMode := mInactive;
end;

function TViEngine.YankSelection: Boolean;
var
  LSelection: IOTAEditBlock;
begin
  LSelection := FBuffer.EditBlock;
  if LSelection.Size = 0 then
    Exit(False);

  FRegisterArray[FSelectedRegister].IsLine := False;
  FRegisterArray[FSelectedRegister].Text := LSelection.Text;
  LSelection.Reset;
  result := True;
end;

{ --- BEGIN OF ACTION PROCEDURES --------------------------------------------- }

// '$'
procedure TViEngine.ActionMoveEOL(ACount: Integer);
begin
  FCursorPosition.MoveEOL;
  // When moving, must stop at last char, not on line break.
  if currentEditMode = emNone then
    FCursorPosition.MoveRelative(0, -1);
end;

// '
procedure TViEngine.ActionMark;
begin
  { TODO : I have no idea what this is }
  FInGotoMark := True;
end;

// *
procedure TViEngine.ActionAsterisk;
begin
  { TODO : Look for asterisk in vi specification }
  FindWordAtCursor(FBuffer.TopView, currentCount);
end;

// .
procedure TViEngine.ActionRepeatLastCommand;
begin
  FInRepeatChange := True;
  currentEditMode := FPreviousAction.FCurrentEditMode;
  FEditCount := FPreviousAction.FEditCount;
  FCurrentCount := FPreviousAction.FCurrentCount;
  HandleChar(FPreviousAction.ActionChar);
  FInRepeatChange := False;
end;

// 0 -> count handling in ActionUpdateCount
procedure TViEngine.ActionMoveBOLorCount(ACount: Integer);
begin
  FCursorPosition.MoveBOL;
end;

// 1-9; 0 if no count input
procedure TViEngine.ActionUpdateCount;
begin
  if (FCurrentChar = '0') and (not hasCountInput) then
    ProcessMovement
  else
    FCurrentCount := 10 * FCurrentCount + (ord(FCurrentChar) - ord('0'));
end;

// <
procedure TViEngine.ActionShiftLeft;
begin
  SavePreviousAction;
  ChangeIndentation(dBack);
end;

// >
procedure TViEngine.ActionShiftRight;
begin
  SavePreviousAction;
  ChangeIndentation(dForward);
end;

// A
procedure TViEngine.ActionAppendEOL;
begin
  FCursorPosition.MoveEOL;
  SwitchToInsertModeOrDoPreviousAction;
end;

// B
procedure TViEngine.ActionMoveNonWhitespaceBack(ACount: Integer);
var
  i: Integer;
begin
  for i := 1 to ACount do
  begin
    FCursorPosition.MoveCursor(mmSkipWhite or mmSkipLeft or mmSkipStream);
    FCursorPosition.MoveCursor(mmSkipNonWhite or mmSkipLeft);
  end;
end;

// C
procedure TViEngine.ActionChangeRestOfLine;
begin
  currentEditMode := emChange;
  FEditCount := currentCount;
  HandleChar('$');
end;

// D
procedure TViEngine.ActionDeleteRestOfLine;
begin
  currentEditMode := emDelete;
  HandleChar('$');
end;

// E
procedure TViEngine.ActionMoveToEndOfWord(ACount: Integer);
var
  i: Integer;
begin
  for i := 1 to ACount do
  begin
    if (FCursorPosition.IsWordCharacter or FCursorPosition.IsSpecialCharacter) and
      (CharAtRelativeLocation(1) = viWhiteSpace) then
      FCursorPosition.MoveRelative(0, 1);

    if FCursorPosition.IsWhiteSpace then
      FCursorPosition.MoveCursor(mmSkipWhite or mmSkipRight or mmSkipStream);

    FCursorPosition.MoveCursor(mmSkipNonWhite or mmSkipRight);
    FCursorPosition.MoveRelative(0, -1);
  end;
end;

// G
procedure TViEngine.ActionGoToLineNumber;
begin
  if hasCountInput then
    FCursorPosition.GotoLine(currentCount)
  else
    FCursorPosition.MoveEOF;
end;

// H
procedure TViEngine.ActionMoveToScreenLine;
begin
  { TODO : Support for Count: Move cursor to count'th line displayed on screen }
  FCursorPosition.Move(FBuffer.TopView.TopRow, 0);
  FCursorPosition.MoveBOL;
end;

// I
procedure TViEngine.ActionInsertBeginningOfLine;
begin
  FCursorPosition.MoveBOL;
  SwitchToInsertModeOrDoPreviousAction;
end;

// J
procedure TViEngine.ActionJoinLines;
begin
  { TODO : Support for Count: Join multiple lines }
  FCursorPosition.MoveEOL;
  FCursorPosition.Delete(1);
end;

// L
procedure TViEngine.ActionMoveToBottomScreenLine;
begin
  { TODO : Support for Count: ith a count, to the first non-white of the
    count'th line from the bottom. Operators affect whole lines when used
    with L(2.3). }
  FCursorPosition.Move(FBuffer.TopView.BottomRow - 1, 0);
  FCursorPosition.MoveBOL;
end;

// M
procedure TViEngine.ActionMoveToMiddleOfScreen;
var
  LView: IOTAEditView;
begin
  LView := FBuffer.TopView;
  FCursorPosition.Move(LView.TopRow + Trunc(((LView.BottomRow - 1) - LView.TopRow) / 2), 0);
  FCursorPosition.MoveBOL;
end;

// N
procedure TViEngine.ActionFindPreviousWordAtCursor;
var
  i: Integer;
begin
  FCursorPosition.SearchOptions.Direction := sdBackward;
  for i := 1 to currentCount do
    FCursorPosition.SearchAgain;
end;

// O
procedure TViEngine.ActionOpenLineAboveCurrent;
begin
  FCursorPosition.MoveBOL;
  FCursorPosition.InsertText(#13#10);
  FCursorPosition.MoveCursor(mmSkipWhite or mmSkipRight);
  FCursorPosition.MoveRelative(-1, 0);
  SwitchToInsertModeOrDoPreviousAction;
  (BorlandIDEServices as IOTAEditorServices).TopView.MoveViewToCursor;
end;

// P
procedure TViEngine.ActionPasteBeforeCursor;
begin
  Paste(FCursorPosition, FBuffer, dBack);
end;

// R
procedure TViEngine.ActionReplaceCharacters;
begin
  // XXX Fix me for '.' command
  FBuffer.BufferOptions.InsertMode := False;
  currentViMode := mInsert;
end;

// S
procedure TViEngine.ActionChangeLines;
begin
  currentEditMode := emChange;
  FCursorPosition.MoveBOL;
  HandleChar('$');
end;

// W
procedure TViEngine.ActionMoveToNextCharacterWord(ACount: Integer);
var
  i: Integer;
begin
  for i := 1 to ACount do
  begin
    // Goto first white space after the end of the word.
    FCursorPosition.MoveCursor(mmSkipNonWhite or mmSkipRight);
    // Now skip all the white space until we're at the start of a word again.
    FCursorPosition.MoveCursor(mmSkipWhite or mmSkipRight or mmSkipStream);
  end;
end;

// X
procedure TViEngine.ActionDeleteSingleCharBeforeCursor;
begin
  currentEditMode := emDelete;
  if DeleteSelection then
    HandleChar('d')
  else
  begin
    FEditCount := currentCount - 1;
    HandleChar('h');
  end
end;

// Y
procedure TViEngine.ActionYankLine;
begin
  currentEditMode := emYank;
  FEditCount := currentCount;
  HandleChar('y');
end;

// ^
procedure TViEngine.ActionFirstNonWhiteInLine;
begin
  FCursorPosition.MoveBOL;
  FCursorPosition.MoveCursor(mmSkipWhite);
end;

// a
procedure TViEngine.ActionAppend;
begin
  FCursorPosition.MoveRelative(0, 1);
  SwitchToInsertModeOrDoPreviousAction;
end;

// b
procedure TViEngine.ActionMoveWordBack(ACount: Integer);
var
  i: Integer;
  LNextChar: TViCharClass;
begin
  for i := 1 to ACount do
  begin
    LNextChar := CharAtRelativeLocation(-1);
    if FCursorPosition.IsWordCharacter and ((LNextChar = viSpecial) or (LNextChar = viWhiteSpace)) then
      FCursorPosition.MoveRelative(0, -1);

    if FCursorPosition.IsSpecialCharacter and ((LNextChar = viWord) or (LNextChar = viWhiteSpace)) then
      FCursorPosition.MoveRelative(0, -1);

    if FCursorPosition.IsWhiteSpace then
    begin
      FCursorPosition.MoveCursor(mmSkipWhite or mmSkipLeft or mmSkipStream);
      FCursorPosition.MoveRelative(0, -1);
    end;

    if FCursorPosition.IsWordCharacter then
      FCursorPosition.MoveCursor(mmSkipWord or mmSkipLeft) // Skip to first non word character.
    else if FCursorPosition.IsSpecialCharacter then
      FCursorPosition.MoveCursor(mmSkipSpecial or mmSkipLeft);
    // Skip to the first non special character
  end;
end;

// c
procedure TViEngine.ActionChange;
begin
  if currentEditMode = emChange then
  begin
    FCursorPosition.MoveBOL;
    HandleChar('$');
  end
  else
  begin
    if DeleteSelection then
      SwitchToInsertModeOrDoPreviousAction
    else
    begin
      currentEditMode := emChange;
      FEditCount := currentCount;
    end
  end;
end;

// d
procedure TViEngine.ActionDelete;
begin
  if currentEditMode = emDelete then
  begin
    ProcessLineDeletion;
  end
  else if not DeleteSelection then
  begin
    currentEditMode := emDelete;
    FEditCount := currentCount;
  end;
end;

// e
procedure TViEngine.ActionMoveEndOfNextWord(ACount: Integer);
var
  i: Integer;
  LNextChar: TViCharClass;
begin
  for i := 1 to ACount do
  begin
    LNextChar := CharAtRelativeLocation(1);
    if (FCursorPosition.IsWordCharacter and (LNextChar = viWhiteSpace) or (LNextChar = viSpecial)) then
      FCursorPosition.MoveRelative(0, 1);

    if (FCursorPosition.IsSpecialCharacter and (LNextChar = viWhiteSpace) or (LNextChar = viWord)) then
      FCursorPosition.MoveRelative(0, 1);

    if FCursorPosition.IsWhiteSpace then
      FCursorPosition.MoveCursor(mmSkipWhite or mmSkipRight or mmSkipStream);

    if FCursorPosition.IsSpecialCharacter then
      FCursorPosition.MoveCursor(mmSkipSpecial or mmSkipRight);

    if FCursorPosition.IsWordCharacter then
      FCursorPosition.MoveCursor(mmSkipWord or mmSkipRight);

    FCursorPosition.MoveRelative(0, -1);
  end;
end;

// g
procedure TViEngine.ActionJump;
begin
  { TODO : Look for better name of this function }
  if FInGo then
  begin
    FCursorPosition.Move(1, 1);
    FInGo := False;
  end
  else
  begin
    FInGo := True;
    FEditCount := currentCount;
  end
end;

// h
procedure TViEngine.ActionMoveLeft(ACount: Integer);
begin
  FCursorPosition.MoveRelative(0, -ACount);
end;

// i
procedure TViEngine.ActionInsert;
begin
  SwitchToInsertModeOrDoPreviousAction;
end;

// j
procedure TViEngine.ActionMoveDown(ACount: Integer);
begin
  FCursorPosition.MoveRelative(ACount, 0);
end;

// k
procedure TViEngine.ActionMoveUp(ACount: Integer);
begin
  FCursorPosition.MoveRelative(-ACount, 0);
end;

// l
procedure TViEngine.ActionMoveRight(ACount: Integer);
begin
  FCursorPosition.MoveRelative(0, ACount);
end;

// m
procedure TViEngine.ActionSetMark;
begin
  FInMark := True;
end;

// n
procedure TViEngine.ActionRepeatLastScan;
begin
  { TODO : Look for better function name }
  FindNextWordAtCursor(currentCount);
end;

// o
procedure TViEngine.ActionOpenLineBelowCurrent;
begin
  FCursorPosition.MoveEOL;
  FCursorPosition.InsertText(#13#10);
  SwitchToInsertModeOrDoPreviousAction;
  (BorlandIDEServices as IOTAEditorServices).TopView.MoveViewToCursor;
end;

// p
procedure TViEngine.ActionPaste;
begin
  Paste(FCursorPosition, FBuffer, dForward);
end;

// s
procedure TViEngine.ActionChangeSingleChar;
begin
  if not DeleteSelection then
    FCursorPosition.Delete(1);
  SwitchToInsertModeOrDoPreviousAction;
end;

// u
procedure TViEngine.ActionUndo;
begin
  { TODO : Jump to position that is undone }
  FBuffer.Undo;
end;

// w
procedure TViEngine.ActionMoveToBeginningOfNextWord(ACount: Integer);
var
  i: Integer;
begin
  for i := 1 to ACount do
  begin
    if FCursorPosition.IsWordCharacter then
      FCursorPosition.MoveCursor(mmSkipWord or mmSkipRight) // Skip to first non word character.
    else if FCursorPosition.IsSpecialCharacter then
      FCursorPosition.MoveCursor(mmSkipSpecial or mmSkipRight or mmSkipStream);
    // Skip to the first non special character
    // If the character is whitespace or EOL then skip that whitespace
    if FCursorPosition.IsWhiteSpace or (FCursorPosition.Character = #$D) then
      FCursorPosition.MoveCursor(mmSkipWhite or mmSkipRight or mmSkipStream);
  end;
end;

// x
procedure TViEngine.ActionDeleteSingleChar;
begin
  if not DeleteSelection then
  begin
    currentEditMode := emDelete;
    FEditCount := currentCount - 1;
    HandleChar('l');
  end;
end;

// y
procedure TViEngine.ActionYank;
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
      FEditCount := currentCount;
  end;
end;

{ ---------------------------------------------------------------------------- }

end.
