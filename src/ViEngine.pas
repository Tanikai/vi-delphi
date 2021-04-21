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

  TViAction = record
    ActionChar: Char;
    FCurrentEditMode: TViEditMode;
    FEditCount, FCurrentCount: Integer;
    FInsertText: String;
  end;

  TViBindings = class(TObject)
  private
    FCursorPosition: IOTAEditPosition;
    FBuffer: IOTAEditBuffer;
    FCurrentViMode: TViMode;
    FCurrentEditMode: TViEditMode;
    FCurrentCount: Integer; // Most recent number input of user
    FEditCount: Integer; // Previous input of number when editing text
    { TODO : Refactor MovementCount to parameter? }
    FMovementCount: Integer; // Just for movement
    FInGo: Boolean;
    FInMark: Boolean;
    FInGotoMark: Boolean;
    FInRepeatChange: Boolean;

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
    procedure ChangeIndentation(ADirection: TDirection);
    function DeleteSelection: Boolean;
    function GetCurrentCount: Integer;
    function GetEditCount: Integer;
    procedure ResetCount;
    procedure ActionUpdateCount;
    function GetPositionForMove(AKey: Char; ACount: Integer = 0): TOTAEditPos;
    procedure ProcessMovement;
    procedure MoveToMarkPosition;
    procedure Paste(const AEditPosition: IOTAEditPosition; const ABuffer: IOTAEditBuffer; ADirection: TDirection);
    procedure SaveMarkPosition;
    function YankSelection: Boolean;
    procedure ApplyActionToSelection(AAction: TBlockAction; AIsLine: Boolean);
    procedure FindNextWordAtCursor(const ACount: Integer);
    procedure ActionFindPreviousWordAtCursor;
    procedure FindWordAtCursor(const AView: IOTAEditView; const ACount: Integer);
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
    function CharAtRelativeLocation(ACol: Integer): TViCharClass;

    { Action Keybinds }
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

    { Movement Actions }
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
    procedure EditKeyDown(AKey, AScanCode: Word; AShift: TShiftState; AMsg: TMsg; var AHandled: Boolean);
    procedure EditChar(AKey, AScanCode: Word; AShift: TShiftState; AMsg: TMsg; var AHandled: Boolean);
    procedure ConfigureCursor;
    property currentCount: Integer read GetCurrentCount;
    property editCount: Integer read GetEditCount;
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

function TViModeHelper.ToString(): String;
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
  WriteLn('test');
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

procedure TViBindings.EditChar(AKey, AScanCode: Word; AShift: TShiftState; AMsg: TMsg; var AHandled: Boolean);
begin
  if currentViMode = mInactive then
    Exit;

  if currentViMode = mInsert then
    Exit;

  FShift := AShift;
  HandleChar(Chr(AKey));
  AHandled := True;
  (BorlandIDEServices as IOTAEditorServices).TopView.Paint;
end;

function TViBindings.CharAtRelativeLocation(ACol: Integer): TViCharClass;
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

procedure TViBindings.EditKeyDown(AKey, AScanCode: Word; AShift: TShiftState; AMsg: TMsg; var AHandled: Boolean);
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

procedure TViBindings.ResetCount;
begin
  FCurrentCount := 0;
end;

procedure TViBindings.ApplyActionToSelection(AAction: TBlockAction; AIsLine: Boolean);
var
  LCount: Integer;
  LPos: TOTAEditPos;
  LSelection: IOTAEditBlock;
  LTemp: String;
begin
  LCount := GetCurrentCount * GetEditCount;
  ResetCount;
  LPos := GetPositionForMove(FChar, LCount);
  if CharInSet(FChar, ['e', 'E']) then
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

procedure TViBindings.ChangeIndentation(ADirection: TDirection);
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

function TViBindings.DeleteSelection: Boolean;
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

procedure TViBindings.FindNextWordAtCursor(const ACount: Integer);
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

procedure TViBindings.FindWordAtCursor(const AView: IOTAEditView; const ACount: Integer);
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

function TViBindings.GetCurrentCount: Integer;
begin
  if FCurrentCount <= 0 then
    result := 1
  else
    result := FCurrentCount;
end;

function TViBindings.GetEditCount: Integer;
begin
  if FEditCount <= 0 then
    result := 1
  else
    result := FEditCount;
end;

function TViBindings.GetHasCountInput: Boolean;
begin
  result := (FCurrentCount > 0);
end;

// Given a movement key and a count return the position in the buffer where that
// movement would take you.
function TViBindings.GetPositionForMove(AKey: Char; ACount: Integer = 0): TOTAEditPos;
var
  LPos: TOTAEditPos;
begin
  FMovementCount := ACount;
  FCursorPosition.Save;

  if FViMoveKeybinds.ContainsKey(AKey) then
    FViMoveKeybinds[AKey]();

  LPos.Col := FCursorPosition.Column;
  LPos.Line := FCursorPosition.Row;
  FCursorPosition.Restore;

  result := LPos;
end;

procedure TViBindings.HandleChar(const AChar: Char);
begin
  FChar := AChar;
  FBuffer := GetEditBuffer;
  FCursorPosition := GetEditPosition(FBuffer);
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
    FCursorPosition := nil;
  end;
end;

procedure TViBindings.ProcessChange;
begin
  if FInRepeatChange then
  begin
    ApplyActionToSelection(baDelete, False);
    FCursorPosition.InsertText(FPreviousAction.FInsertText)
  end
  else
  begin
    if (FChar = 'w') then
      FChar := 'e';
    if (FChar = 'W') then
      FChar := 'E';
    SavePreviousAction;
    ApplyActionToSelection(baDelete, False);
    currentViMode := mInsert;
  end;
  currentEditMode := emNone;
end;

procedure TViBindings.ProcessDeletion;
begin
  if not FInRepeatChange then
    SavePreviousAction;

  ApplyActionToSelection(baDelete, False);
  currentEditMode := emNone;
end;

procedure TViBindings.FillViBindings;
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

  FCursorPosition.MoveBOL;
  FChar := 'j';
  ApplyActionToSelection(baDelete, True);
  currentEditMode := emNone;
end;

procedure TViBindings.ProcessLineYanking;
begin
  FCursorPosition.Save;
  FCursorPosition.MoveBOL;
  FChar := 'j';
  ApplyActionToSelection(baYank, True);
  FCursorPosition.Restore;
  currentEditMode := emNone;
end;

procedure TViBindings.ProcessMovement;
var
  Pos: TOTAEditPos;
begin
  case currentEditMode of
    emNone:
      begin
        Pos := GetPositionForMove(FChar, GetCurrentCount);
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

procedure TViBindings.ProcessYanking;
begin
  FCursorPosition.Save;
  ApplyActionToSelection(baYank, False);
  FCursorPosition.Restore;
  currentEditMode := emNone;
end;

procedure TViBindings.MoveToMarkPosition;
begin
  FCursorPosition.Move(FMarkArray[ord(FChar)].Line, FMarkArray[ord(FChar)].Col);
  FInGotoMark := False;
end;

procedure TViBindings.Paste(const AEditPosition: IOTAEditPosition; const ABuffer: IOTAEditBuffer;
  ADirection: TDirection);
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

procedure TViBindings.SaveMarkPosition;
begin
  FMarkArray[ord(FChar)].Col := FCursorPosition.Column;
  FMarkArray[ord(FChar)].Line := FCursorPosition.Row;
  FInMark := False;
end;

procedure TViBindings.SavePreviousAction;
begin
  // TODO: Save the new actions
  FPreviousAction.ActionChar := FChar;
  FPreviousAction.FCurrentEditMode := currentEditMode;
  FPreviousAction.FEditCount := FEditCount;
  FPreviousAction.FCurrentCount := FCurrentCount;
  // self.FPreviousAction.FInsertText := FInsertText;
end;

procedure TViBindings.SetViMode(ANewMode: TViMode);
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

procedure TViBindings.SetOnModeChanged(ANewProc: TP_ModeChanged);
begin
  FOnModeChanged := ANewProc;
  FOnModeChanged(currentViMode.ToString); // call new procedure immediately
end;

procedure TViBindings.SwitchToInsertModeOrDoPreviousAction;
begin
  if (FInRepeatChange) then
    FCursorPosition.InsertText(FPreviousAction.FInsertText)
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
procedure TViBindings.ActionMoveEOL;
begin
  FCursorPosition.MoveEOL;
  // When moving, must stop at last char, not on line break.
  if currentEditMode = emNone then
    FCursorPosition.MoveRelative(0, -1);
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
  FindWordAtCursor(FBuffer.TopView, currentCount);
end;

// .
procedure TViBindings.ActionRepeatLastCommand;
begin
  FInRepeatChange := True;
  currentEditMode := FPreviousAction.FCurrentEditMode;
  FEditCount := FPreviousAction.FEditCount;
  FCurrentCount := FPreviousAction.FCurrentCount;
  HandleChar(FPreviousAction.ActionChar);
  FInRepeatChange := False;
end;

// 0 -> count handling in ActionUpdateCount
procedure TViBindings.ActionMoveBOLorCount;
begin
  FCursorPosition.MoveBOL;
end;

// 1-9; 0 if no count input
procedure TViBindings.ActionUpdateCount;
begin
  if (FChar = '0') and (not hasCountInput) then
    ProcessMovement
  else
    FCurrentCount := 10 * FCurrentCount + (ord(FChar) - ord('0'));
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
  FCursorPosition.MoveEOL;
  SwitchToInsertModeOrDoPreviousAction;
end;

// B
procedure TViBindings.ActionMoveNonWhitespaceBack;
var
  i: Integer;
begin
  for i := 1 to FMovementCount do
  begin
    FCursorPosition.MoveCursor(mmSkipWhite or mmSkipLeft or mmSkipStream);
    FCursorPosition.MoveCursor(mmSkipNonWhite or mmSkipLeft);
  end;
end;

// C
procedure TViBindings.ActionChangeRestOfLine;
begin
  currentEditMode := emChange;
  FEditCount := currentCount;
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
procedure TViBindings.ActionGoToLineNumber;
begin
  if hasCountInput then
    FCursorPosition.GotoLine(currentCount)
  else
    FCursorPosition.MoveEOF;
end;

// H
procedure TViBindings.ActionMoveToScreenLine;
begin
  { TODO : Support for Count: Move cursor to count'th line displayed on screen }
  FCursorPosition.Move(FBuffer.TopView.TopRow, 0);
  FCursorPosition.MoveBOL;
end;

// I
procedure TViBindings.ActionInsertBeginningOfLine;
begin
  FCursorPosition.MoveBOL;
  SwitchToInsertModeOrDoPreviousAction;
end;

// J
procedure TViBindings.ActionJoinLines;
begin
  { TODO : Support for Count: Join multiple lines }
  FCursorPosition.MoveEOL;
  FCursorPosition.Delete(1);
end;

// L
procedure TViBindings.ActionMoveToBottomScreenLine;
begin
  { TODO : Support for Count: ith a count, to the first non-white of the
    count'th line from the bottom. Operators affect whole lines when used
    with L(2.3). }
  FCursorPosition.Move(FBuffer.TopView.BottomRow - 1, 0);
  FCursorPosition.MoveBOL;
end;

// M
procedure TViBindings.ActionMoveToMiddleOfScreen;
var
  LView: IOTAEditView;
begin
  LView := FBuffer.TopView;
  FCursorPosition.Move(LView.TopRow + Trunc(((LView.BottomRow - 1) - LView.TopRow) / 2), 0);
  FCursorPosition.MoveBOL;
end;

// N
procedure TViBindings.ActionFindPreviousWordAtCursor;
var
  i: Integer;
begin
  FCursorPosition.SearchOptions.Direction := sdBackward;
  for i := 1 to currentCount do
    FCursorPosition.SearchAgain;
end;

// O
procedure TViBindings.ActionOpenLineAboveCurrent;
begin
  FCursorPosition.MoveBOL;
  FCursorPosition.InsertText(#13#10);
  FCursorPosition.MoveCursor(mmSkipWhite or mmSkipRight);
  FCursorPosition.MoveRelative(-1, 0);
  SwitchToInsertModeOrDoPreviousAction;
  (BorlandIDEServices as IOTAEditorServices).TopView.MoveViewToCursor;
end;

// P
procedure TViBindings.ActionPasteBeforeCursor;
begin
  Paste(FCursorPosition, FBuffer, dBack);
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
  FCursorPosition.MoveBOL;
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
    FCursorPosition.MoveCursor(mmSkipNonWhite or mmSkipRight);
    // Now skip all the white space until we're at the start of a word again.
    FCursorPosition.MoveCursor(mmSkipWhite or mmSkipRight or mmSkipStream);
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
    FEditCount := currentCount - 1;
    HandleChar('h');
  end
end;

// Y
procedure TViBindings.ActionYankLine;
begin
  currentEditMode := emYank;
  FEditCount := currentCount;
  HandleChar('y');
end;

// ^
procedure TViBindings.ActionFirstNonWhiteInLine;
begin
  FCursorPosition.MoveBOL;
  FCursorPosition.MoveCursor(mmSkipWhite);
end;

// a
procedure TViBindings.ActionAppend;
begin
  FCursorPosition.MoveRelative(0, 1);
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
procedure TViBindings.ActionChange;
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
procedure TViBindings.ActionDelete;
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
procedure TViBindings.ActionMoveEndOfNextWord;
var
  i: Integer;
  LNextChar: TViCharClass;
begin
  for i := 1 to FMovementCount do
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
procedure TViBindings.ActionJump;
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
procedure TViBindings.ActionMoveLeft;
begin
  FCursorPosition.MoveRelative(0, -FMovementCount);
end;

// i
procedure TViBindings.ActionInsert;
begin
  SwitchToInsertModeOrDoPreviousAction;
end;

// j
procedure TViBindings.ActionMoveDown;
begin
  FCursorPosition.MoveRelative(+FMovementCount, 0);
end;

// k
procedure TViBindings.ActionMoveUp;
begin
  FCursorPosition.MoveRelative(-FMovementCount, 0);
end;

// l
procedure TViBindings.ActionMoveRight;
begin
  FCursorPosition.MoveRelative(0, +FMovementCount);
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
  FindNextWordAtCursor(currentCount);
end;

// o
procedure TViBindings.ActionOpenLineBelowCurrent;
begin
  FCursorPosition.MoveEOL;
  FCursorPosition.InsertText(#13#10);
  SwitchToInsertModeOrDoPreviousAction;
  (BorlandIDEServices as IOTAEditorServices).TopView.MoveViewToCursor;
end;

// p
procedure TViBindings.ActionPaste;
begin
  Paste(FCursorPosition, FBuffer, dForward);
end;

// s
procedure TViBindings.ActionChangeSingleChar;
begin
  if not DeleteSelection then
    FCursorPosition.Delete(1);
  SwitchToInsertModeOrDoPreviousAction;
end;

// u
procedure TViBindings.ActionUndo;
begin
  { TODO : Jump to position that is undone }
  FBuffer.Undo;
end;

// w
procedure TViBindings.ActionMoveToBeginningOfNextWord;
var
  i: Integer;
begin
  for i := 1 to FMovementCount do
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
procedure TViBindings.ActionDeleteSingleChar;
begin
  if not DeleteSelection then
  begin
    currentEditMode := emDelete;
    FEditCount := currentCount - 1;
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
      FEditCount := currentCount;
  end;
end;

{ ---------------------------------------------------------------------------- }

end.
