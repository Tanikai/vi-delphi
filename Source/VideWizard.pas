unit VideWizard;

interface

uses
  ViBindings,
  Classes,
  System.SysUtils,
  ToolsAPI,
  Vcl.AppEvnts,
  Vcl.Forms,
  Winapi.Windows,
  Winapi.Messages;

type

  TVIDEWizard = class(TNotifierObject, IOTAWizard)
  private
    FEvents: TApplicationEvents;
    FViBindings: TViBindings;
    procedure DoApplicationMessage(var Msg: TMsg; var Handled: Boolean);
  protected
    procedure EditKeyDown(Key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
    procedure EditChar(Key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    procedure BeforeDestruction; override;
  end;

procedure Register;

implementation

uses Vcl.Dialogs;

procedure Register;
begin
{$IFDEF CODESITE}CodeSite.TraceMethod('Register', tmoTiming); {$ENDIF}
  RegisterPackageWizard(TVIDEWizard.Create);
end;

function IsEditControl(AControl: TComponent): Boolean;
begin
  Result := (AControl <> nil) and AControl.ClassNameIs('TEditControl') and SameText(AControl.Name, 'Editor');
end;

procedure TVIDEWizard.BeforeDestruction;
begin
  inherited;
  FEvents.Free;
  FViBindings.Free;
end;

constructor TVIDEWizard.Create;
begin
  FEvents := TApplicationEvents.Create(nil);
  FEvents.OnMessage := DoApplicationMessage;
  FViBindings := TViBindings.Create;
end;

destructor TVIDEWizard.Destroy;
begin

  inherited;
end;

procedure TVIDEWizard.DoApplicationMessage(var Msg: TMsg; var Handled: Boolean);
var
  Key: Word;
  ScanCode: Word;
  Shift: TShiftState;
begin
  if ((Msg.message = WM_KEYDOWN) or (Msg.message = WM_KEYUP) or (Msg.message = WM_CHAR)) and
    IsEditControl(Screen.ActiveControl) then
  begin
    Key := Msg.wParam;
    ScanCode := (Msg.lParam and $00FF0000) shr 16;
    Shift := KeyDataToShiftState(Msg.lParam);

    if Msg.message = WM_CHAR then
      EditChar(Key, ScanCode, Shift, Msg, Handled)
    else
    begin
      if Key = VK_PROCESSKEY then
        Key := MapVirtualKey(ScanCode, 1);

      if Msg.message = WM_KEYDOWN then
        EditKeyDown(Key, ScanCode, Shift, Msg, Handled);
    end;
  end;
end;

procedure TVIDEWizard.EditChar(Key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
begin
  FViBindings.EditChar(Key, ScanCode, Shift, Msg, Handled);
end;

procedure TVIDEWizard.EditKeyDown(Key, ScanCode: Word; Shift: TShiftState; Msg: TMsg; var Handled: Boolean);
begin
  FViBindings.EditKeyDown(Key, ScanCode, Shift, Msg, Handled);
end;

procedure TVIDEWizard.Execute;
begin
  FViBindings.ConfigureCursor;
end;

function TVIDEWizard.GetIDString: string;
begin
  Result := 'VIDE.VIDEWizard';
end;

function TVIDEWizard.GetName: string;
begin
  Result := 'VIDE Wizard';
end;

function TVIDEWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

end.
