unit HGM.FMX.StackAnimate;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Defaults,
  System.Generics.Collections, FMX.Controls, System.Types, FMX.Types, FMX.Ani,
  System.UITypes;

type
  TOnEndOrder = procedure(Sender: TObject; WasChanged: Boolean) of object;

  TLastChange = record
    OldIndex: Integer;
    NewIndex: Integer;
  end;

  TStackAnimate = class(TFmxObject)
  private
    FItems: TList<TControl>;
    FMousePos: TPointF;
    FSavePos: TPointF;
    FMoving: Boolean;
    FMovingBtn: TControl;
    FContainer: TControl;
    FAnimationDuration: Single;
    FAnimationInterpolationType: TInterpolationType;
    FVerticalGap: Single;
    FOnChangeOrder: TNotifyEvent;
    FOnEndOrder: TOnEndOrder;
    FSavedOrder: TArray<TControl>;
    FLastChange: TLastChange;
    FAnimateSelected: Boolean;
    function CheckParent: Boolean;
    procedure SetItemPos(const Index: Integer; Animate: Boolean);
    procedure SetAnimationDuration(const Value: Single);
    procedure SetAnimationInterpolationType(const Value: TInterpolationType);
    procedure SetVerticalGap(const Value: Single);
    function CompareArray(const Left, Right: TArray<TControl>): Boolean;
    procedure DoChangeOrder;
    procedure SetOnChangeOrder(const Value: TNotifyEvent);
    procedure SetOnEndOrder(const Value: TOnEndOrder);
    procedure DoOnEndOrder;
    function GetItem(Index: Integer): TControl;
    procedure HookMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure HookMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure HookMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    function GetCount: Integer;
    procedure SetAnimateSelected(const Value: Boolean);
    procedure AnimateFloatProp(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear);
    property Container: TControl read FContainer;
    procedure UpdateStack(Animate: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    /// Обинвить список контролов родителя
    /// </summary>
    procedure UpdateList(Animate: Boolean = True);
    /// <summary>
    /// Ручное управление. Начало перемещения контрола
    /// </summary>
    procedure StartMoving(Control: TControl);
    /// <summary>
    /// Ручное управление. Завершение перемещения контрола
    /// </summary>
    procedure StopMoving(Control: TControl);
    /// <summary>
    /// Ручное управление. Перемещение контрола
    /// </summary>
    procedure MoveControl(Control: TControl);
    /// <summary>
    /// Перехватить события OnMouseDown, OnMouseMove и OnMouseUp и установить совйтсов HitTest в True всех имеющихся контролов
    /// </summary>
    procedure HookAllControls;
    /// <summary>
    /// Длительность анимации
    /// </summary>
    property AnimationDuration: Single read FAnimationDuration write SetAnimationDuration;
    /// <summary>
    /// Тип анимации
    /// </summary>
    property AnimationInterpolationType: TInterpolationType read FAnimationInterpolationType write SetAnimationInterpolationType;
    /// <summary>
    /// Вертикальный отступ между элементами
    /// </summary>
    property VerticalGap: Single read FVerticalGap write SetVerticalGap;
    /// <summary>
    /// Получить актуальный индекс конкретного контрола
    /// </summary>
    function GetItemOrder(Control: TControl): Integer;
    /// <summary>
    /// Получить список контролов в актуальном порядке
    /// </summary>
    function GetItems: TArray<TControl>;
    /// <summary>
    /// Происходит на каждое изменении позиции (не координат) в списке
    /// </summary>
    property OnChangeOrder: TNotifyEvent read FOnChangeOrder write SetOnChangeOrder;
    /// <summary>
    /// Происходит при завершении перемещения контрола (отпускание мыши).
    /// Передает информацию о том, были ли изменения или нет
    /// </summary>
    property OnEndOrder: TOnEndOrder read FOnEndOrder write SetOnEndOrder;
    /// <summary>
    /// Информация о последнем изменении позиции элемента
    /// </summary>
    property LastChange: TLastChange read FLastChange;
    /// <summary>
    /// Получить контрол по индексу (например из LastChange)
    /// </summary>
    property Items[Index: Integer]: TControl read GetItem;
    /// <summary>
    /// Кол-во контролов
    /// </summary>
    property Count: Integer read GetCount;
    property AnimateSelected: Boolean read FAnimateSelected write SetAnimateSelected;
  end;

implementation

uses
  FMX.Forms;

function StopAnimation(const Target: TFmxObject; const APropertyName: string): Boolean;
var
  i: Integer;
begin
  i := Target.ChildrenCount - 1;
  while i >= 0 do
  begin
    if (Target.Children[i] is TCustomPropertyAnimation) and
      (CompareText(TCustomPropertyAnimation(Target.Children[i]).PropertyName, APropertyName) = 0)
      then
      TCustomPropertyAnimation(Target.Children[i]).StopAtCurrent;
    if i > Target.ChildrenCount then
      i := Target.ChildrenCount;
    Dec(i);
  end;
  Result := False;
end;

{ TStackAnimate }

function TStackAnimate.CheckParent: Boolean;
begin
  if Assigned(FContainer) then
    Exit(True);
  if Assigned(Parent) and (Parent is TControl) then
  begin
    FContainer := Parent as TControl;
    Exit(True);
  end;
  Result := False;
end;

constructor TStackAnimate.Create(AOwner: TComponent);
begin
  inherited;
  FLastChange.OldIndex := -1;
  FLastChange.NewIndex := -1;
  FItems := TList<TControl>.Create;
  FAnimationDuration := 0.2;
  FAnimationInterpolationType := TInterpolationType.Linear;
  FVerticalGap := 5;
  FAnimateSelected := True;
end;

destructor TStackAnimate.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TStackAnimate.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TStackAnimate.GetItem(Index: Integer): TControl;
begin
  Result := FItems[Index];
end;

function TStackAnimate.GetItemOrder(Control: TControl): Integer;
begin
  Result := FItems.IndexOf(Control);
end;

function TStackAnimate.GetItems: TArray<TControl>;
begin
  Result := FItems.ToArray;
end;

procedure TStackAnimate.HookAllControls;
begin
  for var Item in FItems do
  begin
    Item.OnMouseDown := HookMouseDown;
    Item.OnMouseMove := HookMouseMove;
    Item.OnMouseUp := HookMouseUp;
    Item.HitTest := True;
  end;
end;

procedure TStackAnimate.HookMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Control: TControl absolute Sender;
begin
  if Button = TMouseButton.mbLeft then
    StartMoving(Control);
end;

procedure TStackAnimate.HookMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  Control: TControl absolute Sender;
begin
  MoveControl(Control);
end;

procedure TStackAnimate.HookMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Control: TControl absolute Sender;
begin
  if Button = TMouseButton.mbLeft then
    StopMoving(Control);
end;

procedure TStackAnimate.MoveControl(Control: TControl);
begin
  if not FMoving then
    Exit;
  Control.Position.Y := FSavePos.Y - (FMousePos.Y - Screen.MousePos.Y);
  UpdateStack(True);
end;

procedure TStackAnimate.StartMoving(Control: TControl);
begin
  if not CheckParent then
    Exit;
  if Assigned(Control.Root) then
    Control.Root.Captured := Control;
  FSavedOrder := FItems.ToArray;
  FMoving := True;
  FMousePos := Screen.MousePos;
  FMovingBtn := Control;
  FLastChange.OldIndex := FItems.IndexOf(Control);
  FSavePos := Control.Position.Point;
  Control.BringToFront;

  if AnimateSelected then
  begin
    TAnimator.AnimateFloat(Control, 'Scale.X', 0.9);
    TAnimator.AnimateFloat(Control, 'Scale.Y', 0.9);
    TAnimator.AnimateFloat(Control, 'RotationAngle', 5);
  end;
end;

procedure TStackAnimate.StopMoving(Control: TControl);
begin
  if not CheckParent then
    Exit;
  FMoving := False;
  FMovingBtn := nil;
  UpdateStack(True);
  FLastChange.NewIndex := FItems.IndexOf(Control);
  DoOnEndOrder;

  if AnimateSelected then
  begin
    TAnimator.AnimateFloat(Control, 'Scale.X', 1);
    TAnimator.AnimateFloat(Control, 'Scale.Y', 1);
    TAnimator.AnimateFloat(Control, 'RotationAngle', 0);
  end;
end;

procedure TStackAnimate.DoOnEndOrder;
begin
  if Assigned(FOnEndOrder) then
    FOnEndOrder(Self, not CompareArray(FSavedOrder, FItems.ToArray));
end;

procedure TStackAnimate.SetAnimateSelected(const Value: Boolean);
begin
  FAnimateSelected := Value;
end;

procedure TStackAnimate.SetAnimationDuration(const Value: Single);
begin
  FAnimationDuration := Value;
end;

procedure TStackAnimate.SetAnimationInterpolationType(const Value: TInterpolationType);
begin
  FAnimationInterpolationType := Value;
end;

procedure TStackAnimate.AnimateFloatProp(const Target: TFmxObject; const APropertyName: string; const NewValue: Single; Duration: Single = 0.2; AType: TAnimationType = TAnimationType.in; AInterpolation: TInterpolationType = TInterpolationType.Linear);
begin
  var i := Target.ChildrenCount - 1;
  var Animate: TFloatAnimation := nil;
  while i >= 0 do
  begin
    if (Target.Children[i] is TFloatAnimation) and
      (CompareText(TFloatAnimation(Target.Children[i]).PropertyName, APropertyName) = 0)
      then
      Animate := TFloatAnimation(Target.Children[i]);
    if i > Target.ChildrenCount then
      i := Target.ChildrenCount;
    Dec(i);
  end;

  if Assigned(Animate) then
  begin
    if Animate.StartValue = NewValue then
      Exit;
    Animate.StopAtCurrent;
  end;

  TAnimator.AnimateFloat(Target,
    APropertyName,
    NewValue,
    Duration,
    AType,
    AInterpolation);
end;

procedure TStackAnimate.SetItemPos(const Index: Integer; Animate: Boolean);
begin
  var Item := FItems[Index];
  var HOffset := 0.0;
  var ItemCount := 0;
  for var i := 0 to Pred(Index) do
    if FItems[i] <> Item then
    begin
      HOffset := HOffset + FItems[i].Height + FItems[i].Margins.Bottom + FItems[i].Margins.Top;
      Inc(ItemCount);
    end;
  var NPos := TPointF.Create(0, HOffset + ItemCount * FVerticalGap);
  NPos.Offset(Container.Padding.Left, Container.Padding.Top);
  NPos.Offset(Item.Margins.Left, Item.Margins.Top);
  if Item.Position.Point <> NPos then
  begin
    if Animate then
    begin
      AnimateFloatProp(Item, 'Position.Y',
        NPos.Y,
        FAnimationDuration,
        TAnimationType.InOut,
        FAnimationInterpolationType);
      AnimateFloatProp(Item, 'Position.X',
        NPos.X,
        FAnimationDuration,
        TAnimationType.InOut,
        FAnimationInterpolationType);
    end
    else
      Item.Position.Point := NPos;
  end;
end;

procedure TStackAnimate.SetOnChangeOrder(const Value: TNotifyEvent);
begin
  FOnChangeOrder := Value;
end;

procedure TStackAnimate.SetOnEndOrder(const Value: TOnEndOrder);
begin
  FOnEndOrder := Value;
end;

procedure TStackAnimate.SetVerticalGap(const Value: Single);
begin
  FVerticalGap := Value;
end;

procedure TStackAnimate.UpdateList(Animate: Boolean);
begin
  if not CheckParent then
    Exit;
  FItems.Clear;
  for var Control in Container.Controls do
    FItems.Add(Control);
  UpdateStack(Animate);
end;

function TStackAnimate.CompareArray(const Left, Right: TArray<TControl>): Boolean;
begin
  for var i := Low(Left) to High(Left) do
    if Left[i] <> Right[i] then
      Exit(False);
  Result := True;
end;

procedure TStackAnimate.DoChangeOrder;
begin
  if Assigned(FOnChangeOrder) then
    FOnChangeOrder(Self);
end;

procedure TStackAnimate.UpdateStack(Animate: Boolean);
begin
  if not CheckParent then
    Exit;
  var SavedOrder := FItems.ToArray;
  FItems.Sort(TComparer<TControl>.Construct(
    function(const Left, Right: TControl): Integer
    begin
      if Left.Position.Y < Right.Position.Y then
        Result := -1
      else if Left.Position.Y > Right.Position.Y then
        Result := 1
      else
        Result := 0;
    end));
  if not CompareArray(SavedOrder, FItems.ToArray) then
    DoChangeOrder;

  for var i := 0 to Pred(FItems.Count) do
    if FItems[i] <> FMovingBtn then
      SetItemPos(i, Animate);
end;

end.

