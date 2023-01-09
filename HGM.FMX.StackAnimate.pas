unit HGM.FMX.StackAnimate;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Defaults,
  System.Generics.Collections, FMX.Controls, System.Types, FMX.Types, FMX.Ani;

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
    property Container: TControl read FContainer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure UpdateList;
    procedure StartMoving(Control: TControl);
    procedure StopMoving(Control: TControl);
    procedure MoveControl(Control: TControl);
    procedure UpdateStack(Animate: Boolean);
    //
    property AnimationDuration: Single read FAnimationDuration write SetAnimationDuration;
    property AnimationInterpolationType: TInterpolationType read FAnimationInterpolationType write SetAnimationInterpolationType;
    //
    property VerticalGap: Single read FVerticalGap write SetVerticalGap;
    //
    function GetItemOrder(Control: TControl): Integer;
    function GetItems: TArray<TControl>;
    //
    property OnChangeOrder: TNotifyEvent read FOnChangeOrder write SetOnChangeOrder;
    property OnEndOrder: TOnEndOrder read FOnEndOrder write SetOnEndOrder;
    property LastChange: TLastChange read FLastChange;
    property Items[Index: Integer]: TControl read GetItem;
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
end;

destructor TStackAnimate.Destroy;
begin
  FItems.Free;
  inherited;
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
  FSavedOrder := FItems.ToArray;
  FMoving := True;
  FMousePos := Screen.MousePos;
  FMovingBtn := Control;
  FLastChange.OldIndex := FItems.IndexOf(Control);
  FSavePos := Control.Position.Point;
  Control.BringToFront;
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
end;

procedure TStackAnimate.DoOnEndOrder;
begin
  if Assigned(FOnEndOrder) then
    FOnEndOrder(Self, not CompareArray(FSavedOrder, FItems.ToArray));
end;

procedure TStackAnimate.SetAnimationDuration(const Value: Single);
begin
  FAnimationDuration := Value;
end;

procedure TStackAnimate.SetAnimationInterpolationType(const Value: TInterpolationType);
begin
  FAnimationInterpolationType := Value;
end;

procedure TStackAnimate.SetItemPos(const Index: Integer; Animate: Boolean);
begin
  var Item := FItems[Index];
  var HOffset := 0.0;
  var ItemCount := 0;
  for var i := 0 to Pred(Index) do
    if FItems[i] <> Item then
    begin
      HOffset := HOffset + FItems[i].Height;
      Inc(ItemCount);
    end;
  var NPos := TPointF.Create(0, HOffset + ItemCount * FVerticalGap);
  NPos.Offset(Container.Padding.Left, Container.Padding.Top);
  if Item.Position.Point <> NPos then
  begin
    if Animate then
    begin
      TAnimator.AnimateFloat(Item, 'Position.Y',
        NPos.Y,
        FAnimationDuration,
        TAnimationType.InOut,
        FAnimationInterpolationType);
      TAnimator.AnimateFloat(Item, 'Position.X',
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

procedure TStackAnimate.UpdateList;
begin
  if not CheckParent then
    Exit;
  FItems.Clear;
  for var Control in Container.Controls do
    FItems.Add(Control);
  UpdateStack(True);
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
    begin
      StopAnimation(FItems[i], 'Position.Y');
      SetItemPos(i, Animate);
    end;
end;

end.

