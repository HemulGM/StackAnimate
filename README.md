# StackAnimate
 
```pascal
Create

StackAnimate := TStackAnimate.Create(Layout1);
StackAnimate.AnimationDuration := 0.1;
Layout1.AddObject(StackAnimate);
StackAnimate.UpdateList;
StackAnimate.OnChangeOrder := FOnChangeOrder;
StackAnimate.OnEndOrder := FOnEndOrder;

Work

procedure TForm2.Button1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Btn: TButton absolute Sender;
begin
  if Button = TMouseButton.mbLeft then
    StackAnimate.StartMoving(Btn);
end;

procedure TForm2.Button1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
  Btn: TButton absolute Sender;
begin
  StackAnimate.MoveControl(Btn);
end;

procedure TForm2.Button1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Btn: TButton absolute Sender;
begin
  if Button = TMouseButton.mbLeft then
    StackAnimate.StopMoving(Btn);
end;
```
