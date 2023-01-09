# StackAnimate
 
```pascal
StackAnimate := TStackAnimate.Create(Layout1);
StackAnimate.AnimationDuration := 0.1;
Layout1.AddObject(StackAnimate);
StackAnimate.UpdateList;
StackAnimate.OnChangeOrder := FOnChangeOrder;
StackAnimate.OnEndOrder := FOnEndOrder;
```
