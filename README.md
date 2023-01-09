# StackAnimate

```pascal
procedure TForm2.FormCreate(Sender: TObject);
begin
  StackAnimate := TStackAnimate.Create(Layout1);
  StackAnimate.AnimationDuration := 0.1;
  Layout1.AddObject(StackAnimate);
  StackAnimate.UpdateList;
  StackAnimate.OnChangeOrder := FOnChangeOrder;
  StackAnimate.OnEndOrder := FOnEndOrder;
  StackAnimate.HookAllControls;
end;

procedure TForm2.FOnChangeOrder(Sender: TObject);
begin
  Memo1.Lines.Add('FOnChangeOrder');
end;

procedure TForm2.FOnEndOrder(Sender: TObject; WasChanged: Boolean);
begin
  if WasChanged then
    Memo1.Lines.Add('WasChanged ' +
      StackAnimate.Items[StackAnimate.LastChange.NewIndex].Name + ' to ' +
      StackAnimate.LastChange.NewIndex.ToString);
end;
```
 
![Sample](https://github.com/HemulGM/StackAnimate/raw/main/media/ezgif-5-e5854c1030.gif)
