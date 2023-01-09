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
  ProgressBar1.Value := 100 / Layout1.ControlsCount * StackAnimate.GetItemOrder(ProgressBar1);
end;

procedure TForm2.FOnEndOrder(Sender: TObject; WasChanged: Boolean);
begin
  if WasChanged then
    Memo1.Lines.Add('WasChanged ' +
      StackAnimate.Items[StackAnimate.LastChange.NewIndex].Name + ' to ' +
      StackAnimate.LastChange.NewIndex.ToString);
end;
```
