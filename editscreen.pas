unit EditScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Menus;

type

  { TEditForm }

  TEditForm = class(TForm)
    EditPopup: TPopupMenu;
    FillRandom: TMenuItem;
    SaveButton: TButton;
    FullNameInput: TEdit;
    CriteriaBox: TGroupBox;
    ParametersBox: TGroupBox;
    FullNameLabel: TLabel;
    WeightMaxLabel: TLabel;
    AgeLabel: TLabel;
    HeightLabel: TLabel;
    WeightLabel: TLabel;
    AgeMinLabel: TLabel;
    AgeMaxLabel: TLabel;
    HeightMinLabel: TLabel;
    HeightMaxLabel: TLabel;
    WeightMinLabel: TLabel;
    AgeInput: TSpinEdit;
    HeightInput: TSpinEdit;
    WeightInput: TSpinEdit;
    AgeMinInput: TSpinEdit;
    AgeMaxInput: TSpinEdit;
    HeightMinInput: TSpinEdit;
    HeightMaxInput: TSpinEdit;
    WeightMinInput: TSpinEdit;
    WeightMaxInput: TSpinEdit;
    procedure FormShow(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private

  public

  end;

var
  EditForm: TEditForm;

implementation

{$R *.lfm}

uses Variables;

{ TEditForm }

procedure TEditForm.SaveButtonClick(Sender: TObject);
begin
  p.FullName := FullNameInput.Text;

  p.Parameters.Age := AgeInput.Value;
  p.Parameters.Height := HeightInput.Value;
  p.Parameters.Weight := WeightInput.Value;

  p.Criteria.AgeMin := AgeMinInput.Value;
  p.Criteria.AgeMax := AgeMaxInput.Value;

  p.Criteria.HeightMin := HeightMinInput.Value;
  p.Criteria.HeightMax := HeightMaxInput.Value;

  p.Criteria.WeightMin := WeightMinInput.Value;
  p.Criteria.WeightMax := WeightMaxInput.Value;
end;

procedure TEditForm.FormShow(Sender: TObject);
begin
  FullNameInput.Text := p.FullName;

  AgeInput.Value := p.Parameters.Age;
  HeightInput.Value := p.Parameters.Height;
  WeightInput.Value := p.Parameters.Weight;

  AgeMinInput.Value := p.Criteria.AgeMin;
  AgeMaxInput.Value := p.Criteria.AgeMax;

  HeightMinInput.Value := p.Criteria.HeightMin;
  HeightMaxInput.Value := p.Criteria.HeightMax;

  WeightMinInput.Value := p.Criteria.WeightMin;
  WeightMaxInput.Value := p.Criteria.WeightMax;
end;

end.
