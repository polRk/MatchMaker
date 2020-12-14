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
  currentPartner.FullName := FullNameInput.Text;

  currentPartner.Parameters.Age := AgeInput.Value;
  currentPartner.Parameters.Height := HeightInput.Value;
  currentPartner.Parameters.Weight := WeightInput.Value;

  currentPartner.Criteria.AgeMin := AgeMinInput.Value;
  currentPartner.Criteria.AgeMax := AgeMaxInput.Value;

  currentPartner.Criteria.HeightMin := HeightMinInput.Value;
  currentPartner.Criteria.HeightMax := HeightMaxInput.Value;

  currentPartner.Criteria.WeightMin := WeightMinInput.Value;
  currentPartner.Criteria.WeightMax := WeightMaxInput.Value;
end;

procedure TEditForm.FormShow(Sender: TObject);
begin
  FullNameInput.Text := currentPartner.FullName;

  AgeInput.Value := currentPartner.Parameters.Age;
  HeightInput.Value := currentPartner.Parameters.Height;
  WeightInput.Value := currentPartner.Parameters.Weight;

  AgeMinInput.Value := currentPartner.Criteria.AgeMin;
  AgeMaxInput.Value := currentPartner.Criteria.AgeMax;

  HeightMinInput.Value := currentPartner.Criteria.HeightMin;
  HeightMaxInput.Value := currentPartner.Criteria.HeightMax;

  WeightMinInput.Value := currentPartner.Criteria.WeightMin;
  WeightMaxInput.Value := currentPartner.Criteria.WeightMax;
end;

end.
