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
  CurrentPartner.FullName := FullNameInput.Text;

  CurrentPartner.Parameters.Age := AgeInput.Value;
  CurrentPartner.Parameters.Height := HeightInput.Value;
  CurrentPartner.Parameters.Weight := WeightInput.Value;

  CurrentPartner.Criteria.AgeMin := AgeMinInput.Value;
  CurrentPartner.Criteria.AgeMax := AgeMaxInput.Value;

  CurrentPartner.Criteria.HeightMin := HeightMinInput.Value;
  CurrentPartner.Criteria.HeightMax := HeightMaxInput.Value;

  CurrentPartner.Criteria.WeightMin := WeightMinInput.Value;
  CurrentPartner.Criteria.WeightMax := WeightMaxInput.Value;
end;

procedure TEditForm.FormShow(Sender: TObject);
begin
  FullNameInput.Text := CurrentPartner.FullName;

  AgeInput.Value := CurrentPartner.Parameters.Age;
  HeightInput.Value := CurrentPartner.Parameters.Height;
  WeightInput.Value := CurrentPartner.Parameters.Weight;

  AgeMinInput.Value := CurrentPartner.Criteria.AgeMin;
  AgeMaxInput.Value := CurrentPartner.Criteria.AgeMax;

  HeightMinInput.Value := CurrentPartner.Criteria.HeightMin;
  HeightMaxInput.Value := CurrentPartner.Criteria.HeightMax;

  WeightMinInput.Value := CurrentPartner.Criteria.WeightMin;
  WeightMaxInput.Value := CurrentPartner.Criteria.WeightMax;
end;

end.
