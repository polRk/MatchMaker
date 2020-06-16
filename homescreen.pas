unit HomeScreen;

{$mode objfpc}{$H+}

interface

uses
  CoupleStack, PartnerStack, HopcroftKarp,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ActnList, ComCtrls, ExtCtrls, StdActns;

type
  { THomeForm }

  THomeForm = class(TForm)
    GroomsFile: TMenuItem;
    GroomsFileOpen: TMenuItem;
    GroomsFileSave: TMenuItem;
    GroomsFileSaveAs: TMenuItem;
    GroomAdd: TMenuItem;
    GroomEdit: TMenuItem;
    GroomRemove: TMenuItem;
    CouplesListPopup: TPopupMenu;
    HomeMenu: TMainMenu;
    MenuFileGroup: TMenuItem;
    MenuEditGroup: TMenuItem;
    MenuFileOpen: TMenuItem;
    MenuFileSave: TMenuItem;
    MenuFileSaveAs: TMenuItem;
    MenuAdd: TMenuItem;
    MenuEdit: TMenuItem;
    CouplesFileSaveAs: TMenuItem;
    CouplesFileSave: TMenuItem;
    MenuRemove: TMenuItem;
    PartnerRemove: TAction;
    PartnerEdit: TAction;
    PartnerAdd: TAction;
    FileSave: TAction;
    Actions: TActionList;
    FileOpen: TFileOpen;
    FileSaveAs: TFileSaveAs;
    GroomsListPopup: TPopupMenu;
    GroomsListBox: TListBox;
    BridesListBox: TListBox;
    CouplesListBox: TListBox;
    BridesFile: TMenuItem;
    BridesFileOpen: TMenuItem;
    BridesFileSave: TMenuItem;
    BridesFileSaveAs: TMenuItem;
    BrideAdd: TMenuItem;
    BrideRemove: TMenuItem;
    BridesDivider: TMenuItem;
    BrideEdit: TMenuItem;
    HusbandGB: TGroupBox;
    Pages: TPageControl;
    BridesPage: TTabSheet;
    GroomsPage: TTabSheet;
    CouplesPage: TTabSheet;
    BridesListPopup: TPopupMenu;

    procedure BridesActionsToggle(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure GroomsActionsToggle(Sender: TObject);

    procedure CouplesPageHide(Sender: TObject);
    procedure CouplesPageShow(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure FileSaveAsAccept(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PartnerAddExecute(Sender: TObject);
    procedure PartnerEditExecute(Sender: TObject);
    procedure PartnerRemoveExecute(Sender: TObject);
  private

  public

  end;

var
  HomeForm: THomeForm;

implementation

uses
  Variables,
  EditScreen;

{$R *.lfm}

{ THomeForm }

procedure THomeForm.BridesActionsToggle(Sender: TObject);
begin
  FileSaveAs.Enabled := False;
  FileSave.Enabled := False;
  PartnerEdit.Enabled := False;
  PartnerRemove.Enabled := False;

  if listOfBrides <> nil then
    FileSaveAs.Enabled := True;

  if bridesFN <> '' then
    FileSave.Enabled := True;

  if BridesListBox.GetSelectedText <> '' then
  begin
    PartnerEdit.Enabled := True;
    PartnerRemove.Enabled := True;
  end;
end;

procedure THomeForm.GroomsActionsToggle(Sender: TObject);
begin
  FileSaveAs.Enabled := False;
  FileSave.Enabled := False;
  PartnerEdit.Enabled := False;
  PartnerRemove.Enabled := False;

  if listOfGrooms <> nil then
    FileSaveAs.Enabled := True;

  if groomsFN <> '' then
    FileSave.Enabled := True;

  if GroomsListBox.GetSelectedText <> '' then
  begin
    PartnerEdit.Enabled := True;
    PartnerRemove.Enabled := True;
  end;
end;

procedure THomeForm.CouplesPageHide(Sender: TObject);
begin
  MenuEditGroup.Enabled := True;
  FileOpen.Enabled := True;
  FileSave.Enabled := False;

  CoupleStack.Free(listOfCouples);
end;

procedure THomeForm.CouplesPageShow(Sender: TObject);
begin
  MenuEditGroup.Enabled := False;
  FileOpen.Enabled := False;
  FileSave.Enabled := False;

  if couplesFN <> '' then
    FileSave.Enabled := True;

  listOfCouples := FindMaxCouples(listOfBrides, listOfGrooms);
  DrawListOfCouples(listOfCouples, CouplesListBox);
end;

procedure THomeForm.FileOpenAccept(Sender: TObject);
begin
  if Pages.ActivePageIndex = BridesPage.PageIndex then
  begin
    bridesFN := FileOpen.Dialog.FileName;

    ProcessPartnersFileOpen(bridesFN, listOfBrides, BridesListBox);
  end;

  if Pages.ActivePageIndex = GroomsPage.PageIndex then
  begin
    groomsFN := FileOpen.Dialog.FileName;

    ProcessPartnersFileOpen(groomsFN, listOfGrooms, GroomsListBox);
  end;
end;

procedure THomeForm.FileSaveExecute(Sender: TObject);
begin
  if (Pages.ActivePageIndex = BridesPage.PageIndex) and (bridesFN <> '') then
  begin
    ProcessPartnersFileSave(bridesFN, listOfBrides);
  end;

  if (Pages.ActivePageIndex = GroomsPage.PageIndex) and (groomsFN <> '') then
  begin
    ProcessPartnersFileSave(groomsFN, listOfGrooms);
  end;

  if (Pages.ActivePageIndex = CouplesPage.PageIndex) and (couplesFN <> '') then
  begin
    ProcessCouplesFileSave(couplesFN, listOfCouples);
  end;
end;

procedure THomeForm.FileSaveAsAccept(Sender: TObject);
begin
  FileSave.Enabled := True;

  if Pages.ActivePageIndex = BridesPage.PageIndex then
  begin
    bridesFN := FileSaveAs.Dialog.FileName;
    ProcessPartnersFileSave(bridesFN, listOfBrides);
  end;

  if Pages.ActivePageIndex = GroomsPage.PageIndex then
  begin
    groomsFN := FileSaveAs.Dialog.FileName;
    ProcessPartnersFileSave(groomsFN, listOfGrooms);
  end;

  if Pages.ActivePageIndex = CouplesPage.PageIndex then
  begin
    couplesFN := FileSaveAs.Dialog.FileName;
    ProcessCouplesFileSave(couplesFN, listOfCouples);
  end;
end;

procedure THomeForm.FormClose(Sender: TObject);
begin
  PartnerStack.Free(listOfBrides);
  PartnerStack.Free(listOfGrooms);
end;

procedure THomeForm.FormCreate(Sender: TObject);
begin
  Pages.PageIndex := 0;
end;

procedure THomeForm.PartnerAddExecute(Sender: TObject);
var
  Result: TModalResult;
begin
  EditForm := TEditForm.Create(Application);
  EditForm.Left := HomeForm.Left;
  EditForm.Top := HomeForm.Top;

  if Pages.ActivePageIndex = BridesPage.PageIndex then
  begin
    p := CreateEmptyPartner(PartnerSex.woman);
    EditForm.Caption := 'Добавить невесту';
  end;

  if Pages.ActivePageIndex = GroomsPage.PageIndex then
  begin
    p := CreateEmptyPartner(PartnerSex.man);
    EditForm.Caption := 'Добавить жениха';
  end;


  Result := EditForm.ShowModal;
  if (Result = mrCancel) or (p.FullName = '') then
    Exit;

  if Pages.ActivePageIndex = BridesPage.PageIndex then
    AddPartner(p, listOfBrides, BridesListBox);

  if Pages.ActivePageIndex = GroomsPage.PageIndex then
    AddPartner(p, listOfGrooms, GroomsListBox);

end;

procedure THomeForm.PartnerEditExecute(Sender: TObject);
var
  Result: TModalResult;
  s: PartnerCell;
begin
  EditForm := TEditForm.Create(Application);
  EditForm.Left := HomeForm.Left;
  EditForm.Top := HomeForm.Top;

  if Pages.ActivePageIndex = BridesPage.PageIndex then
    EditForm.Caption := 'Редактирова невесту';
  if Pages.ActivePageIndex = GroomsPage.PageIndex then
    EditForm.Caption := 'Редактировать жениха';

  if Pages.ActivePageIndex = BridesPage.PageIndex then
    s := FindPartner(BridesListBox.GetSelectedText, listOfBrides);

  if Pages.ActivePageIndex = GroomsPage.PageIndex then
    s := FindPartner(GroomsListBox.GetSelectedText, listOfGrooms);

  if s = nil then
    Exit;

  p := s^.Data;

  Result := EditForm.ShowModal;
  if Result = mrCancel then
    Exit;

  if Pages.ActivePageIndex = BridesPage.PageIndex then
  begin
    PartnerStack.Delete(listOfBrides, s);
    AddPartner(p, listOfBrides, BridesListBox);
    BridesListBox.ItemIndex := -1;
  end;

  if Pages.ActivePageIndex = GroomsPage.PageIndex then
  begin
    PartnerStack.Delete(listOfGrooms, s);
    AddPartner(p, listOfGrooms, GroomsListBox);
    GroomsListBox.ItemIndex := -1;
  end;
end;

procedure THomeForm.PartnerRemoveExecute(Sender: TObject);
var
  s: PartnerCell;
begin
  if MessageDlg('Подтвердите удаление', mtConfirmation,
    [mbYes, mbNo], 0) <> mrYes then
    Exit;

  if Pages.ActivePageIndex = BridesPage.PageIndex then
  begin
    s := FindPartner(BridesListBox.GetSelectedText, listOfBrides);
    PartnerStack.Delete(listOfBrides, s);
    BridesListBox.Items.Delete(BridesListBox.ItemIndex);
    BridesListBox.ItemIndex := -1;
  end;

  if Pages.ActivePageIndex = GroomsPage.PageIndex then
  begin
    s := FindPartner(GroomsListBox.GetSelectedText, listOfGrooms);
    PartnerStack.Delete(listOfGrooms, s);
    GroomsListBox.Items.Delete(GroomsListBox.ItemIndex);
    GroomsListBox.ItemIndex := -1;
  end;
end;

end.
