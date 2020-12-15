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


    procedure CouplesPageHide(Sender: TObject);
    procedure CouplesPageShow(Sender: TObject);

    procedure onBridePopup(Sender: TObject);

    procedure OnGroomPopup(Sender: TObject);

    procedure OnFileSave(Sender: TObject);

    procedure OnFileOpen(Sender: TObject);
    procedure OnFileSaveAs(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OnPartnerAdd(Sender: TObject);
    procedure OnPartnerEdit(Sender: TObject);
    procedure OnPartnerRemove(Sender: TObject);
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

{ События формы }

procedure THomeForm.FormClose(Sender: TObject);
begin
  // Удаляю стэки из памяти
  PartnerStack.Free(listOfBrides);
  PartnerStack.Free(listOfGrooms);
  CoupleStack.Free(listOfCouples);
end;

procedure THomeForm.FormCreate(Sender: TObject);
begin
  Pages.PageIndex := 0;
end;

procedure THomeForm.CouplesPageHide(Sender: TObject);
begin
  // Сбразываю доступность пунктов меню
  FileOpen.Enabled := True;
  FileSave.Enabled := False;
  FileSaveAs.Enabled := False;
  MenuEditGroup.Enabled := True;

  // Очищаю из памяти стэк пар
  CoupleStack.Free(listOfCouples);
end;

procedure THomeForm.CouplesPageShow(Sender: TObject);
begin
  // Сбразываю доступность пунктов меню
  FileOpen.Enabled := False;
  FileSave.Enabled := False;
  FileSaveAs.Enabled := False;
  MenuEditGroup.Enabled := False;

  // Если пользователь открыл файл со списком пар,
  // то позволяю сохранить стэк в уже открытый файл
  if couplesFN <> '' then
    FileSave.Enabled := True;

  // Записываю в глобальную переменную стэк пар
  listOfCouples := FindMaxCouples(listOfBrides, listOfGrooms);

  // Если стэк не пуст, то позволяю сохранить его в файл
  if listOfCouples <> nil then
    FileSaveAs.Enabled := True;

  // Отрисовываю стэк пар на экране
  Variables.DrawListOfCouples(listOfCouples, CouplesListBox);
end;

{ Работа с файлами }

{ Обработчик открытия списка из файла }
procedure THomeForm.OnFileOpen(Sender: TObject);
begin
  // Показываю пользователю диалог выбора места хранения
  // файла со списком партнеров.
  // Сохраняю путь до файла в глобальную переменную

  if Pages.ActivePageIndex = BridesPage.PageIndex then
  begin
    bridesFN := FileOpen.Dialog.FileName;

    Variables.ProcessPartnersFileOpen(bridesFN, listOfBrides, BridesListBox);
  end;

  if Pages.ActivePageIndex = GroomsPage.PageIndex then
  begin
    groomsFN := FileOpen.Dialog.FileName;

    Variables.ProcessPartnersFileOpen(groomsFN, listOfGrooms, GroomsListBox);
  end;
end;

{ Обработчик сохранения списка в открытый файл }
procedure THomeForm.OnFileSave(Sender: TObject);
begin
  // Сохраняю стэк в открытый файл

  if (Pages.ActivePageIndex = BridesPage.PageIndex) and (bridesFN <> '') then
  begin
    Variables.ProcessPartnersFileSave(bridesFN, listOfBrides);
  end;

  if (Pages.ActivePageIndex = GroomsPage.PageIndex) and (groomsFN <> '') then
  begin
    Variables.ProcessPartnersFileSave(groomsFN, listOfGrooms);
  end;

  if (Pages.ActivePageIndex = CouplesPage.PageIndex) and (couplesFN <> '') then
  begin
    Variables.ProcessCouplesFileSave(couplesFN, listOfCouples);
  end;
end;

{ Обработчик сохранения списка в новый файл }
procedure THomeForm.OnFileSaveAs(Sender: TObject);
begin
  // Показываю пользователю диалог выбора места назначения
  // для нового файла (или существующего, тогда произойдет перезапись),
  // и сохраняю стэк в выбранный файл

  if Pages.ActivePageIndex = BridesPage.PageIndex then
  begin
    bridesFN := FileSaveAs.Dialog.FileName;
    Variables.ProcessPartnersFileSave(bridesFN, listOfBrides);
  end;

  if Pages.ActivePageIndex = GroomsPage.PageIndex then
  begin
    groomsFN := FileSaveAs.Dialog.FileName;
    Variables.ProcessPartnersFileSave(groomsFN, listOfGrooms);
  end;

  if Pages.ActivePageIndex = CouplesPage.PageIndex then
  begin
    couplesFN := FileSaveAs.Dialog.FileName;
    Variables.ProcessCouplesFileSave(couplesFN, listOfCouples);
  end;

  // Включаю процедуру сохранения в файл
  FileSave.Enabled := True;
end;

{ Работа с партнерами }

{ Обработчик отображение popup menu на странице невест }
procedure THomeForm.onBridePopup(Sender: TObject);
begin
  // Сбразываю доступность пунктов меню
  FileSave.Enabled := False;
  FileSaveAs.Enabled := False;
  PartnerEdit.Enabled := False;
  PartnerRemove.Enabled := False;

  // Если стэк невест не пуст,
  // то позволяю сохранить стэк в новый файл
  if listOfBrides <> nil then
    FileSaveAs.Enabled := True;

  // Если пользователь открыл файл со списком невест,
  // то позволяю сохранить стэк в уже открытый файл
  if bridesFN <> '' then
    FileSave.Enabled := True;

  // Если пользователь выбрал невесту в списке,
  // то позволяю её редактирование и удаление
  if BridesListBox.GetSelectedText <> '' then
  begin
    PartnerEdit.Enabled := True;
    PartnerRemove.Enabled := True;
  end;
end;

{ Обработчик отображение popup menu на странице женихов }
procedure THomeForm.OnGroomPopup(Sender: TObject);
begin
  // Сбразываю доступность пунктов меню
  FileSave.Enabled := False;
  FileSaveAs.Enabled := False;
  PartnerEdit.Enabled := False;
  PartnerRemove.Enabled := False;

  // Если стэк женихов не пуст,
  // то позволяю сохранить стэк в новый файл
  if listOfGrooms <> nil then
    FileSaveAs.Enabled := True;

  // Если пользователь открыл файл со списком женихов,
  // то позволяю сохранить стэк в уже открытый файл
  if groomsFN <> '' then
    FileSave.Enabled := True;

  // Если пользователь выбрал жениха в списке,
  // то позволяю его редактирование и удаление
  if GroomsListBox.GetSelectedText <> '' then
  begin
    PartnerEdit.Enabled := True;
    PartnerRemove.Enabled := True;
  end;
end;

{ Добавить партнера }
procedure THomeForm.OnPartnerAdd(Sender: TObject);
var
  Result: TModalResult;
begin
  // Создаю форму редактирования партнера
  EditForm := TEditForm.Create(Application);
  EditForm.Left := HomeForm.Left;
  EditForm.Top := HomeForm.Top;

  // Изменяю заголовок формы в зависимости от активной страницы
  if Pages.ActivePageIndex = BridesPage.PageIndex then
  begin
    currentPartner := PartnerStack.NewPartner('', PartnerSex.woman);
    EditForm.Caption := 'Добавить невесту';
  end;

  if Pages.ActivePageIndex = GroomsPage.PageIndex then
  begin
    currentPartner := PartnerStack.NewPartner('', PartnerSex.man);
    EditForm.Caption := 'Добавить жениха';
  end;

  // Показываю форму редактирования, и, если пользователь нажал
  // на кнопку закрытия окна или не заполнил имя, то выхожу из функции
  Result := EditForm.ShowModal;
  if (Result = mrCancel) or (currentPartner.FullName = '') then
    Exit;

  // Добавляю невесту в стэк и в список на экране
  if Pages.ActivePageIndex = BridesPage.PageIndex then
    Variables.AddPartner(currentPartner, listOfBrides, BridesListBox);

  // Добавляю жениха в стэк и в список на экране
  if Pages.ActivePageIndex = GroomsPage.PageIndex then
    Variables.AddPartner(currentPartner, listOfGrooms, GroomsListBox);

end;

{ Обработчик события редактирования выбранного партнера }
procedure THomeForm.OnPartnerEdit(Sender: TObject);
var
  Result: TModalResult;
  TempNode: PartnerNode = nil;
begin
  // Создаю форму редактирования партнера
  EditForm := TEditForm.Create(Application);
  EditForm.Left := HomeForm.Left;
  EditForm.Top := HomeForm.Top;

  // Изменяю заголовок формы в зависимости от активной страницы
  if Pages.ActivePageIndex = BridesPage.PageIndex then
    EditForm.Caption := 'Редактирова невесту';
  if Pages.ActivePageIndex = GroomsPage.PageIndex then
    EditForm.Caption := 'Редактировать жениха';

  // Сохраняю ссылку на узел с выбранным партнером
  if Pages.ActivePageIndex = BridesPage.PageIndex then
    TempNode := FindPartner(BridesListBox.GetSelectedText, listOfBrides);

  if Pages.ActivePageIndex = GroomsPage.PageIndex then
    TempNode := FindPartner(GroomsListBox.GetSelectedText, listOfGrooms);

  // Если партнер не найден, то выхожу
  if TempNode = nil then
    Exit;

  // Записываю партнера в глобальную переменную
  currentPartner := TempNode^.Data;

  // Показываю форму редактирования, и, если пользователь нажал
  // на кнопку закрытия окна, то выхожу из функции
  Result := EditForm.ShowModal;
  if Result = mrCancel then
    Exit;

  // Обновляю партнера
  if Pages.ActivePageIndex = BridesPage.PageIndex then
  begin
    // Удаляю невесту из стэка
    PartnerStack.Delete(listOfBrides, TempNode);
    // Добавляю в стэк и в список на экране измененную невесту
    Variables.AddPartner(currentPartner, listOfBrides, BridesListBox);
    // Сбрасываю индекс выбранной строки в списке на экране
    BridesListBox.ItemIndex := -1;
  end;

  if Pages.ActivePageIndex = GroomsPage.PageIndex then
  begin
    // Удаляю жениха из стэка
    PartnerStack.Delete(listOfGrooms, TempNode);
    // Добавляю в стэк и в список на экране измененного жениха
    Variables.AddPartner(currentPartner, listOfGrooms, GroomsListBox);
    // Сбрасываю индекс выбранной строки в списке на экране
    GroomsListBox.ItemIndex := -1;
  end;
end;

{ Обработчик события удаления выбранного партнера }
procedure THomeForm.OnPartnerRemove(Sender: TObject);
var
  P: PartnerNode;
begin
  // Запрашиваю подтверждение удаления
  if MessageDlg('Подтвердите удаление', mtConfirmation,
    [mbYes, mbNo], 0) <> mrYes then
    Exit;

  // Если активная страница - страница со списком невест,
  // то удаляю выбранную невесту из списка на экарне и стэка
  if Pages.ActivePageIndex = BridesPage.PageIndex then
  begin
    // Ищу указатель на звено в стэке невест
    P := FindPartner(BridesListBox.GetSelectedText, listOfBrides);
    // Удаляю звено из стэка по указателю
    PartnerStack.Delete(listOfBrides, P);
    // Удаляю из списка на экране выбранную невесту
    BridesListBox.Items.Delete(BridesListBox.ItemIndex);
    // Сбрасываю индекс выбранной строки в списке на экране
    BridesListBox.ItemIndex := -1;
  end;

  // Если активная страница - страница со списком женихов,
  // то удаляю выбранного жениха из списка на экране и стэка
  if Pages.ActivePageIndex = GroomsPage.PageIndex then
  begin
    // Ищу указатель на звено в стэке женихов
    P := FindPartner(GroomsListBox.GetSelectedText, listOfGrooms);
    // Удаляю звено из стэка по указателю
    PartnerStack.Delete(listOfGrooms, P);
    // Удаляю из списка на экране выбранного жениха
    GroomsListBox.Items.Delete(GroomsListBox.ItemIndex);
    // Сбрасываю индекс выбранной строки в списке на экране
    GroomsListBox.ItemIndex := -1;
  end;
end;

end.
