unit Variables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, PartnerStack, CoupleStack, HopcroftKarp;

type
  TPartnersFile = file of TPartner;
  TCouplesFile = file of TCouple;

var
  listOfBrides: PartnerNode = nil;
  listOfGrooms: PartnerNode = nil;
  listOfCouples: CoupleNode = nil;

  graphOfPartners: TBipartiteGraph;

  bridesFN, groomsFN, couplesFN: string;

  // Использую для записи данных из окна редактирования
  currentPartner: TPartner;

{ Files }
procedure ProcessPartnersFileOpen(FileName: string; var Top: PartnerNode; L: TListBox);
procedure ProcessPartnersFileSave(FileName: string; var Top: PartnerNode);
procedure ProcessCouplesFileSave(FileName: string; var Top: CoupleNode);

{ Partner }
function PartnerToString(P: TPartner): string;
procedure AddPartner(P: TPartner; var Top: PartnerNode; L: TListBox);
function FindPartner(FullName: string; var Top: PartnerNode): PartnerNode;

{ TListBox }
procedure DrawListOfPartners(var Top: PartnerNode; L: TListBox);
procedure DrawListOfCouples(var Top: CoupleNode; L: TListBox);

implementation

{
Files
}

{ Процедура загрузки типизированного файла списка партнеров }
procedure ProcessPartnersFileOpen(FileName: string; var Top: PartnerNode; L: TListBox);
var
  PartnersFile: TPartnersFile;
  P: TPartner;
begin
  // Назначаю переменной имя внешнего файла
  AssignFile(PartnersFile, FileName);
  Reset(PartnersFile); // открываю файл для чтения

  PartnerStack.Free(Top); // освобождаю память от стэка
  L.Clear; // очищаю список на кране

  // Пока не дошел до конца стэка
  while not EOF(PartnersFile) do
  begin
    Read(PartnersFile, P); // читаю из файла в переменную
    PartnerStack.Push(Top, P); // записываю партнера в стэк
  end;

  PartnerStack.Sort(Top); // сортирую стэк партнеров
  // Отображаю стэк в списке на экране
  DrawListOfPartners(Top, L);
  CloseFile(PartnersFile); // закрываю файл
end;

{ Процедура сохранения партнеров в типизированный файл }
procedure ProcessPartnersFileSave(FileName: string; var Top: PartnerNode);
var
  PartnersFile: TPartnersFile;
  TempNode: PartnerNode;
begin
  // Назначаю переменной имя внешнего файла
  AssignFile(PartnersFile, FileName);
  Rewrite(PartnersFile); // открываю файл для записи

  TempNode := Top; // сохраняю указатель на верх стэка

  // Пока не дошел до конца стэка
  while Top <> nil do
  begin
    Write(PartnersFile, Top^.Data);
    // записываю запись о партнере в файл
    Top := Top^.Next; // передвигаюсь дальше по стэку
  end;

  CloseFile(PartnersFile); // закрываю файлл

  Top := TempNode; // восстанавливаю указатель на верх стэка
end;

{ Процедура сохранения пар в типизированный файл }
procedure ProcessCouplesFileSave(FileName: string; var Top: CoupleNode);
var
  CouplesFile: TCouplesFile;
  TempNode: CoupleNode;
begin
  // Назначаю переменной имя внешнего файла
  AssignFile(CouplesFile, FileName);
  Rewrite(CouplesFile); // открываю файл для записи

  TempNode := Top; // сохраняю указатель на верх стэка

  // Пока не дошел до конца стэка
  while Top <> nil do
  begin
    // Записываю запись о паре в файл
    Write(CouplesFile, Top^.Data);
    Top := Top^.Next; // передвигаюсь дальше по стэку
  end;

  CloseFile(CouplesFile); // закрываю файл

  Top := TempNode; // восстанавливаю указатель на верх стэка
end;

{
Partner
}

{ Представление записи о партнере в виде строки }
function PartnerToString(P: TPartner): string;
begin
  Result := Format('%s: Возраст - %d, Рост - %d, Вес - %d',
    [P.FullName, P.Parameters.Age, P.Parameters.Height, P.Parameters.Weight]);
end;

{ Процедура добавления партнера в приложение }
procedure AddPartner(P: TPartner; var Top: PartnerNode; L: TListBox);
begin
  // Добавляю партнера на верх стэка
  PartnerStack.Push(Top, P);
  PartnerStack.Sort(Top); // сортирую стэк
  DrawListOfPartners(Top, L); // отрисовываю стэк на экране
end;

{ Поиск партнера по полному имени (поле FullName) }
function FindPartner(FullName: string; var Top: PartnerNode): PartnerNode;
var
  TempNode: PartnerNode;
begin
  TempNode := Top; // сохраняю указатель на верх стэка

  // Пока не дошел до конца стэка
  while Top <> nil do
  begin

    // Если имя партнера совпадает с переданным,
    // то записываю указатель в результат и выхожу из функции
    if Top^.Data.FullName = FullName then
    begin
      Result := Top;
      break;
    end;

    Top := Top^.Next; // передвигаюсь дальше по стэку
  end;

  Top := TempNode; // восстанавливаю указатель на верх стэка
end;

{
TListBox
}

{ Процедура отрисовки списка партнеров }
procedure DrawListOfPartners(var Top: PartnerNode; L: TListBox);
var
  TempNode: PartnerNode;
begin
  L.Clear; // очищаю список на экране
  TempNode := Top; // сохраняю указатель на верх стэка

  // Пока не дошел до конца стэка
  while Top <> nil do
  begin
    // Добавляю имя партнера в список на экране
    L.Items.Add(Top^.Data.FullName);
    Top := Top^.Next; // передвигаюсь дальше по стэку
  end;

  Top := TempNode; // восстанавливаю указатель на верх стэка
end;

{ Процедура отрисовки списка пар }
procedure DrawListOfCouples(var Top: CoupleNode; L: TListBox);
var
  TempNode: CoupleNode;
begin
  L.Clear; // очищаю список на экране
  TempNode := Top; // сохраняю указатель на верх стэка

  // Пока не дошел до конца стэка
  while Top <> nil do
  begin
    // Добавляю пару в список на экране
    L.Items.Add(Top^.Data.Bride.FullName + ' - ' + Top^.Data.Groom.FullName);
    Top := Top^.Next; // передвигаюсь дальше по стэку
  end;

  Top := TempNode; // восстанавливаю указатель на верх стэка
end;

end.
