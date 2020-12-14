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
  AssignFile(PartnersFile, FileName);
  Reset(PartnersFile);

  PartnerStack.Free(Top);
  L.Clear;

  while not EOF(PartnersFile) do
  begin
    Read(PartnersFile, P);
    PartnerStack.Push(Top, P);
  end;

  PartnerStack.Sort(Top);
  DrawListOfPartners(Top, L);
  CloseFile(PartnersFile);
end;

{ Процедура сохранения партнеров в типизированный файл }
procedure ProcessPartnersFileSave(FileName: string; var Top: PartnerNode);
var
  PartnersFile: TPartnersFile;
  TempNode: PartnerNode;
begin
  AssignFile(PartnersFile, FileName);
  Rewrite(PartnersFile);

  TempNode := Top;

  while Top <> nil do
  begin
    Write(PartnersFile, Top^.Data);

    Top := Top^.Next;
  end;

  CloseFile(PartnersFile);

  Top := TempNode;
end;

{ Процедура сохранения пар в типизированный файл }
procedure ProcessCouplesFileSave(FileName: string; var Top: CoupleNode);
var
  CouplesFile: TCouplesFile;
  TempNode: CoupleNode;
begin
  AssignFile(CouplesFile, FileName);
  Rewrite(CouplesFile);

  TempNode := Top;

  while Top <> nil do
  begin
    Write(CouplesFile, Top^.Data);

    Top := Top^.Next;
  end;

  CloseFile(CouplesFile);

  Top := TempNode;
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
  PartnerStack.Push(Top, P);
  PartnerStack.Sort(Top);
  DrawListOfPartners(Top, L);
end;

{ Поиск партнера по полному имени (поле FullName) }
function FindPartner(FullName: string; var Top: PartnerNode): PartnerNode;
var
  TempNode: PartnerNode;
begin
  TempNode := Top;

  while Top <> nil do
  begin
    if Top^.Data.FullName = FullName then
    begin
      Result := Top;
      break;
    end;

    Top := Top^.Next;
  end;

  Top := TempNode;
end;

{
TListBox
}

{ Процедура отрисовки списка партнеров }
procedure DrawListOfPartners(var Top: PartnerNode; L: TListBox);
var
  TempNode: PartnerNode;
begin
  L.Clear;
  TempNode := Top;

  while Top <> nil do
  begin
    L.Items.Add(Top^.Data.FullName);
    Top := Top^.Next;
  end;

  Top := TempNode;
end;

{ Процедура отрисовки списка пар }
procedure DrawListOfCouples(var Top: CoupleNode; L: TListBox);
var
  TempCell: CoupleNode;
begin
  L.Clear;
  TempCell := Top;

  while Top <> nil do
  begin
    L.Items.Add(Top^.Data.Bride.FullName + ' - ' + Top^.Data.Groom.FullName);
    Top := Top^.Next;
  end;

  Top := TempCell;
end;

end.
