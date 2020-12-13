unit Variables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, PartnerStack, CoupleStack, HopcroftKarp;

type
  TPartnersFile = file of TPartner;
  TCouplesFile = file of TCouple;

var
  listOfBrides, listOfGrooms: PartnerNode;
  listOfCouples: CoupleNode;

  graphOfPartners: TBipartiteGraph;

  bridesFN, groomsFN, couplesFN: string;

  CurrentPartner: TPartner;

{ Files }
procedure ProcessPartnersFileOpen(fn: string; var Top: PartnerNode; l: TListBox);
procedure ProcessPartnersFileSave(fn: string; var Top: PartnerNode);
procedure ProcessCouplesFileSave(fn: string; var Top: CoupleNode);

{ Partner }
function PartnerToString(p: TPartner): string;
procedure AddPartner(p: TPartner; var Top: PartnerNode; l: TListBox);
function FindPartner(fullName: string; var Top: PartnerNode): PartnerNode;

{ TListBox }
procedure DrawListOfPartners(var Top: PartnerNode; l: TListBox);
procedure DrawListOfCouples(var Top: CoupleNode; l: TListBox);

implementation

{
Files
}

{ Процедура загрузки типизированного файла списка партнеров }
procedure ProcessPartnersFileOpen(fn: string; var Top: PartnerNode; l: TListBox);
var
  f: TPartnersFile;
  p: TPartner;
begin
  AssignFile(f, fn);
  Reset(f);

  PartnerStack.Free(Top);
  l.Clear;

  while not EOF(f) do
  begin
    Read(f, p);
    PartnerStack.Push(Top, p);
  end;

  PartnerStack.Sort(Top);
  DrawListOfPartners(Top, l);
  CloseFile(f);
end;

{ Процедура сохранения партнеров в типизированный файл }
procedure ProcessPartnersFileSave(fn: string; var Top: PartnerNode);
var
  Sentinel: PartnerNode;
  f: TPartnersFile;
begin
  AssignFile(f, fn);
  Rewrite(f);

  Sentinel := Top;

  while Top <> nil do
  begin
    Write(f, Top^.Data);

    Top := Top^.Next;
  end;

  CloseFile(f);

  Top := Sentinel;
end;

{ Процедура сохранения пар в типизированный файл }
procedure ProcessCouplesFileSave(fn: string; var Top: CoupleNode);
var
  Sentinel: CoupleNode;
  f: TCouplesFile;
begin
  AssignFile(f, fn);
  Rewrite(f);

  Sentinel := Top;

  while Top <> nil do
  begin
    Write(f, Top^.Data);

    Top := Top^.Next;
  end;

  CloseFile(f);

  Top := Sentinel;
end;

{
Partner
}

{ Представление записи о партнере в виде строки }
function PartnerToString(p: TPartner): string;
begin
  Result := Format('%s: Возраст - %d, Рост - %d, Вес - %d',
    [p.FullName, p.Parameters.Age, p.Parameters.Height, p.Parameters.Weight]);
end;

{ Процедура добавления партнера в приложение }
procedure AddPartner(p: TPartner; var Top: PartnerNode; l: TListBox);
begin
  PartnerStack.Push(Top, p);
  PartnerStack.Sort(Top);
  DrawListOfPartners(Top, l);
end;

{ Поиск партнера по полному имени (поле FullName) }
function FindPartner(fullName: string; var Top: PartnerNode): PartnerNode;
var
  TempCell: PartnerNode;
begin
  TempCell := Top;

  while Top <> nil do
  begin
    if Top^.Data.FullName = fullName then
    begin
      Result := Top;
      break;
    end

    else
      Top := Top^.Next;
  end;

  Top := TempCell;
end;

{
TListBox
}

{ Процедура отрисовки списка партнеров }
procedure DrawListOfPartners(var Top: PartnerNode; l: TListBox);
var
  TempCell: PartnerNode;
begin
  l.Clear;
  TempCell := Top;

  while Top <> nil do
  begin
    l.Items.Add(Top^.Data.FullName);
    Top := Top^.Next;
  end;

  Top := TempCell;
end;

{ Процедура отрисовки списка пар }
procedure DrawListOfCouples(var Top: CoupleNode; l: TListBox);
var
  TempCell: CoupleNode;
begin
  l.Clear;
  TempCell := Top;

  while Top <> nil do
  begin
    l.Items.Add(Top^.Data.Bride.FullName + ' - ' + Top^.Data.Groom.FullName);
    Top := Top^.Next;
  end;

  Top := TempCell;
end;

end.
