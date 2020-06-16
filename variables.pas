unit Variables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, PartnerStack, CoupleStack, BipartiteGraph;

type
  TPartnersFile = file of TPartner;
  TCouplesFile = file of TCouple;

var
  listOfBrides, listOfGrooms: PartnerCell;
  listOfCouples: CoupleCell;

  graphOfPartners: TBipartiteGraph;

  bridesFN, groomsFN, couplesFN: string;

  p: TPartner;
  c: TCouple;

{ Files }
procedure ProcessPartnersFileOpen(fn: string; var Top: PartnerCell; l: TListBox);
procedure ProcessPartnersFileSave(fn: string; var Top: PartnerCell);
procedure ProcessCouplesFileSave(fn: string; var Top: CoupleCell);

{ Partner }
function PartnerToString(p: TPartner): string;
procedure AddPartner(p: TPartner; var Top: PartnerCell; l: TListBox);
function FindPartner(fullName: string; var Top: PartnerCell): PartnerCell;

{ TListBox }
procedure DrawListOfPartners(var Top: PartnerCell; l: TListBox);
procedure DrawListOfCouples(var Top: CoupleCell; l: TListBox);

implementation

{
Files
}

{ Процедура загрузки типизированного файла списка партнеров }
procedure ProcessPartnersFileOpen(fn: string; var Top: PartnerCell; l: TListBox);
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
procedure ProcessPartnersFileSave(fn: string; var Top: PartnerCell);
var
  Sentinel: PartnerCell;
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
procedure ProcessCouplesFileSave(fn: string; var Top: CoupleCell);
var
  Sentinel: CoupleCell;
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
procedure AddPartner(p: TPartner; var Top: PartnerCell; l: TListBox);
begin
  PartnerStack.Push(Top, p);
  PartnerStack.Sort(Top);
  DrawListOfPartners(Top, l);
end;

{ Поиск партнера по полному имени (поле FullName) }
function FindPartner(fullName: string; var Top: PartnerCell): PartnerCell;
var
  TempCell: PartnerCell;
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
procedure DrawListOfPartners(var Top: PartnerCell; l: TListBox);
var
  TempCell: PartnerCell;
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
procedure DrawListOfCouples(var Top: CoupleCell; l: TListBox);
var
  TempCell: CoupleCell;
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
