unit PartnerStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PartnerSex = (man, woman);

  TPartner = record
    Sex: PartnerSex;
    FullName: string[255];
    Parameters: record { Параметры партнера }
      Age: integer;
      Height: integer;
      Weight: integer;
    end;
    Criteria: record { Критерии поиска партнера }
      AgeMin, AgeMax: integer;
      HeightMin, HeightMax: integer;
      WeightMin, WeightMax: integer;
    end;
  end;

  PartnerCell = ^TPartnerCell;

  TPartnerCell = record
    Data: TPartner;
    Next: PartnerCell;
  end;

function CreateEmptyPartner(Sex: PartnerSex): TPartner;

{ PartnerStack }
function Pop(var Top: PartnerCell): TPartner;
procedure Push(var Top: PartnerCell; Data: TPartner);
procedure Free(var Top: PartnerCell);
procedure Sort(var Top: PartnerCell);
procedure Delete(var Top: PartnerCell; P: PartnerCell);

function IsItMatch(bride, groom: TPartner): boolean;

implementation

{ Создание пустого партнера }
function CreateEmptyPartner(Sex: PartnerSex): TPartner;
begin
  Result.FullName := '';
  Result.Sex := Sex;

  Result.Parameters.Age := 0;
  Result.Parameters.Height := 0;
  Result.Parameters.Weight := 0;

  Result.Criteria.AgeMin := 0;
  Result.Criteria.AgeMax := 0;
  Result.Criteria.HeightMin := 0;
  Result.Criteria.HeightMax := 0;
  Result.Criteria.WeightMin := 0;
  Result.Criteria.WeightMax := 0;
end;

{ Получение элемента с верха стэка }
function Pop(var Top: PartnerCell): TPartner;
var
  TempCell: PartnerCell;
begin
  Result := Top^.Data;
  TempCell := Top;
  Top := Top^.Next;
  Dispose(TempCell);
end;

{ Процедура добавления элемента на верх стэка }
procedure Push(var Top: PartnerCell; Data: TPartner);
var
  NewCell: PartnerCell;
begin
  New(NewCell);
  NewCell^.Data := Data;
  NewCell^.Next := Top;
  Top := NewCell;
end;

{ Процедура освобождения памяти занятой стэком }
procedure Free(var Top: PartnerCell);
var
  TempCell: PartnerCell;
begin
  while Top <> nil do
  begin
    TempCell := Top;
    Top := Top^.Next;
    Dispose(TempCell);
  end;
end;

{ Процедура сортировки стэка }
procedure Sort(var Top: PartnerCell);
var
  TempCell: PartnerCell = nil;
  p: TPartner;
begin
  while Top <> nil do
  begin
    p := Pop(Top);

    while (TempCell <> nil) and (TempCell^.Data.FullName > p.FullName) do
    begin
      Push(Top, Pop(TempCell));
    end;

    Push(TempCell, p);
  end;

  while TempCell <> nil do
    Push(Top, Pop(TempCell));
end;

{ Процедура удаления элемента по указателю из стэка }
procedure Delete(var Top: PartnerCell; P: PartnerCell);
var
  TempCell: PartnerCell;
begin
  if (Top = nil) or (P = nil) then
    Exit;

  TempCell := Top;

  while Top <> nil do
  begin
    if Top = P then
    begin
      Pop(Top);
      break;
    end;

    Top := Top^.Next;
  end;

  Top := TempCell;
end;

{ Проверка пары на совместимость }
function IsItMatch(bride, groom: TPartner): boolean;
begin
  // Проверить невесту на совместимость
  if (bride.Parameters.Age < groom.Criteria.AgeMin) or
    (bride.Parameters.Age > groom.Criteria.AgeMax) or
    (bride.Parameters.Height < groom.Criteria.HeightMin) or
    (bride.Parameters.Height > groom.Criteria.HeightMax) or
    (bride.Parameters.Weight < groom.Criteria.WeightMin) or
    (bride.Parameters.Weight > groom.Criteria.WeightMax) or
    (groom.Parameters.Age < bride.Criteria.AgeMin) or
    (groom.Parameters.Age > bride.Criteria.AgeMax) or
    (groom.Parameters.Height < bride.Criteria.HeightMin) or
    (groom.Parameters.Height > bride.Criteria.HeightMax) or
    (groom.Parameters.Weight < bride.Criteria.WeightMin) or
    (groom.Parameters.Weight > bride.Criteria.WeightMax) then
    Result := False
  else
    Result := True;
end;

end.

