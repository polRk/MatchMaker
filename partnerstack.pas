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

  PartnerNode = ^TPartnerNode;

  TPartnerNode = record
    Data: TPartner;
    Next: PartnerNode;
  end;

function NewPartner(FullName: string; Sex: PartnerSex): TPartner;

{ PartnerStack }
function Pop(var Top: PartnerNode): TPartner;
procedure Push(var Top: PartnerNode; const P: TPartner);
procedure Free(var Top: PartnerNode);
procedure Sort(var Top: PartnerNode);
procedure Delete(var Top: PartnerNode; var P: PartnerNode);

function IsItMatch(Bride, Groom: TPartner): boolean;

implementation

{ Создание нового партнера }
function NewPartner(FullName: string; Sex: PartnerSex): TPartner;
begin
  Result.FullName := FullName;
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

{ Процедура удаления верхнего(последнего) элемента стэка }
function Pop(var Top: PartnerNode): TPartner;
var
  TempNode: PartnerNode = nil;
begin
  Result := Top^.Data;
  // Сохраняю ссылку на верхний элемент во временную переменную
  TempNode := Top;
  // Заменяю ссылку на верхний элемент списка на следующий
  Top := Top^.Next;
  // Освобождаю память
  Dispose(TempNode);
end;

{ Процедура добавления элемента на верх стэка }
procedure Push(var Top: PartnerNode; const P: TPartner);
var
  TempNode: PartnerNode = nil;
begin
  // Выделяю память под новый элемент
  New(TempNode);
  // Записываю значение в новый элемент
  TempNode^.Data := P;
  // Добавляю указатель на след. элемент списка (верхний элемент)
  TempNode^.Next := Top;
  // Заменяю ссылку на верхний элемент списка на созданный
  Top := TempNode;
end;

{ Процедура сортировки стэка }
procedure Sort(var Top: PartnerNode);
var
  TempNode: PartnerNode = nil;
  P: TPartner;
begin
  // Пока не дошли до конца списка
  while Top <> nil do
  begin
    // Удаляю верхний элемента из списка
    P := Pop(Top);

    // Пока временный список не пуст и его верхнее значение
    // больше значения переменной P (значение верхнего элемента списка),
    // удаляю и получаю значение верхнего элемента из временного списка,
    // и добавляю на верх списка полученное значение.
    while (TempNode <> nil) and (TempNode^.Data.FullName > P.FullName) do
    begin
      Push(Top, Pop(TempNode));
    end;

    // Добавляю значения переменной P (значение верхнего элемента списка)
    // на верх временного списка.
    Push(TempNode, P);
  end;

  // Пока не дошли до конца списка.
  // Удаляю и получаю значение верхнего элемента из временного списка,
  // Добавляю на верх списка полученное значение.
  while TempNode <> nil do
  begin
    Push(Top, Pop(TempNode));
  end;
end;

{ Процедура удаления элемента по указателю из стэка }
procedure Delete(var Top: PartnerNode; var P: PartnerNode);
var
  TempNode: PartnerNode;
begin
  if (Top = nil) or (P = nil) then
    Exit;

  // Сохраняю ссылку на верхний элемент списка
  TempNode := Top;

  // Пока не дошли до конца списка
  while Top <> nil do
  begin
    // Если верхний элемент - нужный элемент, то удаляю и выхожу из цикла
    if Top = P then
    begin
      Pop(Top);
      break;
    end;

    // Заменяю верхний элемент списка последующим
    Top := Top^.Next;
  end;

  // Восстанавливаю ссылку на верхний элемент списка
  Top := TempNode;
end;

{ Процедура освобождения памяти занятой стэком }
procedure Free(var Top: PartnerNode);
begin
  while Top <> nil do
    Pop(Top);
end;

{ Проверка пары на совместимость }
function IsItMatch(Bride, Groom: TPartner): boolean;
begin
  // Проверить невесту на совместимость
  if (Bride.Parameters.Age < Groom.Criteria.AgeMin) or
    (Bride.Parameters.Age > Groom.Criteria.AgeMax) or
    (Bride.Parameters.Height < Groom.Criteria.HeightMin) or
    (Bride.Parameters.Height > Groom.Criteria.HeightMax) or
    (Bride.Parameters.Weight < Groom.Criteria.WeightMin) or
    (Bride.Parameters.Weight > Groom.Criteria.WeightMax) or
    (Groom.Parameters.Age < Bride.Criteria.AgeMin) or
    (Groom.Parameters.Age > Bride.Criteria.AgeMax) or
    (Groom.Parameters.Height < Bride.Criteria.HeightMin) or
    (Groom.Parameters.Height > Bride.Criteria.HeightMax) or
    (Groom.Parameters.Weight < Bride.Criteria.WeightMin) or
    (Groom.Parameters.Weight > Bride.Criteria.WeightMax) then
    Result := False
  else
    Result := True;
end;

end.
