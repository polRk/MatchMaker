unit CoupleStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PartnerStack;

type

  TCouple = record
    Bride: TPartner;
    Groom: TPartner;
  end;

  CoupleCell = ^TCoupleCell;

  TCoupleCell = record
    Data: TCouple;
    Next: CoupleCell;
  end;

{ CoupleStack }
function Pop(var Top: CoupleCell): TCouple;
procedure Push(var Top: CoupleCell; Data: TCouple);
procedure Free(var Top: CoupleCell);

implementation

{ Получение элемента с верха стэка }
function Pop(var Top: CoupleCell): TCouple;
var
  TempCell: CoupleCell;
begin
  Result := Top^.Data;
  TempCell := Top;
  Top := Top^.Next;
  Dispose(TempCell);
end;

{ Процедура добавления элемента на верх стэка }
procedure Push(var Top: CoupleCell; Data: TCouple);
var
  NewCell: CoupleCell;
begin
  New(NewCell);
  NewCell^.Data := Data;
  NewCell^.Next := Top;
  Top := NewCell;
end;

{ Процедура освобождения памяти занятой стэком }
procedure Free(var Top: CoupleCell);
var
  TempCell: CoupleCell;
begin
  while Top <> nil do
  begin
    TempCell := Top;
    Top := Top^.Next;
    Dispose(TempCell);
  end;
end;

end.
