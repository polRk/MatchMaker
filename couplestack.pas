unit CoupleStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PartnerStack;

type
  CoupleNode = ^TCoupleNode;

  Couple = ^TCouple;

  TCouple = record
    Bride: TPartner;
    Groom: TPartner;
  end;

  TCoupleNode = record
    Data: TCouple;
    Next: CoupleNode;
  end;

{ CoupleStack }
function NewCouple(const Bride: TPartner; const Groom: TPartner): TCouple;
function Pop(var Top: CoupleNode): TCouple;
procedure Push(var Top: CoupleNode; const C: TCouple);
procedure Free(var Top: CoupleNode);

implementation

{ Создание новой вершины }
function NewCouple(const Bride: TPartner; const Groom: TPartner): TCouple;
var
  C: TCouple;
begin
  C.Bride := Bride;
  C.Groom := Groom;
  Result := C;
end;

{ Процедура удаления верхнего(последнего) элемента стэка }
function Pop(var Top: CoupleNode): TCouple;
var
  TempNode: CoupleNode = nil;
begin
  Result := Top^.Data;
  TempNode := Top;
  Top := Top^.Next;
  Dispose(TempNode);
end;

{ Процедура добавления элемента на верх стэка }
procedure Push(var Top: CoupleNode; const C: TCouple);
var
  TempNode: CoupleNode = nil;
begin
  New(TempNode);
  TempNode^.Data := C;
  TempNode^.Next := Top;
  Top := TempNode;
end;

{ Процедура освобождения памяти занятой стэком }
procedure Free(var Top: CoupleNode);
begin
  while Top <> nil do
    Pop(Top);
end;

end.
