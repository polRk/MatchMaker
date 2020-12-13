unit EdgeStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VertexStack;

type
  EdgeNode = ^TEdgeNode;

  Edge = ^TEdge;

  TEdge = record
    Bride: VertexNode;
    Groom: VertexNode;
  end;

  TEdgeNode = record
    Data: Edge;
    Next: EdgeNode;
  end;

function NewEdge(const Bride: VertexNode; const Groom: VertexNode): Edge;

function Pop(var Top: EdgeNode): Edge;
procedure Push(var Top: EdgeNode; const E: Edge);
procedure Delete(var Top: EdgeNode; var E: EdgeNode);
procedure Free(var Top: EdgeNode);

implementation

{ Создание нового звена }
function NewEdge(const Bride: VertexNode; const Groom: VertexNode): Edge;
var
  E: Edge = nil;
begin
  New(E);
  E^.Bride := Bride;
  E^.Groom := Groom;
  Result := E;
end;

{ Процедура удаления верхнего(последнего) элемента стэка }
function Pop(var Top: EdgeNode): Edge;
var
  TempNode: EdgeNode = nil;
begin
  Result := Top^.Data;
  TempNode := Top;
  Top := Top^.Next;
  Dispose(TempNode);
end;

{ Процедура добавления элемента на верх стэка }
procedure Push(var Top: EdgeNode; const E: Edge);
var
  TempNode: EdgeNode = nil;
begin
  New(TempNode);
  TempNode^.Data := E;
  TempNode^.Next := Top;
  Top := TempNode;
end;

{ Процедура удаления элемента по указателю из стэка }
procedure Delete(var Top: EdgeNode; var E: EdgeNode);
var
  TempNode: EdgeNode = nil;
begin
  if (Top = nil) or (E = nil) then
    Exit;

  // Сохраняю ссылку на верхний элемент списка
  TempNode := Top;

  // Пока не дошли до конца списка
  while Top <> nil do
  begin
    // Если верхний элемент - нужный элемент, то удаляю и выхожу из цикла
    if Top = E then
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
procedure Free(var Top: EdgeNode);
begin
  while Top <> nil do
    Pop(Top);
end;

end.
