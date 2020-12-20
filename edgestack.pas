unit EdgeStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VertexStack;

type
  EdgeNode = ^TEdgeNode; // ссылка на элемент списка ребер графа

  Edge = ^TEdge;

  TEdge = record // ребро графа
    Bride: Vertex;
    Groom: Vertex;
  end;

  TEdgeNode = record // элемент списка ребер графа
    Data: Edge;
    Next: EdgeNode;
  end;

function NewEdge(const Bride: Vertex; const Groom: Vertex): Edge;

function Pop(var Top: EdgeNode): Edge;
procedure Push(var Top: EdgeNode; const E: Edge);
procedure Delete(var Top: EdgeNode; var E: EdgeNode);
procedure Free(var Top: EdgeNode);

implementation

{ Создание нового ребра }
function NewEdge(const Bride: Vertex; const Groom: Vertex): Edge;
var
  E: Edge = nil;
begin
  New(E);
  E^.Bride := Bride;
  E^.Groom := Groom;
  Result := E;
end;

{ Процедура удаления верхнего элемента стэка }
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

  // Пока не дошел до конца стэка
  while Top <> nil do
  begin
    // Если верхний элемент - нужный элемент, то удаляю и выхожу из цикла
    if Top = E then
    begin
      Pop(Top);
      break;
    end;

    // Иные элементы добавляю во временный стэк
    if Top <> nil then
      Push(TempNode, Pop(Top));
  end;

  // Добавляю элементы из временного стэка на верх переданного стэк
  while TempNode <> nil do
    Push(Top, Pop(TempNode));
end;

{ Процедура освобождения памяти занятой стэком }
procedure Free(var Top: EdgeNode);
begin
  while Top <> nil do
    Pop(Top);
end;

end.
