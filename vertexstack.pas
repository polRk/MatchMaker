unit VertexStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PartnerStack;

type
  VertexNode = ^TVertexNode; // ссылка на элемент списка вершин графа

  Vertex = ^TVertex;

  TVertex = record // вершина графа
    Partner: TPartner;
    SuitablePartners: VertexNode; // Подходящие партнеры

    IsFree: boolean;
    IsVisited: boolean;
  end;


  TVertexNode = record // элемент списка вершин графа
    Data: Vertex;
    Next: VertexNode;
  end;

function NewVertex(const P: TPartner; const SP: VertexNode): Vertex;
function Pop(var Top: VertexNode): Vertex;
procedure Push(var Top: VertexNode; const V: Vertex);
procedure PushPartner(var Top: VertexNode; const P: TPartner; const SP: VertexNode);
procedure Free(var Top: VertexNode);

implementation

{ Создание новой вершины }
function NewVertex(const P: TPartner; const SP: VertexNode): Vertex;
var
  V: Vertex = nil;
begin
  New(V);
  V^.Partner := P;
  V^.SuitablePartners := SP;
  Result := V;
end;

{ Процедура удаления верхнего элемента стэка }
function Pop(var Top: VertexNode): Vertex;
var
  TempNode: VertexNode = nil;
begin
  Result := Top^.Data;
  TempNode := Top;
  Top := Top^.Next;
  Dispose(TempNode);
end;

{ Процедура добавления элемента на верх стэка }
procedure Push(var Top: VertexNode; const V: Vertex);
var
  TempNode: VertexNode = nil;
begin
  New(TempNode);
  TempNode^.Data := V;
  TempNode^.Next := Top;
  Top := TempNode;
end;

{ Процедура добавления партнера на верх стэка }
procedure PushPartner(var Top: VertexNode; const P: TPartner; const SP: VertexNode);
var
  V: Vertex = nil;
begin
  New(V);
  V^.Partner := P;
  V^.SuitablePartners := SP;
  V^.IsFree := True;
  V^.IsVisited := False;

  Push(Top, V);
end;

{ Процедура освобождения памяти занятой стэком }
procedure Free(var Top: VertexNode);
begin
  while Top <> nil do
    Pop(Top);
end;

end.
