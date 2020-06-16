unit EdgeStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VertexStack;

type

  Edge = ^TEdge;

  TEdge = record
    Bride: Vertex;
    Groom: Vertex;
    Next: Edge;
  end;

procedure Pop(var Top: Edge);
procedure Push(var Top: Edge; const Bride: Vertex; const Groom: Vertex);
procedure Delete(var Top: Edge; P: Edge);
procedure Free(var Top: Edge);

implementation

{ Получение элемента с верха стэка }
procedure Pop(var Top: Edge);
var
  TempEdge: Edge;
begin
  TempEdge := Top;
  Top := Top^.Next;
  Dispose(TempEdge);
end;

{ Процедура добавления элемента на верх стэка }
procedure Push(var Top: Edge; const Bride: Vertex; const Groom: Vertex);
var
  NewEdge: Edge;
begin
  New(NewEdge);
  NewEdge^.Bride := Bride;
  NewEdge^.Groom := Groom;
  NewEdge^.Next := Top;
  Top := NewEdge;
end;

{ Процедура удаления элемента по указателю из стэка }
procedure Delete(var Top: Edge; P: Edge);
var
  TempEdge: Edge = nil;
begin
  if (Top = nil) or (P = nil) then
    Exit;

  while Top <> nil do
  begin
    if Top = P then
    begin
      Pop(Top);
      break;
    end;

    if Top <> nil then
    begin
      Push(TempEdge, Top^.Bride, Top^.Groom);
      Top := Top^.Next;
    end;
  end;

  while TempEdge <> nil do
  begin
    Push(Top, TempEdge^.Bride, TempEdge^.Groom);
    Pop(TempEdge);
  end;
end;

{ Процедура освобождения памяти занятой стэком }
procedure Free(var Top: Edge);
var
  TempEdge: Edge;
begin
  while Top <> nil do
  begin
    TempEdge := Top;
    Top := Top^.Next;
    Dispose(TempEdge);
  end;
end;

end.
