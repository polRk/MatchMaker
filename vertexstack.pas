unit VertexStack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PartnerStack;

type
  Vertex = ^TVertex;

  VertexList = ^TVertexList;

  TVertex = record
    Partner: TPartner;
    SuitablePartners: VertexList; { Увеличивающийся путь }

    IsFree: boolean;
    IsVisited: boolean;

    Next: Vertex;
  end;

  TVertexList = record
    V: Vertex;
    Next: VertexList;
  end;

function Pop(var Top: Vertex): TPartner;
procedure Push(var Top: Vertex; P: TPartner; SP: VertexList);
procedure Delete(var Top: Vertex; V: Vertex);
procedure Free(var Top: Vertex);

procedure VertexListPush(var Top: VertexList; V: Vertex);
procedure VertexListFree(var Top: VertexList);

implementation

{ Получение элемента с верха стэка }
function Pop(var Top: Vertex): TPartner;
var
  TempVertex: Vertex;
begin
  Result := Top^.Partner;
  TempVertex := Top;
  Top := Top^.Next;
  Dispose(TempVertex);
end;

{ Процедура добавления элемента на верх стэка }
procedure Push(var Top: Vertex; P: TPartner; SP: VertexList);
var
  NewVertex: Vertex;
begin
  New(NewVertex);
  NewVertex^.Partner := P;
  NewVertex^.SuitablePartners := SP;
  NewVertex^.IsFree := True;
  NewVertex^.IsVisited := False;
  NewVertex^.Next := Top;
  Top := NewVertex;
end;

{ Процедура удаления элемента по указателю из стэка }
procedure Delete(var Top: Vertex; V: Vertex);
var
  TempVertex: Vertex;
begin
  if (Top = nil) or (V = nil) then
    Exit;

  TempVertex := Top;

  while Top <> nil do
  begin
    if Top = V then
    begin
      Pop(Top);
      break;
    end;

    Top := Top^.Next;
  end;

  Top := TempVertex;
end;

{ Процедура освобождения памяти занятой стэком }
procedure Free(var Top: Vertex);
var
  TempVertex: Vertex;
begin
  while Top <> nil do
  begin
    VertexListFree(Top^.SuitablePartners);

    TempVertex := Top;
    Top := Top^.Next;
    Dispose(TempVertex);
  end;
end;

{ Процедура добавления элемента на верх стэка }
procedure VertexListPush(var Top: VertexList; V: Vertex);
var
  NewVertexList: VertexList;
begin
  New(NewVertexList);
  NewVertexList^.V := V;
  NewVertexList^.Next := Top;
  Top := NewVertexList;
end;

{ Процедура освобождения памяти занятой стэком }
procedure VertexListFree(var Top: VertexList);
var
  TempVertexList: VertexList;
begin
  while Top <> nil do
  begin
    TempVertexList := Top;
    Top := Top^.Next;
    Dispose(TempVertexList);
  end;
end;

end.

