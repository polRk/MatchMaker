unit HopcroftKarp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BipartiteGraph, PartnerStack, CoupleStack, VertexStack, EdgeStack;

function FindMaxCouples(var Brides: PartnerCell; var Grooms: PartnerCell): CoupleCell;

implementation

procedure CleanVisitedMarks(var Bride: Vertex; var Groom: Vertex);
var
  TempVertex: Vertex;
begin
  TempVertex := Bride;

  while Bride <> nil do
  begin
    Bride^.IsVisited := False;
    Bride := Bride^.Next;
  end;

  Bride := TempVertex;
  TempVertex := Groom;

  while Groom <> nil do
  begin
    Groom^.IsVisited := False;
    Groom := Groom^.Next;
  end;

  Groom := TempVertex;
end;

function FindMaxCouples(var Brides: PartnerCell; var Grooms: PartnerCell): CoupleCell;
var
  TempBrides: PartnerCell;
  TempGrooms: PartnerCell;
  CurrentVertex: Vertex = nil;

  C: TCouple;

  G: TBipartiteGraph = (Brides: nil; Grooms: nil; Matches: nil);
begin
  Result := nil;
  TempBrides := Brides;
  TempGrooms := Grooms;

  while Brides <> nil do
  begin
    VertexStack.Push(G.Brides, Brides^.Data, nil);

    while Grooms <> nil do
    begin

      if IsItMatch(Brides^.Data, Grooms^.Data) then
      begin
        VertexStack.Push(G.Grooms, Grooms^.Data, nil);
        VertexListPush(G.Grooms^.SuitablePartners, G.Brides);
        VertexListPush(G.Brides^.SuitablePartners, G.Grooms);
      end;

      Grooms := Grooms^.Next;
    end;

    Grooms := TempGrooms;
    Brides := Brides^.Next;
  end;

  Brides := TempBrides;

  CurrentVertex := GraphBFS(G.Brides, G.Grooms);
  while CurrentVertex <> nil do
  begin
    if GraphDFS(CurrentVertex, G.Matches) <> True then
      CurrentVertex^.IsFree := False;

    CleanVisitedMarks(G.Brides, G.Grooms);
    CurrentVertex := GraphBFS(G.Brides, G.Grooms);
  end;

  while G.Matches <> nil do
  begin
    C.Bride := G.Matches^.Bride^.Partner;
    C.Groom := G.Matches^.Groom^.Partner;

    CoupleStack.Push(Result, C);
    EdgeStack.Pop(G.Matches);
  end;

  VertexStack.Free(G.Brides);
  VertexStack.Free(G.Grooms);
end;

end.
