
Unit BipartiteGraph;

{$mode objfpc}{$H+}

Interface

Uses 
Classes, SysUtils, PartnerStack, VertexStack, EdgeStack;

Type 
  TBipartiteGraph = Record
    Brides: Vertex;
    Grooms: Vertex;

    Matches: Edge;
  End;

{ Matches }
Procedure MatchAdd(Var Top: Edge; Const Bride: Vertex; Const Groom: Vertex);

{ BipartiteGraph }
Function GraphDFS(Var V: Vertex; Var M: Edge): boolean;
Function GraphBFS(Var Bride: Vertex; Var Groom: Vertex): Vertex;

Implementation

{ Процедура добавления пары }
Procedure MatchAdd(Var Top: Edge; Const Bride: Vertex; Const Groom: Vertex);

Var 
  Sentinel: Edge;
Begin

  If Top = Nil Then
    Begin
      EdgeStack.Push(Top, Bride, Groom);
      Exit;
    End;

  Sentinel := Top;

  While Top <> Nil Do
    Begin
      If (Top^.Bride = Bride) And (Top^.Groom = Groom) Then
        Begin
          EdgeStack.Delete(Sentinel, Top);
          break;
        End;

      Top := Top^.Next;
    End;

  Top := Sentinel;
End;

{ Обход графа в глубину }
Function GraphDFS(Var V: Vertex; Var M: Edge): boolean;

Var 
  TempVertexList: VertexList;
Begin
  If (V = Nil) Or V^.IsVisited Then
    Begin
      Result := False;
      Exit;
    End;

  V^.IsVisited := True;
  TempVertexList := V^.SuitablePartners;

  While V^.SuitablePartners <> Nil Do
    Begin

      If V^.SuitablePartners^.V^.IsFree Or GraphDFS(V^.SuitablePartners^.V, M) Then
        Begin
          If V^.Partner.Sex = PartnerSex.man Then
            MatchAdd(M, V^.SuitablePartners^.V, V)
          Else
            MatchAdd(M, V, V^.SuitablePartners^.V);

          V^.IsFree := False;
          V^.SuitablePartners^.V^.IsFree := False;

          Result := True;
          V^.SuitablePartners := TempVertexList;
          Exit;
        End;

      V^.SuitablePartners := V^.SuitablePartners^.Next;
    End;

  Result := False;
  V^.SuitablePartners := TempVertexList;
End;

{ Обход графа в ширину }
Function GraphBFS(Var Bride: Vertex; Var Groom: Vertex): Vertex;

Var 
  TempBride: Vertex;
  TempGroom: Vertex;
Begin
  Result := Nil;
  TempBride := Bride;
  TempGroom := Groom;

  While (Bride <> Nil) And (Result = Nil) Do
    Begin
      If Bride^.IsFree Then
        Result := Bride;

      Bride := Bride^.Next;
    End;

  While (Groom <> Nil) And (Result = Nil) Do
    Begin
      If Groom^.IsFree Then
        Result := Groom;

      Groom := Groom^.Next;
    End;

  Bride := TempBride;
  Groom := TempGroom;
End;

End.
