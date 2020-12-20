unit HopcroftKarp;

{$mode objfpc}{$H+}

interface

uses
  Dialogs,
  Classes,
  SysUtils,
  PartnerStack,
  CoupleStack,
  VertexStack,
  EdgeStack;

type

  TBipartiteGraph = record
    Brides: VertexNode;
    Grooms: VertexNode;

    Matches: EdgeNode;
    AugmentingPath: EdgeNode;
  end;

{ BipartiteGraph }
function FindMaxCouples(var Brides: PartnerNode; var Grooms: PartnerNode): CoupleNode;
function GraphDFS(var G: TBipartiteGraph; var V: VertexNode): boolean;
function GraphBFS(var G: TBipartiteGraph): VertexNode;

{Helpers}
procedure CleanVisitedMarks(var G: TBipartiteGraph);
procedure GetMatches(var G: TBipartiteGraph);
procedure LoadPartners(var Top: VertexNode; var P: PartnerNode);

implementation

function FindMaxCouples(var Brides: PartnerNode; var Grooms: PartnerNode): CoupleNode;
var
  TempBrides: VertexNode = nil;
  TempGrooms: VertexNode = nil;
  FreeVertex: VertexNode = nil;

  C: TCouple;
  G: TBipartiteGraph = (Brides: nil; Grooms: nil; Matches: nil; AugmentingPath: nil);
begin
  Result := nil;
  LoadPartners(G.Grooms, Grooms);
  LoadPartners(G.Brides, Brides);

  // Составление графа совпадений
  TempBrides := G.Brides;
  while G.Brides <> nil do
  begin
    TempGrooms := G.Grooms;
    while G.Grooms <> nil do
    begin
      if IsItMatch(G.Brides^.Data^.Partner, G.Grooms^.Data^.Partner) then
      begin
        // Добавить невесту в список подходящих невест жениха
        VertexStack.Push(G.Grooms^.Data^.SuitablePartners, G.Brides^.Data);
        // Добавить жениха в список подходящих женихов невесты
        VertexStack.Push(G.Brides^.Data^.SuitablePartners, G.Grooms^.Data);
      end;

      G.Grooms := G.Grooms^.Next;
    end;

    G.Grooms := TempGrooms;
    G.Brides := G.Brides^.Next;
  end;

  G.Brides := TempBrides;

  // Пока есть свободные вершины графа
  FreeVertex := GraphBFS(G);
  while FreeVertex <> nil do
  begin

    // Если нет увеличивающегося пути из свободной вершины,
    // то помечаю вершину как несвободную
    if GraphDFS(G, FreeVertex) <> True then
      FreeVertex^.Data^.IsFree := False
    else
      // Увеличиваю Matches за счет увеличивающегося пути.
      GetMatches(G);

    // Убираю отметки о посещении
    CleanVisitedMarks(G);
    // Удаляю увеличивающийся путь
    EdgeStack.Free(G.AugmentingPath);
    // Получаю следующую свободную вершину
    FreeVertex := GraphBFS(G);
  end;

  while G.Matches <> nil do
  begin
    C := CoupleStack.NewCouple(G.Matches^.Data^.Bride^.Data^.Partner,
      G.Matches^.Data^.Groom^.Data^.Partner);

    CoupleStack.Push(Result, C);
    EdgeStack.Pop(G.Matches);
  end;

  VertexStack.Free(G.Brides);
  VertexStack.Free(G.Grooms);
end;

{ Обход графа в ширину. Поиск первой свободной точки }
function GraphBFS(var G: TBipartiteGraph): VertexNode;

var
  TempBride: VertexNode;
  TempGroom: VertexNode;
begin
  Result := nil;
  TempBride := G.Brides;
  TempGroom := G.Grooms;

  while (G.Grooms <> nil) and (Result = nil) do
  begin
    if G.Grooms^.Data^.IsFree then
      Result := G.Grooms;

    G.Grooms := G.Grooms^.Next;
  end;

  while (G.Brides <> nil) and (Result = nil) do
  begin
    if G.Brides^.Data^.IsFree then
      Result := G.Brides;

    G.Brides := G.Brides^.Next;
  end;

  G.Brides := TempBride;
  G.Grooms := TempGroom;
end;

{ Обход графа в глубину начиная от вершины V. Поиск дополняющего пути }
function GraphDFS(var G: TBipartiteGraph; var V: VertexNode): boolean;
var
  TempNode: VertexNode;
  SuitablePartner: VertexNode;
begin
  if (V = nil) or V^.Data^.IsVisited then
  begin
    Result := False;
    Exit;
  end;

  // Помечаю точку как посещенную
  V^.Data^.IsVisited := True;

  TempNode := V^.Data^.SuitablePartners;
  while V^.Data^.SuitablePartners <> nil do
  begin
    SuitablePartner := V^.Data^.SuitablePartners;

    // Если подходящий партнер свободен или есть увеличивающийся путь,
    // то добавить пару в список совпадений.
    if SuitablePartner^.Data^.IsFree or GraphDFS(G, SuitablePartner) then
    begin

      // Добавляю пару в увеличивающийся путь
      if V^.Data^.Partner.Sex = PartnerSex.man then
        EdgeStack.Push(G.AugmentingPath, EdgeStack.NewEdge(SuitablePartner, V))
      else
        EdgeStack.Push(G.AugmentingPath, EdgeStack.NewEdge(V, SuitablePartner));

      // Отмечаем точку как занятую(есть партнер)
      V^.Data^.IsFree := False;
      // Отмечаем точку подходящего партнера как занятую(есть партнер)
      SuitablePartner^.Data^.IsFree := False;

      Result := True;
      V^.Data^.SuitablePartners := TempNode;
      Exit;
    end;

    V^.Data^.SuitablePartners := V^.Data^.SuitablePartners^.Next;
  end;

  Result := False;
  V^.Data^.SuitablePartners := TempNode;
end;

{ Процедура удаления отметок о посещении }
procedure CleanVisitedMarks(var G: TBipartiteGraph);
var
  TempVertex: VertexNode;
  TempNode: VertexNode;
begin
  TempVertex := G.Brides;

  while G.Brides <> nil do
  begin
    G.Brides^.Data^.IsVisited := False;
    G.Brides := G.Brides^.Next;
  end;
  TempNode := G.Brides;

  G.Brides := TempVertex;
  TempVertex := G.Grooms;

  while G.Grooms <> nil do
  begin
    G.Grooms^.Data^.IsVisited := False;
    G.Grooms := G.Grooms^.Next;
  end;

  G.Grooms := TempVertex;
  G.Grooms := TempNode;
end;

{ Процедура увеличения списка сочетаний }
procedure GetMatches(var G: TBipartiteGraph);
var
  TempAugmentingPath: EdgeNode = nil;
  TempMatches: EdgeNode = nil;
begin

  TempAugmentingPath := G.AugmentingPath;

  while G.AugmentingPath <> nil do
  begin
    TempMatches := G.Matches;

    while G.Matches <> nil do
    begin
      if (G.Matches^.Data^.Bride^.Data = G.AugmentingPath^.Data^.Bride^.Data) and
        (G.Matches^.Data^.Groom^.Data = G.AugmentingPath^.Data^.Groom^.Data) then
      begin
        EdgeStack.Delete(TempAugmentingPath, G.AugmentingPath);
        EdgeStack.Delete(TempMatches, G.Matches);
      end;

      G.Matches := G.Matches^.Next;
    end;

    G.Matches := TempMatches;
    G.AugmentingPath := G.AugmentingPath^.Next;
  end;

  G.AugmentingPath := TempAugmentingPath;

  while G.AugmentingPath <> nil do
  begin
    EdgeStack.Push(G.Matches, G.AugmentingPath^.Data);
    G.AugmentingPath := G.AugmentingPath^.Next;
  end;

  G.AugmentingPath := TempAugmentingPath;
end;

{ Процедура загрузки женихов в граф }
procedure LoadPartners(var Top: VertexNode; var P: PartnerNode);
var
  TempPartner: PartnerNode = nil;
begin
  TempPartner := P;

  while P <> nil do
  begin
    VertexStack.PushPartner(Top, P^.Data, nil);
    P := P^.Next;
  end;

  P := TempPartner;
end;

end.
