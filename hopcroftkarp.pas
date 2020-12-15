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
  // Загружаю стэк невест в граф
  LoadPartners(G.Grooms, Grooms);
  // Загружаю стэк партнеров в граф
  LoadPartners(G.Brides, Brides);

  // Сохраняю ссылку на верх стэка невест
  TempBrides := G.Brides;

  // Составляю граф совпадений
  // Пока не дошел до конца стэка
  while G.Brides <> nil do
  begin
    // Сохраняю ссылку на верх стэка невест
    TempGrooms := G.Grooms;

    // Пока не дошел до конца стэка
    while G.Grooms <> nil do
    begin

      // Есл их критерии совпали
      if IsItMatch(G.Brides^.Data^.Partner, G.Grooms^.Data^.Partner) then
      begin
        // Добавляю невесту в список подходящих партнеров
        VertexStack.Push(G.Grooms^.Data^.SuitablePartners, G.Brides^.Data);
        // Добавялю жениха в список подходящих партнеров
        VertexStack.Push(G.Brides^.Data^.SuitablePartners, G.Grooms^.Data);
      end;

      // Передвигаюсь дальше по стэку женихов
      G.Grooms := G.Grooms^.Next;
    end;

    // Восстанавливаю указатель на верх стэка
    G.Grooms := TempGrooms;
    // Передвигаюсь дальше по стэку невест
    G.Brides := G.Brides^.Next;
  end;

  // Восстанавливаю указатель на верх стэка
  G.Brides := TempBrides;

  // Получаю первую свободную вершину
  FreeVertex := GraphBFS(G);

  // Пока есть свободная вершина
  while FreeVertex <> nil do
  begin
    // Если нет дополняющего пути из свободной вершины,
    // то помечаю вершину как несвободную
    if GraphDFS(G, FreeVertex) <> True then
      FreeVertex^.Data^.IsFree := False
    else
      // Иначе, увеличиваю стэк пар за счет увеличивающегося пути.
      GetMatches(G);

    // Убираю отметки о посещении
    CleanVisitedMarks(G);
    // Удаляю дополняющий путь
    EdgeStack.Free(G.AugmentingPath);
    // Получаю следующую свободную вершину
    FreeVertex := GraphBFS(G);
  end;

  // Пока не дошел до конца стэка
  while G.Matches <> nil do
  begin
    // Создаю новую пару
    C := CoupleStack.NewCouple(G.Matches^.Data^.Bride^.Data^.Partner,
      G.Matches^.Data^.Groom^.Data^.Partner);

    // Добавляю пару в результат работы функции(стэк пар)
    CoupleStack.Push(Result, C);

    // Удаляю совпавщую пару и двигаюсь дальше по циклу
    EdgeStack.Pop(G.Matches);
  end;

  // Освобождаю память
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
  // Сохраняю ссылку на верх стэка невест
  TempBride := G.Brides;
  // Сохраняю ссылку на верх стэка женихов
  TempGroom := G.Grooms;

  // // Пока не дошел до конца стэка и не нашёл свободную вершину
  while (G.Grooms <> nil) and (Result = nil) do
  begin
    // Если вершина свободная, то записываем указатель в результат функции
    if G.Grooms^.Data^.IsFree then
      Result := G.Grooms;

    G.Grooms := G.Grooms^.Next;
  end;

  // Пока не дошел до конца стэка и не нашёл свободную вершину
  while (G.Brides <> nil) and (Result = nil) do
  begin
    // Если вершина свободная, то записываем указатель в результат функции
    if G.Brides^.Data^.IsFree then
      Result := G.Brides;

    G.Brides := G.Brides^.Next;
  end;

  // Восстанавливаю ссылки на верх стэка
  G.Brides := TempBride;
  G.Grooms := TempGroom;
end;

{ Обход графа в глубину начиная от вершины V. Поиск дополняющего пути }
function GraphDFS(var G: TBipartiteGraph; var V: VertexNode): boolean;
var
  TempNode: VertexNode;
  SuitablePartner: VertexNode;
begin
  // Если переданная вершина несуществует или была посещена,
  // то выхожу из функции с результатом того, что нет дополняющего пути
  if (V = nil) or V^.Data^.IsVisited then
  begin
    Result := False;
    Exit;
  end;

  // Помечаю точку как посещенную
  V^.Data^.IsVisited := True;
  // Сохраняю ссылку на верх стэка подходящих партнеров
  TempNode := V^.Data^.SuitablePartners;

  // Пока не дошел до конца стэка
  while V^.Data^.SuitablePartners <> nil do
  begin
    SuitablePartner := V^.Data^.SuitablePartners;

    // Если подходящий партнер свободен или есть дополняющий путь,
    // то добавляю пару в список совпадений.
    if SuitablePartner^.Data^.IsFree or GraphDFS(G, SuitablePartner) then
    begin

      // Добавляю пару в дополняющий путь
      // Невеста всегда идет первая в паре
      if V^.Data^.Partner.Sex = PartnerSex.man then
        EdgeStack.Push(G.AugmentingPath, EdgeStack.NewEdge(SuitablePartner, V))
      else
        EdgeStack.Push(G.AugmentingPath, EdgeStack.NewEdge(V, SuitablePartner));

      // Отмечаем точку как занятую(есть партнер)
      V^.Data^.IsFree := False;
      // Отмечаем точку подходящего партнера как занятую(есть партнер)
      SuitablePartner^.Data^.IsFree := False;

      // Восстанавливаю ссылку на верх стэка подходящих партнеров
      V^.Data^.SuitablePartners := TempNode;

      // Выхожу с положительным результатом
      // (нашли свободного партнера или дополняющий путь)
      Result := True;
      Exit;
    end;

    V^.Data^.SuitablePartners := V^.Data^.SuitablePartners^.Next;
  end;

  // Восстанавливаю ссылку на верх стэка подходящих партнеров
  V^.Data^.SuitablePartners := TempNode;

  // В иных случаях пути не существует.
  Result := False;
end;

{ Процедура удаления отметок о посещении }
procedure CleanVisitedMarks(var G: TBipartiteGraph);
var
  TempVertex: VertexNode;
  TempNode: VertexNode;
begin
  // Сохраняю ссылку на верх стэка невест
  TempNode := G.Brides;

  // Пока не дошел до конца стэка
  while G.Brides <> nil do
  begin
    // Удаляю отметку о песещении
    G.Brides^.Data^.IsVisited := False;
    G.Brides := G.Brides^.Next;
  end;

  // Восстанавливаю ссылку на верх стэка невест
  G.Brides := TempNode;

  // Сохраняю ссылку на верх стэка невест
  TempNode := G.Grooms;

  // Пока не дошел до конца стэка
  while G.Grooms <> nil do
  begin
    // Удаляю отметку о песещении
    G.Grooms^.Data^.IsVisited := False;
    G.Grooms := G.Grooms^.Next;
  end;

  // Восстанавливаю ссылку на верх стэка женихов
  G.Grooms := TempNode;
end;

{ Процедура увеличения списка сочетаний }
procedure GetMatches(var G: TBipartiteGraph);
var
  TempAugmentingPath: EdgeNode = nil;
  TempMatches: EdgeNode = nil;
begin
  // Сохраняю ссылку на верх стэка дополняющего пути
  TempAugmentingPath := G.AugmentingPath;

  // Пока не дошел до конца стэка
  while G.AugmentingPath <> nil do
  begin
    // Сохраняю ссылку на верх стэка совпадений
    TempMatches := G.Matches;

    // Пока не дошел до конца стэка
    while G.Matches <> nil do
    begin
      // Если звено есть и в дополняющем пути, и в стэке совпадений,
      // тогда удаляю это звено из обоих стэков (симметрическая разность)
      if (G.Matches^.Data^.Bride^.Data = G.AugmentingPath^.Data^.Bride^.Data) and
        (G.Matches^.Data^.Groom^.Data = G.AugmentingPath^.Data^.Groom^.Data) then
      begin
        EdgeStack.Delete(TempAugmentingPath, G.AugmentingPath);
        EdgeStack.Delete(TempMatches, G.Matches);
      end;

      G.Matches := G.Matches^.Next;
    end;

    // Восстанавливаю ссылку на верх стэка совпадений
    G.Matches := TempMatches;
    G.AugmentingPath := G.AugmentingPath^.Next;
  end;

  // Восстанавливаю ссылку на верх стэка дополняющего пути
  G.AugmentingPath := TempAugmentingPath;

  // Добавляю новые звенья дополняющего пути в стэк совпадений
  // Пока не дошел до конца стэка
  while G.AugmentingPath <> nil do
  begin
    EdgeStack.Push(G.Matches, G.AugmentingPath^.Data);
    G.AugmentingPath := G.AugmentingPath^.Next;
  end;

  // Восстанавливаю ссылку на верх стэка дополняющего пути
  G.AugmentingPath := TempAugmentingPath;
end;

{ Процедура загрузки женихов в граф }
procedure LoadPartners(var Top: VertexNode; var P: PartnerNode);
var
  TempPartner: PartnerNode = nil;
begin
  // Сохраняю ссылку на верх стэка партнеров
  TempPartner := P;

  // Пока не дошел до конца стэка
  while P <> nil do
  begin
    // Добавляю партнера в стэк вершин графа
    VertexStack.PushPartner(Top, P^.Data, nil);
    P := P^.Next;
  end;

  // Восстанавливаю ссылку на верх стэка партнеров
  P := TempPartner;
end;

end.
