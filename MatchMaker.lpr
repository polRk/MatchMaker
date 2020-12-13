program MatchMaker;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  HomeScreen,
  EditScreen,
  Variables,
  HopcroftKarp,
  PartnerStack,
  CoupleStack,
  VertexStack,
  EdgeStack;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(THomeForm, HomeForm);
  Application.CreateForm(TEditForm, EditForm);
  Application.Run;
end.
