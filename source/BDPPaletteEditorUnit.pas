unit BDPPaletteEditorUnit;

{$mode Delphi}

interface

uses
  vcc_Container2, BDPButtonUnit, mk_sdl2, BDPMessageUnit;

type

  { TBDPaletteEditor }

  TBDPaletteEditor=class(TContainer)
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
  end;

implementation

{ TBDPaletteEditor }

constructor TBDPaletteEditor.Create;
begin

end;

destructor TBDPaletteEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TBDPaletteEditor.Draw;
begin
  inherited Draw;
end;

end.

