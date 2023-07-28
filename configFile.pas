unit configFile;

interface

uses
  system.classes;

type
  TConfigFile = class(TObject)
  strict private
    FFileContents: TStringList;
    FFilePath: string;
    function saveConfigFile: boolean;
  private
    function getValue(const aName: string): string;
    procedure setValue(const aName, aValue: string);
    function getAsInteger(const aName: string): integer;
  public
    constructor create;
    destructor destroy; override;
    function deleteName(aName: string): boolean;
    function initConfigFile(const aFilePath: string): boolean;
    property asInteger[const aName: string]: integer read getAsInteger;
    property value[const aName: string]: string read getValue write setValue;
  end;

function CF: TConfigFile;

implementation

uses
  system.sysUtils;

var
  gCF: TConfigFile;

function CF: TConfigFile;
begin
  case gCF = NIL of TRUE: gCF := TConfigFile.create; end;
  result := gCF;
end;

{ TConfigFile }

constructor TConfigFile.create;
begin
  inherited;
  FFileContents := TStringList.create;
end;

function TConfigFile.deleteName(aName: string): boolean;
begin
  FFileContents.delete(FFileContents.indexOfName(aName));
  saveConfigFile;
end;

destructor TConfigFile.destroy;
begin
  case FFileContents <> NIL of TRUE: FFileContents.free; end;
  inherited;
end;

function TConfigFile.getAsInteger(const aName: string): integer;
begin
  try result := strToIntDef(FFileContents.values[aName], 0); except result := 0; end;
end;

function TConfigFile.getValue(const aName: string): string;
begin
  result := FFileContents.values[aName];
end;

function TConfigFile.initConfigFile(const aFilePath: string): boolean;
begin
  FFilePath := aFilePath;
  case fileExists(FFilePath) of TRUE: FFileContents.loadFromFile(FFilePath); end;
end;

function TConfigFile.saveConfigFile: boolean;
begin
  FFileContents.saveToFile(FFilePath);
end;

procedure TConfigFile.setValue(const aName, aValue: string);
begin
  FFileContents.values[aName] := aValue;
  saveConfigFile;
end;

initialization
  gCF := NIL;

finalization
  case gCF <> NIL of TRUE: gCF.free; end;

end.
