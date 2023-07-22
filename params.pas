unit params;

interface

type
  TPS = class(TObject)
  private
    function getFileFolder: string;
    function getFileName: string;
    function getNoFile: boolean;
    function getFileFolderAndName: string;
  public
    property fileFolder: string read getFileFolder;
    property fileFolderAndName: string read getFileFolderAndName;
    property fileName: string read getFileName;
    property noFile: boolean read getNoFile;
  end;

function PS: TPS; // Param Strings

implementation

uses
  system.sysUtils;

var
  gPS : TPS;

function PS: TPS;
begin
  case gPS = NIL of TRUE: gPS := TPS.create; end;
  result := gPS;
end;

{ TPS }

function TPS.getFileFolder: string;
begin
  case paramCount = 0 of  TRUE: result := '';
                         FALSE: result := extractFilePath(paramStr(1)); end;
end;

function TPS.getFileFolderAndName: string;
begin
  result := fileFolder + fileName;
end;

function TPS.getFileName: string;
begin
  case paramCount = 0 of  TRUE: result := '';
                         FALSE: result := extractFileName(paramStr(1)); end;
end;

function TPS.getNoFile: boolean;
begin
  result := fileFolderAndName = '';
end;

initialization
  gPS := NIL;

finalization
  case gPS <> NIL of TRUE: gPS.free; end;

end.
