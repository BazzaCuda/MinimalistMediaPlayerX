unit MediaInfoDll;

{
  MediaInfoLib (MediaInfo.dll v0.7.7.6) Interface for Delphi
    (c)2008 by Norbert Mereg (Icebob)

    http://MediaArea.net/MediaInfo
}

// Defines how the DLL is called (dynamic or static)
//{$DEFINE STATIC}

interface
uses
{$IFDEF WIN32}
  Windows;
{$ELSE}
  Wintypes, WinProcs;
{$ENDIF}

type TMIStreamKind =
(
    Stream_General,
    Stream_Video,
    Stream_Audio,
    Stream_Text,
    Stream_Other,
    Stream_Image,
    Stream_Menu,
    Stream_Max
);

type TMIInfo =
(
    Info_Name,
    Info_Text,
    Info_Measure,
    Info_Options,
    Info_Name_Text,
    Info_Measure_Text,
    Info_Info,
    Info_HowTo,
    Info_Max
);

type TMIInfoOption =
(
    InfoOption_ShowInInform,
    InfoOption_Reserved,
    InfoOption_ShowInSupported,
    InfoOption_TypeOfValue,
    InfoOption_Max
);


{$IFDEF STATIC}
  // Unicode methods
  function  MediaInfo_New(): Cardinal cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  procedure MediaInfo_Delete(Handle: Cardinal) cdecl  {$IFDEF WIN32} stdcall {$ENDIF}; external 'MediaInfo.Dll';
  function  MediaInfo_Open(Handle: Cardinal; File__: PWideChar): Cardinal cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  procedure MediaInfo_Close(Handle: Cardinal) cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  function  MediaInfo_Inform(Handle: Cardinal; Reserved: Integer): PWideChar cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  function  MediaInfo_GetI(Handle: Cardinal; StreamKind: TMIStreamKind; StreamNumber: Integer; Parameter: Integer; KindOfInfo: TMIInfo): PWideChar cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll'; //Default: KindOfInfo=Info_Text
  function  MediaInfo_Get(Handle: Cardinal; StreamKind: TMIStreamKind; StreamNumber: Integer; Parameter: PWideChar; KindOfInfo: TMIInfo; KindOfSearch: TMIInfo): PWideChar cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll'; //Default: KindOfInfo=Info_Text, KindOfSearch=Info_Name
  function  MediaInfo_Option(Handle: Cardinal; Option: PWideChar; Value: PWideChar): PWideChar cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  function  MediaInfo_State_Get(Handle: Cardinal): Integer cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  function  MediaInfo_Count_Get(Handle: Cardinal; StreamKind: TMIStreamKind; StreamNumber: Integer): Integer cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';

  // Ansi methods
  function  MediaInfoA_New(): Cardinal cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  procedure MediaInfoA_Delete(Handle: Cardinal) cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  function  MediaInfoA_Open(Handle: Cardinal; File__: PAnsiChar): Cardinal cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  procedure MediaInfoA_Close(Handle: Cardinal) cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  function  MediaInfoA_Inform(Handle: Cardinal; Reserved: Integer): PAnsiChar cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  function  MediaInfoA_GetI(Handle: Cardinal; StreamKind: TMIStreamKind; StreamNumber: Integer; Parameter: Integer; KindOfInfo: TMIInfo): PAnsiChar cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll'; //Default: KindOfInfo=Info_Text
  function  MediaInfoA_Get(Handle: Cardinal; StreamKind: TMIStreamKind; StreamNumber: Integer; Parameter: PAnsiChar; KindOfInfo: TMIInfo; KindOfSearch: TMIInfo): PAnsiChar cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll'; //Default: KindOfInfo=Info_Text, KindOfSearch=Info_Name
  function  MediaInfoA_Option(Handle: Cardinal; Option: PAnsiChar; Value: PAnsiChar): PAnsiChar cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  function  MediaInfoA_State_Get(Handle: Cardinal): Integer cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
  function  MediaInfoA_Count_Get(Handle: Cardinal; StreamKind: TMIStreamKind; StreamNumber: Integer): Integer cdecl  {$IFDEF WIN32} stdcall {$ENDIF};external 'MediaInfo.Dll';
{$ELSE}

var
  libHandle: THandle = 0;

  // Unicode methods
  mediaInfo_New:        function  (): THandle cdecl stdcall;
  mediaInfo_Delete:     procedure (Handle: THandle) cdecl stdcall;
  mediaInfo_Open:       function  (Handle: THandle; File__: PWideChar): Cardinal cdecl stdcall;
  mediaInfo_Close:      procedure (Handle: THandle) cdecl stdcall;
  mediaInfo_Inform:     function  (Handle: THandle; Reserved: Integer): PWideChar cdecl stdcall;
  mediaInfo_GetI:       function  (Handle: THandle; StreamKind: TMIStreamKind; StreamNumber: Integer; Parameter: Integer;   KindOfInfo: TMIInfo): PWideChar cdecl stdcall; //Default: KindOfInfo=Info_Text,
  mediaInfo_Get:        function  (Handle: THandle; StreamKind: TMIStreamKind; StreamNumber: Integer; Parameter: PWideChar; KindOfInfo: TMIInfo; KindOfSearch: TMIInfo): PWideChar cdecl stdcall; //Default: KindOfInfo=Info_Text, KindOfSearch=Info_Name
  mediaInfo_Option:     function  (Handle: THandle; Option: PWideChar; Value: PWideChar): PWideChar cdecl stdcall;
  mediaInfo_State_Get:  function  (Handle: THandle): Integer cdecl stdcall;
  mediaInfo_Count_Get:  function  (Handle: THandle; StreamKind: TMIStreamKind; StreamNumber: Integer): Integer cdecl stdcall;

  // Ansi methods
  mediaInfoA_New:       function  (): THandle cdecl stdcall;
  mediaInfoA_Delete:    procedure (Handle: THandle) cdecl stdcall;
  mediaInfoA_Open:      function  (Handle: THandle; File__: PAnsiChar): Cardinal cdecl stdcall;
  mediaInfoA_Close:     procedure (Handle: THandle) cdecl stdcall;
  mediaInfoA_Inform:    function  (Handle: THandle; Reserved: Integer): PAnsiChar cdecl stdcall;
  mediaInfoA_GetI:      function  (Handle: THandle; StreamKind: TMIStreamKind; StreamNumber: Integer; Parameter: Integer; KindOfInfo: TMIInfo): PAnsiChar cdecl stdcall; //Default: KindOfInfo=Info_Text
  mediaInfoA_Get:       function  (Handle: THandle; StreamKind: TMIStreamKind; StreamNumber: Integer; Parameter: PAnsiChar;   KindOfInfo: TMIInfo; KindOfSearch: TMIInfo): PAnsiChar cdecl stdcall; //Default: KindOfInfo=Info_Text, KindOfSearch=Info_Name
  mediaInfoA_Option:    function  (Handle: THandle; Option: PAnsiChar; Value: PAnsiChar): PAnsiChar cdecl stdcall;
  mediaInfoA_State_Get: function  (Handle: THandle): Integer cdecl stdcall;
  mediaInfoA_Count_Get: function  (Handle: THandle; StreamKind: TMIStreamKind; StreamNumber: Integer): Integer cdecl stdcall;

function mediaInfoDLL_Load(const LibPath: string): boolean;

{$ENDIF}

implementation

uses mmpDoProcs;

{$IFNDEF STATIC}
function MI_getProcAddress(name: PChar; var addr: pointer): boolean;
begin
  addr    := getProcAddress(libHandle, name);
  result  := addr <> NIL;
end;

function mediaInfoDLL_Load(const libPath: string): boolean;
begin
  result := FALSE;

  mmpDo(libHandle = 0,  procedure begin libHandle := loadLibrary(PChar(libPath)); end);

  mmpDo(libHandle <> 0, procedure begin
                                    MI_getProcAddress('MediaInfo_New',        @MediaInfo_New);
                                    MI_getProcAddress('MediaInfo_Delete',     @MediaInfo_Delete);
                                    MI_getProcAddress('MediaInfo_Open',       @MediaInfo_Open);
                                    MI_getProcAddress('MediaInfo_Close',      @MediaInfo_Close);
                                    MI_getProcAddress('MediaInfo_Inform',     @MediaInfo_Inform);
                                    MI_getProcAddress('MediaInfo_GetI',       @MediaInfo_GetI);
                                    MI_getProcAddress('MediaInfo_Get',        @MediaInfo_Get);
                                    MI_getProcAddress('MediaInfo_Option',     @MediaInfo_Option);
                                    MI_getProcAddress('MediaInfo_State_Get',  @MediaInfo_State_Get);
                                    MI_getProcAddress('MediaInfo_Count_Get',  @MediaInfo_Count_Get);

                                    MI_getProcAddress('MediaInfoA_New',       @MediaInfoA_New);
                                    MI_getProcAddress('MediaInfoA_Delete',    @MediaInfoA_Delete);
                                    MI_getProcAddress('MediaInfoA_Open',      @MediaInfoA_Open);
                                    MI_getProcAddress('MediaInfoA_Close',     @MediaInfoA_Close);
                                    MI_getProcAddress('MediaInfoA_Inform',    @MediaInfoA_Inform);
                                    MI_getProcAddress('MediaInfoA_GetI',      @MediaInfoA_GetI);
                                    MI_getProcAddress('MediaInfoA_Get',       @MediaInfoA_Get);
                                    MI_getProcAddress('MediaInfoA_Option',    @MediaInfoA_Option);
                                    MI_getProcAddress('MediaInfoA_State_Get', @MediaInfoA_State_Get);
                                    MI_getProcAddress('MediaInfoA_Count_Get', @MediaInfoA_Count_Get);
                                  end);

  result := libHandle <> 0;
end;

{$ENDIF}

end.
