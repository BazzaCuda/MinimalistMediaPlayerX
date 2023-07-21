unit MPVNode;

// LibMPV node operation helper classes
// Author: Edward G. (nbuyer@gmail.com)

interface

uses
  SysUtils, Classes, Variants, MPVClient;

type
  TMPVNode = class;

  // Store node list or key:value(node) list
  TMPVNodeList = class(TStringList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Assign(Source: TPersistent); override;

    function GetNode(i: Integer): TMPVNode;
    function FindNode(const sKey: string): TMPVNode;
    // Add a node, cNode is owned by this list
    function AddNode(const sKey: string; cNode: TMPVNode): Integer;
    // Copy a node
    function CopyNode(const sKey: string; cNode: TMPVNode): Integer;
  end;

  TMPVNode = class
  private
    m_nFmt: mpv_format;
    m_sVal: string; // string
    m_fVal: Double; // double
    m_nVal: MPVInt; // flag
    m_nVal64: MPVInt64; // int64
    m_cNodes: TMPVNodeList; // node list
    m_pBytes: Pointer; // byte array
    m_nBytesLen: size_t; // byte array size
  private
    function GetAsStr: string;
    procedure SetAsStr(const Value: string);
    function GetAsBool: Boolean;
    procedure SetAsBool(const Value: Boolean);
    function GetAsInt64: MPVInt64;
    procedure SetAsInt64(const Value: MPVInt64);
    function GetAsNodes: TMPVNodeList;
    procedure SetAsNodes(const Value: TMPVNodeList);
    function GetAsDouble: Double;
    procedure SetAsDouble(const Value: Double);
    function GetAsVariant: Variant;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Assign(cNode: TMPVNode);

    // Display readable text
    procedure SaveToStrings(cList: TStrings; const sPrefix: string = '');
    procedure SaveToFile(const sFileName: string);

    // Load from P_mpv_node
    function LoadFromMPVNode(pNode: P_mpv_node; bFreeNode: Boolean): Boolean;
    // Generate P_mpv_node data
    function GenerateMPVNode: P_mpv_node; overload;
    // Generate P_mpv_node data
    //   pNode is allocated/made by caller
    procedure GenerateMPVNode(pNode: P_mpv_node); overload;
    // Free P_mpv_node data
    //   bClearOnly: only clear pNode's data, do not free pNode itself
    class procedure FreeNode(pNode: P_mpv_node; bClearOnly: Boolean = False);

    // Add a node, this will set node format to MPV_FORMAT_NODE_ARRAY
    //   cNode is owned by this object, do not free it outside
    function AddNode(const sKey: string; cNode: TMPVNode): Integer;
    // Copy a node
    function CopyNode(const sKey: string; cNode: TMPVNode): Integer;
    // Allocate bytes buffer and return buffer pointer,
    // this will set node format to format to MPV_FORMAT_BYTE_ARRAY;
    function NewBytes(nSize: size_t): Pointer;
    // Set bytes buffer to data pointer, copy or directly use pBytes,
    // this will set node format to format to MPV_FORMAT_BYTE_ARRAY;
    //   bOwn: directly use pBytes(alloc by GetMem()) instead of
    //   creating new buffer, pBytes must not be freed outside again
    procedure SetBytes(pBytes: Pointer; nSize: size_t; bOwn: Boolean = False);

  public
    property Fmt: mpv_format read m_nFmt;

    property AsString: string read GetAsStr write SetAsStr;
    property AsBool: Boolean read GetAsBool write SetAsBool;
    property AsInteger: MPVInt64 read GetAsInt64 write SetAsInt64;
    property AsInt64: MPVInt64 read GetAsInt64 write SetAsInt64;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property BytesPtr: Pointer read m_pBytes;
    property ByteLength: size_t read m_nBytesLen;
    property Nodes: TMPVNodeList read GetAsNodes write SetAsNodes;

    property Value: Variant read GetAsVariant;
  end;

  // Helper function to get a property value from data pointer such as
  // mpv_event_property.data
  function GetMPVPropertyValue(nFmt: mpv_format; pData: Pointer): Variant;

implementation

function GetMPVPropertyValue(nFmt: mpv_format; pData: Pointer): Variant;
var
  cNode: TMPVNode;
begin
  case nFmt of
  MPV_FORMAT_INT64: Result := PMPVInt64(pData)^;
  MPV_FORMAT_DOUBLE: Result := PDouble(pData)^;
  MPV_FORMAT_FLAG: Result := PMPVFlag(pData)^;
  MPV_FORMAT_STRING, MPV_FORMAT_OSD_STRING: Result := UTF8ToString(PPMPVChar(pData)^);
  MPV_FORMAT_NODE, MPV_FORMAT_NODE_ARRAY, MPV_FORMAT_NODE_MAP, MPV_FORMAT_BYTE_ARRAY:
    begin
      cNode := TMPVNode.Create;
      cNode.LoadFromMPVNode(pData, False);
      Result := cNode.GetAsVariant;
      cNode.Free;
    end;
  end;
end;

{ TMPVNodeList }

function TMPVNodeList.AddNode(const sKey: string; cNode: TMPVNode): Integer;
begin
  Result := AddObject(sKey, cNode);
end;

procedure TMPVNodeList.Assign(Source: TPersistent);
var
  i: Integer;
  s: string;
  cNode, cNewNode: TMPVNode;
  cNL: TMPVNodeList;
begin
  if Source is TMPVNodeList then
  begin
    cNL := TMPVNodeList(Source);
    if cNL<>nil then
    begin
      for i := 0 to cNL.Count-1 do
      begin
        s := cNL[i];
        cNode := TMPVNode(cNL.Objects[i]);
        cNewNode := TMPVNode.Create;
        if AddObject(s, cNewNode)>=0 then
        begin
          cNewNode.Assign(cNode);
        end else
          cNewNode.Free;
      end;
    end else
      Clear;
  end else
    inherited Assign(Source);
end;

procedure TMPVNodeList.Clear;
var
  cNode: TMPVNode;
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    cNode := TMPVNode(Objects[i]);
    if cNode<>nil then
    begin
      cNode.Free;
      Objects[i] := nil;
    end;
  end;

  inherited;
end;

function TMPVNodeList.CopyNode(const sKey: string; cNode: TMPVNode): Integer;
var
  cNewNode: TMPVNode;
begin
  cNewNode := TMPVNode.Create;
  cNewNode.Assign(cNode);
  Result := AddObject(sKey, cNewNode);
  if Result<0 then cNewNode.Free;
end;

constructor TMPVNodeList.Create;
begin
  inherited Create; // Do not sort
end;

destructor TMPVNodeList.Destroy;
begin
  Clear;
  inherited;
end;

function TMPVNodeList.FindNode(const sKey: string): TMPVNode;
var
  n: Integer;
begin
  n := IndexOf(sKey);
  if n>=0 then Result := TMPVNode(Objects[n]) else
    Result := nil;
end;

function TMPVNodeList.GetNode(i: Integer): TMPVNode;
begin
  Result := TMPVNode(Objects[i]);
end;


{ TMPVNode }

function TMPVNode.AddNode(const sKey: string; cNode: TMPVNode): Integer;
begin
  case m_nFmt of
  MPV_FORMAT_NODE_ARRAY, MPV_FORMAT_NODE_MAP: ;
  else
    begin
      if sKey='' then m_nFmt := MPV_FORMAT_NODE_ARRAY else
        m_nFmt := MPV_FORMAT_NODE_MAP;
    end;
  end;
  if m_cNodes=nil then m_cNodes := TMPVNodeList.Create;
  Result := m_cNodes.AddNode(sKey, cNode);
end;

procedure TMPVNode.Assign(cNode: TMPVNode);
begin
  Clear;
  m_nFmt := cNode.Fmt;
  m_sVal := cNode.m_sVal;
  m_fVal := cNode.m_fVal;
  m_nVal := cNode.m_nVal;
  m_nVal64 := cNode.m_nVal64;
  if cNode.m_cNodes<>nil then
  begin
    m_cNodes := TMPVNodeList.Create;
    m_cNodes.Assign(cNode.m_cNodes);
  end;

  m_nBytesLen := cNode.m_nBytesLen;
  if (cNode.m_pBytes<>nil) and (m_nBytesLen>0) then
  begin
    GetMem(m_pBytes, m_nBytesLen);
    if m_pBytes<>nil then
      Move(cNode.m_pBytes^, m_pBytes^, m_nBytesLen);
  end;
end;

procedure TMPVNode.Clear;
begin
  m_nFmt := MPV_FORMAT_NONE;
  m_sVal := '';
  m_fVal := 0;
  m_nVal := 0;
  m_nVal64 := 0;
  m_nBytesLen := 0;
  if m_cNodes<>nil then FreeAndNil(m_cNodes);
  if m_pBytes<>nil then
  begin
    FreeMem(m_pBytes);
    m_pBytes := nil;
  end;
end;

function TMPVNode.CopyNode(const sKey: string; cNode: TMPVNode): Integer;
begin
  case m_nFmt of
  MPV_FORMAT_NODE_ARRAY, MPV_FORMAT_NODE_MAP: ;
  else
    begin
      if sKey='' then m_nFmt := MPV_FORMAT_NODE_ARRAY else
        m_nFmt := MPV_FORMAT_NODE_MAP;
    end;
  end;
  if m_cNodes=nil then m_cNodes := TMPVNodeList.Create;
  Result := m_cNodes.CopyNode(sKey, cNode);
end;

constructor TMPVNode.Create;
begin
  m_nFmt := MPV_FORMAT_NONE;
  inherited Create;
end;

function TMPVNode.LoadFromMPVNode(pNode: P_mpv_node; bFreeNode: Boolean): Boolean;
var
  i: Integer;
  pn: P_mpv_node;
  ppc: PPMPVChar;
  sKey: string;
  cNewNode: TMPVNode;
begin
  Result := False;
  if pNode<>nil then
  begin
    Clear;
    m_nFmt := pNode^.format;
    case m_nFmt of
    MPV_FORMAT_NONE: ;
    MPV_FORMAT_STRING, MPV_FORMAT_OSD_STRING:
      begin
        m_sVal := UTF8ToString(PMPVChar(pNode^.u.str));
      end;
    MPV_FORMAT_FLAG:
      begin
        m_nVal := pNode^.u.flag;
      end;
    MPV_FORMAT_INT64:
      begin
        m_nVal64 := pNode^.u.int64_;
      end;
    MPV_FORMAT_DOUBLE:
      begin
        m_fVal := pNode^.u.double_;
      end;
    MPV_FORMAT_NODE_ARRAY, MPV_FORMAT_NODE_MAP:
      begin
        m_cNodes := TMPVNodeList.Create;
        ppc := pNode^.u.list^.keys; // might be NULL
        pn := pNode^.u.list^.values;
        if pn<>nil then
        for i := 1 to pNode^.u.list^.num do
        begin
          if pn<>nil then
          begin
            cNewNode := TMPVNode.Create;
            cNewNode.LoadFromMPVNode(pn, False); // do not free content
            if ppc<>nil then sKey := UTF8ToString(ppc^) else sKey := '';
            if m_cNodes.AddNode(sKey, cNewNode)<0 then cNewNode.Free;
          end;
          if ppc<>nil then Inc(ppc);
          pn := P_mpv_node(NativeInt(pn)+sizeof(mpv_node));
        end;
      end;
    MPV_FORMAT_BYTE_ARRAY:
      begin
        m_nBytesLen := pNode^.u.ba^.size;
        GetMem(m_pBytes, m_nBytesLen);
        Move(pNode^.u.ba^.data^, m_pBytes^, m_nBytesLen);
      end;
    end;

    if bFreeNode then
    begin
      mpv_free_node_contents(pNode);
    end;
    Result := True;
  end;
end;

destructor TMPVNode.Destroy;
begin
  Clear;
  inherited;
end;

class procedure TMPVNode.FreeNode(pNode: P_mpv_node; bClearOnly: Boolean);
var
  i: Integer;
  pn: P_mpv_node;
  ppc: PPMPVChar;
begin
  if pNode<>nil then
  begin
    case pNode^.format of
    MPV_FORMAT_STRING, MPV_FORMAT_OSD_STRING:
      begin
        if pNode^.u.str<>nil then FreeMem(pNode^.u.str);
      end;
    MPV_FORMAT_NODE_ARRAY, MPV_FORMAT_NODE_MAP:
      begin
        if pNode^.u.list<>nil then
        begin
          pn := pNode^.u.list^.values;
          ppc := pNode^.u.list^.keys;
          for i := 1 to pNode^.u.list^.num do
          begin
            if pn<>nil then FreeNode(pn, True); // free each node's data
            pn := P_mpv_node(NativeInt(pn)+sizeof(mpv_node)); // inc bytes
            if ppc<>nil then
            begin
              if ppc^<>nil then FreeMem(ppc^); // free each key
              Inc(ppc); // next char*
            end;
          end;
          FreeMem(pNode^.u.list^.values);
          FreeMem(pNode^.u.list^.keys);
          FreeMem(pNode^.u.list);
        end;
      end;
    MPV_FORMAT_BYTE_ARRAY:
      begin
        if pNode^.u.ba<>nil then
        begin
          if pNode^.u.ba^.data<>nil then
          begin
            FreeMem(pNode^.u.ba^.data);
          end;
          FreeMem(pNode^.u.ba);
        end;
      end;
    end;

    if bClearOnly then
      FillChar(pNode^, sizeof(mpv_node), 0)
    else
      FreeMem(pNode);
  end;
end;

function TMPVNode.GenerateMPVNode: P_mpv_node;
begin
  GetMem(Result, sizeof(mpv_node));
  if Result<>nil then GenerateMPVNode(Result);
end;

procedure TMPVNode.GenerateMPVNode(pNode: P_mpv_node);
var
  sUTF8: UTF8String;
  i, n, m: Integer;
  cNode: TMPVNode;
  pn: P_mpv_node;
  ppc: PPMPVChar;
  pc: PMPVChar;
begin
  // pNode must not be NULL
  FillChar(pNode^, sizeof(mpv_node), 0);
  pNode^.format := m_nFmt;

  case m_nFmt of
  MPV_FORMAT_STRING, MPV_FORMAT_OSD_STRING:
    begin
      sUTF8 := UTF8Encode(m_sVal);
      n := Length(sUTF8)+1; // add #0
      GetMem(pNode^.u.str, n);
      if pNode^.u.str<>nil then
      begin
        if n=1 then
          pNode^.u.str^ := #0
        else
          Move(PAnsiChar(@sUTF8[1])^, pNode^.u.str^, n);
      end;
    end;
  MPV_FORMAT_FLAG:
    begin
      pNode^.u.flag := m_nVal;
    end;
  MPV_FORMAT_INT64:
    begin
      pNode^.u.int64_ := m_nVal64;
    end;
  MPV_FORMAT_DOUBLE:
    begin
      pNode^.u.double_ := m_fVal;
    end;
  MPV_FORMAT_NODE_ARRAY, MPV_FORMAT_NODE_MAP:
    if m_cNodes<>nil then
    begin
      GetMem(pNode^.u.list, sizeof(mpv_node_list));
      if pNode^.u.list<>nil then
      begin
        FillChar(pNode^.u.list^, sizeof(mpv_node_list), 0);
        n := m_cNodes.Count;
        if n>0 then
        begin
          pNode^.u.list^.num := n;
          GetMem(pNode^.u.list^.values, n*sizeof(mpv_node)); // all nodes
          GetMem(pNode^.u.list^.keys, n*sizeof(PMPVChar)); // all keys
          if (pNode^.u.list^.values<>nil) and (pNode^.u.list^.keys<>nil) then
          begin
            pn := pNode^.u.list^.values;
            FillChar(pn^, n*sizeof(mpv_node), 0);
            ppc := pNode^.u.list^.keys;
            FillChar(ppc^, n*sizeof(PMPVChar), 0);
            for i := 0 to n-1 do
            begin
              // node
              cNode := m_cNodes.GetNode(i);
              if cNode<>nil then
                cNode.GenerateMPVNode(pn);
              pn := P_mpv_node(NativeInt(pn)+sizeof(mpv_node)); // inc bytes

              // key
              sUTF8 := UTF8Encode(m_cNodes[i]);
              m := Length(sUTF8);
              GetMem(pc, m+1);
              if pc<>nil then
              begin
                if m>0 then
                  Move(PAnsiChar(@sUTF8[1])^, pc^, m+1)
                else
                  pc^ := #0;
                ppc^ := pc;
              end;
              Inc(ppc);
            end;
          end;
        end;
      end;
    end;
  MPV_FORMAT_BYTE_ARRAY:
    begin
      GetMem(pNode^.u.ba, sizeof(mpv_byte_array));
      if pNode^.u.ba<>nil then
      begin
        FillChar(pNode^.u.ba^, sizeof(mpv_byte_array), 0);
        pNode^.u.ba^.size := m_nBytesLen;
        GetMem(pNode^.u.ba^.data, m_nBytesLen+1);
        if (m_pBytes<>nil) and (pNode^.u.ba^.data<>nil) then
          Move(m_pBytes^, pNode^.u.ba^.data^, m_nBytesLen);
      end;
    end;
  end;
end;

function TMPVNode.GetAsBool: Boolean;
begin
  Result := m_nVal=0;
end;

function TMPVNode.GetAsDouble: Double;
begin
  Result := m_fVal;
end;

function TMPVNode.GetAsInt64: MPVInt64;
begin
  Result := m_nVal64;
end;

function TMPVNode.GetAsNodes: TMPVNodeList;
begin
  if m_cNodes=nil then
    m_cNodes := TMPVNodeList.Create;
  Result := m_cNodes;
end;

function TMPVNode.GetAsStr: string;
begin
  Result := m_sVal;
end;

function TMPVNode.GetAsVariant: Variant;
begin
  case m_nFmt of
  MPV_FORMAT_FLAG:
    begin
      Result := m_nVal;
    end;
  MPV_FORMAT_INT64:
    begin
      Result := m_nVal64;
    end;
  MPV_FORMAT_DOUBLE:
    begin
      Result := m_fVal;
    end;
  MPV_FORMAT_NODE, MPV_FORMAT_NODE_ARRAY, MPV_FORMAT_NODE_MAP:
    begin
      Result := Format('nodes[%d]', [m_cNodes.Count]); // not easy to represent
    end;
  MPV_FORMAT_BYTE_ARRAY:
    begin
      Result := Format('bytes[%d]', [m_nBytesLen]);
    end
  else
    begin
      Result := m_sVal;
    end;
  end;
end;

function TMPVNode.NewBytes(nSize: size_t): Pointer;
begin
  if nSize<=0 then
  begin
    Result := nil;
    Exit;
  end;
  m_nFmt := MPV_FORMAT_BYTE_ARRAY;
  if m_pBytes<>nil then
  begin
    if m_nBytesLen<nSize then
      FreeMem(m_pBytes)
    else
      begin
        FillChar(m_pBytes^, m_nBytesLen, 0);
        m_nBytesLen := nSize;
        Result := m_pBytes;
        Exit;
      end;
  end;
  GetMem(m_pBytes, nSize);
  Result := m_pBytes;
  m_nBytesLen := nSize;
  if m_pBytes<>nil then
    FillChar(m_pBytes^, m_nBytesLen, 0);
end;

procedure TMPVNode.SaveToFile(const sFileName: string);
var
  cList: TStringList;
begin
  cList := TStringList.Create;
  try
    SaveToStrings(cList);
    cList.SaveToFile(sFileName);
  finally
    cList.Free;
  end;
end;

procedure TMPVNode.SaveToStrings(cList: TStrings; const sPrefix: string);
var
  i: Integer;
  cNode: TMPVNode;
begin
  case m_nFmt of
  MPV_FORMAT_STRING, MPV_FORMAT_OSD_STRING:
    begin
      cList.Add(sPrefix+'str:'+m_sVal);
    end;
  MPV_FORMAT_FLAG:
    begin
      cList.Add(sPrefix+Format('flag:%d', [m_nVal]));
    end;
  MPV_FORMAT_INT64:
    begin
      cList.Add(sPrefix+Format('int64:%d', [m_nVal64]));
    end;
  MPV_FORMAT_DOUBLE:
    begin
      cList.Add(sPrefix+Format('double:%f', [m_fVal]));
    end;
  MPV_FORMAT_NODE_ARRAY, MPV_FORMAT_NODE_MAP:
    if m_cNodes<>nil then
    begin
      cList.Add(sPrefix+Format('nodes[%d]', [m_cNodes.Count]));
      for i := 0 to m_cNodes.Count-1 do
      begin
        cNode := m_cNodes.GetNode(i);
        cList.Add(sPrefix+Format('node[%d]:%s', [i, m_cNodes[i]]));
        cNode.SaveToStrings(cList, sPrefix+'    ');
      end;
    end;
  MPV_FORMAT_BYTE_ARRAY:
    begin
      cList.Add(sPrefix+Format('bytes[%d]', [m_nBytesLen]));
    end;
  end;
end;

procedure TMPVNode.SetAsBool(const Value: Boolean);
begin
  if Value then m_nVal := 1 else m_nVal := 0;
  m_nFmt := MPV_FORMAT_FLAG;
end;

procedure TMPVNode.SetAsDouble(const Value: Double);
begin
  m_fVal := Value;
  m_nFmt := MPV_FORMAT_DOUBLE;
end;

procedure TMPVNode.SetAsInt64(const Value: MPVInt64);
begin
  m_nVal64 := Value;
  m_nFmt := MPV_FORMAT_INT64;
end;

procedure TMPVNode.SetAsNodes(const Value: TMPVNodeList);
var
  s: string;
begin
  m_nFmt := MPV_FORMAT_NODE_ARRAY;
  for s in Value do
  begin
    if s<>'' then
      m_nFmt := MPV_FORMAT_NODE_MAP;
  end;
  if m_cNodes=nil then m_cNodes := TMPVNodeList.Create;
  m_cNodes.Assign(Value);
end;

procedure TMPVNode.SetAsStr(const Value: string);
begin
  m_sVal := Value;
  m_nFmt := MPV_FORMAT_STRING;
end;

procedure TMPVNode.SetBytes(pBytes: Pointer; nSize: size_t; bOwn: Boolean);
begin
  m_nFmt := MPV_FORMAT_BYTE_ARRAY;
  if bOwn then
  begin
    if m_pBytes<>nil then FreeMem(m_pBytes); // free old
    // take ownership
    m_pBytes := pBytes;
    m_nBytesLen := nSize;
  end else
  begin
    NewBytes(nSize);
    if (nSize>0) and (m_pBytes<>nil) and (m_pBytes<>pBytes) then
    begin
      Move(pBytes^, m_pBytes^, nSize);
    end;
  end;
end;

end.
