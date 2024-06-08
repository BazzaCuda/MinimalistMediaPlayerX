unit mmpKeyboardUtils;

interface

uses
  winApi.windows;

function mmpKBState: TKeyboardState;
function mmpToggleNumlock: boolean;

implementation

function mmpKBState: TKeyboardState;
begin
  getKeyboardState(result);
end;

function mmpToggleNumlock: boolean;
begin
  var kbState := mmpKBState;
  keybd_event(VK_NUMLOCK, kbState[VK_NUMLOCK] xor 1, KEYEVENTF_EXTENDEDKEY or 0, 0);
  keybd_event(VK_NUMLOCK, kbState[VK_NUMLOCK] xor 1, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
end;

end.
