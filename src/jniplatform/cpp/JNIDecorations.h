#include <stdio.h>
#include <windows.h>
#include <windowsx.h>

class WindowWrapper
{
public:
    bool resizable = true;
    WNDPROC prev_proc;
    COLORREF background = RGB(255, 255, 255);

    int left = 0;
    int right = 0;
    int height = 0;

    static LRESULT CALLBACK WindowProc(_In_ HWND hwnd, _In_ UINT uMsg, _In_ WPARAM wParam, _In_ LPARAM lParam);
};