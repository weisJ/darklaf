/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
#include <windows.h>

class WindowWrapper {
    public:
    bool resizable = true;
    bool popup_menu = false;
    bool moving = false;
    bool move_mode = false;
    bool maximized = false;
    bool skipEraseBg = true;
    bool left_pressed = false;

    // The original window procedure.
    WNDPROC prev_proc;

    // The background brush.
    HBRUSH bgBrush;

    HWND window;
    int width;
    int height;
    int button_width;

    // The window region.
    RECT rgn;

    // The insets for the title bar area that is draggable.
    int left = 0;
    int right = 0;
    int title_height = 0;

    static LRESULT CALLBACK WindowProc(_In_ HWND hwnd, _In_ UINT uMsg, _In_ WPARAM wParam, _In_ LPARAM lParam);static LRESULT WindowProc_Windows11(WindowWrapper*, _In_ UINT uMsg, _In_ WPARAM wParam, _In_ LPARAM lParam);
};
