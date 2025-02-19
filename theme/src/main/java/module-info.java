/*
 * MIT License
 *
 * Copyright (c) 2021-2025 Jannis Weis
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
open module darklaf.theme {
    requires transitive java.desktop;

    requires transitive darklaf.theme.spec;
    requires darklaf.utils;
    requires darklaf.properties;

    requires static darklaf.annotations;
    requires static com.google.auto.service;

    exports com.github.weisj.darklaf.theme;
    exports com.github.weisj.darklaf.theme.info;
    exports com.github.weisj.darklaf.theme.event;
    exports com.github.weisj.darklaf.theme.laf;

    uses com.github.weisj.darklaf.theme.laf.SynthesisedThemedLaf.ThemedLafProvider;

    provides com.github.weisj.darklaf.theme.Theme with
            com.github.weisj.darklaf.theme.IntelliJTheme,
            com.github.weisj.darklaf.theme.DarculaTheme,
            com.github.weisj.darklaf.theme.OneDarkTheme,
            com.github.weisj.darklaf.theme.HighContrastLightTheme,
            com.github.weisj.darklaf.theme.HighContrastDarkTheme,
            com.github.weisj.darklaf.theme.SolarizedLightTheme,
            com.github.weisj.darklaf.theme.SolarizedDarkTheme;
}
